// SSAConversion.hpp
// Copyright (c) Lup Gratian
//
// Implements the 'SSAConversion' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "SSAConversion.hpp"

namespace Optimization {

void SSAConversion::Execute(Function* function) {
    DebugValidator::IsNotNull(function);
    
    funct_ = function;
    domTree_ = new IRDominatorTree(function);
    domTree_->Build();
    domFrontier_ = new IRDominanceFrontier(domTree_);
    domFrontier_->Build();
    varAnalysis_.ComputeAddressTakenAndDefinitions(function);
    IRGenerator irGen(function->ParentUnit());
    folder_ = ConstantFolder(&irGen, GetTarget(), true /* opInfoDisabled */);

    // We we optimize, compute the live variables so that we insert
    // far fewer 'phi' instructions than for "minimal" SSA
    // (this computes the "pruned" SSA variant).
    if(ShouldComputeLiveness()) {
        varAnalysis_.ComputeLiveSets();
    }

    // In order to improve alias analysis, mark the variables 
    // with array/record type as not address-taken if they are
    // used only by instructions that compute an offset.
    varAnalysis_.AnnotateAggregateAddressTaken(function);
    
#if 0
    varAnalysis_.Dump();
#endif

    // Initialize the renaming stacks.
    int varCount = function->VariableCount() + function->ParameterCount();

    for(int i = 0; i < varCount; i++) {
        renameStacks_.Add(RenameStack());
    }

    // Now do the SSA conversion. We must insert 'phi' 
    // instructions for both local variables and parameters.
    InsertPhiInstructions(function->Variables());
    InsertPhiInstructions(function->Parameters());
    RenameVariables(function->FirstBlock());
    RemoveDeadVariables();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::InsertPhiInstructions(const VariableList& variables) {
    // We first insert the 'phi' instructions without having 
    // any incoming value; later, during renaming, the incoming 
    // values will be set. The variables are processed one at a time.
    for(int i = 0; i < variables.Count(); i++) {
        SparseBitVector blockProcessed;
        SparseBitVector phiPlaced;

        // The variable must not have it's address taken, and it must
        // not be of array or record type.
        auto variable = variables[i];

        if(IsCandidate(variable) == false) {
            continue;
        }

        // Initialize the worklist with the blocks where the variable
        // is assigned to.
        BlockWorklist worklist;
        Block* block = nullptr;

        while(auto defBlock = varAnalysis_.GetFirstDefinitionBlock(variable, block)) {
            worklist.Add(defBlock);
            blockProcessed.SetBit(defBlock->Id());
            block = defBlock;
        }

        // Place 'phi' instructions on the iterated dominance frontier.
        while(worklist.IsNotEmpty()) {
            block = worklist.RemoveLast();

            // Place a 'phi' in each block that is on the dominance
            // frontier of this block.
            auto frontier = domFrontier_->GetFrontier(block);
            
            if(frontier == nullptr) {
                continue;
            }

            for(int j = 0; j < frontier->Count(); j++) {
                auto frontierBlock = const_cast<Block*>((*frontier)[j]);

                // Place a 'phi' only if it's used and if it
                // hasn't been placed already.
                if((phiPlaced.IsSet(frontierBlock->Id()) == false) && 
                    ShouldInsertPhi(variable, frontierBlock)) {

                    // Create the 'phi' and mark that we placed it.
                    auto tempOp = Temporary::GetTemporary(variable->GetType());
				    int predCount = frontierBlock->PredecessorCount();
                    auto phiInstr = PhiInstr::GetPhi(tempOp, predCount);
				
                    frontierBlock->InsertInstructionFirst(phiInstr);
                    phiToVar_.Add(phiInstr, variable);
                    phiPlaced.SetBit(frontierBlock->Id());

                    // Add the block on the worklist, because
                    // we need to follow the iterated dominance frontier.
                    if(blockProcessed.IsSet(frontierBlock->Id()) == false) {
                        worklist.Add(frontierBlock);
                        blockProcessed.SetBit(frontierBlock->Id());
                    }
                }           
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::RenameVariables(Block* block) {
    SparseBitVector assignedVars;
    auto instr = block->FirstInstruction();
    ConditionalInfo condInfo;
    Variable* switchVar = nullptr;

    // Check for opportunities to propagate what we know about 
    // conditionals to the children in the dominator tree. For example,
    // if(a == 3) { b = a + 5; } -> { b = 3 + 5; }
    CheckConditionalPropagation(block, condInfo);

    while(instr) {
        auto nextInstr = GetNextLoadStorePhi(instr);

        // 'phi' instructions are processed after all other instruction
        // in the block, but they need to be pushed on the stack now.
        if(auto phiInstr = instr->As<PhiInstr>()) {
            // We make this check because we may run the conversion pass
            // a second time (after SRA, for example), but the 'phi'
            // was created in a previous run.
            if(phiToVar_.ContainsKey(phiInstr) == false) {
                instr = nextInstr;
                continue;
            }

            auto phiVar = phiToVar_[phiInstr];
            bool simplified = false;

            // Try to simplify the 'phi' (for example, all incoming operands
            // might be the same constant, or 'undef'). 
            // Note that we can do this only if we have all the incoming values
            // (which is not the case of a loop, for example, but this is handled
            //  in 'InsertPhiOperands').
            if(phiInstr->OperandCount() == block->PredecessorCount()) {
                if(auto result = SimplifyPhi(phiInstr)) {
                    // Replace the 'phi' with the new value, then remove it.
                    removedInstructions_.Add(phiInstr->ResultOp());
                    PushOnStack(phiVar, result, assignedVars);
                    block->RemoveInstruction(phiInstr, true);
                    simplified = true;
                }
            }

            if(simplified == false) {
                // Push the 'phi' the usual way if not simplified.
                PushOnStack(phiVar, phiInstr->ResultOp(), assignedVars);
            }
        }
        else if(auto loadInstr = instr->As<LoadInstr>()) {
            // We're interested only in references to local variables.
            // The 'load' can be replaced with the latest value from the stack.
            if(IsCandidate(loadInstr->SourceOp()) == false) {
                instr = nextInstr;
                continue;
            }

            // Propagate the latest value to all users of the 'load',
            // then delete it, because it's no longer needed.
            auto variableRef = loadInstr->SourceOp()->As<VariableReference>();
            auto activeOp = GetActiveOperand(variableRef->GetVariable());

            ReplaceResult(loadInstr->ResultOp(), activeOp);
            block->RemoveInstruction(loadInstr, true /* free */);
        }
        else if(auto storeInstr = instr->As<StoreInstr>()) {
            // We're interested only in references to local variables.
            if(IsCandidate(storeInstr->DestinationOp()) == false) {
                instr = nextInstr;
                continue;
            }

            // Push the stored value on the stack, then delete the 'store'.
            // To save memory we push the value only the first time,
            // and for subsequent values we replace the last value with the new one.
            auto variableRef = storeInstr->DestinationOp()->As<VariableReference>();
            PushOnStack(variableRef->GetVariable(), storeInstr->SourceOp(), assignedVars);
            block->RemoveInstruction(storeInstr, true /* free */);
        }

        instr = nextInstr;
    }

    // Set the values for the 'phi' instructions in the successor blocks.
    InsertPhiOperands(block, assignedVars);

    // Rename the variables in all dominated blocks.
    auto& children = domTree_->GetChildrenList(block);
    int oldReplacedPhiCount = replacedPhis_.Count();

    for(int i = 0; i < children.Count(); i++) {
        SparseBitVector condPropVars;
        auto child = children[i]->Block();
        
        // Try to propagate the value of the conditional.
        if(condInfo.Type != Condition_None) {
            PerformConditionalPropagation(block, children[i]->Block(),
                                          condInfo, condPropVars);
        }

        RenameVariables(children[i]->Block());
        PopFromStack(condPropVars); // Pop any conditional variable from the stack.

        if(replacedPhis_.Count() != oldReplacedPhiCount) {
            for(int i = oldReplacedPhiCount; i < replacedPhis_.Count(); i++) {
                if(replacedPhis_[i].Parent == block) {
                    // All children should use the new operand
                    // instead of the 'phi' result.
                    PushOnStack(replacedPhis_[i].PhiVariable,
                                replacedPhis_[i].Replacement, assignedVars);
                }
            }
            oldReplacedPhiCount = replacedPhis_.Count();
        }
    }

    // Pop from the stack all variables that where added for this block.
    PopFromStack(assignedVars);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::CheckConditionalPropagation(Block* block, ConditionalInfo& info){
    // Consider the following cases, which can be improved by propagating
    // what is known about the value tested in the comparison:
    // if(a == 3) { b = a + 5; } -> { b = 3 + 5; }
    // if(a != 3) { ... } else { b = a + 4; } -> { b = 3 + 4; }
    info.Type = Condition_None;

    // If the block doesn't end with an 'if' we have nothing to propagate.
    // 'switch' is handled in a separate method.
    if(block->BranchInstruction()->IsIf() == false) {
        return;
    }

    // Check for a variable that is tested directly, like in 'if(a) {...}'.
    auto conditionOp = block->BranchInstruction()->GetSourceOp(0);

    if(auto loadInstr = conditionOp->DefiningInstrAs<LoadInstr>()) {
        info.Order = Order_NotEqual; // For the 'true' branch.
        info.LeftComparisonVariable = GetLoadedLocal(loadInstr);
        info.Type = info.LeftComparisonVariable ? Condition_Single : Condition_None;
        return;
    }
    
    // The conditional must be a comparison instruction that operates on
    // values loaded from variables, optionally with an addition/subtraction
    // on one of the sides. Note that we consider the case where both operands
    // are loaded variables because at the time the SSA renaming reaches the
    // block end, one of the variables may be a constant.
    // if(a + 3 == 5) -> a = 5 - 3
    // if(a - 3 == 5) -> a = 5 + 3
    auto cmpInstr = conditionOp->DefiningInstrAs<CmpInstrBase>();
    if((cmpInstr == nullptr) || cmpInstr->IsFcmp()) {
        return;
    }

    // We handle only 'equal' and 'not equal' orders.
    if(cmpInstr->IsEquality() == false) {
        return;
    }

    info.Order = cmpInstr->Order();

    // Move the constant to the right, so the we have fewer cases to test.
    if(cmpInstr->LeftOp()->IsConstant()) {
        cmpInstr->InvertOrder(true, false /* invertEquality */);
    }

    // If we have an 'add' or 'sub' as the right operand, move it to the left.
    if(cmpInstr->RightOp()->DefiningInstrIs<AddInstr>() ||
       cmpInstr->RightOp()->DefiningInstrIs<SubInstr>()) {
        cmpInstr->InvertOrder(true, false /* invertEquality */);
    }

    // Determine the type of the expression involved in the comparison.
    info.Type = Condition_None;

    if(auto loadInstr = cmpInstr->LeftOp()->DefiningInstrAs<LoadInstr>()) {
        // This is the most common case.
        info.LeftComparisonVariable = GetLoadedLocal(loadInstr);
        info.Type = info.LeftComparisonVariable ? Condition_Simple : Condition_None;
    }
    else if(info.LeftComparisonVariable = GetAddLoadedLocalConst(cmpInstr->LeftOp(), 
                                                                 info.AddSubConstantOperand)) {
        info.Type = Condition_Add;
    }
    else if(info.LeftComparisonVariable = GetSubLoadedLocalConst(cmpInstr->LeftOp(), 
                                                                 info.AddSubConstantOperand)) {
        info.Type = Condition_Sub;
    }
    else return; // Other cases not supported.

    // Now test the right operand.
    if(info.ConstantOperand = cmpInstr->RightOp()->As<Constant>()) {
        // The type of the result doesn't change.
    }
    else if(auto loadInstr = cmpInstr->RightOp()->DefiningInstrAs<LoadInstr>()) {
        // The type of the result doesn't change.
        info.RightComparisonVariable = GetLoadedLocal(loadInstr);
        info.Type = info.RightComparisonVariable ? info.Type : Condition_None;
    }
    else {
        // Any other case not handled.
        info.Type = Condition_None;
    }

    // The involved variables should not have their address taken.
    if((info.LeftComparisonVariable && info.LeftComparisonVariable->IsAddressTaken()) ||
       (info.RightComparisonVariable && info.RightComparisonVariable->IsAddressTaken())) {
        info.Type = Condition_None;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::PerformConditionalPropagation(Block* block, Block* childBlock,
                                                  ConditionalInfo& info,
                                                  SparseBitVector& assignedVars) {
    // Try to determine a constant that can be propagated 
    // to the true or to the false branch.
    auto unit = block->ParentFunction()->ParentUnit();
    Variable* variable = nullptr;
    Operand* constantOp = nullptr;
    auto ifInstr = block->BranchInstruction()->As<IfInstr>();
    DebugValidator::IsNotNull(ifInstr);

    // The child block should be the true or the false branch.
    // This prevents doing useless computations below.
    if((childBlock == ifInstr->TrueTargetOp()->Target() ||
        childBlock == ifInstr->FalseTargetOp()->Target()) == false) {
        return;
    }

    if(info.Type == Condition_Single) {
        variable = info.LeftComparisonVariable;
        constantOp = unit->Constants().GetInt(variable->GetType(), 0);
    }
    else if(info.Type == Condition_Simple) {
        // We have a variable and a constant, or two variables - in which case
        // one of them should be a constant by now.
        // if(a == 3) / if(a == b)
        if(info.ConstantOperand) {
            variable = info.LeftComparisonVariable;
            constantOp = info.ConstantOperand;
        }
        else {
            auto rightVarOp = GetActiveOperand(info.RightComparisonVariable);

            if(rightVarOp->IsConstant()) {
                variable = info.LeftComparisonVariable;
                constantOp = rightVarOp;
            }
        }
    }
    else if(info.Type == Condition_Add) {
        // a + C1 == C2 / a + C == b
        // The 'add' is guaranteed to be on the left side.
        variable = info.LeftComparisonVariable;
        auto rightConstOp = info.ConstantOperand ? info.ConstantOperand->As<IntConstant>() : nullptr;

        if(rightConstOp == nullptr) {
            DebugValidator::IsNotNull(info.RightComparisonVariable);
            rightConstOp = GetActiveOperand(info.RightComparisonVariable)->As<IntConstant>();
        }

        if(rightConstOp) {
            // Compute the difference of the constants.
            // a + C1 == C2 -> a = C2 - C1
            auto addConstOp = info.AddSubConstantOperand->As<IntConstant>();
            __int64 diff = IA::Sub(rightConstOp, addConstOp);
            constantOp = unit->Constants().GetInt(addConstOp->GetType(), diff);
        }
    }
    else {
        // a - C1 == C2 / a - C == b
        // The 'sub' is guaranteed to be on the left side.
        variable = info.LeftComparisonVariable;
        auto rightConstOp = info.ConstantOperand ? info.ConstantOperand->As<IntConstant>() : nullptr;

        if(rightConstOp == nullptr) {
            DebugValidator::IsNotNull(info.RightComparisonVariable);
            rightConstOp = GetActiveOperand(info.RightComparisonVariable)->As<IntConstant>();
        }

        if(rightConstOp) {
            // Compute the sum of the constants.
            // a - C1 == C2 -> a = C2 + C1
            auto subConstOp = info.AddSubConstantOperand->As<IntConstant>();
            __int64 sum = IA::Add(rightConstOp, subConstOp);
            constantOp = unit->Constants().GetInt(subConstOp->GetType(), sum);
        }
    }

    // Propagate the constant to the right branch.
    // We need to make sure that control can flow into 'childBlock' only through
    // this edge, else the propagation might not be valid. An exception to this are
    // back-edges, because control flows from inside the child (or a dominated block).
    if(variable && constantOp) {
        if(childBlock == ifInstr->TrueTargetOp()->Target()) {
            if((info.Order == Order_Equal) && 
                BlockUtils::DominatesItself(childBlock, block, domTree_)) {
                PushOnStack(variable, constantOp, assignedVars);
            }
        }
        else if((info.Order == Order_NotEqual) && 
                 BlockUtils::DominatesItself(childBlock, block, domTree_)) {
            PushOnStack(variable, constantOp, assignedVars);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable* SSAConversion::GetLoadedLocal(LoadInstr* instr) {
    if(auto variableRef = instr->SourceOp()->As<VariableReference>()) {
        if(variableRef->IsGlobalVariableRef()) {
            return nullptr;
        }

        auto variable = variableRef->GetVariable();

        if(variable->IsAddressTaken() == false) {
            return variable;
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable* SSAConversion::GetAddLoadedLocalConst(Operand* op, Operand*& constantOp) {
    auto addInstr = op->DefiningInstrAs<AddInstr>();
    if(addInstr == nullptr) {
        return nullptr;
    }

    // We should have a loaded variable and a constant.
    // It doesn't matter on which side the constant is placed,
    // because addition is commutative.
    if(constantOp = addInstr->LeftOp()->As<Constant>()) {
        if(auto loadInstr = addInstr->RightOp()->DefiningInstrAs<LoadInstr>()) {
            if(auto loadedVar = GetLoadedLocal(loadInstr)) {
                return loadedVar;
            }
        }
    }
    else if(constantOp = addInstr->RightOp()->As<Constant>()) {
        if(auto loadInstr = addInstr->LeftOp()->DefiningInstrAs<LoadInstr>()) {
            if(auto loadedVar = GetLoadedLocal(loadInstr)) {
                return loadedVar;
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable* SSAConversion::GetSubLoadedLocalConst(Operand* op, Operand*& constantOp) {
    auto subInstr = op->DefiningInstrAs<SubInstr>();
    if(subInstr == nullptr) {
        return nullptr;
    }

    // The right operand should be a constant.
    if(constantOp = subInstr->RightOp()->As<Constant>()) {
        if(auto loadInstr = subInstr->LeftOp()->DefiningInstrAs<LoadInstr>()) {
            if(auto loadedVar = GetLoadedLocal(loadInstr)) {
                return loadedVar;
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::InsertPhiOperands(Block* block, SparseBitVector& assignedVars) {
    StaticList<PhiInstr*, 4> candidates;
    auto successorEnum = block->GetSuccessorEnum();

    while(successorEnum.IsValid()) {
        auto successorBlock = successorEnum.Next();
        auto instr = successorBlock->FirstInstruction();

        while(auto phiInstr = instr->As<PhiInstr>()) {
            auto nextInstr = phiInstr->NextInstruction();

            // We make this check because we may run the conversion pass
            // a second time (after SRA, for example), but the 'phi'
            // was created in a previous run.
            if(phiToVar_.ContainsKey(phiInstr) == false) {
                instr = nextInstr;
                continue;
            }
            
            // Add the incoming value.
            auto phiVar = phiToVar_[phiInstr];
            auto blockRef = funct_->ParentUnit()->References().GetBlockRef(block);
            phiInstr->AddOperand(GetActiveOperand(phiVar), blockRef);

            // Try to simplify the 'phi' (for example, all incoming operands
            // might be the same constant, or 'undef'). 
            // Here we handle the case of a loop, where we don't heave all
            // the incoming values when we process the 'phi' in the loop header.
            if(phiInstr->OperandCount() != 
               successorBlock->PredecessorCount()) {
                instr = nextInstr;
                continue;
            }

            // Test for a back-edge; this guarantees that the 'phi' has been
            // processed and it's result temporary has users.
            if(ShouldSimplify() && domTree_->Dominates(successorBlock, block)) {
                candidates.Add(phiInstr);
            }

            instr = nextInstr;
        }
    }

    // Try to simplify the 'phi's.
    if(candidates.Count() > 0) {
        for(int i = 0; i < candidates.Count(); i++) {
            auto phiInstr = candidates[i];

            if(auto result = SimplifyPhi(phiInstr)) {
                // Replace the 'phi' with the new value, and remove it.
                removedInstructions_.Add(phiInstr->ResultOp());
                ReplaceResult(phiInstr->ResultOp(), result);
             
                // From now on we use the result instead of the 'phi'.
                auto phiVar = phiToVar_[phiInstr];
                PushOnStack(phiVar, result, assignedVars);
                replacedPhis_.Add(ReplacedPhi(phiVar, phiInstr->ParentBlock(), result));
                phiInstr->RemoveFromBlock(true /* free */);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* SSAConversion::SimplifyPhi(PhiInstr* instr) {
    if(instr->IsConstant()) {
        // phi(5,5,5) -> 5
        // phi(undef, undef) -> undef
        return instr->GetConstant();
    }

    // Check for something like 'phi(undef, a, undef, a)'. We can use 'a' 
    // as the result if the definition block of 'a' dominates the 'phi'.
    bool hasUndef = false;
    Operand* sameOp = nullptr;

    for(int i = 0; i < instr->OperandCount(); i++) {
        if(instr->GetOperand(i)->IsUndefinedConstant()) {
            hasUndef = true;
        }
        else if(sameOp && (sameOp != instr->GetOperand(i))) {
            // All the operands that are not undefined should be the same.
            return nullptr;
        }
        else sameOp = instr->GetOperand(i);
    }

    // Check the domination relationship. A constant dominates any block,
    // and for all other cases we use the dominator tree.
    if(sameOp == nullptr) {
        return nullptr;
    }
    else if(sameOp->IsConstant()) {
        return sameOp;
    }
    
    auto sameOpBlock = sameOp->DefiningInstruction()->ParentBlock();    
    if(domTree_->Dominates(sameOpBlock, instr->ParentBlock())) {
        return sameOp;
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* SSAConversion::GetNextLoadStorePhi(Instruction* instr) {
    auto nextInstr = instr->NextInstruction();

    while(nextInstr) {
        if(nextInstr->IsLoad() || nextInstr->IsStore() || nextInstr->IsPhi()) {
            return nextInstr;
        }
        else nextInstr = nextInstr->NextInstruction();
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SSAConversion::IsCandidate(Variable* variable) {
    if(variable->IsAddressTaken()) {
        return false;
    }
    else if(variable->IsArray() || variable->IsRecord()) {
        return false;
    }
    else return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SSAConversion::IsCandidate(Operand* op) {
    auto variableRef = op->As<VariableReference>();
    if((variableRef == nullptr) || (variableRef->IsLocalVariableRef() == false)) {
       return false;
    }

    return variableRef->GetVariable()->IsAddressTaken() == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* SSAConversion::GetActiveOperand(Variable* variable) {
    // We have three cases:
    // 1. an operand is found on the stack -> return it
    // 2. no operand is found, the variable is a parameter -> create a 'Parameter'.
    // 2. no operand is found, the variable is local -> create an 'UndefinedConstant'.
    int count = renameStacks_[variable->Id()].Count();

    if(count > 0) {
        return renameStacks_[variable->Id()][count - 1];
    }
    else if(variable->IsParameter()) {
        return funct_->GetParameter(variable);
    }
    else return funct_->ParentUnit()->Constants().GetUndefined(variable->GetType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::IncrementReference(Operand* op) {
    if(auto reference = op->As<Reference>()) {
        reference->AddUser();
    }
    else if(auto parameter = op->As<Parameter>()) {
        parameter->AddUser();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::DecrementReference(Operand* op) {
    // It's possible that the operand doesn't exist anymore
    // if it was the result of a 'phi' that was simplified (i.e removed).
    if(removedInstructions_.Contains(op)) {
        return;
    }

    if(auto reference = op->As<Reference>()) {
        reference->Free();
    }
    else if(auto parameter = op->As<Parameter>()) {
        parameter->Free();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SSAConversion::ShouldInsertPhi(Variable* variable, Block* block) {
    // We insert a 'phi' if it's live on the entry of the block.
    // This creates "pruned" SSA, which has fewer useless 'phi' instructions 
    // than "minimal" SSA, at the expense of higher computation cost.
    if(ShouldComputeLiveness() == false) {
        return true;
    }
    else return varAnalysis_.IsLiveInBlock(variable, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::PushOnStack(Variable* variable, Operand* op, 
                                SparseBitVector& assignedVars) {
    // We push a new operand on the stack only for the first definition in a block.
    // For later definitions we replace the previous one; this saves memory,
    // and simplifies the removal of the operands later.
    // This technique is presented in the paper "Practical Improvements to the
    // Construction and Destruction of Static Single Assignment Form" by Briggs et al.
    int variableId = variable->Id();
    IncrementReference(op);

    if(variableId == 8) {
        variableId = variableId;
    }

    if(assignedVars.IsSet(variableId)) {
        auto& stack = renameStacks_[variableId];
        auto previous = stack[stack.Count() - 1];
        DecrementReference(previous);

        stack.RemoveAt(stack.Count() - 1);
        stack.Add(op);
    }
    else {
        // First time the variable is defined in the block.
        renameStacks_[variableId].Add(op);
        assignedVars.SetBit(variableId);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::PopFromStack(SparseBitVector& assignedVars) {
    int id = 0;

    while((id = assignedVars.FirstSetBit(id)) != -1) {
        PopFromStack(id);
        id++; // Search after this variable.
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::PopFromStack(int variableId) {
    int last = renameStacks_[variableId].Count() - 1;
    auto lastOp = renameStacks_[variableId][last];
    DecrementReference(lastOp);
    renameStacks_[variableId].RemoveAt(renameStacks_[variableId].Count() - 1);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::ReplaceResult(Temporary* resultOp, Operand* newOp) {
    InstructionWorklist worklist;
    bool simplify = ShouldSimplify();

    // Add the users of the result operand to the worklist, 
    // because they may simplify if the new operand is a constant 
    // or an undefined value. This also includes simplifications 
    // like 'a + 0 -> a' for most arithmetic and logical instructions.
    if(simplify) AddUsersToWorklist(resultOp, worklist);
    resultOp->ReplaceWith(newOp);

    if(simplify == false) {
        // We should not try to simplify (it may be a debug build).
        return;
    }
    
    while(worklist.IsNotEmpty()) {
        auto instr = worklist.RemoveLast();

        // We try to simplify the user only if it doesn't have
        // operands that are 'phi' instructions which are not
        // complete (not all incoming operands have been added,
        // happens in case of loops, for example), otherwise
        // the simplification might be wrong.
        if(HasIncompletePhiOperands(instr)) {
            continue;
        }

        // Try to fold the instruction to a constant. 
        // This happens frequently, so it's worth performing it here.
        auto result = folder_.Fold(instr);
        
        // Try to do some simplifications if it's not a constant.
        if(result == nullptr) {
            result = peephole_.SimplifyInstruction(instr);
        }
        
        if(result) {
            // Add all the users of the current instruction to the worklist,
            // they may simplify too; the instruction can now be removed.
            if(auto destOp = instr->GetDestinationOp()) {
                removedInstructions_.Add(destOp);
                AddUsersToWorklist(destOp, worklist);
                destOp->ReplaceWith(result);
            }

            InstructionSimplified(instr);
            instr->RemoveFromBlock(true /* free */);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SSAConversion::HasIncompletePhiOperands(Instruction* instr, bool phiAllowed) {
    DebugValidator::IsNotNull(instr);

    for(int i = 0; i < instr->SourceOpCount(); i++) {
        if(auto definingInstr = instr->GetSourceOp(i)->DefiningInstruction()) {
            if(auto phiInstr = definingInstr->As<PhiInstr>()) {
                if(phiAllowed == false) {
                    return true;
                }

                // The 'phi' is incomplete if the number of incoming
                // operands is not equal to the number of predecessors
                // of the parent block.
                if(phiInstr->OperandCount() !=
                   phiInstr->ParentBlock()->PredecessorCount()) {
                    return true;
                }

                // Now make sure that none of the incoming operands
                // is a 'phi' or an instruction that uses a 'phi',
                // else we could enter a cycle.
                for(int j = 0; j < phiInstr->OperandCount(); j++) {
                    auto incomingOp = phiInstr->GetOperand(j);

                    if(auto incomingDefInstr = incomingOp->DefiningInstruction()) {
                        if(HasIncompletePhiOperands(incomingDefInstr, false)) {
                            return true;
                        }
                    }
                }
            }
            else if(HasIncompletePhiOperands(definingInstr, phiAllowed)) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::AddUsersToWorklist(Temporary* temp, InstructionWorklist& worklist) {
    auto definingInstr = temp->DefiningInstruction();

    temp->ForEachUser([&worklist, definingInstr](Instruction* user, int index) -> bool {
        if(user != definingInstr) {
            worklist.Add(user);
        }

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::RemoveDeadVariables() {
    // Remove all the variables that where replaced by temporaries.
    auto& variables = funct_->Variables();

    // We remove from last to first, because this reduces the number
    // of elements in the list that need to be shifted.
    for(int i = variables.Count() - 1; i >= 0; i--) {
        if(IsCandidate(variables[i])) {
            variables.RemoveAt(i);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAConversion::InstructionSimplified(Instruction* instr) {
#if 1
	auto block = instr->ParentBlock();
	auto function = instr->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string text = IRPrinter(instr).ToString();
	Log::Warning("SSAConversion simplification in " + functionName + ":" + blockName +
				 ": " + text);
#endif
}

} // namespace Optimization