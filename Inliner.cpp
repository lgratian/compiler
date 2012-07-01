// Inliner.cpp
// Copyright (c) Lup Gratian
//
// Implements the Inliner class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Inliner.hpp"
#include "../IR/IRPrinter.hpp"
#include <fstream>

namespace Optimization {

void Inliner::InlineCall(CallInstr* instr) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(instr->GetCalledFunction());
    DebugValidator::AreEqual(instr->ArgumentCount(),
                             instr->CalledFunctionType()->ParameterCount());

    // Inline the called function, then delete the 'call'.
    auto callee = instr->GetCalledFunction();
    auto returnedOp = InlineFunction(callee, instr->Arguments(), instr);

    // If the 'call' has a result operand replace it
    // with the operand that merges the returned values.
    if(instr->HasDestinationOp()) {
        DebugValidator::IsNotNull(returnedOp);
        instr->GetDestinationOp()->ReplaceWith(returnedOp);
    }

    instr->RemoveFromBlock(true);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Inliner::InlineFunction(Function* source, CallInstr::ArgumentList* argList,
                                 Instruction* splitInstr) {
    DebugValidator::IsNotNull(source);
    DebugValidator::IsNotNull(splitInstr);

    IRGenerator irGen(source->ParentUnit());
    folder_ = ConstantFolder(&irGen, nullptr, false);

    // We split the destination block before 'splitInstr',
    // and between the two resulting blocks we inline 'source'.
    auto dest = splitInstr->ParentFunction();
    auto firstBlock = splitInstr->ParentBlock();
    auto& refs = dest->ParentUnit()->References();

    string secondBlockName = GetUniqueName("#split_block", &dest->Symbols());
    auto secondBlock = firstBlock->SplitAt(splitInstr, secondBlockName);

    // Any incoming operand for 'phi's in successor blocks 
    // must now be incoming from the second block.
    auto successorEnum = secondBlock->GetSuccessorEnum();

    while(successorEnum.IsValid()) {
        auto successorBlock = successorEnum.Next();
        BlockUtils::ReplacePhiOperandsBlock(successorBlock, 
                                            firstBlock, secondBlock);
    }

    // Inline the function, then connect its entry block
    // to 'firstBlock', and its exit block to 'block'.
    Block* entryBlock = nullptr;
    Block* exitBlock = nullptr;
    Operand* returnedOp = nullptr;

#if 1
    {
        IRPrinter p(source);
        std::wofstream f("d:\\test\\out_opt.irl");
        f<<p.ToString().Chars();
        f.close();
    }
#endif

    InlineFunction(source, dest, argList, firstBlock, 
                   entryBlock, exitBlock, returnedOp);      
    
    // 'firstBlock' already contains a 'goto', 
    // but 'exitBlock' does not.
    firstBlock->ReplaceSuccessor(0, entryBlock);
    GotoInstr::GetGoto(refs.GetBlockRef(secondBlock), exitBlock);
    return returnedOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Inliner::InlineFunction(Function* source, Function* dest,
                             CallInstr::ArgumentList* argList,
                             Block* afterBlock, Block*& entryBlock,
                             Block*& exitBlock, Operand*& returnedOp) {
    // To inline the function we follow these steps:
    // 1. Create a list of the 'ret' instructions
    // 2. Copy the variables to the caller
    // 3. Add the arguments corresponding to the parameters
    //    to the operand map.
    // 4. Clone the blocks into the caller
    // 5. Generate assignments from arguments for the parameters
    //    whose address are taken.
    // 6. Process the 'phi' instructions (we do it only now because
    //    we need to know exactly which blocks were copied and which now,
    //    and this is not always possible in (4) because of loops).
    // 7. Create a block where the returned values are merged
    //    (this will be the unique exit block)
    CollectReturnInstructions(source);
    CopyVariables(source, dest);
    AddParametersToMap(source, dest, argList);

    entryBlock = CopyBlock(source->FirstBlock(), dest, afterBlock)->Target();
    GenerateAddressTakenAssignments(argList, source, entryBlock);
    ProcessPhiInstructions(dest);

    PhiInstr* mergePhi = nullptr;
    exitBlock = MergeReturnedValues(dest, entryBlock, mergePhi);
    if(mergePhi) returnedOp = mergePhi->GetDestinationOp();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* Inliner::CopyBlock(Block* source, Function* dest, Block* afterBlock) {
    // In order to copy a block we do the following:
    // 1. Create a new block and insert it into the caller
    // 2. Process each instruction: first try to constant-fold/simplify it,
    //    if not possible the it is cloned and inserted in the new block.
    // 3. Process the branching instruction so that we select which
    //    of the successors to process (we try to constant-fold, so that
    //    we don't clone instructions that later will be definitely removed).
    // 4. Return a reference to the new block.
    Block* copy;

    // Check if the block was already copied.
    if(blockMap_.TryGetValue(source, &copy)) {
        return dest->ParentUnit()->References().GetBlockRef(copy);
    }

    copy = CreateBlock(source, dest, afterBlock);
    blockMap_.Add(source, copy);

    for(auto instr = source->FirstInstruction(); instr;
        instr = instr->NextInstruction()) {
        if(instr->IsBranching()) break;
        ProcessInstruction(instr, copy);
    }
    
    if(source->BranchInstruction()->IsReturn() == false) {
        ProcessBranching(source, copy, dest);        
    }

    // Create the reference for the new block.
    return dest->ParentUnit()->References().GetBlockRef(copy);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Inliner::ProcessBranching(Block* source, Block* copy, Function* dest) {
    bool hasSingleSuccessor = false;
    Block* singleSuccessor = nullptr;
    auto branchingInstr = source->BranchInstruction();

    // Check if we have a single successor, either because the block
    // ends with a 'goto', or because the branch can be folded.
    if(auto gotoInstr = branchingInstr->As<GotoInstr>()) {
        hasSingleSuccessor = true;
        singleSuccessor = gotoInstr->TargetOp()->Target();
    }
    else if(auto block = ConstantFoldBranching(branchingInstr)) {
        hasSingleSuccessor = true;
        singleSuccessor = block;
    }

    if(hasSingleSuccessor) {
        // This is the simplest case, we create a 'goto'
        // to the generated copy.
        auto succRef = CopyBlock(singleSuccessor, dest, copy);
        GotoInstr::GetGoto(succRef, copy);
        return;
    }

    // All successors need to be copied.
    StaticList<BlockReference*, 2> successors;
    auto lastBlock = copy;

    for(int i = 0; i < source->SuccessorCount(); i++) {
        auto successor = CopyBlock(source->SuccessorAt(i), 
                                   dest, lastBlock);
        successors.Add(successor);
        lastBlock = successor->Target();
    }

    // Create the appropriate branching instruction.
    if(auto ifInstr = branchingInstr->As<IfInstr>()) {
        DebugValidator::AreEqual(successors.Count(), 2);
        auto newConditionOp = GetReplacementOperand(ifInstr->ConditionOp());
        IfInstr::GetIf(newConditionOp, successors[0], successors[1], copy);
    }
    else if(auto switchInstr = branchingInstr->As<SwitchInstr>()) {
        DebugValidator::AreEqual(successors.Count(), 
                                 switchInstr->CaseCount() + 1);
        int succCount = successors.Count();
        auto newConditionOp = GetReplacementOperand(switchInstr->ConditionOp());
        auto copySwitchInstr = SwitchInstr::GetSwitch(newConditionOp, succCount, 
                                                      successors[succCount - 1], copy);
        // Initialize the 'case's.
        for(int i = 0; i < (succCount - 1); i++) {
            copySwitchInstr->AddCase(switchInstr->GetCase(i).Value,
                                     successors[i]);
        }
    }
    else DebugValidator::Unreachable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Inliner::ProcessInstruction(Instruction* instr, Block* dest) {
    // 'phi' instructions are just copied here, they will be
    // processed later, because we may have loops and because
    // we don't know yet which predecessors are copied too.
    if(auto phiInstr = instr->As<PhiInstr>()) {
        auto cloned = CreateAndInsertClone(instr, dest, false /* replaceOps */);
        phiInstrs_.Add(cloned->As<PhiInstr>());
        return;
    }

    // Try to constant-fold/simplify the instruction, it's likely
    // that parameters were replaced by constants.
    if(instr->HasDestinationOp()) {
        if(auto result = ConstantFoldAndSimplify(instr)) {
            // No instruction needs to be added, we just create
            // an entry in the operand map.
            operandMap_.Add(instr->GetDestinationOp(), result);
            return;
        }
    }

    // Clone the instruction, and if it has a destination operand add it
    // to the operand map, so that subsequent cloned instructions can use it.
    CreateAndInsertClone(instr, dest);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* Inliner::CreateAndInsertClone(Instruction* instr, Block* dest, 
                                           bool replaceOps) {
    // Clone the instruction and insert it at the end of the block.
    // If requested, we replace each operand with the one
    // that was cloned into the caller.
    auto cloned = instr->Clone();
    dest->InsertInstructionLast(cloned);

    if(replaceOps) {
        for(int i = 0; i < instr->SourceOpCount(); i++) {
            auto originalOp = instr->GetSourceOp(i);
            auto newOp = GetReplacementOperand(originalOp);
            cloned->ReplaceSourceOp(i, newOp);
        }
    }

    // The result operand, if any, is added to the operand map,
    // so that subsequent cloned instructions can use it.
    if(instr->HasDestinationOp()) {
        // Create a new temporary.
        auto destOp = instr->GetDestinationOp();
        auto temp = Temporary::GetTemporary(destOp->GetType());
        cloned->ReplaceDestinationOp(temp);
        operandMap_.Add(destOp, temp);
    }

    return cloned;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Inliner::GetReplacementOperand(Operand* op) {
    // References to global symbols (variables/functions)
    // and constants are valid for the whole unit.
    if(op->IsConstant() ||
       op->IsGlobalVariableRef() ||
       op->IsFunctionReference()) {
       return op;
    }
    else return operandMap_[op];
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Inliner::ConstantFoldAndSimplify(Instruction* instr) {
    DebugValidator::IsFalse(instr->IsPhi());
    DebugValidator::IsFalse(instr->IsBranching());

    // We try to constant-fold/simplify arithmetic, logical,
    // conversion, address and comparison instructions,
    // because the operands that previously were parameters
    // might now be constants.
    if(instr->IsArithmetic() || instr->IsLogical()) {
        auto opA = GetReplacementOperand(instr->GetSourceOp(0));
        auto opB = GetReplacementOperand(instr->GetSourceOp(1));

        if(auto result = folder_.FoldBinary(instr->GetOpcode(), opA, opB)) {
            return result;
        }
        
        // Try to simplify (handles things like 'add a, 0 -> a').
        // We handle only cases where we don't need to create new instructions.
        auto unit = instr->ParentFunction()->ParentUnit();
        return peephole_.SimplifyInstruction(instr->GetOpcode(), opA, opB,
                                             unit, nullptr, false /* createAllowed */);
    }
    else if(auto convInstr = instr->As<ConversionInstr>()) {
        auto op = GetReplacementOperand(convInstr->TargetOp());
        return folder_.FoldConversion(instr->GetOpcode(), op,
                                      convInstr->CastType());
    }
    else if(instr->IsAddress()) {
        auto opA = GetReplacementOperand(instr->GetSourceOp(0));
        auto opB = GetReplacementOperand(instr->GetSourceOp(1));
        return folder_.FoldAddress(opA, opB);
    }
    else if(auto cmpInstr = instr->As<CmpInstrBase>()) {
        auto opA = GetReplacementOperand(cmpInstr->LeftOp());
        auto opB = GetReplacementOperand(cmpInstr->RightOp());
        return folder_.FoldCompare(instr->GetOpcode(), opA, opB,
                                   cmpInstr->Order());
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* Inliner::ConstantFoldBranching(Instruction* instr) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsTrue(instr->IsBranching());

    // Check if the actual condition operand can determine
    // the outcome of the branching instruction. This allows us
    // not to copy blocks that will be later removed anyway.
    auto newConditionOp = GetReplacementOperand(instr->GetSourceOp(0));
    
    if(auto result = folder_.FoldBranching(instr, newConditionOp)) {
        Log::Info("Inliner branch folding in " + *instr->ParentBlock()->Name());
        return result->Target();
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Inliner::ProcessPhiInstructions(Function* dest) {
    // We replace the incoming operands with the cloned ones,
    // and remove the operands that are incoming from blocks
    // that were not copied. After that we try to simplify
    // the 'phi' ('phi {a, B} -> a', for example).
    auto& refs = dest->ParentUnit()->References();

    for(int i = 0; i < phiInstrs_.Count(); i++) {
        auto phiInstr = phiInstrs_[i];

        for(int j = 0; j < phiInstr->OperandCount(); j++) {
            auto incomingBlock = phiInstr->GetOperandBlock(j)->Target();
            auto parentBlock = phiInstr->ParentBlock();
            
            // Because we fold branches, it's possible that we have
            // an incoming operand from a block that was copied,
            // but which is no longer a predecessor.
            if((blockMap_.ContainsKey(incomingBlock) == false) ||
               (parentBlock->HasPredecessor(blockMap_[incomingBlock]) == false)) {
                // The operand shall be removed.
                phiInstr->RemoveOperand(j);
                j--;
            }
            else {
                auto newIncomingOp = GetReplacementOperand(phiInstr->GetOperand(j));
                auto newIncomingBlock = refs.GetBlockRef(blockMap_[incomingBlock]);
                phiInstr->ReplaceOperand(j, newIncomingOp);
                phiInstr->ReplaceOperandBlock(j, newIncomingBlock);
            }
        }

        if(auto result = SimplifyPhi(phiInstr)) {
            phiInstr->GetDestinationOp()->ReplaceWith(result);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Inliner::SimplifyPhi(PhiInstr* instr) {
    // phi 5, 5 -> 5
    if(instr->IsConstant()) {
        return instr->GetConstant();
    }

    // phi a -> a
    // phi a, a, a -> a
    if(instr->HasSingleOperand() ||
       instr->SameOperands()) {
        return instr->GetOperand(0);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Inliner::GetUniqueName(Symbol* source, SymbolTable* symbolTable) {
    DebugValidator::IsNotNull(source);
    DebugValidator::IsNotNull(symbolTable);
    
    // This is called when we need to copy a variable/block.
    return GetUniqueName(*source->Name(), symbolTable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Inliner::GetUniqueName(const string& startName, SymbolTable* symbolTable) {
    int count = 0;
    string name;

    // We append '_inl' to the name, but still check that it's
    // unique, and if not, we append a count, until it is unique.
    do {
        name = string::Format(L"%s_inl%d", startName.Chars(), count);
        count++;
    } while(symbolTable->Contains(&name));

    return name;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Inliner::AddParametersToMap(Function* source, Function* dest,
                                 CallInstr::ArgumentList* argList) {
    DebugValidator::IsNotNull(source);
    DebugValidator::IsNotNull(dest);

    // If the address of the parameter variable is taken we need
    // to create the corresponding variable in the destination function,
    // else we just map from parameter to argument.
    for(int i = 0; i < source->ParameterCount(); i++) {
        auto parameter = source->GetParameter(i);
        auto paramVariable = source->GetParameterVariable(i);

        if(paramVariable->IsAddressTaken()) {
            // Copy the variable in 'dest'. This also creates
            // the appropriate variable reference mapping.
            auto copyRef = CopyVariable(paramVariable, dest);
            paramVariableToCopyRef_.Add(paramVariable, copyRef);
        }
        
        parameters_.Add(parameter);
        operandMap_.Add(parameter, (*argList)[i]);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Inliner::CopyVariables(Function* source, Function* dest) {
    DebugValidator::IsNotNull(source);
    DebugValidator::IsNotNull(dest);

    auto symbolTable = &dest->Symbols();
    auto unit = dest->ParentUnit();
    int sourceVariableCount = source->VariableCount();

    for(int i = 0; i < sourceVariableCount; i++) {
        auto variable = source->GetVariable(i);
        
        // If the variable is unnamed the new one will not get a name either.
        Variable* copyVariable;

        if(variable->HasName()) {
            string name = GetUniqueName(variable, symbolTable);
            copyVariable = Variable::GetVariable(variable->GetType(),
                                                 name, dest);
            // Copy the attributes.
            copyVariable->SetAlignment(variable->Alignment());
            copyVariable->SetDllVisibility(variable->GetDllVisibility());
            copyVariable->SetIsAddresTaken(variable->IsAddressTaken());
            dest->AddVariable(copyVariable);
        }
        else copyVariable = Variable::GetVariable(variable->GetType(),
                                                  nullptr, dest);

        // Add to the operand map the variable reference
        // that should be used instead of the original one.
        auto refType = unit->Types().GetPointer(variable->GetType());
        auto variableRef = unit->References().GetVariableRef(variable, refType);
        auto copyVariableRef = unit->References().GetVariableRef(copyVariable, refType);
        operandMap_.Add(variableRef, copyVariableRef);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
VariableReference* Inliner::CopyVariable(Variable* variable, Function* dest) {
    // If the variable is unnamed the new one will not get a name either.
    Variable* copyVariable;

    if(variable->HasName()) {
        string name = GetUniqueName(variable, &dest->Symbols());
        copyVariable = Variable::GetVariable(variable->GetType(),
                                                name, dest);
        // Copy the attributes.
        copyVariable->SetAlignment(variable->Alignment());
        copyVariable->SetDllVisibility(variable->GetDllVisibility());
        copyVariable->SetIsAddresTaken(variable->IsAddressTaken());
        dest->AddVariable(copyVariable);
    }
    else copyVariable = Variable::GetVariable(variable->GetType(), 
                                              nullptr, dest);

    // Add to the operand map the variable reference
    // that should be used instead of the original one.
    auto unit = dest->ParentUnit();
    auto refType = unit->Types().GetPointer(variable->GetType());
    auto variableRef = unit->References().GetVariableRef(variable, refType);
    auto copyVariableRef = unit->References().GetVariableRef(copyVariable, refType);
    operandMap_.Add(variableRef, copyVariableRef);
    return copyVariableRef;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* Inliner::CreateBlock(Block* source, Function* dest, Block* afterBlock) {
    DebugValidator::IsNotNull(source);
    DebugValidator::IsNotNull(dest);
    DebugValidator::IsNotNull(afterBlock);

    // Create a new block that is the copy of 'source'
    // and add it to the destination function.
    string name = GetUniqueName(source, &dest->Symbols());
    return Block::GetBlock(name, dest, afterBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Inliner::CollectReturnInstructions(Function* function) {
    auto returnInstrs = &returnInstrs_;

    function->ForEachInstruction([returnInstrs](Instruction* instr) -> bool {
        if(auto retInstr = instr->As<ReturnInstr>()) {
            returnInstrs->Add(retInstr);
        }
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Inliner::GenerateAddressTakenAssignments(CallInstr::ArgumentList* argList,
                                              Function* source, Block* entryBlock) {
    for(int i = 0; i < source->ParameterCount(); i++) {
        auto paramVariable = source->GetParameterVariable(i);

        if(paramVariable->IsAddressTaken()) {
            // Generate a 'store' of the corresponding argument
            // into the variable that represents the parameter.
            auto variableCopyRef = paramVariableToCopyRef_[paramVariable];
            auto storeInstr = StoreInstr::GetStore(variableCopyRef, (*argList)[i]);
            entryBlock->InsertInstructionFirst(storeInstr);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* Inliner::MergeReturnedValues(Function* dest, Block* afterBlock,
                                    PhiInstr*& mergePhi) {
    // Create a block where we place the 'phi' (if needed).
    // The block will act as the unique exit point of the inlined function.
    string name = GetUniqueName("#merge_block", &dest->Symbols());
    auto mergeBlock = Block::GetBlock(name, dest, afterBlock);

    // There should be at least one 'ret'.
    DebugValidator::IsLarger(returnInstrs_.Count(), 0);
    bool isVoid = returnInstrs_[0]->IsVoid();
    auto& refs = dest->ParentUnit()->References();

    if(isVoid == false) {
        // We need to create a 'phi' that combines all
        // returned values; it will be used later to replace
        // the 'call' result operand.
        auto type = returnInstrs_[0]->ReturnedOp()->GetType();
        auto resultOp = Temporary::GetTemporary(type);
        mergePhi = PhiInstr::GetPhi(resultOp, returnInstrs_.Count(), mergeBlock);

        for(int i = 0; i < returnInstrs_.Count(); i++) {
            // Check if the block that contains the 'ret'
            // is among the blocks that were copied.
            auto returnBlock = returnInstrs_[i]->ParentBlock();

            if(blockMap_.ContainsKey(returnBlock) == false) {
                continue;
            }

            auto returnedOp = returnInstrs_[i]->ReturnedOp();
            auto newReturnBlock = blockMap_[returnBlock];
            mergePhi->AddOperand(GetReplacementOperand(returnedOp),
                                 refs.GetBlockRef(newReturnBlock));
        }
    }

    // In each copied block that ends with a 'ret'
    // insert a 'goto' to the merge block.
    auto mergeBlockRef = refs.GetBlockRef(mergeBlock);

    for(int i = 0; i < returnInstrs_.Count(); i++) {
        auto returnBlock = returnInstrs_[i]->ParentBlock();

        if(blockMap_.ContainsKey(returnBlock) == false) {
            continue;
        }

        auto newReturnBlock = blockMap_[returnBlock];
        GotoInstr::GetGoto(mergeBlockRef, newReturnBlock);
    }

    return mergeBlock;
}

} // namespace Optimization