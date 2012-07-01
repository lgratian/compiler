// CFGSimplifier.cpp
// Copyright (c) Lup Gratian
//
// Implements the CFGSimplifier pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "CFGSimplifier.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

void CFGSimplifier::Execute(Function* function) {
    // The algorithm uses a worklist that contains the blocks
    // that need processing. When a block is simplified all its
    // successors are added to the worklist, because they may now simplify.
    StaticList<Block*, 128> worklist;
    SparseBitVector inWorklist;

    // Initialize the constant folder, used for jump threading.
    IRGenerator irGen(function->ParentUnit());
    folder_ = ConstantFolder(&irGen, GetTarget());

    // Start by processing all nodes at least one time.
    for(auto block = function->LastBlock(); block; block = block->PreviousBlock()) {
        worklist.Add(block);
        inWorklist.SetBit(block->Id());
    }

    // Process the blocks in the worklist.
    while(worklist.IsNotEmpty()) {
        auto block = worklist.RemoveLast();
        inWorklist.ResetBit(block->Id());

        // Try to perform some simplification.
        if(SimplifyBlock(block)) {
            for(int i = 0; i < block->SuccessorCount(); i++) {
                if(auto successorBlock = block->SuccessorAt(i)) {
                    worklist.Add(successorBlock);
                    inWorklist.SetBit(successorBlock->Id());
                }
            }

            // Reprocess the block, some opportunities may have been exposed.
            worklist.Add(block);
            inWorklist.SetBit(block->Id());
        }
    }

    // Remove identical blocks from the function (such cases appear
    // sometimes with blocks that contain only a return instruction).
    RemoveDumplicateBlocks(function);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::SimplifyBlock(Block* block) {
    // If the block has no predecessors we don't try to optimize it,
    // it's dead and will be removed later anyway.
    if((block->HasPredecessors() == false) &&
       (block->IsFunctionEntry() == false)) {
       return false;
    }

    // The order should not be changed without care, 
    // else some optimization may be missed.
    return ConvertOrChainToSwitch(block)        ||
           ThreadPhiBlocksToDestination(block)  ||
           ConvertSwitchToRangeTest(block)      ||
           ConvertSwitchOnQuestion(block)       ||
           HoistCommonInstructions(block)       ||
           MergeNestedSwitch(block)             ||
           ConvertSwitchToLoadFromGlobal(block) ||
           ConvertIfChainToSwitch(block)        ||
           SimplifyJumpToReturn(block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::ConvertOrChainToSwitch(Block* block) {
    // We try to convert a series of 'or' instructions that compare
    // the same operand with a constant to a 'switch', which can be lowered
    // in a much more efficient way (jump-table, for example).
    //
    // For example 'if(a == 1 || a == 2 || a == 3)' is translated to
    // t1 = cmp eq a, 1          switch a {
    // t2 = cmp eq a, 2              case 1: B1
    // t3 = or t1, t2      =>        case 2: B1
    // t4 = cmp eq a, 3              case 3: B1
    // t5 = or t3, t4                default: B2
    // if t5, B1, B2             }
    auto ifInstr = block->BranchInstruction()->As<IfInstr>();
    if(ifInstr == nullptr) {
        return nullptr;
    }

    auto orInstr = ifInstr->ConditionOp()->DefiningInstrAs<OrInstr>();
    if(orInstr == nullptr) {
        return nullptr;
    }

    // We use a worklist algorithm that identifies all compare instructions
    // on which the 'if' depends.
    CmpValuesList cmpValues;
    InstructionWorklist worklist;
    CmpValuesDict addedValues;
    
    bool valid = true;
    Operand* compareOp = nullptr;
    worklist.Add(orInstr);

    while(valid && worklist.IsNotEmpty()) {
        auto instr = worklist.RemoveLast();

        // We're not allowed to leave the block.
        if(instr->ParentBlock() != block) {
            valid = false;
            break;
        }

        // Check if we have an 'or' or 'cmp'/'ucmp' instruction.
        if(auto orInstr = instr->As<OrInstr>()) {
            HandleOrInOrChain(orInstr, cmpValues, worklist, compareOp, valid);
        }
        else if(auto cmpInstr = instr->As<CmpInstrBase>()) {
            HandleCmpInOrChain(cmpInstr, cmpValues, worklist, compareOp, 
                               valid, addedValues);
        }
    }

    // Now try to create the switch, but only if we don't have
    // an extremely large number of values.
    if(valid && (cmpValues.Count() > 1) && (cmpValues.Count() <= 256)) {
        // All cases target the 'true' block, while the default one
        // targets the 'false' block.
        auto switchInstr = SwitchInstr::GetSwitch(compareOp, cmpValues.Count(),
                                                  ifInstr->FalseTargetOp());

        for(int i = 0; i < cmpValues.Count(); i++) {
            switchInstr->AddCase(cmpValues[i], ifInstr->TrueTargetOp());
        }

        // Replace the 'if' with the 'switch'.
        block->ReplaceInstructionWith(ifInstr, switchInstr);
        ifInstr->Free();
        BlockSimplified(block, 0);
        return true;
    }

    return false;
}         

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CFGSimplifier::HandleOrInOrChain(OrInstr* orInstr, CmpValuesList& cmpValues,
                                      InstructionWorklist& worklist,
                                      Operand*& compareOp, bool& valid) {
    // Check if this 'or' combines the result of two comparisons,
    // or of one comparison and another 'or' instruction.
    bool sawOr = false;

    if(auto cmpInstr = orInstr->LeftOp()->DefiningInstrAs<CmpInstrBase>()) {
        worklist.Add(cmpInstr);
    }
    else if(auto orInstr2 = orInstr->LeftOp()->DefiningInstrAs<OrInstr>()) {
        worklist.Add(orInstr2);
        sawOr = true;
    }

    if(auto cmpInstr = orInstr->RightOp()->DefiningInstrAs<CmpInstrBase>()) {
        worklist.Add(cmpInstr);
    }
    else if(auto orInstr2 = orInstr->RightOp()->DefiningInstrAs<OrInstr>()) {
        // We're allowed to have an 'or' a single time.
        worklist.Add(orInstr2);
        valid = sawOr == false;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CFGSimplifier::HandleCmpInOrChain(CmpInstrBase* cmpInstr, 
                                       CmpValuesList& cmpValues,
                                       InstructionWorklist& worklist,
                                       Operand*& compareOp, bool& valid,
                                       CmpValuesDict& addedValues) {
    // We should have an integer comparison with a constant.
    if(cmpInstr->IsFcmp() || (cmpInstr->RightOp()->IsIntConstant() == false)) {
        valid = false;
        return;
    }

    // Only 'equal', 'less' and 'less or equal' are supported.
    // If it's not 'equal' then the comparison should be unsigned.
    if((cmpInstr->IsEqual() || (cmpInstr->IsUcmp() && 
        (cmpInstr->IsLess() || cmpInstr->IsLessOrEqual()))) == false) {
        valid = false;
        return;
    }

    if(compareOp == nullptr) {
        // This is the first time we see a comparison
        // remember the compared operand.
        compareOp = cmpInstr->LeftOp();
    }
    else if(cmpInstr->LeftOp() != compareOp) {
        // The compared operand should be the same in all cases.
        valid = false;
        return;
    }

    // Now extract the constant. We need to make sure that the constant
    // is not added more than once to the list, because this would
    // create an invalid 'switch' instruction.
    if(cmpInstr->IsEqual()) {
        auto intConst = cmpInstr->RightOp()->As<IntConstant>();
        AddCaseValue(intConst->Value(), cmpValues, addedValues);
    }
    else {
        // If we have 'ucmp lt a, 2' we can add 0 and 1 to the values.
        auto intConst = cmpInstr->RightOp()->As<IntConstant>();
        __int64 limit = intConst->Value();
        if(cmpInstr->IsLessOrEqual()) limit++;

        // Don't allow a large number of values.
        if(limit > 32) {
            valid = false;
            return;
        }
        
        for(int i = 0; i < limit; i++) {
            AddCaseValue(i, cmpValues, addedValues);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- 
void CFGSimplifier::AddCaseValue(__int64 value, CmpValuesList& cmpValues,
                                 CmpValuesDict& addedValues) {
    if(addedValues.ContainsKey(value) == false) {
        cmpValues.Add(value);
        addedValues.Add(value, true);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::HoistCommonInstructions(Block* block) {
    // This transformation reduces code size by hoisting instructions
    // that are common to all successors of the block.
    // B0:                      B0:
    //     if c, B1, B2             t2 = add a, b
    // B1:                          if c, B1, B2
    //     t1 = add a, b   ->   B1:
    //     ...                      ...   
    // B2:                      B2:
    //     t2 = add a, b            ...
    // We have nothing to do if we have a single successor.
    if(block->SuccessorCount() < 2) {
        return false;
    }

    // First we make sure that we don't have a self-loop,
    // and that 'block' dominates all its successors.
    for(int i = 0; i < block->SuccessorCount(); i++) {
        if(auto successorBlock = block->SuccessorAt(i)) {
            if(successorBlock == block) return false;
            if(successorBlock->PredecessorCount() > 1) return false;
        }
        else return false;
    }

    // We consider each instruction from the first successor in turn 
    // and check if it's available and can be hoisted from all other successors.
    auto instr = block->SuccessorAt(0)->FirstInstruction();
    int maxDepth = GetHoistingSearchDepth();
    int createdQuestions = 0;
    bool changed = false;

    while(instr) {
        auto nextInstr = instr->NextInstruction();

        // We ignore 'phi', 'store' and most 'call' instructions.
        if((instr->HasDestinationOp() && 
            CanBeHoistedToBlock(instr, block)) == false) {
            instr = nextInstr;
            continue;
        }
        
        // Try to collect the instructions that are the same.
        StaticList<Instruction*, 8> sameInstrs;

        for(int i = 1; i < block->SuccessorCount(); i++) {
            auto otherSuccessor = block->SuccessorAt(i);
            auto sameInstr = GetSameInstruction(otherSuccessor, instr, maxDepth);

            if(sameInstr) sameInstrs.Add(sameInstr);
            else break;
        }

        // Check if we have the same instruction in all successors.
        if(sameInstrs.Count() == (block->SuccessorCount() - 1)) {
            // Hoist the instruction from the first successor, and replace
            // all other ones by its result.
            HoistToBlock(instr, block, sameInstrs);
            changed = true;
        }
        else if(block->BranchInstruction()->IsIf() &&
                (createdQuestions < MAX_HOISTING_QUESTIONS)) {
            // If we have only two successors we check if we have two
            // instructions that are similar, meaning that they have the same
            // opcode and one of the operands is the same: in this case
            // we create a 'quest' instruction that selects between the 
            // operands that are different based on the condition of the 'if'.
            // index a, 5       index a, 8  becomes
            // t1 = quest cond, 5, 8       index a, t1
            changed = HoistUsingQuestion(instr, block);
            if(changed) createdQuestions++;
        }

        instr = nextInstr;
    }

    if(changed) BlockSimplified(block, 4);
    return changed;
}                                 

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::HoistUsingQuestion(Instruction* instr, Block* block) {
    DebugValidator::IsTrue(block->BranchInstruction()->IsIf());

    // We're interested in two-operand instructions only.
    if(instr->SourceOpCount() != 2) {
        return false;
    }

    // Don't create 'quest' instructions in a loop, it will slow us down.
    if(instr->ParentBlock()->IsInLoop()) {
        return false;
    }

    // 'instr' is the current instruction from the 'true' branch,
    // we search for a similar one in the 'false' branch.
    // 'index a, 5' and 'index a, 8' are similar because they have
    // one operand in common.
    auto otherSuccessor = block->SuccessorAt(1);
    auto otherInstr = otherSuccessor->FirstInstruction();

    while(otherInstr) {
        // If the other instruction was hoisted we skip it.
        if((otherInstr->SameKind(instr) == false) ||
           (otherInstr->HasDestinationOp() &&
           (otherInstr->GetDestinationOp()->HasUsers() == false))) {
            otherInstr = otherInstr->NextInstruction();
            continue;
        }

        // Check if we have a common operand.
        Operand* commonOp = nullptr;
        Operand* otherOpTrue = nullptr;
        Operand* otherOpFalse = nullptr;
        DetectCommonOperand(instr, otherInstr, commonOp, 
                            otherOpTrue, otherOpFalse);

        if(commonOp) {
            // We don't do the hoisting if there isn't a chance
            // to simplify the 'quest' afterwards. This prevents
            // the de-optimization of the code in certain cases.
            if((otherOpTrue->IsConstant() && otherOpFalse->IsConstant()) == false) {
                otherInstr = otherInstr->NextInstruction();
                continue;
            }

            // If we have a common operand we need to check that
            // the other one can be hoisted. If true we create in 'block'
            // a 'quest' that selects between the different operands,
            // hoist 'instr' to 'block' and replace the operand that was not
            // common with the result of the 'quest'.
            if(CanBeHoistedToBlock(otherInstr, block)) {
                return CreateHoistingQuestion(instr, otherInstr, commonOp,
                                              otherOpTrue, otherOpFalse, block);
            }
        }

        otherInstr = otherInstr->NextInstruction();
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void CFGSimplifier::DetectCommonOperand(Instruction* instr, Instruction* otherInstr, 
                                        Operand* &commonOp, Operand* &otherOpTrue, 
                                        Operand* &otherOpFalse) {
    if(instr->GetSourceOp(0) == otherInstr->GetSourceOp(0)) {
        commonOp = instr->GetSourceOp(0);
        otherOpTrue = instr->GetSourceOp(1);
        otherOpFalse = otherInstr->GetSourceOp(1);
    }
    else if(instr->GetSourceOp(1) == otherInstr->GetSourceOp(1)) {
        commonOp = instr->GetSourceOp(1);
        otherOpTrue = instr->GetSourceOp(0);
        otherOpFalse = otherInstr->GetSourceOp(0);
    }
    else if(instr->IsCommutative() &&
        (instr->GetSourceOp(0) == otherInstr->GetSourceOp(1))) {
            commonOp = instr->GetSourceOp(0);
            otherOpTrue = instr->GetSourceOp(1);
            otherOpFalse = otherInstr->GetSourceOp(0);
    }
    else if(instr->IsCommutative() &&
        (instr->GetSourceOp(1) == otherInstr->GetSourceOp(0))) {
            commonOp = instr->GetSourceOp(1);
            otherOpTrue = instr->GetSourceOp(0);
            otherOpFalse = otherInstr->GetSourceOp(1);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::CreateHoistingQuestion(Instruction* instr, Instruction* otherInstr, 
                                           Operand* commonOp, Operand* otherOpTrue, 
                                           Operand* otherOpFalse, Block* block) {
    if(auto elemInstr = instr->As<ElementInstr>()) {
        auto otherElemInstr = otherInstr->As<ElementInstr>();

        // For 'elem' instructions we select between their results,
        // but we do can do this only if the the fields have the same type.
        // t1 = elem a, 2       t2 = elem a, 5   =>  
        // t3 = quest c, t1, t2, use t3 instead of t1, t2
        if(elemInstr->GetField().FieldType !=
           otherElemInstr->GetField().FieldType) {
            return false;
        }

        // Both 'elem' need to be moved to 'block', before the 'quest'.
        elemInstr->RemoveFromBlock();
        otherElemInstr->RemoveFromBlock();
        block->InsertInstructionBefore(elemInstr, block->BranchInstruction());
        block->InsertInstructionBefore(otherElemInstr, block->BranchInstruction());

        auto questResult = Temporary::GetTemporary(elemInstr->ResultOp()->GetType());
        elemInstr->ResultOp()->ReplaceWith(questResult);
        otherElemInstr->ResultOp()->ReplaceWith(questResult);

        
        auto conditionOp = block->BranchInstruction()->As<IfInstr>()->ConditionOp();
        auto questInstr = QuestionInstr::GetQuestion(conditionOp, elemInstr->ResultOp(), 
                                                     otherElemInstr->ResultOp(),
                                                     questResult);
        block->InsertInstructionBefore(questInstr, block->BranchInstruction());
    }
    else {
        // t1 = add a, 5           t2 = add a, 8   =>
        // t3 = quest c, 5, 8      t4 = add a, t3, use t4 instead of t1, t2
        auto questResult = Temporary::GetTemporary(otherOpTrue->GetType());
        auto conditionOp = block->BranchInstruction()->As<IfInstr>()->ConditionOp();
        auto questInstr = QuestionInstr::GetQuestion(conditionOp, otherOpTrue, 
                                                     otherOpFalse, questResult);
        block->InsertInstructionBefore(questInstr, block->BranchInstruction());

        InstructionList replacedInstrs;
        replacedInstrs.Add(otherInstr);
        HoistToBlock(instr, block, replacedInstrs);

        if(instr->GetSourceOp(0) == commonOp) {
            instr->ReplaceSourceOp(1, questResult);
        }
        else {
            instr->ReplaceSourceOp(0, questResult);
        }
    }
    
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::CanBeHoistedToBlock(Instruction* instr, Block* block) {
    // We allow loads and stores to be hoisted, because any possible 
    // exception would have appeared anyway (all successors contain it).
    // Note that this might not be the right decision for a language
    // like Java, where the exception should appear at the original place.

    return GetSafetyInfo()->CanBeSpeculatedInBlock(instr, block) ||
           (instr->IsLoad() && GetSafetyInfo()->OperandsDominateBlock(instr, block)) ||
           (instr->IsStore() && GetSafetyInfo()->OperandsDominateBlock(instr, block));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CFGSimplifier::HoistToBlock(Instruction* instr, Block* block,
                                 InstructionList& replacedInstrs) {
    instr->RemoveFromBlock();
    block->InsertInstructionBefore(instr, block->BranchInstruction());
    auto newOp = instr->GetDestinationOp();

    for(int i = 0; i < replacedInstrs.Count(); i++) {
        // The instruction is now dead, it will be removed 
        // by the Dead Code Elimination pass later.
        replacedInstrs[i]->GetDestinationOp()->ReplaceWith(newOp);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::IsSameInstruction(Instruction* instrA, Instruction* instrB) {
    // The instructions are identical if they perform the same operation
    // on exactly the same operands. For commutative instructions
    // the order of the operands doesn't matter (for example,
    // 'add a, b'  and 'add b, a' are considered to be identical).
    if(instrA->SameKind(instrB) == false) {
        return false;
    }

    if(instrA->IsCommutative()) {
        // This handles 'add', 'mul', 'or', etc.
        // a, b = a, b
        if((instrA->GetSourceOp(0) == instrB->GetSourceOp(0)) &&
           (instrA->GetSourceOp(1) == instrB->GetSourceOp(1))) {
           return true;
        }

        // a, b = b, a
        if((instrA->GetSourceOp(0) == instrB->GetSourceOp(1)) &&
           (instrA->GetSourceOp(1) == instrB->GetSourceOp(0))) {
           return true;
        }

        return false;
    }
    else {
        // Test each operand.
        for(int i = 0; i < instrA->SourceOpCount(); i++) {
            if(instrA->GetSourceOp(i) != instrB->GetSourceOp(i)) {
                return false;
            }
        }

        return true;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* CFGSimplifier::GetSameInstruction(Block* block, Instruction* sameInstr,
                                               int maxDepth) {
    // Search a maximum of 'depth' instructions and check
    // if we have one that is identical to 'instr'. Identical means that
    // it definitely computes the same value, because it has the same opcode
    // and uses the same operands (SSA form makes this easy).
    int count = 0;

    for(auto instr = block->FirstInstruction(); instr && (count < maxDepth);
        instr = instr->NextInstruction()) {

        if(IsSameInstruction(instr, sameInstr)) {
            return instr;
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::ConvertSwitchToRangeTest(Block* block) {
    // Check if we have a 'switch' that actually does a range comparison.
    // The 'case' values should be in ascending order and target the same block.
    // switch a {             t1 = cmp gte a, 0
    //     0 : B1             t2 = cmp lte a, 3
    //     1 : B1         ->  t3 = and t1, t2
    //     2 : B1             if t1, B1, B2
    //     default : B2
    auto switchInstr = block->BranchInstruction()->As<SwitchInstr>();
    
    if(switchInstr == nullptr) {
        return false;
    }
    else if(switchInstr->CaseCount() > 256) {
        return false;
    }

    // Sort the case list, increases the chance that this
    // simplification can be applied.
    switchInstr->SortCases();

    BlockReference* target;
    StaticList<__int64, 64> values;

    // Add the first 'case', then test and add the rest.
    target = switchInstr->GetCase(0).Target;
    values.Add(switchInstr->GetCase(0).Value);
    
    for(int i = 1; i < switchInstr->CaseCount(); i++) {
        if(switchInstr->GetCase(i).Target != target) {
            // The target is not the same.
            return false;
        }

        values.Add(switchInstr->GetCase(i).Value);
    }

    // Sort the values and check that the distance is 1.
    values.Sort();

    for(int i = 1; i < values.Count(); i++) {
        if(values[i] != (values[i - 1] + 1)) {
            // The value is invalid.
            return false;
        }
    }

    // Create the comparisons. Note that in some cases we could emit fever
    // instructions, but they will be simplified later anyway.
    auto defaultTarget = switchInstr->DefaultTargetOp()->Target();
    ReplaceSwitchByRangeTest(switchInstr, values[0], values[values.Count() - 1],
                             target->Target(), defaultTarget);
    BlockSimplified(block, 2);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CFGSimplifier::ReplaceSwitchByRangeTest(SwitchInstr* switchInstr, 
                                             __int64 minValue, __int64 maxValue, 
                                             Block* inRangeBlock, 
                                             Block* notInRangeBlock) {
    // We generate the following code:
    // t1 = cmp gte conditionOp, minValue
    // t2 = cmp lte conditionOp, maxValue
    // t3 = and t1, t2
    // if t3, inRangeBlock, notInRangeBlock
    auto block = switchInstr->ParentBlock();
    IRGenerator irGen(block->ParentFunction()->ParentUnit());

    auto conditionOp = switchInstr->ConditionOp();
    auto minValueOp = irGen.GetIntConst(conditionOp->GetType(), minValue);
    auto maxValueOp = irGen.GetIntConst(conditionOp->GetType(), maxValue);

    // t1 = cmp gte conditionOp, minValue
    auto cmpResult1 = irGen.GetTemporary(conditionOp->GetType());
    cmpResult1->SetIsBoolean(true);
    auto cmpInstr1 = irGen.GetCmp(Order_GreaterOrEqual, conditionOp, 
                                  minValueOp, cmpResult1);
    block->InsertInstructionBefore(cmpInstr1, switchInstr);

    // t2 = cmp lte conditionOp, maxValue
    auto cmpResult2 = irGen.GetTemporary(conditionOp->GetType());
    cmpResult2->SetIsBoolean(true);
    auto cmpInstr2 = irGen.GetCmp(Order_LessOrEqual, conditionOp, 
                                  maxValueOp, cmpResult2);
    block->InsertInstructionBefore(cmpInstr2, switchInstr);
               
    // t3 = and t1, t2
    auto andResult = irGen.GetTemporary(conditionOp->GetType());
    andResult->SetIsBoolean(true);
    auto andInstr = irGen.GetAnd(cmpResult1, cmpResult2, andResult);
    block->InsertInstructionBefore(andInstr, switchInstr);

    // Replace the 'switch' with an 'if'.
    auto inRangeBlockRef = irGen.GetBlockRef(inRangeBlock);
    auto notInRangeBlockRef = irGen.GetBlockRef(notInRangeBlock);
    auto ifInstr = irGen.GetIf(andResult, inRangeBlockRef, notInRangeBlockRef);

    block->ReplaceInstructionWith(switchInstr, ifInstr);
    switchInstr->Free();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::ConvertSwitchOnQuestion(Block* block) {
    // Check if we have a 'switch' with a 'quest' as its condition.
    // switch (quest a, 1, 2) {
    //     1 : B1                 ->  if a, B1, B2
    //     2 : B2
    //     ... }
    auto switchInstr = block->BranchInstruction()->As<SwitchInstr>();
    if(switchInstr == nullptr) {
        return false;
    }

    auto conditionOp = switchInstr->ConditionOp();
    auto questInstr = conditionOp->DefiningInstrAs<QuestionInstr>();

    if(questInstr == nullptr) {
        return false;
    }
    else if(questInstr->HasConstantOperands() == false) {
        return false;
    }

    // Check if there is a 'case' for each operand.
    __int64 trueValue = questInstr->TrueOp()->As<IntConstant>()->Value();
    __int64 falseValue = questInstr->FalseOp()->As<IntConstant>()->Value();
    BlockReference* targets[2] = {nullptr, nullptr};

    for(int i = 0; i < switchInstr->CaseCount(); i++) {
        auto switchCase = switchInstr->GetCase(i);

        if(switchCase.Value == trueValue) {
            targets[0] = switchCase.Target;
        }
        else if(switchCase.Value == falseValue) {
            targets[1] = switchCase.Target;
        }
    }

    // If the values could not be found then the default target is chosen.
    if((targets[0] || targets[1]) == false) {
        auto gotoInstr = GotoInstr::GetGoto(switchInstr->DefaultTargetOp());
        block->ReplaceInstructionWith(switchInstr, gotoInstr);
        switchInstr->Free();
        return true;
    }

    // If both targets could be found then we create an 'if' that
    // targets the corresponding blocks. If one of the values was not found
    // then the default block is used for it.
    if(targets[0] == nullptr) {
        targets[0] = switchInstr->DefaultTargetOp();
    }
    else if(targets[1] == nullptr) {
        targets[1] = switchInstr->DefaultTargetOp();
    }

    auto ifInstr = IfInstr::GetIf(questInstr->ConditionOp(),
                                   targets[0], targets[1]);
    block->ReplaceInstructionWith(switchInstr, ifInstr);
    switchInstr->Free();
    BlockSimplified(block, 3);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::ThreadPhiBlocksToDestination(Block* block) {
    // Here we try to perform a simple form of jump threading.
    // If this block ends with an 'if' and we have predecessors
    // for which we can determine the target block, we can force them 
    // to jump directly to the target. This happens when we have a
    // 'phi' that has constant incoming operands.
    // B1: goto B3                         B1: goto B4  *
    // B2: goto B3                     ->  B2: goto B3                  
    // B3: t1 = phi {2, B1}, {a, B2}       B3: if a, B4, B5
    //     if t1, B4, B5                       
    PhiInstr* phiInstr;
    CmpInstrBase* cmpInstr;
    IfInstr* ifInstr;
    bool changed = false;

    // The block should be small (a 'phi', an 'if' and an optional comparison).
    if(IsThreadingCandidate(block, phiInstr, cmpInstr, ifInstr) == false) {
        return false;
    }

    // If we don't have any incoming constant there is nothing to do.
    if(phiInstr->HasSingleOperand()) {
        return false;
    }

    for(int i = 0; i < phiInstr->OperandCount(); i++) {
        auto incomingOp = phiInstr->GetOperand(i);
        BlockReference* successor = nullptr;

        if(cmpInstr || incomingOp->IsIntConstant()) {
            successor = GetThreadingSuccessor(incomingOp, cmpInstr, ifInstr);
        }

        // Skip if the final target couldn't be determined.
        if(successor == nullptr) {
            continue;
        }

        // Thread all edges from the block that provides the constant
        // to its new target block.
        auto constOpBlock = phiInstr->GetOperandBlock(i)->Target();

        for(int j = 0; j < constOpBlock->SuccessorCount(); j++) {
            auto constOpSucc = constOpBlock->SuccessorAt(j);

            if(constOpSucc == block) {
                // Thread the edge.
                constOpBlock->ReplaceSuccessor(j, successor->Target());                    
                PatchSuccessor(successor->Target(), block, constOpBlock);
            }
        }

        // Remove the operand that was incoming. If the 'phi'
        // remains without operands (meaning that all predecessors
        // could be threaded) replace it with 'undef', the block
        // will be removed later anyway.
        phiInstr->RemoveOperand(i);
        i--;
        changed = true;
    }

    if(phiInstr->OperandCount() == 0) {
        auto unit = block->ParentFunction()->ParentUnit();
        auto type = phiInstr->ResultOp()->GetType();
        auto undef = unit->Constants().GetUndefined(type);
        phiInstr->ResultOp()->ReplaceWith(undef);
        phiInstr->RemoveFromBlock(true);
    }

    if(changed) BlockSimplified(block, 1);
    return changed;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::IsThreadingCandidate(Block* block, PhiInstr*& phiInstr,
                                         CmpInstrBase*& cmpInstr,
                                         IfInstr*& ifInstr) {
    // We try to thread only if the block ends with an 'if'
    // that depends on the 'phi' instruction, or on a comparison
    // that tests the 'phi'. Any other instructions are not allowed.
    if(block->HasPhi() == false) {
        return false;
    }
    else if(block->InstructionCount() > 3) {
        return false;
    }

    // Make sure that the 'phi' is not used outside this block,
    // because we can't handle these cases (it requires the reconstruction
    // of the SSA form, something that we can't do here).
    phiInstr = block->FirstInstruction()->As<PhiInstr>();
    auto phiResult = phiInstr->ResultOp();
    
    for(int i = 0; i < phiResult->UserCount(); i++) {
        if(phiResult->GetUser(i)->ParentBlock() != block) {
            return false;
        }
    }
    
    // Check that the block ends with an 'if'.
    ifInstr = block->BranchInstruction()->As<IfInstr>();
    if(ifInstr == nullptr) {
        return false;
    }

    if(auto definingInstr = ifInstr->ConditionOp()->DefiningInstruction()) {
        if((definingInstr == phiInstr) && (block->InstructionCount() == 2)) {
            cmpInstr = nullptr;
            return true;
        }
        else if(cmpInstr = definingInstr->As<CmpInstrBase>()) {
            if(cmpInstr->RightOp()->IsConstant() &&
               // compares the 'phi' result?
               (cmpInstr->LeftOp()->DefiningInstruction() == phiInstr) &&
               // the only instruction in the block? (except 'if' and 'phi')
               (block->InstructionCount() == 3) &&
               // used only in this block? (by the 'if')
               cmpInstr->ResultOp()->HasSingleUser()) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* CFGSimplifier::GetThreadingSuccessor(Operand* op, CmpInstrBase* cmpInstr,
                                                     IfInstr* ifInstr) {
    // The successor depends on the value of the operand,
    // or on the result of the comparison, if it's the case.
    Operand* result = op;

    if(cmpInstr) result = folder_.FoldCompare(cmpInstr->GetOpcode(), op,
                                              cmpInstr->RightOp(), 
                                              cmpInstr->Order(),
                                              cmpInstr->ParentBlock());
    // It's possible that the comparison couldn't be folded
    // (it's not a constant, value-range information couldn't help).
    if(result == nullptr) {
        return nullptr;
    }
    else return result->IsZeroInt() ? ifInstr->FalseTargetOp() :
                                      ifInstr->TrueTargetOp();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CFGSimplifier::PatchSuccessor(Block* successor, Block* prevPred,
                                   Block* newPred) {
    // All values that are incoming from 'prevPred' must now
    // be incoming from 'newPred' too.
    auto unit = successor->ParentFunction()->ParentUnit();

    for(auto instr = successor->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
        if(auto phiInstr = instr->As<PhiInstr>()) {
            if(auto incomingOp = phiInstr->GetOperandFromBlock(prevPred)) {
                // Change the incoming block.
                auto blockRef = unit->References().GetBlockRef(newPred);
                phiInstr->AddOperand(incomingOp, blockRef);
            }
        }
        else break;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::MergeNestedSwitch(Block* block) {
    // Try to merge a nested 'switch' into the parent 'switch' instruction.
    // switch(a) {                   
    //     case 1: {...}              switch(a) {
    //     default:              ->        case 1: {...}
    //         switch(a) {                 case 3: {...}
    //             case 3: {}         }
    //         }                      
    // }
    auto switchInstr = block->BranchInstruction()->As<SwitchInstr>();
    if(switchInstr == nullptr) {
        return false;
    }

    // Check that in the 'default' block we have a 'switch'
    // that uses the same condition operand as this one.
    // Note that no other instruction is allowed in the block.
    auto defaultBlock = switchInstr->DefaultTargetOp()->Target();
    if(defaultBlock->InstructionCount() > 1) {
        return false;
    }

    // If the block has other predecessors we give up.
    if(defaultBlock->PredecessorCount() > 1) {
        return false;
    }

    auto defaultSwitchInstr = defaultBlock->BranchInstruction()->As<SwitchInstr>();

    if(defaultSwitchInstr == nullptr) {
        return false;
    }
    else if(defaultSwitchInstr->ConditionOp() !=
            switchInstr->ConditionOp()) {
        return false;
    }

    // If there is any 'case' that targets the default target,
    // and the target contains 'phi' instructions, we need to give up
    // because we may invalidate the 'phi' after this transformation.
    // This sometimes happens after the CFG Cleaner is run.
    auto newDefaultBlock = defaultSwitchInstr->DefaultTargetOp();

    if(SwitchTargetsPhiBlock(switchInstr, newDefaultBlock->Target())) {
        return false;
    }


    // There should be no incoming operand from this block
    // in the default target of the nested 'switch',
    // else we can't make the operands that are incoming from the nested
    // 'switch' block to be incoming from this block.
    if(HasIncomingFromBlock(block, newDefaultBlock->Target())) {
        return false;
    }

    // None of the cases of the second 'switch' should be
    // among the ones of the first 'switch'.
    Dictionary<__int64, bool> values;

    for(int i = 0; i < switchInstr->CaseCount(); i++) {
        values.Add(switchInstr->GetCase(i).Value, true);
    }

    for(int i = 0; i < defaultSwitchInstr->CaseCount(); i++) {
        if(values.ContainsKey(defaultSwitchInstr->GetCase(0).Value)) {
            // The values already exists, give up.
            return false;
        }
    }

    // Add the values to the first 'switch', then replace the default
    // target with the default target from the second 'switch'.
    for(int i = 0; i < defaultSwitchInstr->CaseCount(); i++) {
        switchInstr->AddCase(defaultSwitchInstr->GetCase(i).Value,
                             defaultSwitchInstr->GetCase(i).Target);
    }

    switchInstr->SetDefaultTargetOp(newDefaultBlock);
    ChangeIncomingBlock(newDefaultBlock->Target(), defaultBlock, block);
    BlockSimplified(block, 5);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::SwitchTargetsPhiBlock(SwitchInstr* switchInstr, Block* block) {
    for(int i = 0; i < switchInstr->CaseCount(); i++) {
        if(switchInstr->GetCase(i).Target->Target() == block) {
            if(block->HasPhi()) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::HasIncomingFromBlock(Block* fromBlock, Block* toBlock) {
    for(auto instr = toBlock->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
        if(auto phiInstr = instr->As<PhiInstr>()) {
            if(phiInstr->HasOperandFromBlock(fromBlock)) {
                return true;
            }
        }
        else break;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CFGSimplifier::ChangeIncomingBlock(Block* block, Block* prevBlock, 
                                        Block* newBlock) {
    auto unit = block->ParentFunction()->ParentUnit();

    for(auto instr = block->FirstInstruction(); instr;
        instr = instr->NextInstruction()) {
        auto phiInstr = instr->As<PhiInstr>();
		if(phiInstr == nullptr) break;
		
		for(int i = 0; i < phiInstr->OperandCount(); i++) {
			if(phiInstr->GetOperandBlock(i)->Target() == prevBlock) {
				// There is an incoming operand from 'prevBloc',
				// make it to be incoming from 'newBlock'.
				auto blockRef = unit->References().GetBlockRef(newBlock);
				phiInstr->ReplaceOperandBlock(i, blockRef);
			}
		}
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::ConvertSwitchToLoadFromGlobal(Block* block) {
    // If we have a 'switch' that only sets the values of a variable
    // we can replace it by a 'load' from a global variable initialized
    // by the corresponding values. An example (written in C):
    // switch a {                      
    //     case 2: b = 1; break;      int global[3] = {1,5,7}; 
    //     case 3: b = 5; break;  ->  if(a >= 2 && a <= 4) { 
    //     case 4: b = 7; break;          b = global[a - 2]; 
    // }                              }
    auto switchInstr = block->BranchInstruction()->As<SwitchInstr>();
    if(switchInstr == nullptr) {
        return false;
    }

    // It doesn't pay of to do the transformation for just a few values.
    if((switchInstr->CaseCount() < 3) ||
       (switchInstr->CaseCount() > 256)) {
        return false;
    }

    // 1. The default target should contain at least one 'phi' instruction.
    auto defaultBlock = switchInstr->DefaultTargetOp()->Target();
    PhiInstructionList phiInstrs;
    __int64 step; // The difference between to successive 'case' values.

    if(CollectPhiInstructions(defaultBlock, phiInstrs) == false) {
        return false;
    }

    // We maintain a list that maps a 'case' value to a constant
    // (one such list for each 'phi' instruction).
    SwitchConstantList switchConsts;

    for(int i = 0; i < phiInstrs.Count(); i++) {
        switchConsts.Add(ConstantList());
    }

    // Sort the case list, increases the chance that this
    // simplification can be applied.
    switchInstr->SortCases();

    // 2. The difference between the case values must be the same,
    // and there must be an incoming constant from each 'case' block.
    if(CollectSwitchConstants(switchInstr, defaultBlock, 
                              switchConsts, phiInstrs, step) == false) {
        return false;
    }
    
    // Validate the found constants (only under debug).
    ValidateSwitchConstants(switchConsts, phiInstrs);

    // 3. Create global variables that are initialized with the collected constants.
    // If the type of all variables is the same we create a single variable
    // that merges all constants (this improves cache locality). Example:
    // switchConsts[0] = {1,2,3}    switchConsts[1] = {4,5,6}
    // var globalVar [6 int32] = {1,3, 2,5, 3,6}
    bool sameType = true;
    auto firstType = phiInstrs[0]->ResultOp()->GetType();

    for(int i = 1; i < phiInstrs.Count(); i++) {
        if(phiInstrs[i]->ResultOp()->GetType() != firstType) {
            sameType = false;
            break;
        }
    }

    // Now we generate the variables and the code that loads the values.
    // We create a new block where the code is emitted.
    OperandList loadedOps;
    Block* loadBlock;

    if(sameType) {
        // We need to create a single variable.
        CreateSameTypeGlobal(switchInstr, phiInstrs, switchConsts,
                             firstType, step, loadBlock, loadedOps);
    }
    else {
        // We need to create a separate variable for each 'phi'.
        CreateDifferentTypeGlobals(switchInstr, phiInstrs, switchConsts,
                                   step, loadBlock, loadedOps);
    }

    // 4. Connect the load block to the successor of the 'switch' 
    //    (the default block).
    auto& refs = block->ParentFunction()->ParentUnit()->References();
    auto loadGotoInstr = GotoInstr::GetGoto(refs.GetBlockRef(defaultBlock));
    loadBlock->InsertInstructionLast(loadGotoInstr);

    // 5. Create the code that checks the range of the values.
    // If the value is not in range the default block is chosen.
    ReplaceSwitchByRangeTest(switchInstr, switchInstr->GetCase(0).Value,
                             switchInstr->GetCase(switchInstr->CaseCount() - 1).Value,
                             loadBlock, defaultBlock);

    // 6. Patch the 'phi' instructions.
    auto loadBlockRef = refs.GetBlockRef(loadBlock);
    PatchSwitchPhis(phiInstrs, loadBlockRef, loadedOps, defaultBlock, block);
    BlockSimplified(block, 5);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::CollectPhiInstructions(Block* defaultBlock, 
                                           PhiInstructionList& phiInstrs) {
    // Add to the list all valid 'phi' instructions from 'defaultBlock'.
    for(auto instr = defaultBlock->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
        if(auto phiInstr = instr->As<PhiInstr>()) {
            // Only integer and floating constants are allowed.
            if(phiInstr->ResultOp()->IsInteger() || 
               phiInstr->ResultOp()->IsFloating()) {
                phiInstrs.Add(phiInstr);
            }
            else return false;
        }
        else break; // No more 'phi' instructions.
    }

    // We should have at least one 'phi', but not too many,
    // else the program size would increase too much.
    return (phiInstrs.Count() > 0) &&
           (phiInstrs.Count() < 9);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::CollectSwitchConstants(SwitchInstr* switchInstr, 
                                           Block* defaultBlock,
                                           SwitchConstantList& switchConsts, 
                                           PhiInstructionList& phiInstrs, __int64& step) {
    // The difference between the case values must be the same,
    // and there must be an incoming constant from each 'case' block.
    // Examples of valid sequences: 1,2,3,4...    1,5,10,15...
    // Invalid sequences: 1,5,8,9,10
    __int64 lastValue;

    for(int i = 0; i < switchInstr->CaseCount(); i++) {
        auto& switchCase = switchInstr->GetCase(i);

        if((i >= 2) && (switchCase.Value != (lastValue + step))) {
            // The value is not 'last + step', give up.
            return false;
        }
        else if(i == 1) {
            // Compute the step of the values.
            step = switchCase.Value - lastValue;
        }

        // The target should be either the default block, or an empty
        // block that has a 'goto' to the default block.
        // In both cases all 'phi' instructions from the default block
        // should have a constant operand that is incoming for this 'case'.
        lastValue = switchCase.Value;
        Block* caseTarget = switchCase.Target->Target();
        Block* incomingBlock = nullptr;

        if(switchCase.Target->Target() == defaultBlock) {
            incomingBlock = switchInstr->ParentBlock();
        }
        else if(auto gotoInstr = caseTarget->BranchInstruction()->As<GotoInstr>()) {
            if(gotoInstr->TargetOp()->Target() == defaultBlock) {
                incomingBlock = caseTarget;
            }
        }

        // Give up if the CFG doesn't match what we expect.
        if(incomingBlock == nullptr) {
            return false;
        }

        // Now try to collect the constants.
        for(int j = 0; j < phiInstrs.Count(); j++) {
            Constant* constantOp = nullptr;

            if(auto incomingOp = phiInstrs[j]->GetOperandFromBlock(incomingBlock)) {
                // We're interested only in constants.
                constantOp = incomingOp->As<Constant>();
            }

            if(constantOp) {
                switchConsts[j].Add(constantOp);
            }
            else return false; // Not a constant, give up.
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CFGSimplifier::ValidateSwitchConstants(SwitchConstantList& switchConsts,
                                            PhiInstructionList& phiInstrs) {
#if 1
    // Make sure that each 'phi' has the same number of incoming constants.
    int lastCount;

    for(int i = 0; i < switchConsts.Count(); i++) {
        DebugValidator::IsLarger(switchConsts[i].Count(), 
                                 phiInstrs[i]->OperandCount() - 2);
        if(i > 0) {
            DebugValidator::AreEqual(switchConsts[i].Count(), lastCount);
        }
        else lastCount = switchConsts[i].Count();
    }
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CFGSimplifier::CreateSameTypeGlobal(SwitchInstr* switchInstr, 
                                         PhiInstructionList& phiInstrs, 
                                         SwitchConstantList& switchConsts, 
                                         const Type* type, __int64 step,
                                         Block*& loadBlock, 
                                         OperandList& loadedOps) {
    // We create a global variable that is initialized with the constants
    // of all 'phi' instructions; this improves cache locality. Example:
    // switchConsts[0] = {1,2,3}    switchConsts[1] = {4,5,6}
    // var globalVar [6 int32] = {1,3, 2,5, 3,6}
    auto block = switchInstr->ParentBlock();
    int constCount = switchConsts[0].Count();

    // The type of the variable is an array [constType constCount].
    auto& types = block->ParentFunction()->ParentUnit()->Types();
    auto varibleType = types.GetArray(type, constCount);
    auto globalVar = CreateUniqueVariable(varibleType, block, 0);

    // Initialize the variable; the first 'case' value for all 'phi'
    // are added, then the second one and so on...
    auto initializer = InitializerList::GetList();
    globalVar->SetInitializerList(initializer);

    for(int i = 0; i < constCount; i++) {
        for(int j = 0; j < phiInstrs.Count(); j++) {
            auto initConst = Initializer::GetInitializer(switchConsts[j][i]);
            initializer->Add(initConst);
        }
    }

    // Create the block in which the code will be emitted.
    loadBlock = CreateUniqueBlock(block);

    // We emit the following code to load the value(s):
    // t1 = sub switchCondOp, MIN_CASE_VALUE
    // t2 = div t1, STEP
    // t3 = mul t1, PHI_COUNT
    // for each phi[i]
    //     t4 = add t3, i
    //     t5 = index globalVar, t4
    //     t6 = load t5
    IRGenerator irGen(block->ParentFunction()->ParentUnit());
    auto switchCondOp = switchInstr->ConditionOp();
    auto condOpType = switchCondOp->GetType();
        
    // t1 = sub switchCondOp, MIN_CASE_VALUE
    auto minConstOp = irGen.GetIntConst(switchCondOp->GetType(), 
                                        switchInstr->GetCase(0).Value);
    auto subOp = irGen.GetTemporary(switchCondOp->GetType());
    irGen.GetSub(switchCondOp, minConstOp, subOp, loadBlock);

    // t2 = div t1, STEP
    auto stepOp = irGen.GetIntConst(condOpType, step);
    auto divOp = irGen.GetTemporary(condOpType);
    irGen.GetDiv(subOp, stepOp, divOp, loadBlock);

    // t2 = mul t1, PHI_COUNT
    auto mulOp = irGen.GetTemporary(condOpType);
    auto phiCountOp = irGen.GetIntConst(condOpType, phiInstrs.Count());
    irGen.GetMul(divOp, phiCountOp, mulOp, loadBlock);

    // Create code to load the value for each 'phi'.
    for(int i = 0; i < phiInstrs.Count(); i++) {
        // t3 = add t2, i
        auto addOp = irGen.GetTemporary(condOpType);
        auto constantOp = irGen.GetIntConst(condOpType, i);
        irGen.GetAdd(mulOp, constantOp, addOp, loadBlock);

        // t4 = index globalVar, t3
        auto loadType = phiInstrs[i]->ResultOp()->GetType();
        auto indexOp = irGen.GetTemporary(irGen.GetPointer(loadType));
        irGen.GetIndex(irGen.GetVariableRef(globalVar), addOp, 
                       indexOp, loadBlock);

        // t5 = load t4
        auto loadOp = irGen.GetTemporary(loadType);
        irGen.GetLoad(indexOp, loadOp, loadBlock);
        loadedOps.Add(loadOp);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CFGSimplifier::CreateDifferentTypeGlobals(SwitchInstr* switchInstr, 
                                               PhiInstructionList& phiInstrs,
                                               SwitchConstantList& switchConsts, 
                                               __int64 step, Block*& loadBlock, 
                                               OperandList& loadedOps) {
    // We create a sequence of variables (one for each 'phi), that are
    // initialized with the corresponding constants. Example:
    // switchConsts[0] = {1,2,3}    switchConsts[1] = {4.0,5.2,6.0}
    // var globalVar1 [3 int32] = {1,2,3}
    // var globalVar2 [3 float] = {4.0,5.2,6.0}

    // Create the block in which the code will be emitted.
    auto block = switchInstr->ParentBlock();
    auto& types = block->ParentFunction()->ParentUnit()->Types();
    loadBlock = CreateUniqueBlock(block);

    // Create the variables.
    for(int i = 0; i < phiInstrs.Count(); i++) {
        // The type of the variable is an array [constType constCount].
        int constCount = switchConsts[0].Count();
        auto loadType = phiInstrs[0]->ResultOp()->GetType();
        auto varibleType = types.GetArray(loadType, constCount);
        auto globalVar = CreateUniqueVariable(varibleType, block, i);

        // Initialize the variable with the corresponding constants.
        auto initializer = InitializerList::GetList();
        globalVar->SetInitializerList(initializer);
        

        for(int j = 0; j < constCount; j++) {
            auto initConst = Initializer::GetInitializer(switchConsts[i][j]);
            initializer->Add(initConst);
        }
        
        // We emit the following code to load the value:
        // t1 = sub switchCondOp, MIN_CASE_VALUE
        // t2 = div t1, STEP
        // t3 = index globalVar, t2
        // t4 = load t3
        IRGenerator irGen(block->ParentFunction()->ParentUnit());
        auto switchCondOp = switchInstr->ConditionOp();
        auto condOpType = switchCondOp->GetType();

        //  t1 = sub switchCondOp, MIN_CASE_VALUE
        auto minConstOp = irGen.GetIntConst(condOpType, 
                                            switchInstr->GetCase(0).Value);
        auto subOp = irGen.GetTemporary(condOpType);
        irGen.GetSub(switchCondOp, minConstOp, subOp, loadBlock);

        // t2 = div t1, STEP
        auto stepOp = irGen.GetIntConst(condOpType, step);
        auto divOp = irGen.GetTemporary(condOpType);
        irGen.GetDiv(subOp, stepOp, divOp, loadBlock);

        // t2 = index globalVar, t1
        auto indexOp = irGen.GetTemporary(irGen.GetPointer(loadType));
        irGen.GetIndex(irGen.GetVariableRef(globalVar), divOp, 
                       indexOp, loadBlock);

        // t3 = load t2
        auto loadOp = irGen.GetTemporary(loadType);
        irGen.GetLoad(indexOp, loadOp, loadBlock);
        loadedOps.Add(loadOp);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CFGSimplifier::PatchSwitchPhis(PhiInstructionList& phiInstrs, 
                                    BlockReference* loadBlockRef,
                                    OperandList& loadedOps, 
                                    Block* defaultBlock, Block* switchBlock) {
    // All the incoming operands, except the one
    // from the block where the switch is located, are removed and replaced
    // by the value loaded from the global variable.
    auto validIncomingBlock = defaultBlock == phiInstrs[0]->ParentBlock() ?
                              switchBlock : defaultBlock;

    for(int i = 0; i < phiInstrs.Count(); i++) {
        auto phiInstr = phiInstrs[i];

        for(int j = 0; j < phiInstr->OperandCount(); j++) {
            if(phiInstr->GetOperandBlock(j)->Target() != validIncomingBlock) {
                phiInstr->RemoveOperand(j);
                j--;
            }
        }

        // Now add the new incoming operand.
        phiInstr->AddOperand(loadedOps[i], loadBlockRef);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GlobalVariable* CFGSimplifier::CreateUniqueVariable(const Type* type, Block* block,
                                                    int count) {
    // We need a name for the variable, which is a combination
    // of the function name and "switch" with a number suffix.
    auto unit = block->ParentFunction()->ParentUnit();
    string name = "#" + *block->ParentFunction()->Name() + "_switch0";
    
    while(unit->Symbols().Contains(&name)) {
        count++;
        string countStr = string::Format(L"_switch%d", count);
        name = "#" + *block->ParentFunction()->Name() + countStr;
    }

    // Now create the variable.
    auto globalVar = GlobalVariable::GetGlobal(type, name);
    unit->AddVariable(globalVar);
    return globalVar;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* CFGSimplifier::CreateUniqueBlock(Block* block) {
    // The name of the new block needs to be unique.
    auto parentFunct = block->ParentFunction();
    int count = 0;
    string name = "#switch_load0";

    while(parentFunct->Symbols().Contains(&name)) {
        count++;
        name = string::Format(L"#switch_load%d", count);
    }

    Block* loadBlock = Block::GetBlock(name);
    parentFunct->InsertBlockAfter(loadBlock, block);
    return loadBlock;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::ConvertIfChainToSwitch(Block* block) {
    // A sequence of 'if' instructions that compares the same operand
    // for equality to a constant can be converted to a 'switch',
    // which can be lowered to a much more efficiently (jump table, for example).
    // if(a == 1) B1           switch(a) {
    // else if(a == 3) B2  =>      case 1: B1
    // else if(a == 5) B3          case 2: B2
    //                             case 3: B3
    StaticList<IntConstant*, 8> intConsts;
    StaticList<Block*, 8> blocks; // The 'true' blocks.
    StaticList<Instruction*, 8> ifInstrs;
    Dictionary<IntConstant*, bool> available;
    Operand* comparedOp = nullptr;
    Block* falseBlock = nullptr;
    Block* lastBlock;

    if(auto intConst = GetEqualsComparisonValue(block, comparedOp)) {
        intConsts.Add(intConst);
        blocks.Add(block->SuccessorAt(0));
        ifInstrs.Add(block->BranchInstruction());
        
        // Test the 'false' block, then the 'false' block of the 'false' block,
        // and so on until either the block doesn't end with an 'if',
        // or the comparison is no longer valid.
        Block* currentBlock = block->SuccessorAt(1);

        while(currentBlock->SuccessorCount() == 2) {
            // The second successor is the 'false' block.
            auto currentIntConst = GetEqualsComparisonValue(currentBlock, 
                                                            comparedOp);

            if(currentIntConst) {
                // Make sure that the constant is unique, else the 'switch'
                // would be invalid. This should happen seldom, because first
                // of all the case is rare in practice, and Value Numbering and 
                // the CFG Cleaner should have removed the duplicate tests until now.
                if(available.ContainsKey(currentIntConst)) {
                    return false;
                }

                intConsts.Add(currentIntConst);
                blocks.Add(currentBlock->SuccessorAt(0)); // 'true' block.
                ifInstrs.Add(currentBlock->BranchInstruction());
                available.Add(currentIntConst, true);
                lastBlock = currentBlock;
                currentBlock = currentBlock->SuccessorAt(1);
            }
            else break;
        }

        falseBlock = currentBlock;
    }

    // Even if there were comparisons that matched, don't do this transformation
    // if we have few cases, it isn't profitable to use a 'switch'.
    if(intConsts.Count() < 3) {
        return false;
    }

    // Create the 'switch' instruction.
    auto& refs = block->ParentFunction()->ParentUnit()->References();
    auto& consts = block->ParentFunction()->ParentUnit()->Constants();
    auto undefConst = consts.GetUndefined(IntegerType::GetInt32());

    auto switchInstr = SwitchInstr::GetSwitch(comparedOp, intConsts.Count(), 
                                              refs.GetBlockRef(falseBlock));

    for(int i = 0; i < intConsts.Count(); i++) {
        auto blockRef = refs.GetBlockRef(blocks[i]);
        switchInstr->AddCase(intConsts[i]->Value(), blockRef);

        // Invalidate the 'if' so that we don't try
        // to transform it a second time.
        ifInstrs[i]->ReplaceSourceOp(0, undefConst);
    }

    // Any incoming operand from the last block is now incoming
    // from the block where the 'switch' is inserted.
    BlockUtils::ReplacePhiOperandsBlock(falseBlock, lastBlock, block);
    
    // Replace the 'if' with the 'switch'. The block will be dead
    // after this and will be removed later by the CFG Cleaner pass.
    auto ifInstr = block->BranchInstruction();
    block->ReplaceInstructionWith(ifInstr, switchInstr);
    ifInstr->Free();
    BlockSimplified(block, 6);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IntConstant* CFGSimplifier::GetEqualsComparisonValue(Block* block, Operand*& op) {
    // The block (except the first one) should contain only an 'equal'
    // comparison with a constant and an 'if' that test its result.
    if(op && (block->InstructionCount() > 2)) {
        return nullptr;
    }

    auto ifInstr = block->BranchInstruction()->As<IfInstr>();
    if(ifInstr == nullptr) {
        return nullptr;
    }

    // Only integers should be compared.
    auto cmpInstr = ifInstr->ConditionOp()->DefiningInstrAs<CmpInstrBase>();

    if((cmpInstr == nullptr) || 
       (cmpInstr->IsEqual() == false)) {
        return nullptr;
    }
    else if(cmpInstr->IsFcmp() || 
           (cmpInstr->ResultOp()->IsInteger() == false)) {
       return nullptr;
    }

    // Make sure that the operand is the same as for the previous comparisons.
    if(op) {
        if(cmpInstr->LeftOp() != op) {
            return nullptr;
        }

        // The block (except the first one) should have a single predecessor.
        // This guarantees that the block will be later removed.
        if(block->HasSinglePredecessor() == false) {
            return nullptr;
        }
    }
    else op = cmpInstr->LeftOp();

    // The element to which we compare should be a constant.
    return cmpInstr->RightOp()->As<IntConstant>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::SimplifyJumpToReturn(Block* block) {
    // Often we have 'if's that jump to blocks that only
    // return a value (or do a small computation before returning).
    // if c, B1, B2  ->  t1 = quest c, 2, 5
    // B1: ret 2         ret t1
    // B2: ret 5         (B1, B2 disconnected, later removed)
    //
    // There are three main cases we handle:
    // 1. the simple one, exemplified above
    // 2. B1 contains a 'phi' that is returned,
    //    and B2 a 'goto' to B1.
    if(auto gotoInstr = block->BranchInstruction()->As<GotoInstr>()) {
        return SimplifyGotoToReturn(block, gotoInstr);
    }
    else if(auto ifInstr = block->BranchInstruction()->As<IfInstr>()) {
        return SimplifyIfToReturn(block, ifInstr);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::SimplifyGotoToReturn(Block* block, GotoInstr* gotoInstr) {
    // Check if we have something like the following example:
    // B1: goto B3                     ->  B1: ret 1
    // B2: goto B4                         B2: ret 2
    // B3: t1 = phi {1, B1}, {2, B2}       B3: 
    //     ret t1
    auto targetBlock = gotoInstr->TargetOp()->Target();
    if(targetBlock->IsFunctionReturn() == false) {
        return false;
    }

    if(targetBlock->HasPhi() == false) {
        // This might be a simpler case (usually appears
        // when the function doesn't return any value - void).
        return SimplifyGotoToVoidReturn(block, targetBlock);
    }

    // We allow a maximum of two 'phi's and a cheap instruction
    // that uses the 'phi's.
    PhiInstructionList phis;
    OperandList returnedOps;
    Instruction* other = nullptr;
    Instruction* otherClone = nullptr;

    if(CollectPhisAndOther(targetBlock, phis, other) == false) {
        return false;
    }

    // Determine which are the operands incoming from the block.
    for(int i = 0; i < phis.Count(); i++) {
        returnedOps.Add(phis[i]->GetOperandFromBlock(block));
    }

    // Clone the other instruction, if it's the case.
    if(other) {
        otherClone = CloneInstruction(other, block, gotoInstr);

        for(int i = 0; i < otherClone->SourceOpCount(); i++) {
            for(int j = 0; j < phis.Count(); j++) {
                if(otherClone->GetSourceOp(i) == phis[j]->GetDestinationOp()) {
                    // Use the corresponding incoming operand.
                    otherClone->ReplaceSourceOp(i, returnedOps[j]);
                }
            }
        }
    }

    // Select the operand that should be returned.
    auto retInstr = targetBlock->BranchInstruction()->As<ReturnInstr>();
    Operand* returnedOp;

    if(other && (other->GetDestinationOp() == retInstr->ReturnedOp())) {
        returnedOp = otherClone->GetDestinationOp();
    }
    else if(retInstr->ReturnedOp() == phis[0]->GetDestinationOp()) {
        returnedOp = returnedOps[0];
    }
    else if(phis.Count() == 2 && 
            (retInstr->ReturnedOp() == phis[1]->GetDestinationOp())) {
        returnedOp = returnedOps[1];
    }
    else returnedOp = retInstr->ReturnedOp();

    // Create the new return and delete the 'if'.
    block->DropLinks();
    gotoInstr->RemoveFromBlock(true);
    ReturnInstr::GetReturn(returnedOp, block);

    // Remove the incoming operands that are not needed anymore, 
    // and delete the 'phi's that don't have operands after this.
    for(int i = 0; i < phis.Count(); i++) {
        phis[i]->RemoveOperand(block);
        TryRemovePhi(phis[i]);
    }

    BlockSimplified(block, 8);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::SimplifyGotoToVoidReturn(Block* block, Block* targetBlock) {
    DebugValidator::IsTrue(block->BranchInstruction()->IsGoto());
    DebugValidator::IsTrue(targetBlock->IsFunctionReturn());

    // We eliminate a 'goto' if the target block contains only
    // a 'ret' that returns a value or nothing (void).
    if(targetBlock->InstructionCount() > 1) {
        return false;
    }

    auto retInstr = targetBlock->BranchInstruction()->As<ReturnInstr>();
    auto returnedOp = retInstr->IsVoid() ? nullptr : retInstr->ReturnedOp();

    // Remove the 'goto' from 'block' and replace it by a 'ret'
    // that returns the required value (if it's the case).
    block->DropLinks();
    block->BranchInstruction()->RemoveFromBlock(true);
    ReturnInstr::GetReturn(returnedOp, block);
    BlockSimplified(block, 8);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::SimplifyIfToReturn(Block* block, IfInstr* ifInstr) {
    auto trueBlock = ifInstr->TrueTargetOp()->Target();
    auto falseBlock = ifInstr->FalseTargetOp()->Target();

    // If neither of the blocks ends with a return
    // we can't have any of the above cases.
    if((trueBlock->IsFunctionReturn() ||
        falseBlock->IsFunctionReturn()) == false) {
        return false;
    }

    // Case 1?
    if(trueBlock->IsFunctionReturn() &&
       falseBlock->IsFunctionReturn()) {
        return SimplifyIfToReturnReturn(block, ifInstr,
                                        trueBlock, falseBlock);
    }

    // Case 2?
    // Identify the return block and check that
    // the other one is a 'goto' to the return block.
    Block* returnBlock = nullptr;
    Block* gotoBlock = nullptr;
    bool returnIsTrue;

    if(trueBlock->IsFunctionReturn()) {
        returnBlock = trueBlock;
        gotoBlock = falseBlock;
        returnIsTrue = true;
    }
    else {
        returnBlock = falseBlock;
        gotoBlock = trueBlock;
        returnIsTrue = false;
    }

    // Make sure we're allowed to do the optimization.
    if(IsValidGotoReturn(gotoBlock, returnBlock) == false) {
        return false;
    }

    // Collect the 'phi' instructions (no more than 2),
    // and check if there is any other instruction; 
    // it must be really cheap so that we hoist it.
    PhiInstructionList phis;
    Instruction* other = nullptr;
    Instruction* otherClone = nullptr;
    
    if(CollectPhisAndOther(returnBlock, phis, other) == false) {
        return false;
    }

    // All is OK, create the 'quest' instructions that
    // select the values incoming into the 'phi's.
    StaticList<QuestionInstr*, 2> quests;

    for(int i = 0; i < phis.Count(); i++) {
        auto type = phis[i]->GetOperand(0)->GetType();
        auto questOp = Temporary::GetTemporary(type);
        auto trueOp = phis[i]->GetOperandFromBlock(returnIsTrue ?
                                                  block : gotoBlock);
        auto falseOp = phis[i]->GetOperandFromBlock(returnIsTrue ?
                                                   gotoBlock : block);
        auto questInstr = QuestionInstr::GetQuestion(ifInstr->ConditionOp(),
                                                     trueOp, falseOp, questOp);
        block->InsertInstructionBefore(questInstr, ifInstr);
        quests.Add(questInstr);
    }

    // Clone the other instruction, if it's the case.
    if(other) {
        // It's highly probable that the instruction uses the 'phi's
        // as it's operands, make it use the 'quest' results.
        otherClone = CloneInstruction(other, block, ifInstr);

        for(int i = 0; i < otherClone->SourceOpCount(); i++) {
            for(int j = 0; j < phis.Count(); j++) {
                if(otherClone->GetSourceOp(i) == phis[j]->GetDestinationOp()) {
                    // Replace the operand.
                    otherClone->ReplaceSourceOp(i, quests[j]->GetDestinationOp());
                }
            }
        }
    }

    // Select the operand that should be returned.
    auto retInstr = returnBlock->BranchInstruction()->As<ReturnInstr>();
    Operand* returnedOp;

    if(other && (other->GetDestinationOp() == retInstr->ReturnedOp())) {
        returnedOp = otherClone->GetDestinationOp();
    }
    else if(retInstr->ReturnedOp() == phis[0]->GetDestinationOp()) {
        returnedOp = quests[0]->GetDestinationOp();
    }
    else if(phis.Count() == 2 && 
            (retInstr->ReturnedOp() == phis[1]->GetDestinationOp())) {
        returnedOp = quests[1]->GetDestinationOp();
    }
    else returnedOp = retInstr->ReturnedOp();

    // Create the new return and delete the 'if'.
    block->DropLinks();
    ifInstr->RemoveFromBlock(true);
    ReturnInstr::GetReturn(returnedOp, block);

    // Remove the incoming operands that are not needed anymore, 
    // and delete the 'phi's that don't have operands after.
    for(int i = 0; i < phis.Count(); i++) {
        phis[i]->RemoveOperand(block);

        if(gotoBlock->HasPredecessors() == false) {
            phis[i]->RemoveOperand(gotoBlock);
        }

        TryRemovePhi(phis[i]);
    }

    BlockSimplified(block, 8);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsValidGotoReturn(Block* gotoBlock, Block* returnBlock) {
    auto gotoInstr = gotoBlock->BranchInstruction()->As<GotoInstr>();

    if((gotoInstr == nullptr) ||
        (gotoInstr->TargetOp()->Target() != returnBlock)) {
            return false;
    }

    // Make sure that the "goto" block has only
    // a branching instruction, it doesn't pay to speculate it.
    if(gotoBlock->InstructionCount() > 1) {
        return false;
    }

    // There really should be a 'phi' in the return block
    // (else the CFG simplifier would have removed the goto block).
    if(returnBlock->HasPhi() == false) {
        return false;
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::CollectPhisAndOther(Block* returnBlock,
                                        PhiInstructionList& phis,
                                        Instruction*& other) {
    auto instr = returnBlock->FirstInstruction();

    // Allow at most two 'phi' instructions.
    while(auto phiInstr = instr->As<PhiInstr>()) {
        phis.Add(phiInstr);

        if(phis.Count() > 2) {
            return false;
        }

        instr = instr->NextInstruction();
    }

    // Check if there is another, cheap, instruction.
    if(instr->IsReturn() == false) {
        if(IsCheapInstruction(instr)) {
            other = instr;
        }
        else return false;
    }

    // Make sure there isn't any other instruction in the block.
    return returnBlock->InstructionCount() - phis.Count() - 
           (other ? 1 : 0) == 1;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void CFGSimplifier::TryRemovePhi(PhiInstr* instr) {
    if(instr->OperandCount() == 0) {
        if(instr->HasDestinationOp()) {
            // Replace the users with 'undef'.
            auto unit = instr->ParentFunction()->ParentUnit();
            auto type = instr->GetDestinationOp()->GetType();
            auto undef = unit->Constants().GetUndefined(type);
            instr->GetDestinationOp()->ReplaceWith(undef);
        }

        instr->RemoveFromBlock(true);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::SimplifyIfToReturnReturn(Block* block, IfInstr* ifInstr,
                                             Block* trueBlock, Block* falseBlock) {
    // There must be no 'phi' in the blocks, and if there is
    // an instruction beside the 'ret' it should be really cheap,
    // so that we hoist it to 'block'.
    if(trueBlock->HasPhi() || falseBlock->HasPhi()) {
        return false;
    }
    else if((trueBlock->InstructionCount() > 2) ||
            (falseBlock->InstructionCount() > 2)) {
        return false;
    }

    // Make sure that if there is a second instruction, it's cheap,
    // or it should be a 'store' that is also found in the other block 
    // (the destination address must not be necessarily be the same).
    StoreInstr* trueStore = nullptr;
    StoreInstr* falseStore = nullptr;
    Instruction* trueOther = nullptr;
    Instruction* falseOther = nullptr;
    Instruction* trueOtherClone;
    Instruction* falseOtherClone;

    if(trueBlock->InstructionCount() == 2) {
        if(IsCheapInstruction(trueBlock->FirstInstruction())) {
            trueOther = trueBlock->FirstInstruction();
        }
        else {
            // Check if we have a store.
            trueStore = trueBlock->FirstInstruction()->As<StoreInstr>();
            if(trueStore == nullptr) {
                return false;
            }
        }
    }

    if(falseBlock->InstructionCount() == 2) {
        if(IsCheapInstruction(falseBlock->FirstInstruction())) {
            falseOther = falseBlock->FirstInstruction();
        }
        else {
            // If the other block had a 'store', then there
            // should be one in this block too, to an address
            // with the same type.
            if(trueStore) {
                falseStore = falseBlock->FirstInstruction()->As<StoreInstr>();

                if(falseStore == nullptr) {
                    return false;
                }
                else if(falseStore->DestinationOp()->GetType() !=
                        trueStore->DestinationOp()->GetType()) {
                    return false;
                }
            }
            else return false;
        }
    }

    // If we have 'store's create one in 'block'.
    if(trueStore && falseStore) {
        // Create the source operand 'quest'.
        auto sourceType = trueStore->SourceOp()->GetType();
        auto sourceOp = Temporary::GetTemporary(sourceType);
        auto sourceQuestInstr = QuestionInstr::GetQuestion(ifInstr->ConditionOp(),
                                                           trueStore->SourceOp(),
                                                           falseStore->SourceOp(),
                                                           sourceOp);
        block->InsertInstructionBefore(sourceQuestInstr, ifInstr);

        // Create the destination operand 'quest'.
        auto destType = trueStore->DestinationOp()->GetType();
        auto destOp = Temporary::GetTemporary(destType);
        auto destQuestInstr = QuestionInstr::GetQuestion(ifInstr->ConditionOp(),
                                                         trueStore->DestinationOp(),
                                                         falseStore->DestinationOp(),
                                                         destOp);
        block->InsertInstructionBefore(destQuestInstr, ifInstr);

        // Now create the 'store' that uses the 'quest' results.
        auto storeInstr = StoreInstr::GetStore(destOp, sourceOp);
        block->InsertInstructionBefore(storeInstr, ifInstr);
    }
    else if(trueStore || falseStore) {
        return false;
    }
        
    // Clone the other instructions, if it's the case,
    // but use another destination temporary.
    if(trueOther) {
        trueOtherClone = CloneInstruction(trueOther, block, ifInstr);
    }

    if(falseOther) {
        falseOtherClone = CloneInstruction(falseOther, block, ifInstr);
    }

    // Create the 'quest' that selects the returned value,
    // but only if the function returns something.
    auto trueReturn = trueBlock->BranchInstruction()->As<ReturnInstr>();
    auto falseReturn = falseBlock->BranchInstruction()->As<ReturnInstr>();
    Operand* returnedOp = nullptr;

    if(trueReturn->IsVoid() == false) {
        // If the returns use the result of the other instructions
        // (most probably true), then the 'quest' should use them too.
        auto trueOp = trueOther && 
                      trueOther->GetDestinationOp() == trueReturn->ReturnedOp() ?
                      trueOtherClone->GetDestinationOp() : trueReturn->ReturnedOp();
        auto falseOp = falseOther && 
                       falseOther->GetDestinationOp() == falseReturn->ReturnedOp() ?
                       falseOtherClone->GetDestinationOp() : falseReturn->ReturnedOp();

        auto returnType = block->ParentFunction()->ReturnType();
        returnedOp = Temporary::GetTemporary(returnType);
        auto questInstr = QuestionInstr::GetQuestion(ifInstr->ConditionOp(),
                                                     trueOp, falseOp,
                                                     returnedOp);
        block->InsertInstructionBefore(questInstr, ifInstr);
    }

    // Create the new return and delete the 'if'.
    block->DropLinks();
    ifInstr->RemoveFromBlock(true);
    ReturnInstr::GetReturn(returnedOp, block);
    BlockSimplified(block, 8);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void CFGSimplifier::RemoveDumplicateBlocks(Function* function) {
    Dictionary<BlockHash, Block*> blockDict;

    for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
        if(CheckIfBlockIsDumplicate(block) == false) {
            // The block is not simple enough to be worth
            // checking if it's a duplicate.
            continue;
        }

        // Check if there is a block that is identical.
        BlockHash blockHash(block);
        Block* sameBlock;

        if(blockDict.TryGetValue(blockHash, &sameBlock)) {
            // All predecessors need to jump to 'sameBlock' instead.
            // This block will be removed later by the CFG Cleaner.
            while(block->HasPredecessors()) {
                auto predecessorBlock = block->PredecessorAt(0);
                block->RemovePredecessorAt(0);
                predecessorBlock->ReplaceSuccessor(block, sameBlock);
            }
        }
        else blockDict.Add(blockHash, block);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
unsigned CFGSimplifier::BlockHash::GetHashCode() const {
    unsigned hash = 2166136261;
    
    HashedBlock->ForEachInstruction([&hash](Instruction* instr) -> bool {
        hash = (hash * 16777619) ^ (unsigned)instr->GetOpcode();
        return true;
    });

    return hash;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::BlockHash::operator== (const BlockHash& other) const {
    if(HashedBlock == other.HashedBlock) {
        return true;
    }

    if((HashedBlock->InstructionCount() != other.HashedBlock->InstructionCount()) ||
       (HashedBlock->SuccessorCount() != other.HashedBlock->SuccessorCount())) {
        return false;
    }

    ValueNumber valueNumber;
    ScopedExpressionTable table(&valueNumber);
    auto instr = HashedBlock->FirstInstruction();
    auto otherInstr = other.HashedBlock->FirstInstruction();

    while(instr) {
        // If the instructions don't have the same opcode
        // they can't be the same.
        if(instr->SameKind(otherInstr) == false) {
            return false;
        }

        if(instr->HasDestinationOp() != otherInstr->HasDestinationOp()) {
            return false;
        }

        // We use the value number of the instructions
        // to verify that they are the same for most instructions.
        // For branching ones we check that they have the same
        // condition operand and the same successors.
        if(instr->IsBranching() == false) {
            // If the instruction is a 'call' we accept it
            // only if it is a math intrinsic.
            if(auto callInstr = instr->As<CallInstr>()) {
                if(callInstr->GetIntrinsicAs<MathIntrinsic>() == nullptr) {
                    return false;
                }
            }

            unsigned instrValueNumber = table.GetValueNumber(instr);
            unsigned otherInstrValueNumber = table.GetValueNumber(otherInstr);

            if(instrValueNumber != otherInstrValueNumber) {
                return false;
            }
        }
        else if(instr->SourceOpCount() == 1) {
            auto instrSource = instr->GetSourceOp(0);
            auto otherInstrSource = otherInstr->GetSourceOp(0);

            if(instrSource->HasDefiningInstruction()) {
                if(otherInstrSource->HasDefiningInstruction() == false) {
                    return false;
                }

                auto sourceA = instrSource->DefiningInstruction();
                auto sourceB = otherInstrSource->DefiningInstruction();
                unsigned sourceAValNumber = table.GetValueNumber(sourceA);
                unsigned sourceBValNumber = table.GetValueNumber(sourceB);

                if(sourceAValNumber != sourceBValNumber) {
                    return false;
                }
            }
            else if(instrSource != otherInstrSource) {
                return false;
            }
        }

        instr = instr->NextInstruction();
        otherInstr = otherInstr->NextInstruction();
    }

    // Make sure that the blocks have exactly the same successors.
    for(int i = 0; i < HashedBlock->SuccessorCount(); i++) {
        if(HashedBlock->SuccessorAt(i) != other.HashedBlock->SuccessorAt(i)) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::CheckIfBlockIsDumplicate(Block* block) {
    if(block->HasPhi()) {
        return false;
    }
    else if((block->SuccessorCount() > 1) ||
            (block->InstructionCount() > 3)) {
        return false;
    }
    else if((block->InstructionCount() == 1) &&
             block->BranchInstruction()->IsGoto()) {
        return false;
    }

    // If the block contains any 'store' instructions we don't consider it,
    // because the value numbering doesn't support them and crashes.
    bool hasStore = false;

    block->ForEachInstruction([&hasStore](Instruction* instr) -> bool {
        if(instr->IsStore()) {
            hasStore = true;
            return false; // Stop foreach.
        }
        else return true;
    });

    return hasStore == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Instruction* CFGSimplifier::CloneInstruction(Instruction* instr, Block* block,
                                             Instruction* beforeInstr) {
    auto clone = instr->Clone();
    block->InsertInstructionBefore(clone, beforeInstr);

    // Create a new destination operand.
    if(instr->HasDestinationOp()) {
        auto type = instr->GetDestinationOp()->GetType();
        auto cloneDestOp = Temporary::GetTemporary(type);
        clone->ReplaceDestinationOp(cloneDestOp);
    }

    return clone;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsCheapInstruction(Instruction* instr) {
    // TODO; check target if shifts are cheap or not
    return instr->IsAdd()     || 
           instr->IsSub()     ||
           instr->IsLogical() || // and, or, xor, shl, shr, ushr
           instr->IsTrunc()   ||
           instr->IsZext()    ||
           instr->IsSext()    ||
           instr->IsPtop()    ||
           instr->IsCmp()     ||
           instr->IsUcmp()    ||
           instr->IsElement() ||
           (instr->IsAddress() && 
            instr->GetSourceOp(1)->IsConstant());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void CFGSimplifier::BlockSimplified(Block* block, int type) {
#if 1
	auto function = block->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
    string text = "Block simplified in " + functionName + ":" + blockName;

    switch(type) {
        case 0: text += " (ConvertOrChainToSwitch)"; break;
        case 1: text += " (ThreadPhiBlocksToDestination)"; break;
        case 2: text += " (ConvertSwitchToRangeTest)"; break;
        case 3: text += " (ConvertSwitchOnQuestion)"; break;
        case 4: text += " (HoistCommonInstructions)"; break;
        case 5: text += " (MergeNestedSwitch)"; break;
        case 6: text += " (ConvertSwitchToLoadFromGlobal)"; break;
        case 7: text += " (ConvertIfChainToSwitch)"; break;
        case 8: text += " (SimplifyJumpToReturn)"; break;
    }
	
	Log::Warning(text);
    //IRPrinter(function).Dump();
    //function->ViewCFG();
#endif
}

} // namespace Optimization