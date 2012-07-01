// EdgeEquivalenceInfo.cpp
// Copyright (c) Lup Gratian
//
// Implements the EdgeEquivalenceInfo class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "EdgeEquivalenceInfo.hpp"

namespace Analysis {

void EdgeEquivalenceInfo::ProcessFunction(Function* function) {
    // We process the blocks in dominator-tree order.
    // This guarantees that the discovered facts are propagated
    // only to the blocks where they hold (to the blocks dominated
    // by the block where the facts were discovered).
    //! TODO: FIXME 
    domTree_ = new IRDominatorTree(function);
    domTree_->Build();
    ProcessBlock(function->FirstBlock(), nullptr);
    delete domTree_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::ProcessBlock(Block* block, Block* immDomBlock) {
    // Create a Range Tag for this block. The found equivalences
    // are recording in it, corresponding to the taken branch.
    auto tag = GetOrCreateTag(block, immDomBlock);

    // Some information can be derived by "executing" the instructions
    // in the block. For example, after 'div a, b' we know 
    // that 'b' can't be zero, because if it were, it would have trapped.
    ExtractRangesFromInstructions(block, tag);

    // We may obtain some information only for 'if' and 'switch'
    // branching instructions ('goto' doesn't provide anything).
    if(auto ifInstr = block->BranchInstruction()->As<IfInstr>()) {
        CreateRangesForIf(ifInstr, tag);
    }
    else if(auto switchInstr = block->BranchInstruction()->As<SwitchInstr>()) {
        CreateRangesForSwitch(switchInstr, tag);
    }
     
#if 1
    tag->Dump();
#endif

    // Now process all the blocks that are dominated by this one.
    // All facts recorded are still valid in the dominated blocks.
    auto& children = domTree_->GetChildrenList(block);

    for(int i = 0; i < children.Count(); i++) {
        ProcessBlock(children[i]->Block(), block);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::RemoveRangeTags(Function* function) {
    // Remove all the Range Tags from the blocks.
    for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
        block->RemoveTag<RangeTag>();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::ExtractRangesFromInstructions(Block* block, RangeTag* tag) {
    // Some information can be derived by "executing" the instructions
    // in the block. For example, after 'div a, b' we know 
    // that 'b' can't be zero, because if it were, it would have trapped.
    // The same can be done for 'load', 'store' and 'call' instructions.
    if(block->HasSuccessors() == false) {
        // This is an exit block, there is no reason to scan it.
        return;
    }
    
    // Scan all instructions.
    for(auto instr = block->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
        switch(instr->GetOpcode()) {
            case Instr_Div:
            case Instr_Udiv:
            case Instr_Mod:
            case Instr_Umod: {
                auto rightOp = instr->GetSourceOp(1);

                if(rightOp->IsTemporary() || rightOp->IsParameter()) {
                    PushNotZeroToSuccessors(rightOp, block, tag);
                }
                break;
            }
            case Instr_Load: {
                auto loadedOp = instr->GetSourceOp(0);

                // Check if we know something about the loaded value
                // in case it's from an initialized global constant.
                if(ExtractRangeFromGlobalConstant(loadedOp, instr->GetDestinationOp(), 
                                                  block, tag) == false) {
                    // No range could be determined, but at least we know
                    // that the pointer is not null.
                    if(loadedOp->IsTemporary() || loadedOp->IsParameter()) {
                        PushNotZeroToSuccessors(loadedOp, block, tag);
                    }
                }
                break;
            }
            case Instr_Store: {
                // We create a range only for temporaries, because for
                // references we already know that the 'store' can't trap.
                auto destOp = instr->GetSourceOp(1);

                if(destOp->IsTemporary() || destOp->IsParameter()) {
                    PushNotZeroToSuccessors(destOp, block, tag);
                }
                break;
            }
            case Instr_Call: {
                // If we call a function through a pointer (the target
                // is unknown), after the call we know that the pointer is valid.
                auto functionOp = instr->GetSourceOp(0);

                if(functionOp->IsTemporary() || functionOp->IsParameter()) {
                    PushNotZeroToSuccessors(functionOp, block, tag);
                }
                break;
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool EdgeEquivalenceInfo::ExtractRangeFromGlobalConstant(Operand* op, Operand* resultOp,
                                                         Block* block, RangeTag* tag) {
    // A loaded value from a global variable that is initialized
    // can be estimated as being in the range [min, max], where 'min' and 'max'
    // are the minimum/maximum values of the constants in the initializer.
    // For example, if we have 'const int v[] = {1,3,6,9,4}; t = v[i];'
    // we know that 'v[i]' is somewhere in the range [1, 9].

    // We do the analysis only for loads of integers and pointers.
    // For pointers we could conclude that the value is never null
    // (useful for arrays of function pointers).
    if(resultOp == nullptr) {
        return false;
    }

    auto pointerType = op->GetType()->As<PointerType>();
    
    if((pointerType->PointeeType()->IsInteger() ||
        pointerType->PointeeType()->IsPointer()) == false) {
        return false;
    }

    // Get the deepest operand that acts as a base; 
    // it should be a global variable.
    auto baseOp = GetBaseOperand(op);
    
    if((baseOp == nullptr) || 
       (baseOp->IsGlobalVariableRef() == false)) {
        return false;
    }

    // Check if the variable is an initialized constant.
    // We're interested only in initializer lists 
    // (zero initializers are unlikely to be used with global constants).
    auto globalVar = baseOp->As<VariableReference>()->GetGlobal();

    if((globalVar->IsConstant() == false) ||
       (globalVar->HasInitializerList() == false)) {
        return false;
    }

    // 
    bool isPointer = pointerType->PointeeType()->IsPointer();
    auto initList = globalVar->GetInitializerList();

    if(isPointer == false) {
        __int64 minValue = std::numeric_limits<__int64>::max();
        __int64 maxValue = std::numeric_limits<__int64>::min();

        if(FindInitializersMinMax(initList, minValue, maxValue)) {
            Range range(minValue, maxValue);
            PushRangeToSuccessors(range, resultOp, block, tag);
            return true;
        }
    }
    else if(AreInitializedPointersNotNull(initList)) {
        PushNotZeroToSuccessors(resultOp, block, tag);
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool EdgeEquivalenceInfo::FindInitializersMinMax(InitializerList* initList,
                                                 __int64& minValue, __int64& maxValue) {
    // Scan the list of initializers and determine 
    // the minimum/maximum integer constant values.
    for(int i = 0; i < initList->Count(); i++) {
        Initializer* initializer = (*initList)[i];

        if(initializer->IsInitializerList()) {
            // We have an initializer list, recourse.
            auto initList = static_cast<InitializerList*>(initializer);
            
            if(FindInitializersMinMax(initList, minValue, maxValue) == false) {
                // Something invalid, give up.
                return false;
            }
        }
        else {
            // The initializer should not have any casts or adjustment.
            if(initializer->HasAdjustment() || 
               initializer->HasConversion()) {
                return false;
            }
            
            // We ignore types that are not integer because
            // no casts where involved before the load.
            if(auto intConst = initializer->Value()->As<IntConstant>()) {
                minValue = std::min(minValue, intConst->Value());
                maxValue = std::max(maxValue, intConst->Value());
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool EdgeEquivalenceInfo::AreInitializedPointersNotNull(InitializerList* initList) {
    // Check if all pointer values are initialized with a
    // function/variable reference (meaning they are not null).
    for(int i = 0; i < initList->Count(); i++) {
        Initializer* initializer = (*initList)[i];

        if(initializer->IsInitializerList()) {
            // We have an initializer list, recourse.
            auto initList = static_cast<InitializerList*>(initializer);

            if(AreInitializedPointersNotNull(initList) == false) {
                return false;
            }
        }
        else {
            // The initializer should not have adjustment.
            if(initializer->HasAdjustment()) {
                return false;
            }

            // The value should be either a function/variable reference,
            // or an non-zero integer cast to a pointer.
            if((initializer->Value()->IsFunctionReference() == false) &&
               (initializer->Value()->IsVariableReference() == false)) {
                // Check the integer cast case.
                if((initializer->Conversion() != InitConv_IntToPointer) ||
                   (initializer->Value()->IsZeroInt() == false)) {
                    return false;
                }
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* EdgeEquivalenceInfo::GetBaseOperand(Operand* op) {
    if(op->IsVariableReference()) {
        return op;
    }

    // We don't skip over 'ptop' casts, it's too dangerous in this case.
    if(op->DefiningInstrIs<IndexInstr>()   ||
       op->DefiningInstrIs<AddressInstr>() ||
       op->DefiningInstrIs<ElementInstr>()) {
        auto baseOp = op->DefiningInstruction()->GetSourceOp(0);
        return GetBaseOperand(baseOp);
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::PushNotZeroToSuccessors(Operand* op, Block* block,
                                                  RangeTag* tag) {
    // Create an anti-range that indicates the the operand is not zero/null.
    // Before doing that we check if there is a known range for the operand,
    // and if it is, if it already indicates that the operand is not zero.
    // If it does, don't create an anti-range, because we may lose information,
    // and anti-ranges are useful in fever cases.
    bool create = true;
    Range existingRange;
    Range newRange;

    // We can't handle floating operands.
    if(op->IsFloating()) return;

    if(tag->GetRange(op, existingRange) &&
       (tag->IsNotZero(op) != Result_Yes)) {
        // When the range doesn't specify that the operand is not zero
        // we can improve it in some cases.
        // [0, 5] -> [1, 5]
        // (-INF, 0] -> (-INF, -1)
        if(existingRange.IsRange &&
           (existingRange.HasBaseOperand() == false)) {
            if(existingRange.Low.Constant == 0) {
                newRange = Range(1, existingRange.High.Constant);
                create = false;
            }
            else if(existingRange.High.Constant == 0) {
                newRange = Range(existingRange.Low.Constant, -1);
                create = false;
            }
            else {
                // The existing range is probably better.
                return;
            }
        }
    }
    
    if(create) {
        // An anti-range needs to be created.
        newRange = Range(-1, 1, false /* isRange */);
    }

    PushRangeToSuccessors(newRange, op, block, tag);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::PushRangeToSuccessors(Range range, Operand* op, 
                                                Block* block, RangeTag* tag) {
    tag->AddRange(op, range);

    for(int i = 0; i < block->SuccessorCount(); i++) {
        tag->AddRangeForSuccessor(block->SuccessorAt(i), op, range);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeTag* EdgeEquivalenceInfo::GetOrCreateTag(Block* block, Block* immDomBlock) {
    DebugValidator::IsNotNull(block);

    // Check if a Range Tag already exists for this block.
    RangeTag* tag = block->GetTag<RangeTag>();

    if(tag == nullptr) {
        // A tag needs to be created and it must be connected
        // to it's immediate dominator's tag.
        RangeTag* immDomTag = nullptr;
        
        if(immDomBlock) {
            // Because we process the blocks in dominator order,
            // the immediate dominator shall have already a Range Tag.
            immDomTag = immDomBlock->GetTag<RangeTag>();
            DebugValidator::IsNotNull(immDomTag);
        }

        // Now create the new tag.
        tag = RangeTag::GetRangeTag(block, immDomTag,
                                    IsLoopHeader(block, immDomBlock),
                                    IsFunctionExitBlock(block));
        block->AddTag(tag);

        if(immDomTag) {
            // Make the new tag the child of it's immediate dominator's tag.
            immDomTag->Children().Add(tag);
        }
    }

    return tag;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool EdgeEquivalenceInfo::IsLoopHeader(Block* block, Block* immDomBlock) {
    if(block->PredecessorCount() < 2) {
        return false;
    }

    // We need to check if 'block' represents a single-entry 
    // loop header with control flowing from 'immDomBlock'.
    for(int i = 0; i < block->PredecessorCount(); i++) {
        auto predecessorBlock = block->PredecessorAt(i);

        if(predecessorBlock == immDomBlock) {
            // Make sure that all other edges are back-edges
            // by checking that all other predecessors are dominated by 'block'.
            for(int j = 0; j < block->PredecessorCount(); j++) {
                auto otherPredBlock = block->PredecessorAt(j);
                
                if((otherPredBlock != predecessorBlock) &&
                   (domTree_->Dominates(block, otherPredBlock) == false)) {
                    return false;
                }
            }

            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool EdgeEquivalenceInfo::IsFunctionExitBlock(Block* block) {
    // Check if we have a 'ret', or a 'call' to any of
    // 'exit', 'abort' or 'longjmp'.
    if(block->BranchInstruction()->IsReturn()) return true;

    for(auto instr = block->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
        if(auto callInstr = instr->As<CallInstr>()) {
            auto type = StdlibRecognizer::Recognize(callInstr);

            if((type == Stdlib_exit)  ||
               (type == Stdlib_abort) ||
               (type == Stdlib_longjmp)) return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::CreateRangesForIf(IfInstr* instr, RangeTag* tag) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(tag);

    // Try to find the ranges for the operand(s) that form the condition.
    // For the 'true' branch these ranges hold, for the 'false' branch
    // these ranges need to be inverted (anti-ranges).
    auto conditionOp = instr->ConditionOp();
    bool createInverse = true;
    bool isSigned = true;
    RangeList ranges;

    if(auto cmpInstr = conditionOp->DefiningInstrAs<CmpInstrBase>()) {
        // The range is combined with a previous range that involves
        // the same operand, if it exists.
        // We don't process floating-point comparisons.
        if(instr->IsFcmp()) {
            return;
        }

        CreateRangeFromComparison(cmpInstr, ranges, tag);
        isSigned = cmpInstr->IsCmp();
    }
    else if(auto andInstr = conditionOp->DefiningInstrAs<AndInstr>()) {
        // An 'and' can create many ranges, or make the range
        // for an operand more precise.
        CollectAndRanges(andInstr, ranges, tag);
    }
    else if(auto orInstr = conditionOp->DefiningInstrAs<OrInstr>()) {
        // Sometimes we can deduce a range from an 'or'.
        CollectOrRanges(orInstr, ranges, tag);
        createInverse = false;
    }
    else if(conditionOp->IsConstant() == false) {
        // For any other operand we know just that it is not zero.
        ranges.Add(OperandRange(conditionOp, Range(-1, 1, false /* isRange */)));
    }

    // Add the found ranges to the Range Tag.
    // If any of the operands is a constant we don't add anything.
    for(int i = 0; i < ranges.Count(); i++) {
        auto op = ranges[i].OperandInfo;

        if(op->IsConstant() == false) {
            AddIfRange(instr, ranges[i].OperandInfo,
                       ranges[i].RangeInfo, tag, createInverse, isSigned);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::AddIfRange(IfInstr* ifInstr, Operand* op, 
                                     Range range, RangeTag* tag, 
                                     bool createInverse, bool isSigned) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsNotNull(tag);

    // Combine the range with what is known from the immediate dominator.
    bool oldRangeUsed;
    range = IntersectWithKnownRange(op, range, tag, oldRangeUsed);
    
    // Nothing needs to be changed for the 'true' branch.
    if(oldRangeUsed == false) {
        tag->AddRangeForSuccessor(ifInstr->TrueTargetOp()->Target(), 
                                  op, range);
    }

    // The range must be inverted for the 'false' branch
    // (for example, '(-INF, -1] U [1, +INF)' becomes '[0, 0]').
    // Note that currently we can't invert pointer ranges.
    if(createInverse == false) {
        return;
    }

    // For pointers the only ranges we can invert are the ones
    // that represent the fact that the pointer is/is not null.
    if(op->GetType()->IsPointer()) {
        if(IsNullPointer(range)) {
            // [0, 0] -> (-INF, -1] U [1, +INF)
            tag->AddRangeForSuccessor(ifInstr->FalseTargetOp()->Target(), 
                                      op, Range(-1, 1, false /* isRange */));
        }
        else if(IsNotNullPointer(range)) {
            // (-INF, -1] U [1, +INF) -> [0, 0]
            tag->AddRangeForSuccessor(ifInstr->FalseTargetOp()->Target(), 
                                      op, Range(0LL, 0LL));
        }
        return;
    }

    tag->AddRangeForSuccessor(ifInstr->FalseTargetOp()->Target(), 
                              op, range.Invert(op, isSigned));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool EdgeEquivalenceInfo::IsNullPointer(Range& range) {
    // Test for [0, 0].
    return  range.IsRange && 
           !range.HasBaseOperand() && 
            range.IsSingleValue() &&
            range.Low.Constant == 0;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool EdgeEquivalenceInfo::IsNotNullPointer(Range& range) {
    // Test for (-INF, -1] U [1, +INF).
    return (range.IsRange == false) &&
           (range.HasBaseOperand() == false) &&
           (range.Low.Constant == -1) &&
           (range.High.Constant == 1);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::CreateRangesForSwitch(SwitchInstr* instr, RangeTag* tag) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(tag);
    
    // Try to propagate the value of the condition operand to the 'case' 
    // target blocks. This can be done only if there is a single value 
    // that targets the same block, else we don't known which of the values 
    // was selected. For these cases we still can propagate an approximated
    // range, and if this is not possible, at least the fact that the value
    // is definitely not zero. Example (in C syntax):
    // switch(a) {
    //     case 1: {B1}  -> a = [1, 1]
    //     case 2:
    //     case 3: {B2}  -> a = [2, 3]
    //     case 4:
    //     case 9: {B3}  -> a = [4, +INF)
    //     default: -> nothing, because the values are not consecutive
    ValueDict targetValues;
    auto& caseList = instr->CaseList();

    // Group the 'case' values based on the target block.
    for(int i = 0; i < caseList.Count(); i++) {
        auto switchCase = caseList[i];

        if(targetValues.ContainsKey(switchCase.Target) == false) {
            // This is the first time we see a 'case'
            // targeting this block.
            targetValues.Add(switchCase.Target, CaseList());
        }

        targetValues[switchCase.Target].Add(switchCase.Value);
    }

    // Try to propagate the selected value in the 'case' targets.
    auto defaultTarget = instr->DefaultTargetOp();
    auto conditionOp = instr->ConditionOp();
    SwitchValueList valueList;

    PropagateCaseValues(defaultTarget, conditionOp, 
                        targetValues, valueList, tag);

    // Try to propagate something about the default case.
    if(valueList.Count () == caseList.Count()) {
        PropagateDefaultValue(valueList, conditionOp, 
                              instr->DefaultTargetOp(), tag);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::PropagateCaseValues(BlockReference* defaultTarget,
                                              Operand* conditionOp, ValueDict &targetValues, 
                                              SwitchValueList& valueList, RangeTag* tag) {
    // Based on the number of values associated with a target block
    // we propagate the following range, but only if the block
    // is not the default target of the 'switch':
    // - one value: propagate [VALUE, VALUE]
    // - more values:
    //      - if values are consecutive propagate [MIN_VALUE, MAX_VALUE]
    //      - else propagate [MIN_VALUE, +INF)
    targetValues.ForEach([tag, defaultTarget, conditionOp, &valueList]
                         (ValueDict::TPair& pair) -> bool {
        if(pair.Key == defaultTarget) {
            return true; // continue
        }

        if(pair.Value.Count() == 1) {
            valueList.Add(pair.Value[0]);
            Range range(pair.Value[0], pair.Value[0]);
            tag->AddRangeForSuccessor(pair.Key->Target(), conditionOp, range);
        }
        else {
            // Sort the values, it's easier to work with.
            pair.Value.Sort();
            int count = pair.Value.Count();
            bool consecutive = true;

            for(int i = 1; i < count; i++) {
                if((pair.Value[i] - pair.Value[i - 1]) != 1) {
                    consecutive = false;
                    break;
                }
            }

            // Add all the values that are involved to the list.
            valueList.AddRange(pair.Value.Array(), pair.Value.Count());

            if(consecutive) {
                // Propagate range [MIN_VALUE, MAX_VALUE].
                Range range(pair.Value[0], pair.Value[count - 1]);
                tag->AddRangeForSuccessor(pair.Key->Target(), conditionOp, range);
            }
            else {
                // Propagate range [MIN_VALUE, +INF).
                Range range(pair.Value[0], Bound::GetMaximum(conditionOp, true));
                tag->AddRangeForSuccessor(pair.Key->Target(), conditionOp, range);
            }
        }
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::PropagateDefaultValue(SwitchValueList& valueList, 
                                                Operand* conditionOp,
                                                BlockReference* defaultTarget, 
                                                RangeTag* tag) {
    // If the 'case's take consecutive values between 'MIN_VALUE'
    // and 'MAX_VALUE' then the condition operand is in the range 
    // (-INF, MIN_VALUE - 1] U [MAX_VALUE + 1, +INF).
    valueList.Sort();
    int count = valueList.Count();
    bool consecutive = true;

    for(int i = 1; i < count; i++) {
        if((valueList[i] - valueList[i - 1]) != 1) {
            consecutive = false;
            break;
        }
    }

    if(consecutive) {
        Range range(valueList[0] - 1, valueList[count - 1] + 1, 
                    false /* isRange */);
        tag->AddRangeForSuccessor(defaultTarget->Target(), 
                                  conditionOp, range);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::CreateRangeFromComparison(CmpInstrBase* instr,
                                                    RangeList& list, RangeTag* tag, 
                                                    bool createMirrored) {
    DebugValidator::IsNotNull(instr);

    // We don't process floating-point comparisons.
    if(instr->IsFcmp()) {
        return;
    }

    // The comparison is first put into a canonical form
    // (this reduces the number of cases we need to test for).
    CanonicalizeComparison(instr);

    // We test for the following cases: (OP is any of =, !=, <, <=, > and >=)
    // 1. a OP C
    // 2. a OP b +- C
    // Note that we presume that cases like 'a + 2 < 5' 
    // have already been simplified to 'a < 3' by Peephole.
    Operand* opA = instr->LeftOp();
    Operand* opB = instr->RightOp();
    OrderType order = instr->Order();
    bool isSigned = instr->IsCmp();
    
    // These are used only for case 2.
    bool isOpBAdd = false;
    bool isOpBSub = false;
    Operand* opBTemp = nullptr;
    IntConstant* opBConst = nullptr;

    // Check for case 2. If 'opB' is an 'add' or 'sub' instruction
    // that involves a constant the arguments are set.
    IsOperandAddSub(opB, isOpBAdd, opBTemp, opBConst, isOpBSub);

    // Some example ranges:
    // a < 5 -> (-INF, 4]
    // a > b + 2 -> [b - 1, +INF]
    if(auto intConst = opB->As<IntConstant>()) {
        // This is the most common case, and it's easier to handle.
        auto result = CreateRangeForConstCmp(intConst, order, 
                                             isSigned, tag);
        list.Add(OperandRange(opA, result));
    }
    else if(opA->IsPointer()) {
        auto result = CreateRangeFromPointerCmp(opA, opB, order, tag);
        list.Add(OperandRange(opA, result));
    }
    else if(isOpBAdd) {
        auto resultA = CreateRangeForAddCmp(opBTemp, opBConst, order, 
                                            isSigned, tag);
        list.Add(OperandRange(opA, resultA));

        // We can create two ranges for this case, because, for example,
        // 'a < b + 5' implies that 'b > a - 5'.
        if(createMirrored) {
            OrderType inverted = CmpInstrBase::InvertedOrder(order, false);
            auto resultB = CreateRangeForSubCmp(opA, opBConst, inverted, 
                                                isSigned, tag);
            list.Add(OperandRange(opBTemp, resultB));
        }
    }
    else if(isOpBSub) {
        auto resultA = CreateRangeForSubCmp(opBTemp, opBConst, order, 
                                            isSigned, tag);
        list.Add(OperandRange(opA, resultA));

        // We can create two ranges for this case, because, for example,
        // 'a < b - 5' implies that 'b > a + 5'.
        if(createMirrored) {
            OrderType inverted = CmpInstrBase::InvertedOrder(order, false);
            auto unit = instr->ParentFunction()->ParentUnit();
            auto resultB = CreateRangeForAddCmp(opA, opBConst, inverted, 
                                                isSigned, tag);
            list.Add(OperandRange(opBTemp, resultB));
        }
    }
    else {
        auto resultA = CreateRangeForGenericCmp(opB, order, isSigned, tag);
        list.Add(OperandRange(opA, resultA));
        
        // We can create two ranges for this case, because, for example, 
        // 'a < b' implies that 'b > a'.
        if(createMirrored) {
            OrderType inverted = CmpInstrBase::InvertedOrder(order, false);
            auto resultB = CreateRangeForGenericCmp(opA, inverted, 
                                                    isSigned, tag);
            list.Add(OperandRange(opB, resultB));
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool EdgeEquivalenceInfo::IsOperandAddSub(Operand* opB, bool &isOpBAdd, Operand* &opBTemp, 
                                          IntConstant* &opBConst, bool &isOpBSub) {
    // Check if the operand is a 'add' or 'sub' instruction, and if it is
    // also make sure that one of the operands is a constant.
    if(opB->IsConstant()) return false;

    if(auto addInstr = opB->DefiningInstrAs<AddInstr>()) {
        if(auto intConst = addInstr->RightOp()->As<IntConstant>()) {
            // We can use this information only if overflow
            // is undefined (which is the case for C).
            if(addInstr->HasUndefinedOverflow()) {
                isOpBAdd = true;
                opBTemp = addInstr->LeftOp();
                opBConst = intConst;
                return true;
            }
        }
    }
    else if(auto subInstr = opB->DefiningInstrAs<SubInstr>()) {
        if(auto intConst = subInstr->RightOp()->As<IntConstant>()) {
            // We can use this information only if overflow
            // is undefined (which is the case for C).
            if(addInstr->HasUndefinedOverflow()) {
                isOpBSub = true;
                opBTemp = subInstr->LeftOp();
                opBConst = intConst;
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateRangeForConstCmp(IntConstant* intConst, 
                                                  OrderType order, bool isSigned, 
                                                  RangeTag* tag) {
    // In this case the range we get is precise.
    switch(order) {
        case Order_Equal: {
            // a == C -> [C, C]
            return Range(intConst->Value(), intConst->Value());
        }
        case Order_NotEqual: {
            // a != C -> (-INF, C - 1] U [C + 1, +INF) (anti-range)
            auto result = Range(intConst->Value() - 1, 
                                intConst->Value() + 1, 
                                false /* isRange */);
            return PatchAntiRange(result, intConst, isSigned);
        }
        case Order_Less: {
            // a < C -> (-INF, C - 1]
            return Range(Bound::GetMinimum(intConst, isSigned), 
                         intConst->Value() - 1);
        }
        case Order_LessOrEqual: {
            // a <= C -> (-INF, C]
            return Range(Bound::GetMinimum(intConst, isSigned), 
                         intConst->Value());
        }
        case Order_Greater: {
            // a > C -> [C + 1, +INF)
            return Range(intConst->Value() + 1, 
                         Bound::GetMaximum(intConst, isSigned));
        }
        case Order_GreaterOrEqual: {
            // a >= C -> [C, +INF)
            return Range(intConst->Value(), 
                         Bound::GetMaximum(intConst, isSigned));
        }
        default: {
            DebugValidator::Unreachable();
            return Range::GetUnknown(intConst, isSigned);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateRangeForAddCmp(Operand* op, IntConstant* intConst,
                                                OrderType order, bool isSigned,
                                                RangeTag* tag) {
    // We can't create a range for a pointer.
    if(op->IsPointer()) {
        return Range::GetUnknown();
    }

    // In this case the range is relative to 'op'.
    // If the operand has a known range we make this new range
    // to be relative to it (we avoid creating a symbolic range).
    // For example, if we know that 'a < b + 3' and 'b = [2, 5]',
    // then we can conclude that 'a = (-INF, 4]'.
    Range opRange;

    if(tag->GetRange(op, opRange)) {
        return CreateRangeForGenericCmpWithRange(op, opRange, intConst,
                                                 false, order, isSigned);
    }

    switch(order) {
        case Order_Equal: {
            // a == b + C -> [b + C, b + C]
            return Range(op, intConst->Value(), op, intConst->Value());
        }
        case Order_NotEqual: {
            // a != b + C -> (-INF, op + C - 1] U [op + C + 1, +INF) (anti-range)
            // If there is overflow when we add/subtract 1,
            // then we are conservative and presume we don't know the range.
            if(IA::AddOverflows(intConst->Value(), 1, intConst->GetType()) ||
               IA::SubOverflows(intConst->Value(), 1, intConst->GetType())) {
                    return Range::GetUnknown(op, isSigned);
            }

            auto result = Range(op, intConst->Value() - 1, 
                                op, intConst->Value() + 1, 
                                false /* isRange */);
            return PatchAntiRange(result, op, isSigned);
        }
        case Order_Less: {
            // a < b + C -> (-INF, b + C - 1]
            return Range(nullptr, Bound::GetMinimum(intConst, isSigned), 
                         op, intConst->Value() - 1);
        }
        case Order_LessOrEqual: {
            // a <= b + C -> (-INF, b + C]
            return Range(nullptr, Bound::GetMinimum(intConst, isSigned),
                         op, intConst->Value());
        }
        case Order_Greater: {
            // a > b + C -> [b + C + 1, +INF)
            return Range(op, intConst->Value() + 1,
                         nullptr, Bound::GetMaximum(intConst, isSigned));
        }
        case Order_GreaterOrEqual: {
            // a >= b + C -> [b + C, +INF)
            return Range(op, intConst->Value(),
                         nullptr, Bound::GetMaximum(intConst, isSigned));
        }
        default: {
            DebugValidator::Unreachable();
            return Range::GetUnknown(op, isSigned);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateRangeForSubCmp(Operand* op, IntConstant* intConst,
                                                OrderType order, bool isSigned,
                                                RangeTag* tag) {
    // We can't create a range for a pointer.
    if(op->IsPointer()) {
        return Range::GetUnknown();
    }

    // In this case the range is relative to 'op'.
    // If the operand has a known range we make this new range
    // to be relative to it (we avoid creating a symbolic range).
    // For example, if we know that 'a < b + 3' and 'b = [2, 5]',
    // then we can conclude that 'a = (-INF, 4]'.
    Range opRange;

    if(tag->GetRange(op, opRange)) {
        return CreateRangeForGenericCmpWithRange(op, opRange, intConst,
                                                 true, order, isSigned);
    }

    switch(order) {
        case Order_Equal: {
            // a == b - C -> [b - C, b - C]
            return Range(op, -intConst->Value(), op, -intConst->Value());
        }
        case Order_NotEqual: {
            // a != b - C -> (-INF, b - C - 1] U [b - C + 1, +INF) (anti-range)
            // If there is overflow when we add/subtract 1,
            // then we are conservative and presume we don't know the range.
            if(IA::AddOverflows(-intConst->Value(), 1, intConst->GetType()) ||
               IA::SubOverflows(-intConst->Value(), 1, intConst->GetType())) {
                    return Range::GetUnknown(op, isSigned);
            }

            auto result = Range(op, -intConst->Value() - 1, 
                                op, -intConst->Value() + 1, 
                                false /* isRange */);
            return PatchAntiRange(result, op, isSigned);
        }
        case Order_Less: {
            // a < b - C -> (-INF, b - C - 1]
            return Range(nullptr, Bound::GetMinimum(intConst, isSigned),
                         op, -intConst->Value() - 1);
        }
        case Order_LessOrEqual: {
            // a <= b - C -> (-INF, b - C]
            return Range(nullptr, Bound::GetMinimum(intConst, isSigned),
                         op, -intConst->Value());
        }
        case Order_Greater: {
            // a > b - C -> [b - C + 1, +INF)
            return Range(op, -intConst->Value() + 1,
                         nullptr, Bound::GetMaximum(intConst, isSigned));
        }
        case Order_GreaterOrEqual: {
            // a >= b - C -> [b - C, +INF)
            return Range(op, -intConst->Value(),
                         nullptr, Bound::GetMaximum(intConst, isSigned));
        }
        default: {
            DebugValidator::Unreachable();
            return Range::GetUnknown(op, isSigned);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateRangeForGenericCmp(Operand* op, OrderType order,
                                                    bool isSigned, RangeTag* tag) {
    // We can't create a range for a pointer.
    if(op->IsPointer()) {
        return Range::GetUnknown();
    }

    // If the operand has a known range we make this new range
    // to be relative to it (we avoid creating a symbolic range).
    // For example, if we know that 'a < b' and 'b = [2, 5]',
    // then we can conclude that 'a = (-INF, 1]'.
    Range opRange;

    if(tag->GetRange(op, opRange)) {
        return CreateRangeForGenericCmpWithRange(op, opRange, nullptr,
                                                 false, order, isSigned);
    }

    // This case provides the smallest amount of info.
    switch(order) {
        case Order_Equal: {
            // a == b -> [b, b]
            return Range(op, op);
        }
        case Order_NotEqual: {
            // a != b -> (-INF, b - 1] U [b + 1, +INF) (anti-range)
            auto result = Range(op, -1, op, 1, false /* isRange */);
            return PatchAntiRange(result, op, isSigned);
        }
        case Order_Less: {
            // a < b - C -> (-INF, b - 1]
            return Range((Operand*)nullptr, Bound::GetMinimum(op, isSigned), op, -1);
        }
        case Order_LessOrEqual: {
            // a <= b - C -> (-INF, b]
            return Range((Operand*)nullptr, Bound::GetMinimum(op, isSigned), op, 0);
        }
        case Order_Greater: {
            // a > b - C -> [b + 1, +INF)
            return Range(op, 1, nullptr, Bound::GetMaximum(op, isSigned));
        }
        case Order_GreaterOrEqual: {
            // a >= b - C -> [b, +INF)
            return Range(op, 0, nullptr, Bound::GetMaximum(op, isSigned));
        }
        default: {
            DebugValidator::Unreachable();
            return Range::GetUnknown(op, isSigned);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateRangeForGenericCmpWithRange(Operand* op, Range opRange, 
                                                             IntConstant* intConst, 
                                                             bool negateConst, 
                                                             OrderType order,
                                                             bool isSigned) {
    // We can't combine ranges that represent pointers.
    if(op->IsPointer()) {
        return Range::GetUnknown();
    }
    
    // In this case we know the relation between two ranges,
    // but also know the range for one of the ranges too.
    // 'a < b' and 'b == [3, 7]' implies that 'a = (-INF, 2]'.
    auto type = op->GetType();
    __int64 constValue = intConst ? intConst->Value() : 0;
    if(negateConst) constValue = -constValue;

    switch(order) {
        case Order_Equal: {
            return CreateRangeForEqualCmp(opRange, constValue, op, isSigned);
        }
        case Order_NotEqual: {
            return CreateRangeForNotEqualCmp(opRange, constValue, op, isSigned);
        }
        case Order_Less:
        case Order_LessOrEqual: {
            return CreateRangeForLessCmp(opRange, constValue, op, isSigned, order);
        }
        case Order_Greater:
        case Order_GreaterOrEqual: {
            return CreateRangeForGreaterCmp(opRange, constValue, op, isSigned, order);
        }
        default: {
            DebugValidator::Unreachable();
            return Range::GetUnknown(op, isSigned);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateRangeForEqualCmp(Range &opRange, __int64 constValue, 
                                                  Operand* op, bool isSigned) {
    // 'a == [x, y] + C' -> [x + C, y + C]
    // If there is overflow when adding the constant 
    // we are conservative and presume we don't know anything.
    if(IA::AddOverflows(opRange.Low.Constant, constValue, op->GetType()) ||
       IA::AddOverflows(opRange.High.Constant, constValue, op->GetType())) {
        return Range::GetUnknown(op, isSigned);
    }
    else return Range(opRange.Low.Base, 
                      opRange.Low.Constant + constValue,
                      opRange.High.Base, 
                      opRange.High.Constant + constValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateRangeForNotEqualCmp(Range &opRange, __int64 constValue, 
                                                     Operand* op, bool isSigned) {
    // If we have an anti-range then we don't know anything.
    // This may seem strange, but consider the following anti-range:
    // 'b = (-INF, 4] U [7, +INF]'. Knowing that 'a != b' doesn't imply
    // 'a = [5, 6]' because 'b' could be 2, while 'a' could be 9,
    // and 'a != b' would be true.
    if(opRange.IsRange == false) {
        return Range::GetUnknown(op, isSigned);
    }

    // 'a != [x, x] + C' -> (-INF, x + C - 1] U [x + C + 1, +INF)
    // We need to have a single value in the range, else we
    // create an invalid new range. For example, if 'b = [2, 4]'
    // then the range '(-INF, 1] U [5, +INF)' for 'a' is invalid,
    // because it depends directly on the value of 'b'.
    if(opRange.IsSingleValue()) {
        // If there is overflow when adding the constant 
        // we are conservative and presume we don't know anything.
        if(IA::AddOverflows(opRange.Low.Constant, 
                            constValue - 1, op->GetType()) ||
                            IA::AddOverflows(opRange.High.Constant, 
                            constValue + 1, op->GetType())) {
            return Range::GetUnknown(op, isSigned);
        }

        __int64 value = opRange.Low.Constant + constValue;
        auto result = Range(opRange.Low.Base, value - 1,
                            opRange.Low.Base, value + 1, false /* isRange */);
        return PatchAntiRange(result, op, isSigned);
    }
    else return Range::GetUnknown(op, isSigned); 
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateRangeForLessCmp(Range &opRange, __int64 constValue, 
                                                 Operand* op, bool isSigned,
                                                 OrderType order) {
    // If we have an anti-range then we don't know anything.
    if(opRange.IsRange == false) {
        return Range::GetUnknown(op, isSigned);
    }

    // 'a < [x, y] + C' -> (-INF, x + C - 1]
    // 'a < (-INF, y] + C' -> (-INF, y + C - 1]
    // For <= 1 is not subtracted.
    __int64 adjust = order == Order_Less ? 1 : 0;

    if(opRange.Low.IsMinusInfinity(op, isSigned)) {
        // If the addition overflows then we are conservative
        // and presume that we don't know the range.
        if(IA::AddOverflows(opRange.High.Constant, 
                            constValue - adjust, op->GetType())) {
            return Range::GetUnknown(op, isSigned);
        }
        else {
            __int64 value = opRange.High.Constant + constValue;
            return Range(nullptr, Bound::GetMinimum(op, isSigned),
                         opRange.High.Base, value - adjust);
        }
    }
    else {
        // If the addition overflows then we are conservative
        // and presume that we don't know the range.
        if(IA::AddOverflows(opRange.Low.Constant, 
                            constValue - adjust, op->GetType())) {
            return Range::GetUnknown(op, isSigned);
        }
        else {
            __int64 value = opRange.Low.Constant + constValue;
            return Range(nullptr, Bound::GetMinimum(op, isSigned),
                         opRange.Low.Base, value - adjust);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateRangeForGreaterCmp(Range &opRange, __int64 constValue, 
                                                    Operand* op, bool isSigned, 
                                                    OrderType order) {
    // If we have an anti-range then we don't know anything.
    if(opRange.IsRange == false) {
        return Range::GetUnknown(op, isSigned);
    }

    // 'a > [x, y] + C' -> [y + C + 1, +INF)
    // 'a > [x, +INF) + C' -> [x + C + 1, +INF)
    // For >= 1 is not added.
    __int64 adjust = order == Order_Greater ? 1 : 0;

    if(opRange.High.IsPlusInfinity(op, isSigned)) {
        // If the addition overflows then we are conservative
        // and presume that we don't know the range.
        if(IA::AddOverflows(opRange.Low.Constant, 
                            constValue + adjust, op->GetType())) {
            return Range::GetUnknown(op, isSigned);
        }
        else {
            __int64 value = opRange.Low.Constant + constValue;
            return Range(opRange.Low.Base, value + adjust,
                         nullptr, Bound::GetMaximum(op, isSigned));
        }
    }
    else {
        // If the addition overflows then we are conservative
        // and presume that we don't know the range.
        if(IA::AddOverflows(opRange.High.Constant, 
                            constValue + adjust, op->GetType())) {
            return Range::GetUnknown(op, isSigned);
        }
        else {
            __int64 value = opRange.High.Constant + constValue;
            return Range(opRange.High.Base, value + adjust,
                         nullptr, Bound::GetMaximum(op, isSigned));
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateRangeFromPointerCmp(Operand* opA, Operand* opB,
                                                     OrderType order, RangeTag* tag) {
    // If we already know that the operand is not NULL
    // then we leave this information, it's what we're interested in.
    if((tag->IsNotZero(opA) == Result_Yes) &&
       (order == Order_NotEqual)) {
        return Range(-1, 1, false /* isRange */);
    }

    // Don't create a range for a 'nullptr'.
    if(opA->IsNullConstant()) {
        return Range::GetUnknown();
    }

    // First check for a comparison to a null pointer constant.
    // 'a !=/</> NULL' -> a = (-INF, -1] U [1, +INF)
    // 'a == NULL' -> a = [0, 0]
    if(opB->IsNullConstant()) {
        return CreateRangeFromPointerCmpWithNull(order);
    }

    // Check if we know something about the right operand.
    // If we know that it's definitely not NULL then we
    // may be able to tell that the left operand is not NULL too.
    auto result = tag->IsNotZero(opB);

    if(result == Result_Yes) {
        // 'a == b' -> a != NULL, if 'b != NULL'
        // 'a > b' -> a != NULL, if 'b != NULL'. This is valid
        // because pointer arithmetic doesn't overflow, so we can't get 0.
        if((order == Order_Equal) || 
           (order == Order_Greater)) {
            return Range(-1, 1, false /* isRange */);
        }
    }
    else if(result == Result_No) {
        // 'a == b' -> a == NULL, if 'b == NULL'
        // 'a != b' -> a != NULL, if 'b == NULL'
        // 'a > b' -> a != NULL, if 'b == NULL'
        // 'a < b' -> a != NULL, if 'b == NULL'
        if(order == Order_Equal) {
            return Range(0LL, 0LL);
        }
        else if((order == Order_NotEqual) ||
                (order == Order_Less)     ||
                (order == Order_Greater)) {
            return Range(-1, 1, false /* isRange */);
        }
    }
    
    // Create a symbolic range for any other case.
    // It's less precise, but better than nothing.
    return CreateSymbolicRangeFromPointerCmp(order, opB);
 }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateRangeFromPointerCmpWithNull(OrderType order) {
    switch(order) {
        case Order_Equal: {
            return Range(0LL, 0LL);
        }
        case Order_NotEqual:
        case Order_Less:
        case Order_Greater: {
            return Range(-1, 1, false /* isRange */);
        }
        default: return Range::GetUnknown();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::CreateSymbolicRangeFromPointerCmp(OrderType order, Operand* op) {
    // Create a symbolic range that represents the relation
    // between an operand and 'op' according to 'order'.
    switch(order) {
        case Order_Equal: {
            // a == b -> [b, b]
            return Range(op, op);
        }
        case Order_NotEqual: {
            // a != b -> (-INF, b - 1] U [b + 1, +INF) (anti-range)
            auto result = Range(op, -1, op, 1, false /* isRange */);
        }
        case Order_Less: {
            // a < b - C -> (-INF, b - 1]
            __int64 min = IA::GetMin(GetTarget()->GetPointerType(), false);
            return Range((Operand*)nullptr, min, op, -1);
        }
        case Order_LessOrEqual: {
            // a <= b - C -> (-INF, b]
            __int64 min = IA::GetMin(GetTarget()->GetPointerType(), false);
            return Range((Operand*)nullptr, min, op, 0);
        }
        case Order_Greater: {
            // a > b - C -> [b + 1, +INF)
            __int64 max = IA::GetMax(GetTarget()->GetPointerType(), false);
            return Range(op, 1, nullptr, max);
        }
        case Order_GreaterOrEqual: {
            // a >= b - C -> [b, +INF)
            __int64 max = IA::GetMax(GetTarget()->GetPointerType(), false);
            return Range(op, 0, nullptr, max);
        }
        default: {
            DebugValidator::Unreachable();
            return Range::GetUnknown();
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::IntersectWithKnownRange(Operand* op, Range newRange, 
                                                   RangeTag* tag, bool& oldRangeUsed) {
    // Check if there is a range computed already for this operand
    // (in it's dominator's Range Tag, for example). If there is, 
    // we combine it with the one determined here. For example, 
    // 'a = [4, +INF]' and 'a = [-INF, 7]'  are combined into 'a = [4, 7]'.
    Range oldRange;

    // If the range is for a pointer we do nothing.
    if(op->IsPointer()) {
        oldRangeUsed = false;
        return newRange;
    }

    // If the new range represents a constant we keep it.
    if((newRange.HasBaseOperand() == false) &&
       (newRange.Low.Constant == newRange.High.Constant)) {
        oldRangeUsed = false;
        return newRange;
    }

    if(tag->GetRange(op, oldRange)) {
        // If the new range involves infinity while the old one does not,
        // we keep the old one, because it's more precise.
        if(newRange.Low.IsMinusInfinity(op) ||
           newRange.High.IsPlusInfinity(op)) {
            if((oldRange.Low.IsMinusInfinity(op) == false) &&
               (oldRange.High.IsPlusInfinity(op) == false)) {
                oldRangeUsed = true;
                return oldRange;
            }

            // If the new range is symbolic while the old one
            // is not, we keep the old one.
            if(newRange.HasBaseOperand() &&
               (oldRange.HasBaseOperand() == false)) {
                oldRangeUsed = true;
                return oldRange;
            }
        }

        // If the result is an unknown range we keep the old one.
        auto result = tag->IntersectRanges(oldRange, newRange, op);

        if(result.IsUnknown(op, true)) {
            oldRangeUsed = true;
            return oldRange;
        }
        else {
            oldRangeUsed = false;
            return result;
        }
    }

    oldRangeUsed = false;
    return newRange;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::CollectAndRanges(AndInstr* instr, RangeList& list,
                                           RangeTag* tag) {
    // We collect all ranges from the comparisons that are involved in the 'and'.
    // The operands of the 'and' should be comparisons or other 'and' instructions.
    // After all ranges are collected they are sorted based on the operand,
    // and all ranges that target the same operand are combined.
    // For example, 'and (cmp gt a, 3), (cmp lt a, 9)' produces the ranges
    // 'a = [4, +INF]' and 'a = (-INF, 7]', combined 'a = [4, 7]'. 
    CollectAndRangesImpl(instr, list, tag);

    // Try to combine the ranges.
    list.Sort();

    for(int i = 0; i < list.Count(); i++) {
        // Check how many ranges involve the same operand and combine them.
        int start = i;
        int stop;
        Operand* operand = list[start].OperandInfo;

        // We consider only integer operands.
        if(operand->IsInteger() == false) {
            continue;
        }

        for(stop = i + 1; stop < list.Count(); stop++) {
            if(list[stop].OperandInfo != operand) {
                break;
            }
        }

        if(stop > (start + 1)) {
            // If there is a known range for this operand
            // use it, it may enhance the precision.
            Range result;

            if(tag->GetRange(operand, result)) {
                result = tag->IntersectRanges(result, list[start].RangeInfo, operand);
            }
            else result = list[start].RangeInfo;
            
            for(int j = start + 1; j < stop; j++) {
                result = tag->IntersectRanges(result, list[j].RangeInfo, operand);
            }

            // Remove all previous ranges and insert the combined one.
            while(stop-- > start) {
                list.RemoveAt(start);
            }

            list.Insert(start, OperandRange(operand, result));
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::CollectAndRangesImpl(AndInstr* instr, RangeList& list,
                                               RangeTag* tag) {
    // We're interested only and comparison and other 'and' instructions.
    // Other instructions introduce uncertainty, so we give up early.
    if((instr->LeftOp()->DefiningInstrIs<CmpInstrBase>() ||
        instr->LeftOp()->DefiningInstrIs<AndInstr>()) == false) {
        return;
    }

    if((instr->RightOp()->DefiningInstrIs<CmpInstrBase>() ||
        instr->RightOp()->DefiningInstrIs<AndInstr>()) == false) {
        return;
    }

    // Limit the number of ranges, it's costly to process too many,
    // and accuracy doesn't improve much.
    if(list.Count() > 16) {
        return;
    }

    // Process the left operand.
    if(auto cmpInstr = instr->LeftOp()->DefiningInstrAs<CmpInstrBase>()) {
        CreateRangeFromComparison(cmpInstr, list, tag);
    }
    else if(auto andInstr = instr->LeftOp()->DefiningInstrAs<AndInstr>()) {
        CollectAndRangesImpl(andInstr, list, tag);
    }
    else DebugValidator::Unreachable();

    // Process the right operand.
    if(auto cmpInstr = instr->RightOp()->DefiningInstrAs<CmpInstrBase>()) {
        CreateRangeFromComparison(cmpInstr, list, tag);
    }
    else if(auto andInstr = instr->RightOp()->DefiningInstrAs<AndInstr>()) {
        CollectAndRangesImpl(andInstr, list, tag);
    }
    else DebugValidator::Unreachable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::CollectOrRanges(OrInstr* instr, RangeList& list,
                                          RangeTag* tag) {
    // Try to extract a range from the comparisons that the 'or' targets.
    // We can do this only if the same operand is referred by all
    // comparisons, like in 'a == 0 || a == 1 || a == 2'.
    CollectOrRangesImpl(instr, list, tag);
    if(list.Count() == 0) return;

    bool valid = true;
    bool allPositive = true;
    bool allNotZero = true;
    bool allNegative = true;
    bool hasMinMax = true;
    __int64 minValue = std::numeric_limits<__int64>::max();
    __int64 maxValue = std::numeric_limits<__int64>::min();
    Operand* requiredOp = nullptr;

    for(int i = 0; i < list.Count(); i++) {
        if(requiredOp) {
            if(list[i].OperandInfo != requiredOp) {
                valid = false;
                break;
            }
        }
        else {
            // If the operand is not an integer we give up.
            requiredOp = list[i].OperandInfo;

            if(requiredOp->IsInteger() == false) {
                valid = false;
                break;
            }
        }

        // Check if we have a range that represents equality.
        // Even if the range is symbolic we may obtain
        // some helpful information from it.
        auto range = list[i].RangeInfo;

        if(range.IsRange == false) {
            // Check if the anti-range indicates 
            // that the operand is not zero.
            if((range.HasBaseOperand() == false) &&
                (range.Low.Constant < 0) &&
                (range.High.Constant > 0)) {
                hasMinMax = false;
                allPositive = false;
                allNegative = false;
            }
            else {
                valid = false;
                break;
            }
        }

        if(range.HasBaseOperand() == false) {
            // The range is not symbolic, take the minimum value.
            // This is generated for something like 'a == 3'.
            minValue = std::min(minValue, range.Low.Constant);
            maxValue = std::max(maxValue, range.High.Constant);
            allPositive = allPositive && (minValue >= 0);
            allNegative = allNegative && (minValue  < 0);
            allNotZero  = allNotZero  && (minValue != 0);
        }
        else if(range.IsSingleValue() &&
                (range.Low.Constant == 0) &&
                (range.High.Constant == 0)) {
            // This is generated for something like 'a == b'.
            // Check what we already know about 'b'.
            auto op = range.Low.Base;
            hasMinMax = false;

            allPositive = allPositive && tag->IsPositive(op);
            allNotZero = allNotZero && tag->IsNotZero(op);
            allNegative = allNegative && tag->IsNegative(op);
        }
        else {
            valid = false;
            return;
        }
    }

    // Try to create a range from the information we have.
    // Remove the ranges that are currently in the list.
    list.Clear();
    
    if(valid) {
        CreateOrRange(list, requiredOp, minValue, maxValue, 
                      allPositive, allNotZero, allNegative, hasMinMax);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::CreateOrRange(RangeList &list, Operand* requiredOp, 
                                        __int64 minValue, __int64 maxValue, 
                                        bool allPositive, bool allNotZero, 
                                        bool allNegative, bool hasMinMax) {
    if(hasMinMax) {
        // Create the range [MIN_VALUE, MAX_VALUE]
        list.Add(OperandRange(requiredOp, Range(minValue, maxValue)));
    }
    else if(allPositive) {
        if(allNotZero) {
            // Create the range [1, +INF).
            __int64 max = Bound::GetMaximum(requiredOp, true);
            list.Add(OperandRange(requiredOp, Range(1, max)));
        }
        else {
            // Create the range [0, +INF).
            __int64 max = Bound::GetMaximum(requiredOp, true);
            list.Add(OperandRange(requiredOp, Range(0, max)));
        }
    }
    else if(allNotZero) {
        // Create the range (-INF, -1] U [1, +INF).s
        list.Add(OperandRange(requiredOp, Range(-1, 1, true)));
    }
    else if(allNegative) {
        // Create the range (-INF, -1].
        __int64 min = Bound::GetMinimum(requiredOp, true);
        list.Add(OperandRange(requiredOp, Range(min, -1)));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::CollectOrRangesImpl(OrInstr* instr, RangeList& list,
                                              RangeTag* tag) {
    // We're interested only and comparison and other 'or' instructions.
    // Other instructions introduce uncertainty, so we give up early.
    if((instr->LeftOp()->DefiningInstrIs<CmpInstrBase>() ||
        instr->LeftOp()->DefiningInstrIs<OrInstr>()) == false) {
        return;
    }

    if((instr->RightOp()->DefiningInstrIs<CmpInstrBase>() ||
        instr->RightOp()->DefiningInstrIs<OrInstr>()) == false) {
        return;
    }

    // Limit the number of ranges, it's costly to process too many,
    // and accuracy doesn't improve much.
    if(list.Count() > 16) return;

    // Process the left operand.
    if(auto cmpInstr = instr->LeftOp()->DefiningInstrAs<CmpInstrBase>()) {
        CreateRangeFromComparison(cmpInstr, list, tag,
                                  false /* createMirrored */);
    }
    else if(auto orInstr = instr->LeftOp()->DefiningInstrAs<OrInstr>()) {
        CollectOrRangesImpl(orInstr, list, tag);
    }
    else DebugValidator::Unreachable();

    // Process the right operand.
    if(auto cmpInstr = instr->RightOp()->DefiningInstrAs<CmpInstrBase>()) {
        CreateRangeFromComparison(cmpInstr, list, tag,
                                  false /* createMirrored */);
    }
    else if(auto orInstr = instr->RightOp()->DefiningInstrAs<OrInstr>()) {
        CollectOrRangesImpl(orInstr, list, tag);
    }
    else DebugValidator::Unreachable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void EdgeEquivalenceInfo::CanonicalizeComparison(CmpInstrBase* instr) {
    if(instr->RightOp()->IsConstant()) {
        return;
    }

    // Make sure that the constant (if any) is on the right side.
    // This simplifies our work, because we have fewer cases to test for.
    bool invert = false;
    
    if(instr->LeftOp()->IsConstant()) {
        invert = true;
    }

    // If we have an 'add' or 'sub' on the left side
    // move it to the right side, but only if the right operand is a constant.
    if(auto addInstr = instr->LeftOp()->DefiningInstrAs<AddInstr>()) {
        invert = addInstr->RightOp()->IsIntConstant() &&
                 ((instr->RightOp()->DefiningInstrIs<AddInstr>() ||
                   instr->RightOp()->DefiningInstrIs<SubInstr>()) == false);
    }
    else if(auto subInstr = instr->LeftOp()->DefiningInstrAs<SubInstr>()) {
        invert = subInstr->RightOp()->IsIntConstant() &&
                 ((instr->RightOp()->DefiningInstrIs<AddInstr>() ||
                   instr->RightOp()->DefiningInstrIs<SubInstr>()) == false);
    }

    if(invert) {
        instr->InvertOrder(true /* invertOperands */, 
                           false /* invertEquality */);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range EdgeEquivalenceInfo::PatchAntiRange(Range range, Operand* op, bool isSigned) {
    DebugValidator::IsFalse(range.IsRange);
    
    // Try to convert an anti-range to a range,
    // because this representation is more precise.
    bool isMinusInf = range.Low.IsMinusInfinity(op, isSigned);
    bool isPlusInf = range.High.IsPlusInfinity(op, isSigned);

    // If we have an empty anti-range we create an unknown range.
    // Else we try to convert to a range.
    if((isMinusInf && isPlusInf) == false) {
        if(isMinusInf) {
            // (-INF, -INF) U [a, +INF) -> [a, +INF)
            return Range(range.High.Constant, 
                         Bound::GetMaximum(op, isSigned));
        }
        else if(isPlusInf) {
            // (-INF, a] U (+INF, +INF) -> (-INF, a]
            return Range(Bound::GetMinimum(op, isSigned),
                         range.Low.Constant);
        }
        else return range;
    }
    else return Range::GetUnknown(op, isSigned);
}

} // namespace Analysis