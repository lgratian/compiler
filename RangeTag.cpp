// RangeTag.cpp
// Copyright (c) Lup Gratian
//
// Implements the RangeTag class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "RangeTag.hpp"

namespace Analysis {

void SuccesorRanges::AllocateInitial(Block* block) {
    // For many successor blocks we use a dictionary,
    // it's more efficient when we search, but uses more memory.
    if(count_ > 2) {
        manySuccessors_ = new TSuccessorDict();

        // We create the required data structures now.
        for(int i = 0; i < count_; i++) {
            auto successorBlock = block->SuccessorAt(i);
            manySuccessors_->Add(successorBlock, BlockRangesPair(successorBlock));
        }
    }
    else {
        // 'if' or 'goto' case.
        // We create the required data structures now.
        for(int i = 0; i < count_; i++) {
            auto successorBlock = block->SuccessorAt(i);
            fewSuccessors_.Add(BlockRangesPair(successorBlock));
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockRangesPair& SuccesorRanges::GetRangesPair(Block* block) {
    if(manySuccessors_) {
        if(manySuccessors_->ContainsKey(block) == false) {
            // The dictionary needs to be created now.
            manySuccessors_->Add(block, BlockRangesPair(block));
            count_++;
        }

        return (*manySuccessors_)[block];
    }

    // Check if the block is in the list. 
    // If it is not, we move to a dictionary and add it there.
    for(int i = 0; i < count_; i++) {
        if(fewSuccessors_[i].RangesBlock == block) {
            return fewSuccessors_[i];
        }
    }

    // Create a dictionary and copy the ranges.
    manySuccessors_ = new TSuccessorDict();

    for(int i = 0; i < count_; i++) {
        manySuccessors_->Add(fewSuccessors_[i].RangesBlock,
                             fewSuccessors_[i]);
        fewSuccessors_[i].Ranges.Clear();
    }

    // Add the new block.
    manySuccessors_->Add(block, BlockRangesPair(block));
    count_++;
    return (*manySuccessors_)[block];
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SuccesorRanges::AddRange(Operand* op, Range range, Block* block) {
    auto& ranges = GetRangesPair(block).Ranges;
    
    for(int i = 0; i < ranges.Count(); i++) {
        if(ranges[i].OperandInfo == op) {
            ranges[i].RangeInfo = range;
            return;
        }
    }

    ranges.Add(OperandRange(op, range));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SuccesorRanges::GetRange(Operand* op, Block* block, Range& range) {
    auto& ranges = GetRangesPair(block);

    for(int i = 0; i < ranges.Ranges.Count(); i++) {
        if(ranges.Ranges[i].OperandInfo == op) {
            range = ranges.Ranges[i].RangeInfo;
            return true;
        }
    }
    
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeTag* RangeTag::GetRangeTag(Block* block, RangeTag* parent, 
                                bool isLoopHeader, bool isExitBlock) {
    DebugValidator::IsNotNull(block);
    return new RangeTag(block, parent, isLoopHeader, isExitBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RangeTag::AddRangeForSuccessor(Block* successorBlock, Operand* op, Range range) {
    DebugValidator::IsNotNull(successorBlock);
    DebugValidator::IsNotNull(op);

    succRanges_.AddRange(op, range, successorBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RangeTag::AddRange(Operand* op, Range range) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsFalse(ranges_.Contains(OperandRange(op, range)));

    ranges_.Add(OperandRange(op, range));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeTag::GetRange(Operand* op, Range& range) {
    DebugValidator::IsNotNull(op);

    // We first check if there is a valid range in this block.
    // If we can't find one we search in the immediate dominator
    // a range that is valid only for this successor, and so on.
    for(int i = 0; i < ranges_.Count(); i++) {
        if(ranges_[i].OperandInfo == op) {
            range = ranges_[i].RangeInfo;
            return true;
        }
    }

    if(parent_) {
        if(ValidToWalkUpDominatorTree()) {
            return parent_->GetRangeForSuccessor(op, block_, range);
        }
    }
    
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeTag::ValidToWalkUpDominatorTree() {
    // The block shouldn't have more than one predecessor, 
    // else the obtained range is not valid. For example, in
    // B1: if(a > 5)   B2: ...
    // B3: if(a < 2)   B4: ...
    // 'B3' is the target of the false branch of 'B1', but also
    // the unique successor of 'B2', so neither 'a = [6, +INF)'
    // nor 'a = (-INF, 4]' are valid at this point.
    if((block_->PredecessorCount() == 1) || IsLoopHeader()) {
        return true;
    }

    // An exception to all of this is when the block is a loop header,
    // or when all other predecessors are exit blocks (a block that ends
    // with a return, or there is a call to 'abort', 'exit' or 'longjmp');
    auto predEnum = block_->GetPredecessorEnum();

    while(predEnum.IsValid()) {
        auto predecessorBlock = predEnum.Next();

        if(predecessorBlock != block_) {
            auto predecessorTag = predecessorBlock->GetTag<RangeTag>();

            if((predecessorTag == nullptr) ||
               (predecessorTag->IsExitBlock() == false)) {
                return false;
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeTag::GetRangeForSuccessor(Operand* op, Block* successorBlock, Range& range) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsNotNull(successorBlock);

    if(succRanges_.GetRange(op, successorBlock, range)) {
        return true;
    }

    // Search up the dominator tree.
    return GetRange(op, range);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RangeTag::DetachedFromParent(TaggedBase* parent) {
    // Remove the tag from the children list
    // of it's immediate dominator (if it's the case).
    auto block = static_cast<Block*>(parent);

    if(parent_) {
        parent_->Children().Remove(this);
    }

    // The parent block will be removed, so we need to make
    // all children tags point to the immediate dominator's tag
    // instead of this one.
    for(int i = 0; i < children_.Count(); i++) {
        children_[i]->SetParent(parent_);
        if(parent_) parent_->Children().Add(children_[i]);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RangeTag::InstructionRemoved(Instruction* instr, Block* parent) {
    // An instruction has been removed from this block, now we need 
    // to remove it from the Range Tags too. We need to do this because 
    // the memory location could be reused for another operand.
    if(instr->HasDestinationOp() == false) return;
    auto resultOp = instr->GetDestinationOp();
    if(resultOp == nullptr) return;

    // Remove the ranges that apply to the block.
    for(int i = 0; i < ranges_.Count(); i++) {
        if(ReferesOperand(ranges_[i], resultOp)) {
            ranges_.RemoveAt(i);
            i--;
        }
    }

    // Remove the ranges that apply only to successors.
    for(int i = 0; i < parent->SuccessorCount(); i++) {
        auto successorBlock = parent->SuccessorAt(i);
        BlockRangesPair& ranges = succRanges_.GetRangesPair(successorBlock);

        for(int j = 0; j < ranges.Ranges.Count(); j++) {
            if(ReferesOperand(ranges.Ranges[j], resultOp)) {
                ranges.Ranges.RemoveAt(j);
                j--;
            }
        }
    }

    // Remove the ranges from the cache.


    // The result operand may be referenced in the dominated
    // blocks, it must be removed from there too.
    for(int i = 0; i < children_.Count(); i++) {
        auto childBlock = children_[i]->ParentBlock();
        children_[i]->InstructionRemoved(instr, childBlock);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeTag::RangeQueryResult::ResultFromValue(unsigned short value, 
                                                 RangeResult& result) {
    if(value == 3) {
        return false;
    }
    else {
        result = (RangeResult)value;
        return true;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeTag::RangeQueryResult::GetResult(RangeQueryType queryType, 
                                           RangeResult& result) {
    switch(queryType) {
        case Query_AreEqual:         return ResultFromValue(AreEqual, result);
        case Query_AreNotEqual:      return ResultFromValue(AreNotEqual, result);
        case Query_IsSmaller:        return ResultFromValue(IsSmaller, result);
        case Query_IsSmallerOrEqual: return ResultFromValue(IsSmallerOrEqual,result);
        case Query_IsGreater:        return ResultFromValue(IsGreater, result);
        case Query_IsGreaterOrEqual: return ResultFromValue(IsGreaterOrEqual,result);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RangeTag::RangeQueryResult::SetResult(RangeQueryType queryType, 
                                           RangeResult result) {
    switch(queryType) {
        case Query_AreEqual:         { AreEqual         = result; break; }       
        case Query_AreNotEqual:      { AreNotEqual      = result; break; }    
        case Query_IsSmaller:        { IsSmaller        = result; break; }      
        case Query_IsSmallerOrEqual: { IsSmallerOrEqual = result; break; }
        case Query_IsGreater:        { IsGreater        = result; break; }      
        case Query_IsGreaterOrEqual: { IsGreaterOrEqual = result; break; }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeTag::ReferesOperand(OperandRange& opRange, Operand* op) {
    return (opRange.OperandInfo == op) ||
           (opRange.RangeInfo.Low.Base == op) ||
           (opRange.RangeInfo.High.Base == op);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range RangeTag::IntersectRanges(Range a, Range b, Operand* op, bool isSigned) {
    // Test for the range/range case first, it's more common.
    if(a.IsRange && b.IsRange) {
        // If the operands are the same (or both ranges don't have operands)
        // then we can compute a precise range, but only
        // if the intersection of the ranges produce a range.
        if(HaveCommonPoint(a, b)) {
            return Range(IntersectLowBounds(a.Low, b.Low, op, isSigned),
                         IntersectHighBounds(a.High, b.High, op, isSigned));
        }
        else return Range::GetUnknown(op, isSigned);
    }

    Range range;
    Range antiRange;
    bool hasRangeAndAntirange = false;

    // If we have two anti-ranges, we prefer the one that is most restricted.
    // a = (-INF, 1] U [10, +INF)
    // b = (-INF, 2] U [5, +INF)
    // a & b = (-INF, 2) U [5, +INF)
    if((a.IsRange || b.IsRange) == false) {
        if((a.HasBaseOperand() || b.HasBaseOperand()) == false) {
            if((a.High.Constant - a.Low.Constant) <
               (b.High.Constant - b.Low.Constant)) {
                return a;
            }
            else return b;
        }
    }
    else if(a.IsRange) {
        range = a;
        antiRange = b;
        hasRangeAndAntirange = true;
    }
    else {
        range = b;
        antiRange = a;
        hasRangeAndAntirange = true;
    }

    // Check if we have a range/anti-range combination.
    if(hasRangeAndAntirange) {
        return IntersectRangeWithAntiRange(range, antiRange, op, isSigned);
    }
    else return Range::GetUnknown(op, isSigned);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range RangeTag::IntersectRangeWithAntiRange(Range range, Range antiRange, 
                                            Operand* op, bool isSigned) {
    // If the bound of the anti-range are exactly near
    // the bounds of the range we don't know anything,
    // because the ranges present a contradiction.
    // (-INF, -1] U [1, +INF) & [0, 0] -> ???
    // (a number can't be zero and not zero at the same time).
    if((antiRange.Low.Constant == range.Low.Constant - 1) &&
       (antiRange.High.Constant == range.High.Constant + 1)) {
        return Range::GetUnknown(op, isSigned);
    }

    // There are four cases we need to test for:
    // 1. The anti-range is completely within the range.
    //    In this case we take the range that is more precise.
    //    (-INF, 4] U [6, +INF) & [2, 8] -> [2, 8]
    if((IsGreater(antiRange.Low, range.Low) == Result_Yes) &&
       (IsSmaller(antiRange.High, range.High) == Result_Yes)) {
        // We take the anti-range if the range covers
        // the whole domain of the type.
        if(range.IsUnknown(op, isSigned)) return antiRange;
        else return range;
    }

    // 2. The anti-range is completely outside the range.
    //    In this case we take the range.
    //    (-INF, 2] U [8, +INF) & [4, 6] -> [4, 6]
    if((IsSmaller(antiRange.Low, range.Low) == Result_Yes) &&
       (IsGreater(antiRange.High, range.High) == Result_Yes)) {
        return range;
    }

    // 3. The anti-range intersects the range with the it's high bound.
    //    In this case we create a new range that uses
    //    the anti-range's high bound as it's low bound.
    //    (-INF, 1] U [6, +INF) & [2, 8] -> [6, 8]
    if(IsGreater(antiRange.High, range.Low) == Result_Yes) {
        return Range(antiRange.High.Base, antiRange.High.Constant,
                     range.High.Base, range.High.Constant);
    }

    // 4. The anti-range intersects the range with the it's low bound.
    //    In this case we create a new range that uses
    //    the anti-range's low bound as it's high bound.
    //    (-INF, 5] U [9, +INF) & [2, 8] -> [5, 8]
    if(IsSmaller(antiRange.Low, range.High) == Result_Yes) {
        return Range(range.Low.Base, range.Low.Constant,
                     antiRange.Low.Base, antiRange.Low.Constant);
    }

    return Range::GetUnknown(op, isSigned);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Bound RangeTag::IntersectLowBounds(Bound a, Bound b, Operand* op, bool isSigned) {
    // If the operands are the same (or both ranges don't have operands)
    // then we can compute a precise lower bound.
    if(a.Base == b.Base) {
        return Bound(a.Base, std::max(a.Constant, b.Constant));
    }

    // If one bound has an operand, and the other one not,
    // we prefer to keep the one without the operand.
    if(a.Base && (b.Base == nullptr)) {
        return Bound(nullptr, b.Constant);
    }
    else if(b.Base && (a.Base == nullptr)) {
        return Bound(nullptr, a.Constant);
    }

    // For all other cases we need to be conservative.
    return Bound::GetMinusInfinity(op, isSigned);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Bound RangeTag::IntersectHighBounds(Bound a, Bound b, Operand* op, bool isSigned) {
    // If the operands are the same (or both ranges don't have operands)
    // then we can compute a precise higher bound.
    if(a.Base == b.Base) {
        return Bound(a.Base, std::min(a.Constant, b.Constant));
    }

    // If one bound has an operand, and the other one not,
    // we prefer to keep the one without the operand.
    if(a.Base && (b.Base == nullptr)) {
        return Bound(nullptr, b.Constant);
    }
    else if(b.Base && (a.Base == nullptr)) {
        return Bound(nullptr, a.Constant);
    }

    // For all other cases we need to be conservative.
    return Bound::GetPlusInfinity(op, isSigned);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Range Range::Invert(Operand* op, bool isSigned) const {
    DebugValidator::IsNotNull(op);

    // In most cases a range can be inverted, but anti-ranges not.
    // [3, 6] -> (-INF, 2] U [7, +INF)
    auto type = op->GetType();

    // For p0onters we can have only two ranges,
    // one that represents '!= NULL' (an anti-range),
    // and one that represents '== NULL' (the range [0, 0]).
    if(op->IsPointer()) {
        if(IsRange) {
            return Range(-1, 1, false /* isRange */);
        }
        else return Range(0LL, 0LL, true /* isRange */);
    }

    if(IsRange) {
        // [1, +INF) -> [0, 0] for unsigned numbers
        if((isSigned == false) && High.IsPlusInfinity(op, false) &&
           (Low.HasBaseOperand() == false) && (Low.Constant == 1)) {
            return Range(0LL, 0LL, true /* isRange */);
        }

        // (-INF, b] -> [b + 1, +INF)
        if(Low.IsMinusInfinity(op, isSigned)) {
            if(IA::AddOverflows(High.Constant, 1, type)) {
                return GetUnknown(op, isSigned);
            }
            else return Range(High.Base, High.Constant + 1,
                              nullptr, Bound::GetMaximum(op, isSigned));
        }
        
        // [a, +INF) -> (-INF, a - 1]
        if(High.IsPlusInfinity(op, isSigned)) {
            if((Low.Constant == 0) || 
               IA::SubOverflows(Low.Constant, 1, type)) {
                return GetUnknown(op, isSigned);
            }
            else return Range(nullptr, Bound::GetMinimum(op, isSigned),
                              Low.Base, Low.Constant - 1);
        }

        // [a, b] -> (-INF, a - 1] U [b + 1, +INF)
        // An anti-range needs to be created for this case.
        if((Low.Constant == 0) || 
           IA::SubOverflows(Low.Constant, 1, type) ||
           IA::AddOverflows(High.Constant, 1, type)) {
            return GetUnknown(op, isSigned);
        }
        else if((isSigned == false) && ((Low.Constant - 1) < 0)) {
            // For unsigned numbers a negative range makes no sense,
            // it might pessimize other optimizations.
            return Range(0, High.Constant + 1);
        }
        else {
            // Create an anti-range.
            return Range(Low.Base, Low.Constant - 1,
                         High.Base, High.Constant + 1, false /* isRange */);
        }
    }
    else if(HasBaseOperand() == false &&
            ((High.Constant - Low.Constant) == 2)) {
        // (-INF, a - 1] U [a + 1, +INF) -> [a, a]
        if(IA::AddOverflows(Low.Constant, 1, type) ||
           IA::SubOverflows(High.Constant, 1, type)) {
            return GetUnknown(op, isSigned);
        }
        else return Range(Low.Constant + 1, High.Constant - 1);
    }
    
    // For all other cases we need to be conservative.
    return GetUnknown(op, isSigned);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeTag::HaveCommonPoint(Range a, Range b) {
    // The ranges have a common point if their intersection
    // produces a range (i.e the high bound of the first range
    // is not smaller than the low bound of the second range,
    // and the low bound of the first range is not larger
    // than the high bound of the second range).
    return (IsSmaller(a.Low, b.High) == Result_Yes) &&
           (IsGreater(a.High, b.Low) == Result_Yes);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsSmaller(Bound a, Bound b, bool checkReverse, int level) {
    if(level > MAX_QUERY_LEVEL) {
        return Result_Maybe;
    }

    // If the base operand is the same we can directly compare 
    // the constants. Else we require the bounds to be completely symbolic
    // (i.e the constants are zero), and compare the base operands only.
    if((a.Base || b.Base) == false) {
        if(a.Constant < b.Constant) {
            return Result_Yes;
        }
        else return Result_No;
    }
    else if(a.Base && b.Base &&
            (a.Constant == 0) && (b.Constant == 0)) {
        if(IsSmaller(a.Base, b.Base, true, 
                     false, level + 1) == Result_Yes) {
            return Result_Yes;
        }
        else if(checkReverse && 
                (IsGreater(a.Base, b.Base, true, 
                           false, level + 1) == Result_Yes)) {
            return Result_No;
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsGreater(Bound a, Bound b, bool checkReverse, int level) {
    if(level > MAX_QUERY_LEVEL) {
        return Result_Maybe;
    }

    // If the base operand is the same we can directly compare 
    // the constants. Else we require the bounds to be completely symbolic
    // (i.e the constants are zero), and compare the base operands only.
    if((a.Base || b.Base) == false) {
        if(a.Constant > b.Constant) {
            return Result_Yes;
        }
        else return Result_No;
    }
    else if(a.Base && b.Base &&
            (a.Constant == 0) && (b.Constant == 0)) {
        if(IsGreater(a.Base, b.Base, true, 
                     false, level + 1) == Result_Yes) {
            return Result_Yes;
        }
        else if(checkReverse && 
                (IsSmaller(a.Base, b.Base, true, 
                           false, level + 1) == Result_Yes)) {
            return Result_No;
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::AreEqual(Operand* a, Operand* b, bool checkReverse, int level) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    
    if(a->IsIntConstant() && b->IsIntConstant()) {
        return CompareConstants(a, b, Order_Equal);
    }

    // Try to retrieve the result from the cache.
    RangeResult result;

    if(ResultFromCache(a, b, Query_AreEqual, result)) {
        return result;
    }

    // If one of the operands is a 'phi' or 'load' we give up,
    // because we may end in a cycle.
    if(IsUncertainOperand(a) || IsUncertainOperand(b)) {
        return Result_Maybe;
    }

    // The most common case is when an operand is compared with a constant.
    // This is also the case when we can give more precise results.
    IntConstant* intConst = nullptr;
    Operand* other = nullptr;

    if(intConst = b->As<IntConstant>()) {
        other = a;
    }
    else if(intConst = a->As<IntConstant>()) {
        other = b;
    }

    if(intConst) {
        result = IsEqualRangeWithConst(other, intConst, 
                                       checkReverse, level);
    }
    else result = IsEqualRangeWithRange(a, b, checkReverse, level);

    AddResultToCache(a, b, Query_AreEqual, result);
    return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsEqualRangeWithConst(Operand* other, IntConstant* intConst,
                                            bool checkReverse, int level) {
    if(level > MAX_QUERY_LEVEL) {
        return Result_Maybe;
    }

    // We should have a known range for the other operand, else we give up.
    Range range;

    if(GetRange(other, range) == false) {
        return Result_Maybe;
    }

    // If we have a range we have equality only if the range 
    // is not symbolic and both bounds are equal to the constant.
    if(range.IsRange) {
        if(range.HasBaseOperand() == false) {
            // [C, C] == C -> Yes
            if((range.Low.Constant == intConst->Value()) &&
               (range.High.Constant == intConst->Value())) {
                return Result_Yes;
            }
        }

        // We don't know if the range is equal, but we may know
        // that it's definitely not equal if the low bound
        // is larger than the constant, or if the high bound
        // is smaller.
        Bound constBound(intConst->Value());

        if(checkReverse) {
            if((IsGreater(range.Low, constBound, 
                          false, level + 1) == Result_Yes) ||
               (IsSmaller(range.High, constBound, 
                          false, level + 1)  == Result_Yes)) {
                return Result_No;
            }
        }
    }
    else if(checkReverse) {
        // For an anti-range the only result we can get
        // is the fact that we definitely don't have equality.
        // (-INF, 2] U [5, +INF) == 3 -> No
        Bound constBound(intConst->Value());

        if((IsSmaller(range.Low, constBound, 
                      false, level + 1) == Result_Yes) &&
           (IsGreater(range.High, constBound, 
                      false, level + 1) == Result_Yes)) {
            return Result_No;
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsEqualRangeWithRange(Operand* a, Operand* b, 
                                            bool checkReverse, int level) {
    if(level > MAX_QUERY_LEVEL) {
        return Result_Maybe;
    }

    // We should have a known range for at least one
    // of the operands, else we give up.
    Range rangeA;
    Range rangeB;
    bool hasRangeA = GetRange(a, rangeA);
    bool hasRangeB = GetRange(b, rangeB);

    if((hasRangeA || hasRangeB) == false) {
        return Result_Maybe;
    }

    // Test for the case when we have both ranges.
    if((hasRangeA && hasRangeB)) {
         // If both operands have ranges we test for the following cases:
        if(rangeA.IsRange && rangeB.IsRange) {
            // 1. a = [x + C, x + C], b = [x + C, x + C] -> Yes
            if((rangeA.Low.Base == rangeA.High.Base) &&
               (rangeB.Low.Base == rangeB.High.Base) &&
               (rangeA.Low.Base == rangeB.Low.Base) &&
               (rangeA.Low.Constant == rangeA.High.Constant) &&
               (rangeB.Low.Constant == rangeB.High.Constant) &&
               (rangeA.Low.Constant == rangeB.Low.Constant)) {
                return Result_Yes;
            }

            // 2. a = [x, y], b = [z, t] -> No
            // if (y < z) or (x > t)
            // This tests that the ranges are disjoint.
            if(checkReverse) {
                if((IsSmaller(rangeA.High, rangeB.Low,
                              false, level + 1) == Result_Yes) ||
                   (IsGreater(rangeA.Low, rangeB.High, 
                              false, level + 1) == Result_Yes)) {
                    return Result_No;
                }
            }
        }
        else if(checkReverse) {
            // We have a range/anti-range combination.
            // If the range is completely within the region
            // not covered by the anti-range then we can't have equality.
            // a = (-INF, 2] U [8, +INF), b = [4, 6] -> No
            Range antiRange = rangeA.IsRange ? rangeB : rangeA;
            Range range = rangeA.IsRange ? rangeA : rangeB;

            if((IsGreater(range.Low, antiRange.Low, 
                          false, level + 1) == Result_Yes) &&
               (IsSmaller(range.High, antiRange.High, 
                          false, level + 1) == Result_Yes)) {
                return Result_No;
            }
        }
    }

    // Check if one of the ranges is based on the other operand.
    Operand* other = nullptr;
    Range range;

    if(hasRangeA && (rangeA.Low.Base == b) && 
                    (rangeA.High.Base == b)) {
        other = b;
        range = rangeA;
    }
    else if(hasRangeB && (rangeB.Low.Base == a) && 
                         (rangeB.High.Base == a)) {
        other = a;
        range = rangeB;
    }

    if(other == nullptr) {
        return Result_Maybe;
    }

    // We test for the following cases:
    if(range.IsRange) {
        if((range.Low.Base == other) &&
           (range.High.Base == other)) {
            // 1. a = [b, b], a == b -> Yes
            if((range.Low.Constant == 0) &&
               (range.High.Constant == 0)) {
                return Result_Yes;
            }

            // 2. a = [b + C1, b + C2], a == b -> No
            // This is valid only if both C1 and C2 are not zero,
            // and their sign is the same, meaning that the interval
            // doesn't contain the value 'b + 0'.
            if(checkReverse &&
               (range.Low.Constant != 0) &&
               (range.High.Constant != 0) &&
               (IA::GetSignBit(range.Low.Constant, IR_Int64) ==
                IA::GetSignBit(range.High.Constant, IR_Int64))) {
                return Result_No;
            }
        }

        if(auto addInstr = other->DefiningInstrAs<AddInstr>()) {
            auto intConst = addInstr->RightOp()->As<IntConstant>();
            if((intConst == nullptr) || intConst->IsZero()) {
                return Result_Maybe;
            }

            if(((range.Low.Base == addInstr->LeftOp()) &&
                (range.High.Base == addInstr->LeftOp())) == false) {
                return Result_Maybe;
            }
                        
            // 3. a = [b + C, b + C], a == b + C -> Yes
            if((range.Low.Constant == intConst->Value()) &&
               (range.High.Constant == intConst->Value())) {
                return Result_Yes;
            }
                        
            // 4. a = [b + C1, b + C2], a == b + C3 -> No
            // This can be done only if C1 < C3 and C2 < C3,
            // or C1 > C3 and C2 > C3.
            if(checkReverse &&
               ((range.Low.Constant < intConst->Value()) &&
                (range.High.Constant < intConst->Value())) ||
               ((range.Low.Constant < intConst->Value()) &&
                (range.High.Constant < intConst->Value()))) {
                return Result_No;
            }
        }
    }
    else if(checkReverse) {
        // For anti-ranges we test the following cases:
        // 1. a = (-INF, b - C1] U [b + C2, +INF), a == b -> No
        // if both constant are not zero.
        if((range.Low.Base == other) &&
           (range.High.Base == other) &&
           (range.Low.Constant < 0) &&
           (range.High.Constant > 0)) {
            return Result_No;
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::AreNotEqual(Operand* a, Operand* b, bool checkReverse, int level) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    
    if(a->IsIntConstant() && b->IsIntConstant()) {
        return CompareConstants(a, b, Order_NotEqual);
    }

    // Try to retrieve the result from the cache.
    RangeResult result;

    if(ResultFromCache(a, b, Query_AreNotEqual, result)) {
        return result;
    }

    // The most common case is when an operand is compared with a constant.
    // This is also the case when we can give more precise results.
    IntConstant* intConst = nullptr;
    Operand* other = nullptr;

    if(intConst = b->As<IntConstant>()) {
        other = a;
    }
    else if(intConst = a->As<IntConstant>()) {
        other = b;
    }

    // We might have a non-null pointer check.
    if(other && intConst) {
        if(other->IsPointer() && intConst->IsZeroInt()) {
            return IsPointerNotNull(other);
        }
    }

    // If one of the operands is a 'phi' or 'load' we give up,
    // because we may end in a cycle.
    if(IsUncertainOperand(a) || IsUncertainOperand(b)) {
        return Result_Maybe;
    }

    if(intConst) {
        result = IsNotEqualRangeWithConst(other, intConst, 
                                          checkReverse, level);
    }
    else result = IsNotEqualRangeWithRange(a, b, checkReverse, level);

    AddResultToCache(a, b, Query_AreNotEqual, result);
    return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsPointerNotNull(Operand* pointerOp) {
    // We should have a known range for the other operand, else we give up.
    Range range;

    if(GetRange(pointerOp, range) == false) {
        return Result_Maybe;
    }

    if((range.IsRange == false) && (range.HasBaseOperand() == false)) {
        if((range.Low.Constant == -1) &&
           (range.High.Constant == 1)) {
            return Result_Yes;
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsNotEqualRangeWithConst(Operand* other, IntConstant* intConst,
                                               bool checkReverse, int level) {
    if(level > MAX_QUERY_LEVEL) {
        return Result_Maybe;
    }

    // We should have a known range for the other operand, else we give up.
    Range range;

    if(GetRange(other, range) == false) {
        return Result_Maybe;
    }

    if(range.IsRange == false) {
        // An anti-range is definitely not equal
        // if the constant falls in the range that is not covered.
        // (-INF, 2] U [5, +INF) != 3 -> Yes
        Bound constBound(intConst->Value());
        
        if((IsSmaller(range.Low, constBound, 
                      false, level + 1) == Result_Yes) &&
           (IsGreater(range.High, constBound,
                      false, level + 1) == Result_Yes)) {
            return Result_Yes;
        }
    }
    else {
        // A range is definitely not equal if it's completely
        // below or above the constant.
        // [2, 4] != 8 -> Yes
        // [2, 4] != 0 -> Yes
        Bound constBound(intConst->Value());

        if((IsSmaller(range.High, constBound, 
                      false, level + 1) == Result_Yes) ||
           (IsGreater(range.Low, constBound, 
                      false, level + 1) == Result_Yes)) {
            return Result_Yes;
        }

        // If the range is exactly the constant
        // then the response is definitely 'No'.
        if(checkReverse &&
           (range.HasBaseOperand() == false) &&
           (range.Low.Constant == intConst->Value()) &&
           (range.High.Constant == intConst->Value())) {
            return Result_No;
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsNotEqualRangeWithRange(Operand* a, Operand* b,
                                               bool checkReverse, int level) {
    if(level > MAX_QUERY_LEVEL) {
        return Result_Maybe;
    }

    // We should have a known range for at least one
    // of the operands, else we give up.
    Range rangeA;
    Range rangeB;
    bool hasRangeA = GetRange(a, rangeA);
    bool hasRangeB = GetRange(b, rangeB);

    if((hasRangeA || hasRangeB) == false) {
        return Result_Maybe;
    }
    
    if(hasRangeA && hasRangeB) {
        if(rangeA.IsRange && rangeB.IsRange) {
            // A range is definitely not equal to another range if they are
            // disjoint (one completely above or below the other one).
            // [2, 5] != [8, 9] -> Yes
            if(HaveCommonPoint(rangeA, rangeB) == false) {
                return Result_Yes;
            }

            // Two ranges are definitely equal if they represent
            // equality (i.e the low and high bounds are the same)
            // [2, 2] != [2, 2] -> No
            if(checkReverse &&
               rangeA.IsSingleValue() && 
               rangeB.IsSingleValue() &&
               (rangeA.Low.Base == rangeB.Low.Base) &&
               (rangeA.Low.Constant == rangeB.Low.Constant)) {
                return Result_No;
            }
        }
        else {
            // We have range/anti-range combination.
            Range antiRange = rangeA.IsRange ? rangeB : rangeA;
            Range range = rangeA.IsRange ? rangeA : rangeB;

            // If the range is completely within the region
            // that is not covered by the anti-range then we know
            // that 'not equal' can't be true.
            // a = (-INF, 2] U [8, +INF), b = [4, 6] -> Yes
            if((IsGreater(range.Low, antiRange.Low, 
                          false, level + 1) == Result_Yes) &&
               (IsSmaller(range.High, antiRange.High, 
                          false, level + 1) == Result_Yes)) {
                return Result_Yes;
            }
        }
    }
    else {
        // One operand doesn't have a known range.
        auto other = hasRangeA ? b : a;
        Range range = hasRangeA ? rangeA : rangeB;

        if(range.IsRange) {
            // a = [b + C1, b + C2], a != b -> Yes,
            // if both C1 and C2 are not zero, and their sign is the same.s
            if((range.Low.Base == other) &&
               (range.High.Base == other) &&
               (range.Low.Constant != 0) &&
               (range.High.Constant != 0) &&
               (IA::GetSignBit(range.Low.Constant, IR_Int64) ==
                IA::GetSignBit(range.High.Constant, IR_Int64))) {
                return Result_Yes;
            }

            // a = [b, b], a != b -> No
            if(checkReverse &&
               range.IsSingleValue() &&
               (range.Low.Base == other) &&
               (range.Low.Constant == 0)) {
                return Result_No;
            }
        }
        else {
            // An anti-range is definitely not equal to an operand
            // if the operand if present in the bounds and
            // the low constant is negative, while the high one is positive.
            // a = (-INF, b - C1] U [b + C2, +INF), a != b -> Yes
            if((range.Low.Base == other) &&
               (range.High.Base == other) &&
               (range.Low.Constant < 0) &&
               (range.High.Constant > 0)) {
                return Result_Yes;
            }
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsSmaller(Operand* a, Operand* b, bool isSigned,
                                bool checkReverse, int level) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    
    if(a->IsIntConstant() && b->IsIntConstant()) {
        return CompareConstants(a, b, Order_Less, isSigned);
    }

    // Try to retrieve the result from the cache.
    RangeResult result;
    bool hasResult = false;

    if(ResultFromCache(a, b, Query_IsSmaller, result)) {
        return result;
    }

    if(auto phiInstrA = a->DefiningInstrAs<PhiInstr>()) {
        result = ComparePhiRanges(phiInstrA, b, isSigned, checkReverse, 
                                  level, &RangeTag::IsSmaller, false);
    }
    else if(auto phiInstrB = b->DefiningInstrAs<PhiInstr>()) {
        result = ComparePhiRanges(phiInstrB, a, isSigned, checkReverse, 
                                  level, &RangeTag::IsSmaller, true);
    }
    else if(auto questInstrA = a->DefiningInstrAs<QuestionInstr>()) {
        result = CompareQuestRanges(questInstrA, b, isSigned, checkReverse, 
                                    level, &RangeTag::IsSmaller, false);
    }
    else if(auto questInstrB = b->DefiningInstrAs<QuestionInstr>()) {
        result = CompareQuestRanges(questInstrB, a, isSigned, checkReverse, 
                                    level, &RangeTag::IsSmaller, true);
    }

    if(hasResult) {
        AddResultToCache(a, b, Query_IsSmaller, result);
        return result;
    }

    // If one of the operands 'load' we give up, because we don't know
    // anything sure about the loaded value.
    if(IsUncertainOperand(a) || IsUncertainOperand(b)) {
        return Result_Maybe;
    }

    // Constants are treated separately, because they are the most
    // common case and we can obtain better results for them.
    if(auto intConstB = b->As<IntConstant>()) {
        result = IsSmallerRangeWithConst(a, intConstB, isSigned, 
                                         checkReverse, level);
    }
    else if(auto intConstA = a->As<IntConstant>()) {
        result = IsGreaterRangeWithConst(b, intConstA, isSigned, 
                                         false, level);
    }
    else result = IsSmallerRangeWithRange(a, b, isSigned, 
                                          checkReverse, level);

    AddResultToCache(a, b, Query_IsSmaller, result);
    return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsSmallerRangeWithConst(Operand* op, IntConstant* intConst,
                                              bool isSigned, bool checkReverse,
                                              int level) {
    if(level > MAX_QUERY_LEVEL) return Result_Maybe;

    // Check if there is a known range for the operand.
    Range range;

    if(GetRange(op, range) == false) {
        return Result_Maybe;
    }

    // Anti-ranges can't respond to 'smaller' queries.
    if(range.IsRange == false) {
        return Result_Maybe;
    }

    // Any number in the range is smaller than the constant
    // if the high bound is smaller than the constant.
    auto intKind = intConst->GetType()->As<IntegerType>()->GetSubtype();
    __int64 value = intConst->Value();

    // If the range is not symbolic we can answer the query faster.
    if(range.High.HasBaseOperand() == false) {
        // If the range extends to +INF we don't know anything.
        if(range.High.IsPlusInfinity(op, isSigned) == false) {
            if(isSigned) {
                if(IA::IsSmaller(range.High.Constant, value, intKind)) {
                    return Result_Yes;
                }
            }
            else if(IA::IsSmallerUnsigned(range.High.Constant, value, intKind)) {
                return Result_Yes;
            }
        }
    }
    else {
        // The range is symbolic. In this case we need to compare
        // the operand of the high bound with the constant, properly
        // adjusted if a constant is also involved in the high bound.
        // 'a = [x, y + C1]', 'a < C2' -> 'y < C2 - C1'
        if(range.High.Constant != 0) {
            auto highConst = GetIntConstant(range.High.Constant, op);

            // If there is overflow we give up.
            if(IA::SubOverflows(intConst, highConst)) {
                return Result_Maybe;
            }
            else intConst = GetIntConstant(IA::Sub(intConst, highConst), op);
        }

        return IsSmallerRangeWithConst(range.High.Base, intConst, 
                                       isSigned, checkReverse, level + 1);
    }

    // If we don't know that the range is smaller than the constant,
    // we may know that it definitely can't be smaller by checking
    // if the low bound bound is larger than the constant.
    // This is equivalent with testing that the range is greater.
    if(checkReverse &&
       (IsGreaterRangeWithConst(op, intConst, isSigned, 
                                false, level + 1) == Result_Yes)) {
        return Result_No;
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsSmallerRangeWithRange(Operand* a, Operand* b, 
                                              bool isSigned, bool checkReverse,
                                              int level) {
    if(level > MAX_QUERY_LEVEL) {
        return Result_Maybe;
    }

    // We should have a known range for at least one
    // of the operands, else we give up.
    Range rangeA;
    Range rangeB;
    bool hasRangeA = GetRange(a, rangeA);
    bool hasRangeB = GetRange(b, rangeB);

    if((hasRangeA || hasRangeB) == false) {
        return Result_Maybe;
    }

    if(hasRangeA && hasRangeB) {
        // A range is smaller than another one if it's high bound
        // is below the low bound of the other range.
        // [2, 4] < [6, 8] -> Yes
        if(IsSmaller(rangeA.High, rangeB.Low, 
                     false, level + 1) == Result_Yes) {
            return Result_Yes;
        }

        // The range is definitely larger if the low bound
        // is above the high bound of the other range.
        // [6, 8] < [2, 4] -> No
        if(checkReverse && 
           IsGreater(rangeA.Low, rangeB.High, 
                     false, level + 1) == Result_Yes) {
            return Result_No;
        }
    }

    // Check if one of the ranges is based on the other operand.
    Operand* other = nullptr;
    Range range;

    if(hasRangeA && 
       ((rangeA.Low.Base == b) ||
         rangeA.Low.IsMinusInfinity(b, isSigned)) &&
       ((rangeA.High.Base == b) || 
         rangeA.High.IsPlusInfinity(b, isSigned))) {
        other = b;
        range = rangeA;
    }
    else if(hasRangeB &&
           ((rangeB.Low.Base == a) || 
             rangeB.Low.IsMinusInfinity(a, isSigned)) 
             &&
           ((rangeB.High.Base == a) || 
             rangeB.High.IsPlusInfinity(a, isSigned))) {
        other = a;
        range = rangeB;
    }

    if(other == nullptr) return Result_Maybe;

    // We test for the following cases:
    // 1. a = [b + C1, b + C2], a < b -> No (1) / Yes (2)
    // if both C1 and C2 are greater than zero (1)
    // if both C1 and C2 are smaller than zero (2)
    if((range.Low.Base == other) &&
       (range.High.Base == other)) {
        if(checkReverse &&
           (range.Low.Constant > 0) &&
           (range.High.Constant > 0)) {
            return Result_No;
        }
        else if((range.Low.Constant < 0) &&
                (range.High.Constant < 0)) {
            return Result_Yes;
        }
    }

    // 2. a = (-INF, b + C], a < b -> Yes, if C < 0
    if((range.High.Base == other) &&
       range.Low.IsMinusInfinity(other, isSigned) &&
       (range.High.Constant < 0)) {
        return Result_Yes;
    }

    // 3. a = [b + C, +INF), a < b -> No, if C > 0
    if(checkReverse &&
       (range.Low.Base == other) &&
       (range.Low.Constant > 0) &&
       range.High.IsPlusInfinity(other, isSigned)) {
        return Result_No;
    }

    // 4. a = [b + C1, b + C2], a < b + C3 -> No (1) / Yes (2)
    // if both C1-C3 and C2-C3 are greater than zero (1)
    // if both C1-C3 and C2-C3 are smaller than zero (2)
    // and signed overflow is undefined (1)(2)
    if(auto addInstr = other->DefiningInstrAs<AddInstr>()) {
        if(addInstr->HasUndefinedOverflow() &&
           addInstr->RightOp()->IsIntConstant() &&
           (range.Low.Base == other) &&
           (range.High.Base == other)) {
            __int64 value = addInstr->RightOp()->As<IntConstant>()->Value();

            if(checkReverse &&
               ((range.Low.Constant - value) > 0) &&
               ((range.High.Constant - value) > 0)) {
                return Result_No;
            }
            else if(((range.Low.Constant - value) < 0) &&
                    ((range.High.Constant - value) < 0)) {
                return Result_Yes;
            }
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsSmallerOrEqual(Operand* a, Operand* b, bool isSigned, 
                                       bool checkReverse, int level) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    
    if(a->IsIntConstant() && b->IsIntConstant()) {
        return CompareConstants(a, b, Order_LessOrEqual, isSigned);
    }

    // Try to retrieve the result from the cache.
    RangeResult result;
    bool hasResult = false;

    if(ResultFromCache(a, b, Query_IsSmallerOrEqual, result)) {
        return result;
    }

    if(auto phiInstrA = a->DefiningInstrAs<PhiInstr>()) {
        hasResult = true;
        result = ComparePhiRanges(phiInstrA, b, isSigned, checkReverse, 
                                  level, &RangeTag::IsSmallerOrEqual, false);
    }
    else if(auto phiInstrB = b->DefiningInstrAs<PhiInstr>()) {
        hasResult = true;
        result = ComparePhiRanges(phiInstrB, a, isSigned, checkReverse, 
                                  level, &RangeTag::IsSmallerOrEqual, true);
    }
    else if(auto questInstrA = a->DefiningInstrAs<QuestionInstr>()) {
        hasResult = true;
        result = CompareQuestRanges(questInstrA, b, isSigned, checkReverse, 
                                    level, &RangeTag::IsSmallerOrEqual, false);
    }
    else if(auto questInstrB = b->DefiningInstrAs<QuestionInstr>()) {
        hasResult = true;
        result = CompareQuestRanges(questInstrB, a, isSigned, checkReverse, 
                                    level, &RangeTag::IsSmallerOrEqual, true);
    }

    if(hasResult) {
        AddResultToCache(a, b, Query_IsSmallerOrEqual, result);
        return result;
    }

    // If one of the operands 'load' we give up, because we don't know
    // anything sure about the loaded value.
    if(IsUncertainOperand(a) || IsUncertainOperand(b)) {
        return Result_Maybe;
    }

    // Constants are treated separately, because they are the most
    // common case and we can obtain better results for them.
    // If we have a constant we do the 'smaller or equal' test,
    // else we do the more conservative 'smaller' test.
    if(auto intConstB = b->As<IntConstant>()) {
        // [a, b] <= C -> [a, b] < C + 1
        auto oneConst = GetIntConstant(1, intConstB);

        if(IA::AddOverflows(intConstB, oneConst) == false) {
            auto newConst = GetIntConstant(IA::Add(intConstB, oneConst), intConstB);
            result = IsSmallerRangeWithConst(a, newConst, isSigned, 
                                             checkReverse, level);
        }
        else result = IsSmallerRangeWithConst(a, intConstB, isSigned, 
                                              checkReverse, level);
    }
    else if(auto intConstA = a->As<IntConstant>()) {
        // C <= [a, b] -> C - 1 < [a, b]
        auto oneConst = GetIntConstant(1, intConstA);

        if((intConstA->IsZeroInt() == false) &&
           (IA::SubOverflows(intConstA, oneConst) == false)) {
            auto newConst = GetIntConstant(IA::Sub(intConstA, oneConst), intConstA);
            result = IsGreaterRangeWithConst(a, newConst, isSigned, 
                                             false, level);
        }
        else result = IsGreaterRangeWithConst(b, intConstA, isSigned, 
                                              false, level);
    }
    else if(IsSmallerRangeWithRange(a, b, isSigned) == Result_Yes) {
        // This response is conservative, but a 'No' response
        // is not because it doesn't include the <= case.
        result = Result_Yes;
    }
    else result = Result_Maybe;

    AddResultToCache(a, b, Query_IsSmallerOrEqual, result);
    return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsGreater(Operand* a, Operand* b, bool isSigned, 
                                bool checkReverse, int level) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    
    if(a->IsIntConstant() && b->IsIntConstant()) {
        return CompareConstants(a, b, Order_Greater, isSigned);
    }

    // Try to retrieve the result from the cache.
    RangeResult result;
    bool hasResult = false;
    
    if(ResultFromCache(a, b, Query_IsGreater, result)) {
        return result;
    }

    if(auto phiInstrA = a->DefiningInstrAs<PhiInstr>()) {
        hasResult = true;
        result = ComparePhiRanges(phiInstrA, b, isSigned, checkReverse, 
                                  level, &RangeTag::IsGreater, false);
    }
    else if(auto phiInstrB = b->DefiningInstrAs<PhiInstr>()) {
        hasResult = true;
        result = ComparePhiRanges(phiInstrB, a, isSigned, checkReverse, 
                                  level, &RangeTag::IsGreater, true);
    }
    else if(auto questInstrA = a->DefiningInstrAs<QuestionInstr>()) {
        hasResult = true;
        result = CompareQuestRanges(questInstrA, b, isSigned, checkReverse, 
                                    level, &RangeTag::IsGreater, false);
    }
    else if(auto questInstrB = b->DefiningInstrAs<QuestionInstr>()) {
        hasResult = true;
        result = CompareQuestRanges(questInstrB, a, isSigned, checkReverse, 
                                    level, &RangeTag::IsGreater, true);
    }

    if(hasResult) {
        AddResultToCache(a, b, Query_IsGreater, result);
        return result;
    }

    // If one of the operands 'load' we give up, because we don't know
    // anything sure about the loaded value.
    if(IsUncertainOperand(a) || IsUncertainOperand(b)) {
        return Result_Maybe;
    }

    // Constants are treated separately, because they are the most
    // common case and we can obtain better results for them.
    if(auto intConstB = b->As<IntConstant>()) {
        result = IsGreaterRangeWithConst(a, intConstB, isSigned, 
                                         checkReverse, level);
    }
    else if(auto intConstA = a->As<IntConstant>()) {
        result = IsSmallerRangeWithConst(b, intConstA, isSigned, 
                                         checkReverse, level);
    }
    else {
        result = IsGreaterRangeWithRange(a, b, isSigned, 
                                         checkReverse, level);
    }

    AddResultToCache(a, b, Query_IsGreater, result);
    return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsGreaterRangeWithConst(Operand* op, IntConstant* intConst, 
                                              bool isSigned, bool checkReverse,
                                              int level) {
    if(level > MAX_QUERY_LEVEL) {
        return Result_Maybe;
    }

    // Check if there is a known range for the operand.
    Range range;
    
    if(GetRange(op, range) == false) {
        return Result_Maybe;
    }

    // Anti-ranges can't respond to 'greater' queries.
    if(range.IsRange == false) {
        return Result_Maybe;
    }
    // Any number in the range is greater than the constant
    // if the low bound is greater than the constant.
    auto intKind = intConst->GetType()->As<IntegerType>()->GetSubtype();
    __int64 value = intConst->Value();

    // If the range is not symbolic we can answer the query faster.
    if(range.Low.HasBaseOperand() == false) {
        // If the range doesn't extend to -INF we can answer 'with Yes'.
        // a = [2, 5], b = 1, a > b = Yes
        if(range.Low.IsMinusInfinity(op, isSigned) == false) {
            if(isSigned) {
                if(IA::IsLarger(range.Low.Constant, value, intKind)) {
                    return Result_Yes;
                }
            }
            else if(IA::IsLargerUnsigned(range.Low.Constant, value, intKind)) {
                return Result_Yes;
            }
        }
    }
    else {
        // The range is symbolic. In this case we need to compare
        // the operand of the low bound with the constant, properly
        // adjusted if a constant is also involved in the low bound.
        // 'a = [x + C1, y]', 'a > C2' -> 'x > C2 - C1'
        if(range.Low.Constant != 0) {
            auto lowConst = GetIntConstant(range.Low.Constant, op);

            // If there is overflow we give up.
            if(IA::SubOverflows(intConst, lowConst)) {
                return Result_Maybe;
            }
            else intConst = GetIntConstant(IA::Sub(intConst, lowConst), op);

            return IsGreaterRangeWithConst(range.Low.Base, intConst, isSigned,
                                           false, level + 1);
        }
        else if(checkReverse && 
                (IsSmallerRangeWithConst(range.Low.Base, intConst, 
                                         isSigned, false, level + 1) == Result_Yes)) {
            return Result_No;
        }
    }

    // If we don't know that the range is greater than the constant,
    // we may know that it definitely can't be greater by checking
    // if the high bound bound is smaller than the constant.
    // This is equivalent with testing that the range is smaller.
    if(checkReverse &&
       (IsSmallerRangeWithConst(op, intConst, isSigned, 
                                false, level + 1) == Result_Yes)) {
        return Result_No;
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsGreaterRangeWithRange(Operand* a, Operand* b, bool isSigned, 
                                              bool checkReverse, int level) {
    if(level > MAX_QUERY_LEVEL) {
        return Result_Maybe;
    }

    // We should have a known range for at least one
    // of the operands, else we give up.
    Range rangeA;
    Range rangeB;
    bool hasRangeA = GetRange(a, rangeA);
    bool hasRangeB = GetRange(b, rangeB);

    if((hasRangeA || hasRangeB) == false) {
        return Result_Maybe;
    }

    if(hasRangeA && hasRangeB) {
        // A range is greater than another one if it's low bound
        // is above the high bound of the other range.
        // [6, 8] > [2, 4] -> Yes
        if(IsGreater(rangeA.Low, rangeB.High, 
                     false, level + 1) == Result_Yes) {
            return Result_Yes;
        }

        // The range is definitely smaller if the high bound
        // is below the low bound of the other range.
        // [2, 4] > [6, 8] -> No
        if(checkReverse &&
           IsSmaller(rangeA.High, rangeB.Low, 
                     false, level + 1) == Result_Yes) {
            return Result_No;
        }
    }

    // Check if one of the ranges is based on the other operand.
    Operand* other = nullptr;
    Range range;

    if(hasRangeA && 
       ((rangeA.Low.Base == b) || rangeA.Low.IsMinusInfinity(b, isSigned)) &&
       ((rangeA.High.Base == b) || rangeA.High.IsPlusInfinity(b, isSigned))) {
        other = b;
        range = rangeA;
    }
    else if(hasRangeB &&
            ((rangeB.Low.Base == a) || rangeB.Low.IsMinusInfinity(a, isSigned)) &&
            ((rangeB.High.Base == a) || rangeB.High.IsPlusInfinity(a, isSigned))) {
        other = a;
        range = rangeB;
    }

    if(other == nullptr) {
        return Result_Maybe;
    }

    // We test for the following cases:
    // 1. a = [b + C1, b + C2], a > b -> No (1) / Yes (2)
    // if both C1 and C2 are smaller than zero (1)
    // if both C1 and C2 are greater than zero (2)
    if((range.Low.Base == other) &&
       (range.High.Base == other)) {
        if((range.Low.Constant > 0) &&
           (range.High.Constant > 0)) {
            return Result_Yes;
        }
        else if(checkReverse &&
                (range.Low.Constant < 0) &&
                (range.High.Constant < 0)) {
            return Result_No;
        }
    }

    // 2. a = (-INF, b + C], a > b -> No, if C < 0
    if(checkReverse &&
       (range.High.Base == other) &&
       (range.High.Constant < 0) &&
       range.Low.IsMinusInfinity(other, isSigned)) {
        return Result_No;
    }

    // 3. a = [b + C, +INF), a > b -> Yes, if C > 0
    if((range.Low.Base == other) &&
       range.High.IsPlusInfinity(other, isSigned)) {
       if(range.Low.Constant > 0) {
            return Result_Yes;
       }
    }

    // 4. a = [b + C1, b + C2], a > b + C3 -> No (1) / Yes (2)
    // if both C1-C3 and C2-C3 are smaller than zero (1)
    // if both C1-C3 and C2-C3 are greater than zero (2)
    // and signed overflow is undefined (1)(2)
    if(auto addInstr = other->DefiningInstrAs<AddInstr>()) {
        if(addInstr->HasUndefinedOverflow() &&
           addInstr->RightOp()->IsIntConstant() &&
           (range.Low.Base == other) &&
           (range.High.Base == other)) {
            __int64 value = addInstr->RightOp()->As<IntConstant>()->Value();

            if(((range.Low.Constant - value) > 0) &&
               ((range.High.Constant - value) > 0)) {
                return Result_Yes;
            }
            else if(checkReverse &&
                    ((range.Low.Constant - value) < 0) &&
                    ((range.High.Constant - value) < 0)) {
                return Result_No;
            }
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsGreaterOrEqual(Operand* a, Operand* b, bool isSigned, 
                                       bool checkReverse, int level) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    
    if(a->IsIntConstant() && b->IsIntConstant()) {
        return CompareConstants(a, b, Order_GreaterOrEqual, isSigned);
    }

    // Try to retrieve the result from the cache.
    RangeResult result;
    bool hasResult = false;

    if(ResultFromCache(a, b, Query_IsGreaterOrEqual, result)) {
        return result;
    }

    if(auto phiInstrA = a->DefiningInstrAs<PhiInstr>()) {
        hasResult = true;
        result = ComparePhiRanges(phiInstrA, b, isSigned, checkReverse, 
                                  level, &RangeTag::IsGreaterOrEqual, false);
    }
    else if(auto phiInstrB = b->DefiningInstrAs<PhiInstr>()) {
        hasResult = true;
        result = ComparePhiRanges(phiInstrB, a, isSigned, checkReverse, 
                                  level, &RangeTag::IsGreaterOrEqual, true);
    }
    else if(auto questInstrA = a->DefiningInstrAs<QuestionInstr>()) {
        hasResult = true;
        result = CompareQuestRanges(questInstrA, b, isSigned, checkReverse, 
                                    level, &RangeTag::IsGreaterOrEqual, false);
    }
    else if(auto questInstrB = b->DefiningInstrAs<QuestionInstr>()) {
        hasResult = true;
        result = CompareQuestRanges(questInstrB, a, isSigned, checkReverse, 
                                    level, &RangeTag::IsGreaterOrEqual, true);
    }

    if(hasResult) {
        AddResultToCache(a, b, Query_IsGreaterOrEqual, result);
        return result;
    }

    // If one of the operands 'load' we give up, because we don't know
    // anything sure about the loaded value.
    if(IsUncertainOperand(a) || IsUncertainOperand(b)) {
        return Result_Maybe;
    }

    // Constants are treated separately, because they are the most
    // common case and we can obtain better results for them.
    // If we have a constant we do the 'greater or equal' test,
    // else we do the more conservative 'greater' test.
    if(auto intConstB = b->As<IntConstant>()) {
        if(intConstB->IsZeroInt()) {
            return IsGreaterOrEqualToZero(a, isSigned);
        }

        // [a, b] >= C -> [a, b] > C - 1
        auto oneConst = GetIntConstant(1, intConstB);

        if(IA::SubOverflows(intConstB, oneConst) == false) {
            auto newConst = GetIntConstant(IA::Sub(intConstB, oneConst), intConstB);
            result = IsGreaterRangeWithConst(a, newConst, isSigned, 
                                             checkReverse, level);
        }
        else result = IsGreaterRangeWithConst(a, intConstB, isSigned, checkReverse);
    }
    else if(auto intConstA = a->As<IntConstant>()) {
        // C >= [a, b] -> C + 1 > [a, b]
        auto oneConst = GetIntConstant(1, intConstA);

        if(IA::AddOverflows(intConstA, oneConst) == false) {
            auto newConst = GetIntConstant(IA::Add(intConstA, oneConst), intConstA);
            result = IsSmallerRangeWithConst(a, newConst, isSigned, 
                                             false, level);
        }
        else result = IsSmallerRangeWithConst(b, intConstA, isSigned, 
                                              false, level);
    }
    else if(IsGreaterRangeWithRange(a, b, isSigned, 
                                    checkReverse, level) == Result_Yes) {
        // This response is conservative, but a 'No' response
        // is not because it doesn't include the <= case.
        result = Result_Yes;
    }
    else result = Result_Maybe;

    AddResultToCache(a, b, Query_IsGreaterOrEqual, result);
    return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsGreaterOrEqualToZero(Operand* op, bool isSigned) {
    // Check if there is a known range for the operand.
    Range range;
    
    if(GetRange(op, range) == false) {
        return Result_Maybe;
    }

    // Anti-ranges and symbolic ranges are not used.
    if((range.IsRange == false) || range.Low.HasBaseOperand()) {
        return Result_Maybe;
    }
    // Any number in the range is greater than the constant
    // if the low bound is greater than the constant.
    auto intKind = op->GetType()->As<IntegerType>()->GetSubtype();

    if(range.Low.IsMinusInfinity(op, isSigned) == false) {
        if(isSigned) {
            if(IA::IsLarger(range.Low.Constant, 0, intKind)) {
                return Result_Yes;
            }
        }
        else if(IA::IsLargerUnsigned(range.Low.Constant, 0, intKind)) {
            return Result_Yes;
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsNotZero(Operand* op, bool checkReverse) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsFalse(op->IsConstant());
    
    // We test by comparing the operand with the zero constant.
    auto zeroConst = GetIntConstant(0, op);

    if(AreNotEqual(op, zeroConst, false) == Result_Yes) {
        return Result_Yes;
    }
    else if(checkReverse &&
            AreEqual(op, zeroConst, false) == Result_Yes) {
        return Result_No;
    }
    else return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsZero(Operand* op, bool checkReverse) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsFalse(op->IsConstant());
    
    // We test by comparing the operand with the zero constant.
    auto zeroConst = GetIntConstant(0, op);

    if(AreEqual(op, zeroConst, false) == Result_Yes) {
        return Result_Yes;
    }
    else if(checkReverse &&
            AreNotEqual(op, zeroConst, false) == Result_Yes) {
        return Result_No;
    }
    else return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsPositive(Operand* op, bool checkReverse) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsFalse(op->IsConstant());
    
    // We test by comparing the operand with the zero constant.
    auto zeroConst = GetIntConstant(0, op);

    if(IsGreaterOrEqual(op, zeroConst, true, false) == Result_Yes) {
        return Result_Yes;
    }
    else if(checkReverse &&
            IsSmaller(op, zeroConst) == Result_Yes) {
        return Result_No;
    }
    else return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::IsNegative(Operand* op, bool checkReverse) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsFalse(op->IsConstant());
    
    // We test by comparing the operand with the zero constant.
    auto zeroConst = GetIntConstant(0, op);

    if(IsSmaller(op, zeroConst, true, false) == Result_Yes) {
        return Result_Yes;
    }
    else if(checkReverse &&
            IsGreaterOrEqual(op, zeroConst) == Result_Yes) {
        return Result_No;
    }
    else return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IntConstant* RangeTag::GetAsConstant(Operand* op) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsFalse(op->IsConstant());

    // Check if the operand has a range that turns it
    // into a constant. This happen for cases like
    // op = [4, 4] -> 4
    Range range;

    if(GetRange(op, range)) {
        if(range.IsRange && range.IsSingleValue() &&
           (range.HasBaseOperand() == false)) {
            return GetIntConstant(range.Low.Constant, op);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IntConstant* RangeTag::GetIntConstant(__int64 value, Operand* op) {
    auto unit = block_->ParentFunction()->ParentUnit();
    auto type = op->GetType()->IsPointer() ?
                IntegerType::GetInt32() : op->GetType();
    
    return unit->Constants().GetInt(type, value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::ComparePhiRanges(PhiInstr* phiInstr, Operand* otherOp, 
                                       bool isSigned, bool checkReverse, int level,
                                       OperandComparer comparer, bool inverted) {
    // If the other operand is also a 'phi' instruction, or the 'phi'
    // has many operands there is no reason to continue.
    if((level > MAX_QUERY_LEVEL) || 
       (phiInstr->OperandCount() > 4) ||
        otherOp->DefiningInstrIs<PhiInstr>()) {
        return Result_Maybe;
    }

    // If any result is "Maybe", or we have a combination of "Yes" and "No"
    // then the result is "Maybe".
    int yesCount = 0;
    int noCount = 0;

    for(int i = 0; i < phiInstr->OperandCount(); i++) {
        auto result = inverted ? (this->*comparer)(otherOp, phiInstr->GetOperand(i),
                                                   isSigned, checkReverse, level + 1) :
                                 (this->*comparer)(phiInstr->GetOperand(i), otherOp,
                                                   isSigned, checkReverse, level + 1);
        if(result == Result_Maybe) {
            return Result_Maybe;
        }
        else if(result == Result_Yes) {
            if(noCount == 0) yesCount++;
            else return Result_Maybe;
        }
        else { // Result_No
            if(checkReverse == false) return Result_Maybe;
            else if(yesCount == 0) noCount++;
            else return Result_Maybe;
        }
    }

    return yesCount > 0 ? Result_Yes : Result_No;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::CompareQuestRanges(QuestionInstr* questInstr, Operand* otherOp, 
                                         bool isSigned, bool checkReverse, 
                                         int level, OperandComparer comparer,
                                         bool inverted) {
    // If the other operand is a 'phi' or 'quest' instruction
    // there is no reason to continue.
    if(otherOp->DefiningInstrIs<QuestionInstr>() ||
       otherOp->DefiningInstrIs<PhiInstr>()) {
       return Result_Maybe;
    }

    // If any result is "Maybe", or we have a combination of "Yes" and "No"
    // then the result is "Maybe".
    int yesCount = 0;
    int noCount = 0;

    for(int i = 0; i < 2; i++) {
        auto result = inverted ? (this->*comparer)(otherOp, questInstr->GetSourceOp(i + 1),
                                                   isSigned, checkReverse, level + 1) :
                                 (this->*comparer)(questInstr->GetSourceOp(i + 1), otherOp,
                                                   isSigned, checkReverse, level + 1);
        if(result == Result_Maybe) {
            return Result_Maybe;
        }
        else if(result == Result_Yes) {
            if(noCount == 0) yesCount++;
            else return Result_Maybe;
        }
        else { // Result_No
            if(checkReverse == false) return Result_Maybe;
            else if(yesCount == 0) noCount++;
            else return Result_Maybe;
        }
    }

    return yesCount > 0 ? Result_Yes : Result_No;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult RangeTag::CompareConstants(Operand* opA, Operand* opB, 
                                       OrderType order, bool isSigned) {
    DebugValidator::IsTrue(opA->IsIntConstant());
    DebugValidator::IsTrue(opB->IsIntConstant());

    auto icA = opA->As<IntConstant>();
    auto icB = opB->As<IntConstant>();

    switch(order) {
		case Order_Equal: {
            return ResultFromBool(IA::AreEqual(icA, icB));
        }
		case Order_NotEqual: {
            return ResultFromBool(IA::AreNotEqual(icA, icB));
        }
		case Order_Less: {
			if(isSigned) return ResultFromBool(IA::IsSmaller(icA, icB));
            else return ResultFromBool(IA::IsSmallerUnsigned(icA, icB));
		}
		case Order_LessOrEqual: {
			if(isSigned) return ResultFromBool(IA::IsSmallerOrEqual(icA, icB));
            else return ResultFromBool(IA::IsSmallerOrEqualUnsigned(icA, icB));
		}
		case Order_Greater: {
			if(isSigned) return ResultFromBool(IA::IsLarger(icA, icB));
            else return ResultFromBool(IA::IsLargerUnsigned(icA, icB));
		}
		case Order_GreaterOrEqual: {
			if(isSigned) return ResultFromBool(IA::IsLargerOrEqual(icA, icB));
            else return ResultFromBool(IA::IsLargerOrEqualUnsigned(icA, icB));
		}
		default: DebugValidator::Unreachable();
    }

    return Result_No; // Unreachable.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeTag::ResultFromCache(Operand* a, Operand* b, 
                               RangeQueryType queryType, RangeResult& result) {
    // Check if the operand pair is in the cache.
    for(int i = 0; i < rangeCache_.Count(); i++) {
        auto& cachedResult = rangeCache_[i];

        if((cachedResult.A == a) &&
           (cachedResult.B == b)) {
            return cachedResult.Result.GetResult(queryType, result);
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RangeTag::AddResultToCache(Operand* a, Operand* b, 
                                RangeQueryType queryType, RangeResult result) {
    // Check if the pair of operands already has a cache entry.
    for(int i = 0; i < rangeCache_.Count(); i++) {
        auto& cachedResult = rangeCache_[i];

        if((cachedResult.A == a) &&
           (cachedResult.B == b)) {
            cachedResult.Result.SetResult(queryType, result);
            return;
        }
    }

    // If there is no space in the cache remove the oldest entry.
    if(rangeCache_.Count() == 4) {
        rangeCache_.RemoveAt(0);
    }

    rangeCache_.Add(CachedRangeResult(a, b));
    rangeCache_[rangeCache_.Count() - 1].Result.SetResult(queryType, result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Bound::ToString(int level) const {
    string text;

    if(Base) text = Base->ToString() + " + ";
    text += string::Format(L"%d", Constant);
    return text;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Range::ToString(int level) const {
    string text;

    if(IsRange) {
        text = "[" + Low.ToString() + ", " + High.ToString() + "]";
    }
    else text = "(-INF, " + Low.ToString() + "], [" + High.ToString() + ", +INF)";
    return text;
} 

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string RangeTag::ToString(int level) {
    StringBuilder sb("Block Ranges: " + *block_->Name() + "\n");

    ranges_.ForEach([&sb](const OperandRange& or) -> bool {
        sb.AppendLine(or.OperandInfo->ToString() + ":  " + or.RangeInfo.ToString());
        return true;
    });

    sb.AppendLine("\nSuccessor Ranges\n");

    for(int i = 0; i < block_->SuccessorCount(); i++) {
        sb.AppendLine(string::Format(L"Successor %d\n", i));
        auto& ranges = succRanges_.GetRangesPair(block_->SuccessorAt(i));

        ranges.Ranges.ForEach([&sb](const OperandRange& or) -> bool {
            sb.AppendLine(or.OperandInfo->ToString() + ":  " + or.RangeInfo.ToString());
            return true;
        });
    }

    return sb.ToString();
}

} // namespace Analysis