// RangeTag.hpp
// Copyright (c) Lup Gratian
//
// Defines a tag that can be used to store values ranges
// for the operands defined in a block, but also ranges that
// are valid only for specific successor blocks.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_RANGE_TAG_HPP
#define PC_ANALYSIS_RANGE_TAG_HPP

#include "IntArithmetic.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Tag.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

// Represents the result of a range query.
// Note that no optimization should be done based on 
// a 'Result_Maybe' response!!!
enum RangeResult {
    Result_Yes,
    Result_No,
    Result_Maybe
};


// Represents the low or high bounds of a range.
// Representation: Base Operand + Constant
struct Bound {
    Operand* Base;
    __int64 Constant;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Bound() : Base(nullptr), Constant(0) {}

    explicit Bound(__int64 constant) : 
            Base(nullptr), Constant(constant) {}

    explicit Bound(Operand* base, __int64 constant = 0) :
            Base(base), Constant(constant) {}

    Bound(const Bound& other) :
            Base(other.Base), Constant(other.Constant) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    static __int64 GetMinimum(Operand* op, bool isSigned) {
        DebugValidator::IsNotNull(op);
        DebugValidator::IsTrue(op->IsInteger());
        auto intType = op->GetType()->As<IntegerType>();
        return IntArithmetic::GetMin(intType, isSigned);
    }

    static __int64 GetMinimum() {
        return IntArithmetic::GetMin(IR_Int64, true);
    }

    static __int64 GetMaximum(Operand* op, bool isSigned) {
        DebugValidator::IsNotNull(op);
        DebugValidator::IsTrue(op->IsInteger());
        auto intType = op->GetType()->As<IntegerType>();
        return IntArithmetic::GetMax(intType, isSigned);
    }

    static __int64 GetMaximum() {
        return IntArithmetic::GetMax(IR_Int64, true);
    }

    static Bound GetMinusInfinity(Operand* op, bool isSigned) {
        return Bound(GetMinimum(op, isSigned));
    }

    static Bound GetMinusInfinity() {
        return Bound(GetMinimum());
    }

    static Bound GetPlusInfinity(Operand* op, bool isSigned = true) {
        return Bound(GetMaximum(op, isSigned));
    }

    static Bound GetPlusInfinity() {
        return Bound(GetMaximum());
    }

    bool IsMinusInfinity(Operand* op, bool isSigned = true) const {
        return (Base == nullptr) && 
               (Constant <= GetMinimum(op, isSigned));
    }

    bool IsPlusInfinity(Operand* op, bool isSigned = true) const {
        return (Base == nullptr) && 
               (Constant >= GetMaximum(op, isSigned));
    }

    bool HasBaseOperand() const {
        return Base;
    }

    string ToString(int level = 0) const;

    void Dump() {
        ObjectDumper(ToString(0).Chars(), "Bound").Dump();
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    bool operator== (const Bound& other) const {
        return (Base == other.Base) &&
               (Constant == other.Constant);
    }

    bool operator!= (const Bound& other) const {
        return !operator== (other);
    }
};


// Represents a range that is associated with an operand.
// Range:      Operand >= (Low Op + Low Const)  &&  Operand <= (High Op + High Const)
// Anti-Range: Operand <= (Low Op + Low Const)  ||  Operand >= (High Op + High Const)
struct Range {
    typedef IntArithmetic IA;

    Bound Low;
    Bound High;
    bool IsRange; // Range/anti-range

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Range() : IsRange(true) {}

    explicit Range(Operand* lowBase, __int64 lowConst, 
          Operand* highBase, __int64 highConst, bool isRange = true) :
            Low(lowBase, lowConst), High(highBase, highConst), IsRange(isRange) {}

    explicit Range(Operand* lowBase, Operand* highBase, bool isRange = true) :
            Low(lowBase), High(highBase), IsRange(isRange) {}

    explicit Range(__int64 lowConst, __int64 highConst, bool isRange = true) :
            Low(lowConst), High(highConst), IsRange(isRange) {}

    explicit Range(Bound lowBounds, Bound highBounds, bool isRange = true) :
            Low(lowBounds), High(highBounds), IsRange(isRange) {}

    Range(const Range& other) :
            Low(other.Low), High(other.High), IsRange(other.IsRange) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns a range that covers the entire domain
    // of the integer type used by the specified operand.
    static Range GetUnknown(Operand* op, bool isSigned) {
        return Range(Bound::GetMinusInfinity(op, isSigned),
                     Bound::GetPlusInfinity(op, isSigned));
    }

    static Range GetUnknown() {
        return Range(Bound::GetMinusInfinity(),
                     Bound::GetPlusInfinity());
    }

    bool IsUnknown(Operand* op, bool isSigned) const {
        return Low.IsMinusInfinity(op, isSigned) ||
               High.IsPlusInfinity(op, isSigned);
    }

    // Returns 'true' if the low and high bounds are the same.
    bool IsSingleValue() const {
        return (Low.Base == High.Base) &&
               (Low.Constant == High.Constant);
    }

    // Returns 'true' if the range is symbolic.
    bool HasBaseOperand() const {
        return Low.Base || High.Base;
    }

    // Returns the inverted range (range -> anti-range, anti-range -> range).
    // For example, [a, b] -> (-INF, a - 1] U [b + 1, +INF).
    Range Invert(Operand* op, bool isSigned = true) const;

    string ToString(int level = 0) const;

    void Dump() {
        ObjectDumper(ToString(0).Chars(), "Range").Dump();
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    bool operator== (const Range& other) const {
        return (IsRange == other.IsRange) &&
               (Low == other.Low) &&
               (High == other.High);
    }

    bool operator!= (const Range& other) const {
        return !operator== (other);
    }
};


// A pair that consists of an operand
// and its associated value range information.
struct OperandRange {
    Operand* OperandInfo;
    Range RangeInfo;

    // - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - -
    OperandRange() {}

    OperandRange(Operand* op, Range range) :
            OperandInfo(op), RangeInfo(range) {}

    OperandRange(const OperandRange& other) :
            OperandInfo(other.OperandInfo), 
            RangeInfo(other.RangeInfo) {}

    bool operator== (const OperandRange& other) const {
        return OperandInfo == other.OperandInfo;
    }

    bool operator!= (const OperandRange& other) const {
        return OperandInfo != other.OperandInfo;
    }

    bool operator< (const OperandRange& other) const {
        return OperandInfo < other.OperandInfo;
    }
};


// Represents a pair consisting of a block and a dictionary
// that contains the ranges for each of its successors.
struct BlockRangesPair {
    Block* RangesBlock;
    StaticList<OperandRange, 4> Ranges;

    // - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - -
    BlockRangesPair() : RangesBlock(nullptr) {}

    BlockRangesPair(Block* block) :
            RangesBlock(block) {}

    BlockRangesPair(const BlockRangesPair& other) :
            RangesBlock(other.RangesBlock), Ranges(other.Ranges) {}
};


// Stores the ranges that are valid only for
// specific successor blocks. Uses an space-efficient representation
// that considers that most blocks have at most two successors.
class SuccesorRanges {
private:
    typedef Dictionary<Block*, BlockRangesPair> TSuccessorDict;

    StaticList<BlockRangesPair, 2> fewSuccessors_;
    TSuccessorDict* manySuccessors_;
    int count_;

    // - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - -
    void AllocateInitial(Block* block);

public:
    SuccesorRanges(Block* block) : 
            manySuccessors_(nullptr), count_(block->SuccessorCount()) {
        AllocateInitial(block);
    }

    ~SuccesorRanges() {
        if(manySuccessors_) {
            delete manySuccessors_;
        }
    }

    // - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - -
    // Adds the range associated with the specified operand
    // to the block ranges.
    void AddRange(Operand* op, Range range, Block* block);

    // Tries to obtain a range that is valid only for the specified
    // operand and block. Returns 'true' if such a range was found.
    bool GetRange(Operand* op, Block* block, Range& range);

    // Returns the dictionary that contains the ranges
    // for the specified block.
    BlockRangesPair& GetRangesPair(Block* block);
};


class RangeTag : public BlockTag {
private:
    // The type of the cached query result.
    enum RangeQueryType {
        Query_AreEqual,
        Query_AreNotEqual,
        Query_IsSmaller,
        Query_IsSmallerOrEqual,
        Query_IsGreater,
        Query_IsGreaterOrEqual
    };

    // Represents the cached results for all possible queries.
    struct RangeQueryResult {
        unsigned short AreEqual         : 2;
        unsigned short AreNotEqual      : 2;
        unsigned short IsSmaller        : 2;
        unsigned short IsSmallerOrEqual : 2;
        unsigned short IsGreater        : 2;
        unsigned short IsGreaterOrEqual : 2;

        // - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - -
        RangeQueryResult() {
            // All values are unknown at first.
            *((unsigned short*)this) = 0xFFFF;
        }

        RangeQueryResult(const RangeQueryResult& other) {
            *((unsigned short*)this) = *((unsigned short*)&other);
        }

        // - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - -
        // Extracts the range result from the specified value.
        // If this is not possible it returns 'false'.
        bool ResultFromValue(unsigned short value, RangeResult& result);

        // Tries to get the range result for the specified query.
        // If this is not possible it returns 'false'.
        bool GetResult(RangeQueryType queryType, RangeResult& result);

        // Sets the range result for the specified query.
        void SetResult(RangeQueryType queryType, RangeResult result);
    };

    // Represents the cached results for queries involving two operands.
    struct CachedRangeResult {
        Operand* A;
        Operand* B;
        RangeQueryResult Result;

        // - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - -
        CachedRangeResult() {}

        CachedRangeResult(Operand* a, Operand* b) :
                A(a), B(b) {}

        CachedRangeResult(const CachedRangeResult& other) :
                A(other.A), B(other.B), Result(other.Result) {}
    };

    // - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - -
    typedef StaticList<RangeTag*, 4> TRangeTagList;
    typedef StaticList<RangeTag*, 4> TChildrenList;
    typedef StaticList<OperandRange, 4> RangeList;
    typedef IntArithmetic IA;
    typedef RangeResult (RangeTag::*OperandComparer)(Operand* a, Operand* b,
                                                     bool isSignedtrue, 
                                                     bool checkReverse, int level);

    //! TODO: use a control
    static const int MAX_QUERY_LEVEL = 5;

    RangeTag* parent_;           // The immediate dominator's tag.
    Block* block_;               // The associated block.
    TChildrenList children_;     // The list with the dominated tags.
    RangeList ranges_;          // Ranges valid inside the block.
    SuccesorRanges succRanges_;  // Ranges valid only for successors.
    StaticList<CachedRangeResult, 4> rangeCache_;
    bool isLoopHeader_;
    bool isExitBlock_;

    // - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - -
    RangeTag();                            // Should not be created.
	RangeTag(const RangeTag&);             // Should not be copied.
	RangeTag& operator= (const RangeTag&); // Should not be assigned.

    RangeTag(Block* block, RangeTag* parent, bool isLoopHeader, bool isExitBlock) : 
            block_(block), parent_(parent), succRanges_(block),
            isLoopHeader_(isLoopHeader), isExitBlock_(isExitBlock) {}

    // - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - -
    // Methods for testing the relationship between two ranges.
    RangeResult IsEqualRangeWithRange(Operand* a, Operand* b,
                                      bool checkReverse = true, int level = 0);
    RangeResult IsNotEqualRangeWithRange(Operand* a, Operand* b,
                                         bool checkReverse = true, int level = 0);
    RangeResult IsSmallerRangeWithRange(Operand* a, Operand* b, bool isSigned, 
                                        bool checkReverse = true, int level = 0);
    RangeResult IsGreaterRangeWithRange(Operand* a, Operand* b, bool isSigned, 
                                        bool checkReverse = true, int level = 0);

    // Methods for testing the relationship between a range and a constant.
    RangeResult IsEqualRangeWithConst(Operand* other, IntConstant* intConst,
                                      bool checkReverse = true, int level = 0);
    RangeResult IsNotEqualRangeWithConst(Operand* other, IntConstant* intConst,
                                         bool checkReverse = true, int level = 0);
    RangeResult IsSmallerRangeWithConst(Operand* op, IntConstant* intConst, 
                                        bool isSigned, bool checkReverse = true,
                                        int level = 0);
    RangeResult IsGreaterRangeWithConst(Operand* op, IntConstant* intConst, 
                                        bool isSigned, bool checkReverse = true,
                                        int level = 0);

    // Methods for testing the relationship between two bounds.
    RangeResult IsSmaller(Bound a, Bound b, bool checkReverse = true, int level = 0);
    RangeResult IsGreater(Bound a, Bound b, bool checkReverse = true, int level = 0);

    // Methods for intersecting bounds.
    Bound IntersectLowBounds(Bound a, Bound b, Operand* op, bool isSigned);
    Bound IntersectHighBounds(Bound a, Bound b, Operand* op, bool isSigned);

    // Returns 'true' if the ranges are not disjoint.
    bool HaveCommonPoint(Range a, Range b);

    IntConstant* GetIntConstant(__int64 value, Operand* op);
    bool ReferesOperand(OperandRange& opRange, Operand* op);

    bool IsUncertainOperand(Operand* op) {
        return op->DefiningInstrIs<PhiInstr>() ||
               op->DefiningInstrIs<LoadInstr>();
    }

    // Compares each 'phi' operand with the other one.
    // If each comparison is "Yes" it returns "Yes", the same for "No",
    // and in any other case "Maybe".
    RangeResult ComparePhiRanges(PhiInstr* phiInstr, Operand* otherOp, 
                                 bool isSigned, bool checkReverse, 
                                 int level, OperandComparer comparer, bool inverted);

    // Compares ech 'quest' operand with the other one.
    // Works the same like 'ComparePhiRanges'.
    RangeResult CompareQuestRanges(QuestionInstr* questInstr, Operand* otherOp, 
                                   bool isSigned, bool checkReverse, 
                                   int level, OperandComparer comparer, bool inverted);
    
    // Returns the result for comparing the two specified constants.
    RangeResult CompareConstants(Operand* opA, Operand* opB, 
                                 OrderType order, bool isSigned = false);

    RangeResult ResultFromBool(bool value) {
        return value ? Result_Yes : Result_No;
    }

    // Tries to obtain from the cache the query result
    // for the two specified operands.
    bool ResultFromCache(Operand* a, Operand* b, 
                         RangeQueryType queryType, RangeResult& result);

    // Adds to the cache the query result for the two specified operands.
    // If there is no more free space in the cache the oldest entry is evicted.
    void AddResultToCache(Operand* a, Operand* b, 
                          RangeQueryType queryType, RangeResult result);

public:
    static const int Id = 0x4bab3e30;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    static RangeTag* GetRangeTag(Block* block, RangeTag* parent = nullptr,
                                 bool isLoopHeader = false, bool isExitBlock = false);

    virtual void DetachedFromParent(TaggedBase* parent);

    virtual void InstructionRemoved(Instruction* instr, Block* parent);

    virtual int GetId() const {
        return Id;
    }

    // Returns the list with the Range Tags for the blocks that are 
    // dominated by the parent block (children in the dominator tree).
    TRangeTagList& Children() {
        return children_;
    }

    RangeTag* Parent() {
        return parent_;
    }

    void SetParent(RangeTag* tag) {
        parent_ = tag;
    }

    Block* ParentBlock() {
        return block_;
    }

    bool IsLoopHeader() const {
        return isLoopHeader_;
    }

    bool IsExitBlock() const {
        return isExitBlock_;
    }

    // Searches for a known range for the specified operand.
    // If one is found it returns 'true' and copies the range in 'range'.
    bool GetRange(Operand* op, Range& range);

    bool ValidToWalkUpDominatorTree();

    // Searches for a range that is valid only for the specified successor.
    // If one is found it returns 'true' and copies the range in 'range'.
    bool GetRangeForSuccessor(Operand* op, Block* successorBlock, Range& range);

    // Adds a range that is valid for the associated block
    // and any of its successors.
    void AddRange(Operand* op, Range range);

    // Adds a range that is valid only for the specified successor.
    void AddRangeForSuccessor(Block* successorBlock, Operand* op, Range range);

    // Returns a range that represents the intersection
    // of the two specified ranges. If such a range cannot be computed,
    // an Unknown range is returned (covers the entire domain).
    Range IntersectRanges(Range a, Range b, Operand* op, bool isSigned = true);

    // Returns a range that represents the intersection of a range
    // with an anti-range.  If such a range cannot be computed,
    // an Unknown range is returned (covers the entire domain).
    Range IntersectRangeWithAntiRange(Range range, Range antiRange, 
                                      Operand* op, bool isSigned = true);

    // Returns 'Result_Yes' if the operand has a known range
    // and it can be deduced that it can't take the value zero.
    // Returns 'Result_No' if the operand is definitely zero.
    RangeResult IsNotZero(Operand* op, bool checkReverse = true);

    // Returns 'Result_Yes' if the operand has a known range
    // and it can be deduced that it takes only the value zero.
    // Returns 'Result_No' if the operand is definitely not zero.
    RangeResult IsZero(Operand* op, bool checkReverse = true);

    // Returns 'Result_Yes' if the operand has a known range
    // and it can be deduced that it takes only positive values.
    // Returns 'Result_No' if the operand is definitely negative.
    RangeResult IsPositive(Operand* op, bool checkReverse = true);

    // Returns 'Result_Yes' if the operand has a known range
    // and it can be deduced that it takes only negative values.
    // Returns 'Result_No' if the operand is definitely positive.
    RangeResult IsNegative(Operand* op, bool checkReverse = true);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that the operands are definitely equal,
    // or 'Result_No' if the operands are definitely not equal.
    RangeResult AreEqual(Operand* a, Operand* b, 
                         bool checkReverse = true, int level = 0);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that the operands are definitely not equal,
    // or 'Result_No' if the operands are definitely equal.
    RangeResult AreNotEqual(Operand* a, Operand* b, 
                            bool checkReverse = true, int level = 0);

    RangeResult IsPointerNotNull(Operand* pointerOp);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that 'a' is definitely smaller than 'b',
    // or 'Result_No' if 'a' is definitely greater than 'b'.
    RangeResult IsSmaller(Operand* a, Operand* b, bool isSigned = true,
                          bool checkReverse = true, int level = 0);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that 'a' is definitely smaller or equal to 'b',
    // or 'Result_No' if 'a' is definitely greater than 'b'.
    RangeResult IsSmallerOrEqual(Operand* a, Operand* b, bool isSigned = true,
                                 bool checkReverse = true, int level = 0);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that 'a' is definitely greater than 'b',
    // or 'Result_No' if 'a' is definitely smaller than 'b'.
    RangeResult IsGreater(Operand* a, Operand* b, bool isSigned = true,
                          bool checkReverse = true, int level = 0);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that 'a' is definitely greater or equal to 'b',
    // or 'Result_No' if 'a' is definitely smaller than 'b'.
    RangeResult IsGreaterOrEqual(Operand* a, Operand* b, bool isSigned = true,
                                 bool checkReverse = true, int level = 0);

    RangeResult IsGreaterOrEqualToZero(Operand* op, bool isSigned = true);

    // If the operand is definitely a constant, it returns 
    // that constant, else it return 'nullptr'. 
    // For example, op = [4, 4] -> 4
    IntConstant* GetAsConstant(Operand* op);

    string ToString(int level = 0);

    void Dump() {
        ObjectDumper(ToString(0).Chars(), "Range Tag").Dump();
    }
};

} // namespace Analysis

namespace IR {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct TagPromoter<Analysis::RangeTag> {
		static bool Is(const Tag* tag) {
			return tag->GetId() == Analysis::RangeTag::Id;
		}

		static Analysis::RangeTag* As(Tag* tag) {
			return Is(tag) ? static_cast<Analysis::RangeTag*>(tag) : nullptr;
		}
	};
} // namespace Detail
} // namespace IR
#endif