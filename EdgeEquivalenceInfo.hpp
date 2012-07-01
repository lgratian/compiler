// EdgeEquivalenceInfo.cpp
// Copyright (c) Lup Gratian
//
// Defines a pass that tries to derive value ranges
// for the operands used by branching instructions and by
// some special instructions like 'div' and 'load'.
// Uses Range Tags to store the found ranges.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_EDGE_EQUIVALENCE_INFO_HPP
#define PC_ANALYSIS_EDGE_EQUIVALENCE_INFO_HPP

#include "RangeTag.hpp"
#include "IRDominators.hpp"
#include "IntArithmetic.hpp"
#include "StdLibRecognizer.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Tag.hpp"
#include "../IR/Unit.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/String.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace IR;
using namespace Base;
using namespace CompilationPass;

namespace Analysis {

class EdgeEquivalenceInfo : public Pass {
private:
    typedef StaticList<__int64, 2> CaseList;
    typedef StaticList<__int64, 32> SwitchValueList;
    typedef Dictionary<BlockReference*, CaseList> ValueDict;
    typedef StaticList<OperandRange, 4> RangeList;      
    typedef IntArithmetic IA;

    IRDominatorTree* domTree_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns the Range Tag associated with the specified block.
    // If such a tag doesn't yet exists it is created now and
    // it is connected to its immediate dominator's tag.
    RangeTag* GetOrCreateTag(Block* block, Block* immDomBlock);

    // Returns 'true' if the block is a single-entry loop header.
    bool IsLoopHeader(Block* block, Block* immDomBlock);

    // Returns 'true' if the function is exited if control reaches
    // the block. This can happen if the block contains a return,
    // or a call to standard library methods like 'exit' and 'abort'.
    bool IsFunctionExitBlock(Block* block);

    // Transforms a comparison instruction so that fewer cases
    // need to be tested for (moves constant on the right, for example).
    void CanonicalizeComparison(CmpInstrBase* instr);

    // Tries to create ranges from the instruction on which
    // the condition operand used in an 'if' depends.
    void CreateRangesForIf(IfInstr* instr, RangeTag* tag);

    // Creates ranges for the operand used as the condition in a 'switch'.
    void CreateRangesForSwitch(SwitchInstr* instr, RangeTag* tag);

    void PropagateCaseValues(BlockReference* defaultTarget, Operand* conditionOp,
                             ValueDict &targetValues, SwitchValueList& valueList, 
                             RangeTag* tag);

    void PropagateDefaultValue(SwitchValueList& valueList, Operand* conditionOp,
                               BlockReference* defaultTarget, RangeTag* tag);

    // Creates ranges for the operands that are used in a comparison.
    // Handles cases like 'a < 5', 'a > b' and 'a <= b + 3'.
    void CreateRangeFromComparison(CmpInstrBase* instr, RangeList& list,
                                   RangeTag* tag, bool createMirrored = true);

    bool IsOperandAddSub(Operand* opB, bool &isOpBAdd, Operand* &opBTemp,
                         IntConstant* &opBConst, bool &isOpBSub );

    // Creates a range based on the comparison of an operand
    // with an integer constant. For example, 'a < 5' -> (-INF, 4].
    Range CreateRangeForConstCmp(IntConstant* intConst, OrderType order,
                                 bool isSigned, RangeTag* tag);

    // Creates a range based on the comparison of an operand
    // with another operand plus an integer constant. 
    // For example, 'a < b + 5' -> (-INF, b + 4].
    Range CreateRangeForAddCmp(Operand* op, IntConstant* intConst,
                               OrderType order, bool isSigned, RangeTag* tag);

    // Creates a range based on the comparison of an operand
    // with another operand minus an integer constant. 
    // For example, 'a < b - 5' -> (-INF, b - 6].
    Range CreateRangeForSubCmp(Operand* op, IntConstant* intConst,
                               OrderType order, bool isSigned, RangeTag* tag);

    // Creates a range based on the comparison of an operand
    // with another operand. If the other operand has a known range
    // we prefer to use it instead of creating a purely symbolic range.
    Range CreateRangeForGenericCmp(Operand* op, OrderType order, bool isSigned, 
                                   RangeTag* tag);

    // Creates a range based on the comparison of an operand
    // with another operand that has a known range.
    // Optionally, there may be an integer constant that is added
    // or subtracted to the known range.
    // For example, 'a < [2, 4] + 5' -> [7, 9].
    Range CreateRangeForGenericCmpWithRange(Operand* op, Range opRange, 
                                            IntConstant* intConst, bool negateConst,
                                            OrderType order, bool isSigned);

    Range CreateRangeForEqualCmp(Range &opRange, __int64 constValue, 
                                 Operand* op, bool isSigned);

    Range CreateRangeForNotEqualCmp(Range &opRange, __int64 constValue, 
                                    Operand* op, bool isSigned);

    Range CreateRangeForLessCmp(Range &opRange, __int64 constValue, 
                                Operand* op, bool isSigned, OrderType order);

    Range CreateRangeForGreaterCmp(Range &opRange, __int64 constValue, 
                                   Operand* op, bool isSigned, OrderType order);

    // Creates a range from the comparison of two pointers.
    Range CreateRangeFromPointerCmp(Operand* opA, Operand* opB,
                                    OrderType order, RangeTag* tag);

    Range CreateSymbolicRangeFromPointerCmp( OrderType order, Operand* opB );

    Range CreateRangeFromPointerCmpWithNull( OrderType order );

    // Adds to the list all ranges that can be deduced
    // from the comparisons that are combined using an 'and'.
    // The operands of the 'and' should be comparisons or other
    // 'and' instructions (handled recursively).
    void CollectAndRanges(AndInstr* instr, RangeList& list, RangeTag* tag);

    // The implementation of the above method; 
    // does the actual discovering of the ranges.
    void CollectAndRangesImpl(AndInstr* instr, RangeList& list, RangeTag* tag);

    // Adds to the list all ranges that can be deduced
    // from the comparisons that are combined using an 'and'.
    // The operands of the 'and' should be comparisons or other
    // 'and' instructions (handled recursively).
    void CollectOrRanges(OrInstr* instr, RangeList& list, RangeTag* tag);

    void CreateOrRange(RangeList &list, Operand* requiredOp, 
                       __int64 minValue, __int64 maxValue, 
                       bool allPositive, bool allNotZero,
                       bool allNegative, bool hasMinMax);

    // The implementation of the above method; 
    // does the actual discovering of the ranges.
    void CollectOrRangesImpl(OrInstr* instr, RangeList& list, RangeTag* tag);

    // We found a range for the specified operand, and we want
    // to intersect it with a (possible) range that is already known
    // for the same operand. For example, if we found that 'a = [2, +INF)'
    // and we already known that 'a = (-INF, 8]', then 'a = [2, 8]'.
    Range IntersectWithKnownRange(Operand* op, Range newRange, 
                                  RangeTag* tag, bool& oldRangeUsed);

    // Makes the found range available for the successor blocks.
    // The range for the 'false' block is the inverted range.
    void AddIfRange(IfInstr* ifInstr, Operand* op, Range range, 
                    RangeTag* tag, bool createInverse, bool isSigned);

    // Tests if a range represents the values of a null/not pointer.
    bool IsNullPointer(Range& range);

    bool IsNotNullPointer(Range& range);

    // Tries to extract ranges from the operands used by the branching
    // instruction ('if' and 'switch'), but also from operands used 
    // in certain instructions ('div', 'load', 'call', for example).
    // Creates a Range Tag that is associated with the block.
    void ProcessBlock(Block* block, Block* immDomBlock);

    // Scans all instructions in the specified block and tries
    // to deduce some ranges from instructions like 'div' and 'load'.
    // For example, after 'div a, b' we know that b != 0.
    void ExtractRangesFromInstructions(Block* block, RangeTag* tag);

    // Tries to determine the range of a value loaded from a global
    // constant variable that has an initializer.
    bool ExtractRangeFromGlobalConstant(Operand* op, Operand* resultOp,
                                        Block* block, RangeTag* tag);

    bool FindInitializersMinMax(InitializerList* initList, 
                                __int64& minValue, __int64& maxValue);

    bool AreInitializedPointersNotNull(InitializerList* initList);

    // Processes all the blocks in the function using 
    // a dominator-tree order.
    void ProcessFunction(Function* function);

    // Tries to convert an anti-range to a range,
    // because we can extract more information from ranges.
    Range PatchAntiRange(Range range, Operand* op, bool isSigned);

    // Creates an anti-range that indicates that the operand is not zero.
    // This anti-range will be available for all successors.
    void PushNotZeroToSuccessors(Operand* op, Block* block, RangeTag* tag);

    void PushRangeToSuccessors(Range range, Operand* op, 
                               Block* block, RangeTag* tag);

    Operand* GetBaseOperand(Operand* op);

public:
    void Execute(Function* function) {
        ProcessFunction(function);
    }

    void RemoveRangeTags(Function* function);
};

} // namespace Analysis
#endif