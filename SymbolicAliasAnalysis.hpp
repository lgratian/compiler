// SymbolicAliasAnalysis.hpp
// Copyright (c) Lup Gratian
//
// Implements a simple rule-based alias analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SYMBOLIC_ALIAS_ANALYSIS_HPP
#define PC_ANALYSIS_SYMBOLIC_ALIAS_ANALYSIS_HPP

#include "AliasInfo.hpp"
#include "AliasAnalyzer.hpp"
#include "CFamilyTag.hpp"
#include "OperandInfo.hpp"
#include "../Base/DebugValidator.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Intrinsics.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class SymbolicAliasAnalysis : public AliasAnalyzer {
private:
    // Represents a basic induction variable consisting
    // of the start value and the step for each loop iteration.
    struct SimpleIV {
        Operand* IV;
        Operand* StartValue;
        Operand* EndValue;
        Operand* Step;
        bool HasInclusiveEndValue; // i <= n
        bool HasNotEqualOrder;     // p1 != p2
        bool HasNegativeStep;      // i--
        bool HasUndefinedOverflow;
    };

    // Represents an array index of the form 'IV * factor + adjustment'.
    // For example, in 'a[i*4 + 1]', i is the 'Index', 4 the 'Factor' 
    // and 1 the 'Adjustment'.
    struct SimpleIndex {
        Operand* Index;
        Operand* Factor;     // Considered 1 if 'nullptr'
        Operand* Adjustment; // Considered 0 if 'nullptr'
        bool HasNegativeAdjustment;

        bool HasFactor() const {
            return Factor;
        }

        bool HasNoFactor() const {
            return HasFactor() == false;
        }

        bool HasAdjustment() const {
            return Adjustment;
        }

        bool HasNoAdjustment() const {
            return HasAdjustment() == false;
        }
    };

    StaticList<SimpleIV, 8> ivCache_;
    int cachedIVs_;
    int oldestIV_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Computes the offset of the location relative to the base operand
    // by looking through the chain of addressing instructions.
    // For example, in 'int a[4]' 'a[2]' has offset 8 relative to 'a'.
    AliasLocation ComputeOffset(AliasLocation& location);

    AliasLocation ComputeOffset(Operand* base, __int64 offset, __int64 size) {
        return ComputeOffset(AliasLocation(base, offset, size));
    }

    // Checks if there is alias by comparing the offsets of the accessed ranges.
    AliasResult ComputeOffsetAlias(AliasLocation& locationA,
                                   AliasLocation& locationB);

    //
    bool CanResultBeValid(AliasLocation& locationA,
                          AliasLocation& locationB);

    // Methods for testing the values an induction variable can take.
    bool IsGreaterThanZero(SimpleIV iv, Block* testBlock = nullptr);

    bool IsPositive(SimpleIV iv, Block* testBlock = nullptr);

    // Returns 'true' if the value of the first IV is always
    // greater than the value of the second one.
    bool IsGreater(SimpleIV ivA, SimpleIV ivB, Block* testBlock = nullptr);

    // Methods for testing the value of an operand.
    bool IsNotZero(Operand* op, Block* testBlock = nullptr);

    bool IsGreaterThanZero(Operand* op, Block* testBlock = nullptr);

    bool IsSmallerThanZero(Operand* op, Block* testBlock = nullptr);

    bool IsGreaterOrEqualToZero(Operand* op, Block* testBlock = nullptr);

    // Methods for testing the relation of two operands.
    bool IsGreater(Operand* opA, Operand* opB, Block* testBlock = nullptr);

    bool IsSmaller(Operand* opA, Operand* opB, Block* testBlock = nullptr);

    bool IsGreaterOrEqual(Operand* opA, Operand* opB, Block* testBlock = nullptr);

    bool AreNotEqual(Operand* opA, Operand* opB, Block* testBlock = nullptr);

    bool AreEqual(Operand* opA, Operand* opB, Block* testBlock = nullptr);

    // Returns 'true' if 'opA' is equal to 'opB' plus/minus a constant.
    bool IsAddedSubtractedOperand(Operand* opA, Operand* opB);

    // Checks if the specified operand forms a simple induction variable.
    bool IdentifyIV(Operand* op, SimpleIV& ivInfo);

    // Tries to retrieve the IV associated with the specified
    // operand from the cache.
    bool RetrieveCachedIV(Operand* op, SimpleIV& ivInfo);

    // Adds the specified from the cache, 
    // evicting another one if necessary.
    void AddIVToCache(SimpleIV& ivInfo);

    // Tries to identify the operation that increments/decrements
    // the specified IV and begins with 'op'.
    // 'isNegativeStep' is set to 'true' if the IV is/might be decremented.
    // 'isOverflowUndefined' is set to true if the overflow is undefined
    // for the found operation (pointer arithmetic or operation marked 'uso').
    Operand* IdentifyIVIncrement(Operand* op, Operand* requiredIV,
                                 bool& isNegativeStep, bool& isOverflowUndefined);

    // Tries to identify a pointer IV increment formed from one
    // or more 'addr' instructions. The meaning of the two flags is as above.
    Operand* IdentifyAddrIVIncrement(Operand* op, Operand* requiredIV,
                                     bool& isNegativeStep, bool& isOverflowUndefined);

    // Returns 'true' if the operand might be negative (if we're not
    // certain it is positive we presume it is negative).
    // If 'isAdd' is 'false' the operand is considered to be subtracted.
    bool IsNegativeStep(Operand* stepOp, Block* testBlock, bool isAdd);

    // Tries to identify the last value the specified IV can take.
    // 'comparisonOrder' is set to the order of the comparison which
    // decides the execution of the loop. 
    // 'isEndValueInclusive' is set to 'true' if the order is <= or >=.
    Operand* IdentifyEndValue(PhiInstr* ivInstr, OrderType& comparisonOrder,
                              bool& isEndValueInclusive);

    // Returns 'true' if we can handle an IV that uses the order
    // of the specified comparison instruction.
    bool IsEligibleOrder(CmpInstrBase* cmpInstr, Operand* ivOp) {
        return (cmpInstr->IsEquality() == false) ||
               (ivOp->IsPointer() && cmpInstr->IsNotEqual());
    }

    // Returns 'true' if the result of the specified comparison instruction
    // decides if the loop with header 'loopHeader' executes or not.
    bool DecidesLoopExecution(CmpInstrBase* cmpInstr, Block* loopHeader);

    // Returns 'true' if 'body' is the only block of a small loop.
    bool IsSingleBlockLoop(Block* body, Block* loopHeader);

    // Returns 'true' if we can't handle an IV having a specific
    // combination of comparison order and positive/negative step.
    bool IsInvalidEndValue(OrderType comparisonOrder, bool isNegativeStep);

    // Identifies the components (index, factor, adjustment)
    // that form the index of an 'index'/'addr' instruction.
    bool ExtractIndex(AliasLocation& location, SimpleIndex& indexInfo);

    // Tries to extract the index component for an expression
    // of the following form: '(i + C1) * C2'.
    Operand* ExtractFactoredIndex(Operand* indexOp, Operand* mulFactor,
                                  Operand*& adjustment, Block* block);

    // Tries to fold the location offset into the index adjustment.
    bool FoldOffsetIntoIndex(AliasLocation& location, __int64& adjustment);

    // Tries to fold the offset induced by an 'addr' instruction
    // that acts as the base operand into the index adjustment.
    bool FoldAddressIntoIndex(AliasLocation& location, __int64& adjustment);

    // Returns 'true' if the indices are definitely independent
    // (they have no common point or the common point lies out
    //  of range of the values the induction variable can take).
    // Uses the GCD dependence test.
    bool AreIndicesIndependent(SimpleIndex& indexA, 
                               SimpleIndex& indexB, 
                               AliasLocation& locationA, 
                               AliasLocation& locationB,
                               Block* testBlock = nullptr);

    // Tests if two indices are independent using simple and fast rules.
    AliasResult AreIndicesIndependentSimple(SimpleIndex& indexA, 
                                            SimpleIndex& indexB,
                                            AliasLocation& locationA, 
                                            AliasLocation& locationB,
                                            Block* testBlock = nullptr);

    // Test if two indices are independent for the case when
    // the factors are the same but different IVs are used.
    bool AreIndicesIndependent(SimpleIndex& indexA, 
                               SimpleIndex& indexB, 
                               AliasLocation& locationA, 
                               AliasLocation& locationB,
                               __int64 adjustmentA, 
                               __int64 adjustmentB,
                               Block* testBlock = nullptr);

    // Uses the GCD dependence test to determine if
    // the specified indices are independent or not.
    bool AreIndicesIndependentSameIndex(SimpleIndex& indexA, 
                                        SimpleIndex& indexB, 
                                        AliasLocation& locationA, 
                                        AliasLocation& locationB,
                                        __int64 factorA, __int64 factorB,
                                        __int64 adjustmentA, 
                                        __int64 adjustmentB,
                                        Block* testBlock = nullptr);

    // Acts as a fallback when the GCD test reports the indices
    // might not be independent. Tests if the point when the indices
    // are equal are outside the range of the induction variable.
    bool IntersectionPointIsOutOfRange(Operand* indexOp, 
                                       __int64 factorA, 
                                       __int64 factorB,
                                       __int64 adjustmentA, 
                                       __int64 adjustmentB,
                                       Block* testBlock = nullptr);

    // Computes the final index factor and adjustment by summing
    // the original ones with the ones resulted from folding offsets/addr.
    void ComputeIndexValues(SimpleIndex& indexA, 
                            SimpleIndex& indexB, 
                            __int64& factorA, 
                            __int64& factorB,
                            __int64& adjustmentA, 
                            __int64& adjustmentB);

    // Tries to express 'indexB' in terms of the induction variable
    // of 'indexA', possibly introducing a new factor and adjustment.
    bool ReplaceIndexOperand(SimpleIndex& indexA, 
                             SimpleIndex& indexB,
                             __int64& factorB, 
                             __int64& adjustmentB);

    bool ReplaceAddedOperand(SimpleIndex& index, 
                             Operand* otherIndex,
                             IntConstant* intConst, 
                             __int64& factor,
                             __int64& adjustment);

    bool ReplaceMultipliedOperand(SimpleIndex& index, 
                                  Operand* otherIndex,
                                  IntConstant* intConst,
                                  __int64& factor,
                                  __int64& adjustment);

    bool ReplaceMultipliedAddedOperand(SimpleIndex& index, 
                                       Operand* otherIndex,
                                       IntConstant* addConst, 
                                       IntConstant* mulConst,
                                       __int64& factor, 
                                       __int64& adjustment);

    bool ReplaceAddedMultipliedOperand(SimpleIndex& index, 
                                       Operand* otherIndex,
                                       IntConstant* addConst, 
                                       IntConstant* mulConst,
                                       __int64& factor, 
                                       __int64& adjustment);

    // Computes the greatest common divisor of the specified numbers.
    __int64 ComputeGCD(__int64 a, __int64 b);

    // Determines the alias of two potentially record accesses.
    AliasResult ComputeRecordAlias(AliasLocation& locationA,
                                   AliasLocation& locationB);

    AliasResult ComputeRecordAlias(ElementInstr* instrA, 
                                   ElementInstr* instrB,
                                   AliasLocation& locationA,
                                   AliasLocation& locationB);

    ElementInstr* GetAncestor(ElementInstr* elemInstrA, 
                              ElementInstr* elemInstrB,
                              __int64& offsetDifference);

    // Methods for determining the alias of two potentially array accesses.
    AliasResult ComputeArrayAlias(AliasLocation& locationA,
                                  AliasLocation& locationB);

    AliasResult ComputeSingleArrayAlias(AddressInstr* instr, 
                                        Operand* other,
                                        AliasLocation& locationA,
                                        AliasLocation& locationB);

    AliasResult ComputeBasicArrayAlias(AddressInstr* instrA, 
                                       AddressInstr* instrB,
                                       AliasLocation& locationA,
                                       AliasLocation& locationB);

    AliasResult ComputeVariableArrayAlias(AddressInstr* instrA,
                                          AddressInstr* instrB,
                                          AliasLocation& locationA,
                                          AliasLocation& locationB);

    AliasResult ComputeVariableArrayAlias(AddressInstr* instrA,
                                          AddressInstr* instrB,
                                          Instruction* indexInstrA,
                                          Instruction* indexInstrB,
                                          AliasLocation& locationA,
                                          AliasLocation& locationB);

    AliasResult ComputeVariableArrayAlias(AddressInstr* instrA,
                                          AddressInstr* instrB,
                                          Instruction* instr,
                                          Operand* other,
                                          AliasLocation& locationA,
                                          AliasLocation& locationB);

    // Returns 'true' if a two array accesses using a constant index
    // can't overlap, taking any possible offset into consideration.
    bool IsNoArrayElementOverlap(__int64 constIndexA,
                                 __int64 constIndexB,
                                 AddressInstr* instrA, 
                                 AddressInstr* instrB,
                                 AliasLocation& locationA,
                                 AliasLocation& locationB);

    // Returns the element size of the array access.
    __int64 GetElementSize(AddressInstr* instr);

    // Tries to determine if there is alias using the GCD test.
    AliasResult ComputeDependenceAlias(AliasLocation locationA,
                                       AliasLocation locationB);

    // Returns the block that should be used when querying Operand Info.
    Block* GetTestBlock(AliasLocation& locationA,
                        AliasLocation& locationB);

    // Methods for determining if there is alias between two pointers.
    AliasResult ComputePointerAlias(AliasLocation& locationA,
                                    AliasLocation& locationB);

    AliasResult ComputePointerAliasWithTwoIVs(AliasLocation& locationA,
                                              AliasLocation& locationB,
                                              SimpleIV ivA, SimpleIV ivB,
                                              Block* testBlock = nullptr);

    AliasResult ComputePointerAliasWithSingleIV(AliasLocation& locationA,
                                                AliasLocation& locationB,
                                                SimpleIV ivA, SimpleIV ivB,
                                                bool hasIVA, bool hasIVB,
                                                Block* testBlock = nullptr);

    AliasResult ComputePointerAliasWithEndValue(AliasLocation& ivLocation,
                                                AliasLocation& otherLocation,
                                                SimpleIV& singleIV, Operand* other);

    // Tries to fold the offset induced by an 'addr' base operand
    // into the offset of the location.
    void FoldAddrIntoLocation(AliasLocation& location);

    // Methods for moving the constant operand on the right side.
    void CanonicalizeAdd(Instruction* instr);

    void CanonicalizeMul(Instruction* instr);

    // Methods for creating integer constants.
    IntConstant* GetIntConstant(__int64 value, Block* block) {
        auto unit = block->ParentFunction()->ParentUnit();
        return unit->Constants().GetInt64(value);
    }

    IntConstant* GetZeroInt(Block* block) {
        return GetIntConstant(0, block);
    }

public:
    SymbolicAliasAnalysis(AliasInfo* parent) : 
            AliasAnalyzer(parent), cachedIVs_(0), oldestIV_(0) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    virtual AliasResult ComputeAlias(AliasLocation locationA, 
                                     AliasLocation locationB);

    virtual string Name() {
        return "Symbolic Alias Analysis";
    }
};

} // namespace Analysis
#endif