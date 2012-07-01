// OperandInfo.hpp
// Copyright (c) Lup Gratian
//
// Provides various information about operands, 
// like which bits are definitely one and zero.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_OPERAND_INFO_HPP
#define PC_ANALYSIS_OPERAND_INFO_HPP

#include "TypeInfo.hpp"
#include "IntArithmetic.hpp"
#include "FloatArithmetic.hpp"
#include "RangeTag.hpp"
#include "KnownBitsTag.hpp"
#include "CFamilyTag.hpp"
#include "SparseBitVector.hpp"
#include "ProfileInfo.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Operand.hpp"
#include "../IR/IRTypes.hpp"
#include "../IR/Constants.hpp"
#include "../IR/References.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Targets/TargetInfo.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
using namespace IR;
using namespace Target;
using namespace Base;

namespace Analysis {

// The certainty that an operand has a specified value.
enum ProbabilityType {
    Probability_Always, // The value is statically known.
    Probability_Percent // The operand might change at runtime, but it's known
                        // to be a certain value in some cases.
};


struct ValueProbability {
    Constant* Value;      // The known/probable value of the operand.
    ProbabilityType Type; // How the value holds (always or with a probability).
    float Probability;    // The probability that the operand has the value.

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ValueProbability() {}

    ValueProbability(Constant* value, ProbabilityType type = Probability_Always,
                     float probability = 1.0) :
            Value(value), Type(type), Probability(probability) {}

    ValueProbability(const ValueProbability& other) :
            Value(other.Value), Type(other.Type), Probability(other.Probability) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    float GetProbability() const {
        if(Type == Probability_Always) return 1.0f;
        else return Probability;
    }

    bool operator== (const ValueProbability& other) const {
        return false;
    }

    bool operator< (const ValueProbability& other) const {
        if(Type != other.Type) {
            return Type == Probability_Always ? -1 : 1;
        }
        else if(Type == Probability_Percent) {
            return Probability > other.Probability;
        }
        else return 0;
    }
};


// Forward declarations.
class ConstantFolder;

class OperandInfo {
public:
    typedef StaticList<ValueProbability, 4> ValueList;

private:
	typedef IntArithmetic IA;
    typedef FloatArithmetic FA;
    typedef unsigned __int64 Mask;


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    //! TODO: make them controls
    static const int MAXIMUM_PROBABLE_LEVEL = 5;
    static const int MAXIMUM_PROBABLE_VALUES = 2;
    static const int MAXIMUM_PHI_INCOMING = 8;
    static const int MAXIMUM_BITS_DEPTH = 8;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Unit* unit_;
	TargetInfo* target_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ProfileInfo* GetProfileInfo() {
        return nullptr;
    }

    // Methods for estimating the bits that are definitely zero.
	void EstimateZeroBitsImpl(Operand* op, unsigned __int64& bits, int depth);
    void EstimateZeroBitsShl(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsShr(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsUshr(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsAddSub(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsMul(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsDiv(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsUdiv(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsMod(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsUmod(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsZextTrunc(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsSext(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsPtoi(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsItop(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsPhi(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsCall(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateZeroBitsQuest(Instruction* definingInstr, Mask& bits, int depth);

    bool TryTakeZeroBitsFromCache(Operand* op, Mask& bits);
    bool IsOperandPositive(Operand* op, IntConstant*& intConst);

    // Methods for estimating the bits that are definitely one.
    void EstimateOneBitsImpl(Operand* op, unsigned __int64& bits, int depth);
    void EstimateOneBitsShr(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateOneBitsAddSub(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateOneBitsUmod(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateOneBitsMod(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateOneBitsZextTrunc(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateOneBitsSext(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateOneBitsPhi(Instruction* definingInstr, Mask& bits, int depth);
    void EstimateOneBitsQuest(Instruction* definingInstr, Mask& bits, int depth);

    bool TryTakeOneBitsFromCache(Operand* op, Mask& bits);
    bool IsOperandNegative(Operand* op, IntConstant*& intConst);

    // Tries to return the range tag associated with the specified parameter
    // in the entry block (this range tag is usually set by interprocedural
    // propagation algorithms).
    RangeTag* GetRangeTagForEntry(Parameter* parameter);

    // Returns the operand that acts as the base (starting point)
    // for a series of addressing instructions ('addr', 'index', 'elem').
    Operand* GetBaseOperand(Operand* op);

    // Adds to 'valueList' the most probable values of the specified operand.
    // 'testBlock' indicates the block where the operand is used,
    // 'visited' marks the blocks that were processed (used to prevent infinite loops).
    int ComputeProbableValues(Operand* op, ValueList& valueList,
                              Block* testBlock, SparseBitVector& visited,
                              ConstantFolder& folder, int level);

    // Computes the probability of the result operand 
    // based on its two source operands using the rule P(result) = P(a) * P(b).
    void ComputeProbability(ValueProbability& a, ValueProbability& b, 
                            ValueProbability& result);
    
    // Returns the probability that the specified CFG edge is taken.
    // The probability is based on the successors of 'fromBlock'.
    float EdgeProbability(Block* fromBlock, Block* toBlock);

    // Methods to compute the probable values for various instructions.
    int ComputeProbableValuesArithLogical(Instruction* instr, ValueList& valueList,
                                          Block* testBlock, SparseBitVector& visited,
                                          ConstantFolder& folder, int level);

    int ComputeProbableValuesCompare(CmpInstrBase* instr, ValueList& valueList,
                                     Block* testBlock, SparseBitVector& visited,
                                     ConstantFolder& folder, int level);

    int ComputeProbableValuesQuestion(QuestionInstr* instr, ValueList& valueList,
                                      Block* testBlock, SparseBitVector& visited,
                                      ConstantFolder& folder, int level);

    int ComputeProbableValuesConversion(ConversionInstr* instr, ValueList& valueList,
                                        Block* testBlock, SparseBitVector& visited,
                                        ConstantFolder& folder, int level);

    int ComputeProbableValuesLoad(LoadInstr* instr, ValueList& valueList,
                                  Block* testBlock, SparseBitVector& visited,
                                  ConstantFolder& folder, int level);

    int ComputeProbableValuesPhi(PhiInstr* instr, ValueList& valueList,
                                 Block* testBlock, SparseBitVector& visited,
                                 ConstantFolder& folder, int level);

    // Tries to compute the most probable values for the specified operand
    // using range information. If the operand is known to be in an interval
    // [A, B] the probable values are considered to be A, A + 1, A + 2, ... B,
    // each with the same probability.
    int ComputeProbableValuesRange(Operand* op, ValueList& valueList, 
                                   Block* testBlock);

public:	
    OperandInfo(Unit* unit = nullptr, TargetInfo* target = nullptr) : 
            unit_(unit), target_(target) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Returns the size for the type of the specified operand.
	static int Size(const Operand* op, const TargetInfo* target) {
		// The size of the operand depends entirely on its type.
		return Analysis::TypeInfo::GetSize(op->GetType(), target);
	}

	// Returns the size of the variable referenced by the specified operand.
	// If no operand is not a variable it returns 0.
	static int GetVariableSize(const Operand* op, const TargetInfo* target);

	// Returns the alignment for the type of the specified operand.
	static int GetAlignment(const Operand* op, const TargetInfo* target) {
		return Analysis::TypeInfo::GetAlignment(op->GetType(), target);
	}

	// Returns the alignment of the variable referenced by the specified operand.
	// If no variables is reference this returns 0.
	static int GetVariableAlignment(const Operand* op, const TargetInfo* target);

	// Returns the length, in characters, of the string represented
    // by the specified constant global variable, or -1 if the operand is not valid.
	// Note that the zero-terminator character is not counted.
	int GetStringLength(const Operand* op);

    // Tries to compute the bits of the specified operand
    // that are definitely zero. Note that if a bit is not zero it doesn't mean
    // it's one, instead it should be treated as "unknown".
	void EstimateZeroBits(Operand* op, Mask& bits, int depth = -1);

    // Tries to compute the bits of the specified operand
    // that are definitely one. Note that if a bit is not one it doesn't mean
    // it's zero, instead it should be treated as "unknown".
    void EstimateOneBits(Operand* op, Mask& bits, int depth = -1);

    // Returns the (conservative) maximum value for the specified operand.
    // If 'testBlock' is specified then value-range information
    // valid for the block is used and preferred.
    __int64 GetMaximumValue(Operand* op, bool isSigned = true, 
                            Block* testBlock = nullptr);

    // Returns the (conservative) minimum value for the specified operand.
    // If 'testBlock' is specified then value-range information
    // valid for the block is used and preferred.
    __int64 GetMinimumValue(Operand* op, bool isSigned = true, 
                            Block* testBlock = nullptr);

    // Returns 'true' if the specified operand is definitely >= 0.
    // If 'testBlock' is specified then value-range information
    // valid for the block is used and preferred.
    bool IsPositive(Operand* op, Block* testBlock = nullptr, int level = 3);

    // Returns 'true' if the specified operand is definitely < 0.
    // If 'testBlock' is specified then value-range information
    // valid for the block is used and preferred.
    bool IsNegative(Operand* op, Block* testBlock = nullptr, int level = 3);

    // Returns 'true' if the specified operand is definitely zero.
    // If 'testBlock' is specified then value-range information
    // valid for the block is used and preferred.
    bool IsZero(Operand* op, Block* testBlock = nullptr, int level = 3);

    // Returns 'true' if the specified operand is definitely not zero.
    // If 'testBlock' is specified then value-range information
    // valid for the block is used and preferred.
    bool IsNotZero(Operand* op, Block* testBlock = nullptr, int level = 3);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that the operands are definitely equal,
    // or 'Result_No' if the operands are definitely not equal.
    // Should be used only for integer and pointer operands.
    RangeResult AreEqual(Operand* a, Operand* b, Block* testBlock = nullptr);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that the operands are definitely not equal,
    // or 'Result_No' if the operands are definitely equal.
    // Should be used only for integer and pointer operands.
    RangeResult AreNotEqual(Operand* a, Operand* b, Block* testBlock = nullptr);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that 'a' is definitely smaller than 'b',
    // or 'Result_No' if 'a' is definitely greater than 'b'.
    // Should be used only for integer and pointer operands.
    RangeResult IsSmaller(Operand* a, Operand* b, bool isSigned = true,
                          Block* testBlock = nullptr);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that 'a' is definitely smaller or equal to 'b',
    // or 'Result_No' if 'a' is definitely greater than 'b'.
    // Should be used only for integer and pointer operands.
    RangeResult IsSmallerOrEqual(Operand* a, Operand* b, bool isSigned = true,
                                 Block* testBlock = nullptr);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that 'a' is definitely greater than 'b',
    // or 'Result_No' if 'a' is definitely smaller than 'b'.
    // Should be used only for integer and pointer operands.
    RangeResult IsGreater(Operand* a, Operand* b, bool isSigned = true,
                          Block* testBlock = nullptr);

    // Returns 'Result_Yes' if, based on the known ranges,
    // it can be determined that 'a' is definitely greater or equal to 'b',
    // or 'Result_No' if 'a' is definitely smaller than 'b'.
    // Should be used only for integer and pointer operands.
    RangeResult IsGreaterOrEqual(Operand* a, Operand* b, bool isSigned = true,
                                 Block* testBlock = nullptr);

    // Returns 'true' if the addition of the specified operands
    // can overflow (uses estimated one/zero bit information).
    bool CanAddOverflow(Operand* opA, Operand* opB, 
                        bool hasUndefinedOverflow = false);

    // Returns 'true' if the specified bit is definitely zero.
    // Note that a 'false' response doesn't mean that the bit is one,
    // instead it means that its value is unknown.
    bool IsBitZero(Operand* op, int index, Block* testBlock = nullptr);

    // Returns 'true' if the specified bit is definitely one.
    // Note that a 'false' response doesn't mean that the bit is zero,
    // instead it means that its value is unknown.
    bool IsBitOne(Operand* op, int index, Block* testBlock = nullptr);

    // Returns 'true' if all bits in the specified range are
    // definitely zero. Note that a 'false' response doesn't mean 
    // that all bits are one, instead it means that their value is unknown.
    bool AreBitsZero(Operand* op, int firstIndex, int lastIndex,
                     Block* testBlock = nullptr);

    // Returns 'true' if all bits in the specified range are
    // definitely one. Note that a 'false' response doesn't mean 
    // that all bits are zero, instead it means that their value is unknown.
    bool AreBitsOne(Operand* op, int firstIndex, int lastIndex, 
                    Block* testBlock = nullptr);

    // Returns 'true' if the specified pointer is definitely null.
    // If 'testBlock' is specified then value-range information
    // valid for the block is used and preferred.
    bool IsPointerNull(Operand* op, Block* testBlock = nullptr);

    // Returns 'true' if the specified pointer is definitely not null.
    // If 'testBlock' is specified then value-range information
    // valid for the block is used and preferred.
    bool IsPointerNotNull(Operand* op, Block* testBlock = nullptr,
                          bool handlePhi = true);

    // Verifies if the specified operand is an integer constant,
    // and returns it if it's the case. If it's not it returns 'nullptr'.
    // Note that this may return a constant even if the operand 
    // is not an 'IntConstant' object, because it uses value-range 
    // (if 'testBlock' is set) and known one/zero bits information.
    IntConstant* GetIntConstant(Operand* op, Block* testBlock = nullptr);

    // Adds to 'valueList' the most probable values the operand can have.
    // These values are estimated based on range and profile information.
    // Returns the number of probable values found (it can be zero).
    int GetProbableIntConstant(Operand* op, ValueList& valueList,
                               Block* testBlock = nullptr);

    // Tries to return the range tag associated with the specified operand
    // valid in the test block. If the test block is not specified and
    // the operand is a parameter the range for the entry block is considered.
    RangeTag* GetRangeTag(Operand* op, Block* testBlock = nullptr);

    // Searches for a known range for the operand starting
    // with the specified block. If a valid range is found
    // it is stored in 'range' and 'true' is returned.
    bool GetRange(Operand* op, Block* testBlock, Range& range);
};

} // namespace Analysis
#endif