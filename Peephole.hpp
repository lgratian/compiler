// Peephole.hpp
// Copyright (c) Lup Gratian
//
// Defines the module that performs peephole optimizations. This is not exactly
// the "peephole" optimization described in the literature, but it's close enough.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_PEEPHOLE_HPP
#define PC_OPTIMIZATION_PEEPHOLE_HPP

#include "../IR/Instructions.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Constants.hpp"
#include "../IR/References.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Analysis/PatternMatching.hpp"
#include "../Analysis/ConstantFolder.hpp"
#include "../Analysis/IntArithmetic.hpp"
#include "../Analysis/FloatArithmetic.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/OperandInfo.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Log.hpp"
using namespace IR;
using namespace Analysis;
using namespace Base;
using namespace CompilationPass;

namespace Optimization {

// Used to log where a peephole replacement takes place.
// Useful when we want to know witch pattern matched.
#ifdef LOG
    #error "LOG already defined"
#elif 1
    #define LOG(value) LogReplacement(__FILE__, __LINE__, #value), value
#else
    #define LOG(value) value
#endif


class Peephole : public Pass {
private:
    enum CmpCmpComparand {
        CmpCmp_Zero,
        CmpCmp_Left,
        CmpCmp_Right
    };


	// Represents a value range from a constant global variable.
	struct ConstantRange {
		__int64 Value;      // The value represented by the range.
		__int64 StartIndex; // The first position where the value is found.
		__int64 Count;      // The number of consecutive values.

		ConstantRange() {}

		ConstantRange(__int64 value, __int64 startIndex, __int64 count = 0) :
		Value(value), StartIndex(startIndex), Count(count) {}

		ConstantRange(const ConstantRange& other) :
		Value(other.Value), StartIndex(other.StartIndex), Count(other.Count) {}
	};


	typedef IntArithmetic IA;
	typedef FloatArithmetic FA;
    typedef Analysis::TypeInfo TI;
	typedef StaticList<ConstantRange, 64> RangeList;
	typedef StaticList<__int64, 4> IndexList;
    typedef StaticList<Instruction*, 32> InstructionList;
    typedef StaticList<Operand*, 32> OperandList;
    typedef StaticList<Block*, 32> BlockList;

	Operand* resultOp_;       // The result of the simplification.
	IRGenerator  irGen_;      // The helper IR generator.
	ConstantFolder folder_;
    List<Instruction*> worklist_;
    OperandInfo opInfo_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    __int64 GetMaxConstArraySize() {
		//! This should consider the optimization level!!! (256 for max - an ASCII table)
		return 64;
	}

	bool ShouldLoadFromConstStruct() {
		//! This should return false for lower optimization levels!!!
		return true;
	}

	int MaxConstNestedLevels() {
		//! This should return 5-6 for maximum optimization level!!!
		return 3;
	}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Methods for logging.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    void LogReplacement(const char* file, int line, const char* value);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for obtaining a constant having a specific value.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	IntConstant* GetZeroInt(const Type* type) {
		return irGen_.GetIntConst(type, 0);
	}

	IntConstant* GetZeroInt(Operand* base) {
        return GetZeroInt(base->GetType());
	}

	IntConstant* GetOneInt(const Type* type) {
		return irGen_.GetIntConst(type, 1);
	}

	IntConstant* GetOneInt(Operand* base) {
        return GetOneInt(base->GetType());
	}

    FloatConstant* GetOneFloat(const Type* type) {
		return irGen_.GetFloatingConst(type, 1.0);
	}

	FloatConstant* GetOneFloat(Operand* base) {
        return GetOneFloat(base->GetType());
	}

	IntConstant* GetBool(bool state, const Type* type = nullptr) {
		// If no type was provided we use 'int32'.
		if(type == nullptr) {
			type = IntegerType::GetInt32();
		}

		if(state) return GetOneInt(type);
		else return GetZeroInt(type);
	}
	
	IntConstant* GetBool(bool state, Operand* base) {
		return GetBool(state, base->GetType());
	}

	IntConstant* GetMinusOneInt(const Type* type) {
		return irGen_.GetIntConst(type, -1);
	}

	IntConstant* GetMinusOneInt(Operand* base) {
		const IntegerType* type = base->GetType()->As<IntegerType>();
		return GetMinusOneInt(type);
	}

	FloatConstant* GetZeroFloat(const Type* type) {
		return irGen_.GetFloatingConst(type, 0.0);
	}

	FloatConstant* GetZeroFloat(Operand* base) {
		const FloatingType* type = base->GetType()->As<FloatingType>();
		return GetZeroFloat(type);
	}

	UndefinedConstant* GetUndefined(const Type* type) {
		return irGen_.GetUndefinedConst(type);
	}

	UndefinedConstant* GetUndefined(Operand* base) {
		return irGen_.GetUndefinedConst(base->GetType());
	}

	NullConstant* GetNullptr(const Type* type) {
		return irGen_.GetNullConst(type);
	}

	NullConstant* GetNullptr(Operand* base) {
		return irGen_.GetNullConst(base->GetType());
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Some helper methods.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	void InstructionSimplified(Instruction* before, Operand* after);

	// Returns a temporary having the type of the specified operand.
	Operand* GetTemporary(Operand* base) {
		return irGen_.GetTemporary(base->GetType());
	}

    Operand* GetTemporary(const Type* type) {
        return irGen_.GetTemporary(type);
    }

    IntConstant* AsIntConstant(Operand* op, Block* testBlock) {
        OperandInfo opInfo(irGen_.GetUnit(), GetTarget());
        return opInfo.GetIntConstant(op, testBlock);
    }

    bool IsZeroInt(Operand* op, Block* testBlock) {
        if(op->IsZeroInt()) return true;
        else {
            OperandInfo opInfo(irGen_.GetUnit(), GetTarget());
            return opInfo.IsZero(op, testBlock);
        }
    }

    bool IsNotZeroInt(Operand* op, Block* testBlock) {
        if(op->IsZeroInt()) return true;
        else {
            OperandInfo opInfo(irGen_.GetUnit(), GetTarget());
            return opInfo.IsNotZero(op, testBlock);
        }
    }

    bool IsOneInt(Operand* op, Block* testBlock) {
        if(op->IsOneInt()) return true;
        else if(auto intConst = AsIntConstant(op, testBlock)) {
            return intConst->IsOne();
        }
        else return false;
    }

    bool IsMinusOneInt(Operand* op, Block* testBlock) {
        if(op->IsMinusOneInt()) return true;
        else if(auto intConst = AsIntConstant(op, testBlock)) {
            return intConst->IsMinusOne();
        }
        else return false;
    }

    bool AreEqual(Operand* a, Operand* b, Block* testBlock) {
        if(a == b) return true;

        OperandInfo opInfo(irGen_.GetUnit());
        return opInfo.AreEqual(a, b, testBlock) == Result_Yes;
    }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying frequent tasks.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // If 'opA' is a constant and 'opB' is not it swaps the operands.
	bool MoveConstantToRight(Operand*& opA, Operand*& opB);

	// Returns 'true' if the operand performs '~a'.
	bool IsLogicalNot(Operand* op, Operand** result = nullptr);

	// Returns the specified integer operand negated.
	Operand* NegateInt(Operand* op);

	// Returns the specified floating operand negated.
	Operand* NegateFloat(Operand* op);

    // Returns 'true' if 'b == a + 1'.
	bool IsLeftRightMinusOne(IntConstant* a, IntConstant* b);

    // Creates a 'ptop' instruction if the operand's type
    // is not 'originalType'.
    Operand* InsertPtopIfRequired(Operand* op, const Type* originalType);

    // Creates a 'sext' or 'zext' instruction (depends on 'isSigned')
    // if the operand's type is not 'type'.
    Operand* CreateIntCastIfRequired(Operand* op, const Type* type, 
                                     bool isSigned = true);
    
    // Strips all pointer cast from the specified operand.
    Operand* WithoutPointerCasts(Operand* op);

    // Creates a binary instruction ('add', 'or', for ex.).
    Operand* CreateBinaryInstruction(Opcode opcode, Operand* leftOp, 
                                     Operand* rightOp, const Type* resultType,
                                     Instruction* insertionPoint = nullptr,
                                     Instruction* previousInsertionPoint = nullptr,
                                     FloatMode mode = FP_Exact);

    // Returns '~op' (the bitwise complement).
	Operand* GetNegated(Operand* op) {
		auto xorOp = GetTemporary(op);
		irGen_.GetXor(op, GetMinusOneInt(op), xorOp);
		return xorOp;
	}

    // Transfers the 'undefined signed overflow' flag
    // from 'instr' to the instruction that defines 'op'.
    Operand* PreserveUSO(Instruction* instr, Operand* op);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying the task of creating comparisons.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Creates an instruction that compares the two operands
    // using the specified order.
	Operand* CreateCompare(Operand* opA, Operand* opB, OrderType order, 
						   const Type* resultType, bool isUcmp = false);
	
	// Creates an instruction that verifies if the operand
    // lies between the specified integer constants.
    // 'op > leftConst && op < rightConst'
	Operand* CreateRangeCompare(Operand* op, IntConstant* leftConst,
								IntConstant* rightConst, const Type* resultType,
								bool isUcmp = false, bool noLeftInc = false);

    // Creates an instruction that verifies if the operand
    // lies outside the specified integer constants.
    // 'op < leftConst || op > rightConst'
	Operand* CreateOutOfRangeCompare(Operand* op, IntConstant* leftConst,
								     IntConstant* rightConst, const Type* resultType,
								     bool isUcmp = false, bool noLeftInc = false);


	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'add' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleAdd(Operand* opA, Operand* opB, Block* block, 
                       bool constMoveAllowed = true);
	Operand* HandleAddSimple(Operand* opA, Operand* opB, Block* block,
                             bool createAllowed = true);
	Operand* HandleAddComplex(Operand* opA, Operand* opB, Block* block);
    Operand* HandleAddDistributive(Operand* opA, Operand* opB, Block* block);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'fadd' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleFadd(Operand* opA, Operand* opB, FloatMode mode);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'sub' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleSub(Operand* opA, Operand* opB, Block* block);
	Operand* HandleSubSimple(Operand* opA, Operand* opB, Block* block,
                             bool createAllowed = true);
	Operand* HandleSubAssociative(Operand* opA, Operand* opB, Block* block);
	Operand* HandleSubComplex(Operand* opA, Operand* opB, Block* block);
    Operand* HandleSubPointers(Operand* opA, Operand* opB);

    Operand* CreateBaseDifference(Operand* baseOpA, Operand* baseOpB,
                                  const Type* castType);

    Operand* CreateMultipliedOffset(const Type* offsetType,Operand* offsetOp, 
                                    const Type* resultType, 
                                    bool negatedOffsetOp = false);

    Operand* CreateFieldOffset(ElementInstr* elemInstr, const Type* resultType,
                               bool negateOffset = false);

    Operand* HandleSubPointersAddrIndex(AddressInstr* addrInstrA, 
                                        AddressInstr* addrInstrB, 
                                        const Type* castType);
	

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'fsub' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleFsub(Operand* opA, Operand* opB, FloatMode mode);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'mul' and 'fadd' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleMul(Operand* opA, Operand* opB, Block* block,
                       bool constMoveAllowed = true);
	Operand* HandleMulSimple(Operand* opA, Operand* opB, Block* block,
                             bool createAllowed = true);
	Operand* HandleMulComplex(Operand* opA, Operand* opB, Block* block);

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'fmul' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleFmul(Operand* opA, Operand* opB, FloatMode mode);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'div' and 'udiv' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleDiv(Operand* opA, Operand* opB, Block* block,
                       bool hasNoRemainder = false);
	Operand* HandleUdiv(Operand* opA, Operand* opB, Block* block);
	Operand* HandleDivCommon(Operand* opA, Operand* opB, Block* block, bool isDiv);
	

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'fdiv' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleFdiv(Operand* opA, Operand* opB, FloatMode mode);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'mod' and 'umod' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleMod(Operand* opA, Operand* opB, Block* block);
	Operand* HandleUmod(Operand* opA, Operand* opB, Block* block);
	Operand* HandleModCommon(Operand* opA, Operand* opB, Block* block, bool isMod);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'and' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleAnd(Operand* opA, Operand* opB, Block* block,
                       bool constMoveAllowed = true);
    Operand* HandleAndSimple(Operand* opA, Operand* opB, Block* block,
                             bool createAllowed = true);
    Operand* HandleAndCmpCmp(Operand* opA, Operand* opB, Block* block);
	Operand* HandleAndOneConst(Operand* opA, Operand* opB, Block* block);
	Operand* HandleAndTwoConst(Operand* opA, Operand* opB, Block* block);
	Operand* HandleAndIntCmp(Operand* opA, Operand* opB, Block* block);

    Operand* HandleAndIntCmpConst(CmpInstrBase* leftCmp, CmpInstrBase* rightCmp,
								  IntConstant* leftConst, IntConstant* rightConst);

	Operand* HandleAndIntCmpSameConst(CmpInstrBase* leftCmp, CmpInstrBase* rightCmp,
                                      Block* block);

	Operand* HandleAndFloatCmp(Operand* opA, Operand* opB);
	Operand* HandleAndBit(Operand* opA, Operand* opB, Block* block);
    Operand* HandleAndOperandInfo(Operand* opA, Operand* opB, Block* block);

    
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'or' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleOr(Operand* opA, Operand* opB, Block* block,
                      bool constMoveAllowed = true);
    Operand* HandleOrSimple(Operand* opA, Operand* opB, Block* block,
                            bool createAllowed = true);
    Operand* HandleOrCmpCmp(Operand* opA, Operand* opB, Block* block);
    Operand* HandleOrOperandInfo(Operand* opA, Operand* opB);

	Operand* HandleOrIntCmp(Operand* opA, Operand* opB, Block* block);
	Operand* HandleOrIntCmpConst(CmpInstrBase* leftCmp, CmpInstrBase* rightCmp,
								 IntConstant* leftConst, IntConstant* rightConst);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'xor' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleXor(Operand* opA, Operand* opB, Block* block,
                       bool constMoveAllowed = true);
    Operand* HandleXorSimple(Operand* opA, Operand* opB, Block* block,
                             bool createAllowed = true);
	Operand* HandleXorIntCmp(Operand* opA, Operand* opB, Block* block);
	Operand* HandleXorConst(Operand* opA, Operand* opB, Block* block);

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying shift instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleShl(ShlInstr* instr);
	Operand* HandleShr(ShrInstr* instr);
	Operand* HandleUshr(UshrInstr* instr);
	Operand* HandleShiftCommon(LogicalInstr* instr);
    Operand* HandleShiftSimple(Operand* opA, Operand* opB, bool isShl, 
                               Block* block, bool createAllowed = true);
	Operand* HandleShiftConst(Operand* opA, Operand* opB, LogicalInstr* instr);

	bool CanSimplifyShiftWithConst(LogicalInstr* instr);
	bool CanSimplifyShiftWithConstImpl(Operand* op, IntConstant* shiftAmount, bool isLeft);
	Operand* SimplifyShiftWithConst(Operand* op, IntConstant* shiftAmount, bool isLeft);

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying comparison instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleCmp(CmpInstr* instr);
	Operand* HandleUcmp(UcmpInstr* instr);
	Operand* HandleFcmp(FcmpInstr* instr);
	Operand* HandleFcmpFraction(FcmpInstr* instr, ConversionInstr* itofInstr,
								FloatConstant* floatConst, __int64 floatToInt);

	Operand* TestItofRange(FcmpInstr* instr, ConversionInstr* itofInstr,
						   FloatConstant* floatConst);

    void ShrinkIntegerCmpConstants(CmpInstrBase* instr);
    void CanonicalizeIntegerCmp(CmpInstrBase* instr);

    bool MatchCmpCmpOperands(CmpInstrBase* cmpInstrA, CmpInstrBase* cmpInstrB,
                             CmpCmpComparand comp, bool expectAnd,
                             Operand*& a, Operand*& b, Operand*& d);
    Operand* HandleUcmpAddress(UcmpInstr* instr);
	Operand* HandleIntegerCmp(CmpInstrBase* instr);
    Operand* HandleIntegerCmpWithZero(Operand* opA, Operand* opB,
                                      Operand* resultOp, OrderType order, 
                                      bool isUnsigned, Block* block);

	Operand* HandleIntegerCmpConstGeneric(Operand* opA, IntConstant* intConst, 
										  Operand* resultOp, OrderType order, 
										  bool isSigned, Block* block);

	Operand* HandleIntegerCmpConst(Operand* opA, IntConstant* intConst, 
                                   Operand* resultOp, OrderType order,
                                   Block* block);

	Operand* HandleIntegerCmpBool(Operand* opA, Operand* opB, Operand* resultOp, 
								  OrderType order, bool isSigned, Block* block);

	Operand* HandleIntegerCmpExtension(Operand* opA, Operand* opB, Operand* resultOp, 
									   OrderType order, bool isSigned, Block* block);

	bool EstimateOperandRange(Operand* opA, IntConstant* intConst, 
                              bool& minInclusive, bool& maxInclusive, 
                              __int64& rangeMin, __int64& rangeMax,
                              Block* block);

	Operand* HandleIntegerCmpPtr(Operand* opA, Operand* opB, Operand* resultOp, 
								 OrderType order);

	Operand* HandleIntegerCmpBin(Operand* opA, Operand* opB, Operand* resultOp, 
								 OrderType order, bool isSigned, Block* block);

    Operand* HandleIntegerCmpAddSubConst(Operand* opA, Operand* opB, Operand* resultOp, 
								         OrderType order, bool isSigned, Block* block);

    Operand* HandleIntegerCmpNegated(Operand* opA, Operand* opB, Operand* resultOp, 
								     OrderType order, bool isSigned, Block* block);

	Operand* HandleIntegerCmpLogical(LogicalInstr* logicalInstrA, 
									 LogicalInstr* logicalInstrB, Operand* resultOp, 
									 OrderType order, bool isSigned, Block* block);

	Operand* HandleIntegerCmpMul(MulInstr* mulInstrA, MulInstr* mulInstrB, 
								 Operand* resultOp, OrderType order, 
                                 bool isSigned, Block* block);

	Operand* HandleIntegerCmpXor(XorInstr* xorInstr, IntConstant* intConst, 
								 Operand* resultOp, OrderType order, 
                                 bool isSigned, Block* block);

	Operand* HandleIntegerCmpAnd(AndInstr* andInstr, IntConstant* intConst, 
								 Operand* resultOp, OrderType order, 
                                 bool isSigned, Block* block);

	Operand* HandleIntegerCmpAndZeroBits(AndInstr* andInstr, IntConstant* intConst, 
										 Operand* resultOp, OrderType order, 
                                         bool isSigned, Block* block);

	Operand* HandleIntegerCmpOr(OrInstr* orInstr, IntConstant* intConst, 
								 Operand* resultOp, OrderType order, 
                                 bool isSigned, Block* block);

	Operand* HandleIntegerCmpShl(ShlInstr* shlInstr, IntConstant* intConst,
								 Operand* resultOp, OrderType order, 
                                 bool isSigned, Block* block);

	Operand* HandleIntegerCmpShr(LogicalInstr* shrInstr, IntConstant* intConst, 
								 Operand* resultOp, OrderType order, 
                                 bool isSigned, Block* block);

	Operand* HandleIntegerCmpDiv(ArithmeticInstr* divInstr, IntConstant* intConst, 
								 Operand* resultOp, OrderType order, 
                                 bool isSigned, Block* block);

	__int64 ComputeIntegerCmpDivBounds(ArithmeticInstr* divInstr, IntConstant* C1,
									   IntConstant* C2, OrderType& order,
									   __int64& low, __int64& high, bool& lowOverflow,
									   bool& highOverflow, bool& lowOverflowLess,
									   bool& highOverflowLess);

	Operand* HandleIntegerCmpEquality(Instruction* instr, IntConstant* intConst, 
									  Operand* resultOp, OrderType order, 
                                      bool isSigned, Block* block);

	Operand* HandleIntegerCmpEqualityAnd(AndInstr* instr, IntConstant* intConst, 
									     Operand* resultOp, OrderType order,
                                         bool isSigned, Block* block);

	Operand* HandleIntegerCmpEqualityXor(XorInstr* instr, IntConstant* intConst, 
									     Operand* resultOp, OrderType order, 
                                         bool isSigned, Block* block);

    Operand* HandleIntegerCmpEqualityMul(MulInstr* instr, IntConstant* intConst, 
									     Operand* resultOp, OrderType order, 
                                         bool isSigned, Block* block);

	Operand* HandleIntegerCmpXorEquality(Operand* opA, Operand* opB, Operand* resultOp, 
										 OrderType order, bool isSigned, Block* block);

	Operand* HandleIntegerCmpSelf(Operand* opA, Operand* opB, Operand* resultOp, 
								  OrderType order, bool isSigned);

	Operand* HandleIntegerCmpLoad(LoadInstr* instr, IntConstant* intConst, 
								  Operand* resultOp, OrderType order, bool isSigned,
								  IntConstant* intConstInstr = nullptr,
								  Opcode opcode = Instr_Add);

    Operand* HandleCmpOnQuestion(CmpInstrBase* instr);

    Operand* HandleCmpOnQuestQuest(QuestionInstr* instrA, QuestionInstr* instrB,
                                   OrderType order, bool isSigned);

    // Methods for combining two comparison orders.
	OrderType CombineOrdersAnd(OrderType a, OrderType b, bool& orderFalse);
	OrderType CombineOrdersOr(OrderType a, OrderType b, bool& orderTrue);
	OrderType CombineOrdersXor(OrderType a, OrderType b, bool& orderFalse, 
                               bool& orderTrue);


	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying conversion instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleTrunc(TruncInstr* instr);
    Operand* HandleTruncOperandInfo(TruncInstr* instr);

	Operand* HandleZext(ZextInstr* instr);
    Operand* HandleZextOperandInfo(ZextInstr* instr);
	Operand* HandleZextOnTrunc(ZextInstr* instr);
	Operand* HandleZextOnCmp(CmpInstrBase* instr, const IntegerType* castType);

	Operand* HandleSext(SextInstr* instr);
    Operand* HandleSextOperandInfo(SextInstr* instr);

	Operand* HandleCastCast(Opcode opcode, Operand* op, const Type* castType);
    Operand* HandleCastCastTrunc(ConversionInstr* innerCast, const Type* castType);
    Operand* HandleCastCastSext(ConversionInstr* innerCast, const Type* castType);
    Operand* HandleCastCastZext(ConversionInstr* innerCast, const Type* castType);
    Operand* HandleCastCastItop(ConversionInstr* innerCast, const Type* castType);
    Operand* HandleCastCastPtoi(ConversionInstr* innerCast, const Type* castType);

	Operand* HandleFtrunc(FtruncInstr* instr);
	Operand* HandleFext(FextInstr* instr);
	Operand* HandleFtoi(FtoiInstr* instr);
	Operand* HandleFtoui(FtouiInstr* instr);
	Operand* HandleFtoiItofPair(ConversionInstr* instr);
	Operand* HandleItof(ItofInstr* instr);
	Operand* HandleItop(ItopInstr* instr);
	Operand* HandlePtoi(PtoiInstr* instr);
	Operand* HandleUitof(UitofInstr* instr);
    Operand* HandlePtop(PtopInstr* instr);

    int GetMaximumConversionDepth() const {
        //! TODO: should be a control
        //? should consider optimization level
        return 8;
    }

	bool CanBeTruncated(Operand* op, const Type* truncType, 
                        int depth = 0);

	bool CanBeZeroExtended(Operand* op, const Type* zextType, 
                           int& maskedBits, int depth = 0);

    bool CanBeZeroExtendedInstruction(Instruction* instr, const Type* zextType, 
        int& maskedBits, int depth);

    bool CanBeZeroExtendedInstructionUshr(Instruction* instr, const Type* zextType, 
        int& maskedBits, int depth);

	bool CanBeSignExtended(Operand* op, const Type* sextType, 
                           int depth = 0);

	Operand* ChangeExpressionType(Operand* op, const Type* newType, 
                                  Opcode opcode);

	Operand* CreateShlShrPair(Operand* op, IntConstant* amountOp, 
                              const Type* type);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'call' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Operand* HandleCall(CallInstr* instr);
    Operand* HandleCallPtop(CallInstr* instr, PtopInstr* ptopInstr);

    bool VerifyCallPtop(CallInstr* instr, PtopInstr* ptopInstr,
                        const FunctionType* functionType);

    Operand* HandleCallVarargs(CallInstr* instr, const FunctionType* functionType);
    UndefinedConstant* GetCallUndefined(CallInstr* instr);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'if' and 'switch' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    bool HandleIf(IfInstr* instr);
    void InvertIfTargets(IfInstr* instr);
    
    bool HandleSwitch(SwitchInstr* instr);
    bool HandleSwitchOnAddSub(SwitchInstr* switchInstr, ArithmeticInstr* arithInstr);
    bool HandleSwitchOnShl(SwitchInstr* switchInstr, ShlInstr* shlInstr);
    bool HandleSwitchOnMod(SwitchInstr* switchInstr);
    bool HandleSwitchOnExtension(SwitchInstr* switchInstr);
    bool HandleSwitchOperandInfo(SwitchInstr* switchInstr);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'addr' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Operand* HandleAddress(AddressInstr* instr);
    Operand* HandleAddrAddr(AddressInstr* addrInstr, Operand* baseOp, Operand* indexOp);

    Operand* HandleAddrItop(IntConstant* intConst, const Type* pointerType,
                            Operand* baseOp, IntConstant* indexOp);

    Operand* HandleAddrPtopAddr(PtopInstr* ptopInstr, AddressInstr* addrInstr, 
                                Operand* baseOp, Operand* indexOp);

    Operand* HandleAddrPtopIndex(PtopInstr* ptopInstr, IndexInstr* indexInstr, 
                                 Operand* baseOp, Operand* indexOp);

    Operand* HandleAddrPtopElem(PtopInstr* ptopInstr, ElementInstr* elemInstr, 
                                Operand* baseOp, Operand* indexOp);

    Operand* HandleAddrPtopArray(PtopInstr* ptopInstr, const ArrayType* arrayType, 
                                 Operand* baseOp, Operand* indexOp);

    Operand* HandleAddrPtopRecord(PtopInstr* ptopInstr, const RecordType* recordType, 
                                 Operand* baseOp, Operand* indexOp);

    Operand* HandleAddrIndex(IndexInstr* indexInstr, Operand* baseOp, Operand* indexOp);

    Operand* HandleAddrElem(ElementInstr* elemInstr, Operand* baseOp, Operand* indexOp);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'elem' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Operand* HandleElement(ElementInstr* instr);

    int FieldAtOffset(const RecordType* recordType, __int64 offset);

    bool ElementAtOffset(const ArrayType* arrayType, __int64 offset, 
                         __int64& fieldIndex);

    Operand* CreateAddressOfField(Operand* baseOp, int fieldIndex, 
                                  const RecordType* recordType);

    Operand* CreateAddressOfElement(Operand* baseOp, int index, 
                                    const ArrayType* arrayType);

	Operand* CreateAddressOfElement(Operand* baseOp, Operand* indexOp, 
                                    const ArrayType* arrayType);
    
    const RecordType* GetPointedRecord(Operand* op);

    const ArrayType* GetPointedArray(Operand* op);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'index' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Operand* HandleIndex(IndexInstr* instr);
    Operand* HandleIndexDifference(Operand* opA, Operand* opB);

    Operand* SubtractIndexOps(Operand* opA, Operand* opB, const Type* resultType);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying associative, commutative
    // and distributive instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Operand* HandleAssociative(Opcode opcode, Operand* opA, Operand* opB, 
                               Block* block, FloatMode mode = FP_Exact);

    Operand* HandleAssociativeAndCommutative(Opcode opcode, Operand* opA, 
                                             Operand* opB, Block* block,
                                             FloatMode mode = FP_Exact);

    Operand* HandleDistributive(Opcode opcode, Operand* opA, Operand* opB, 
                                Block* block, FloatMode mode = FP_Exact);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying calls to intrinsics.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleIntrinsic(CallInstr* instr, Intrinsic* intrinsic);
    Operand* HandleCopyMemoryIntr(CallInstr* instr);
    Operand* HandleSetMemoryIntr(CallInstr* instr);
    Operand* HandleFmulIntrinsics(Operand* opA, Operand* opB, FloatMode mode);
    Operand* HandleFdivIntrinsics(Operand* opA, Operand* opB, FloatMode mode);

    Operand* CreateSetMemoryValue(Operand* valueOp, int size, 
								  const IntegerType* storeType);

    Operand* HandleAbsIntrinsic(CallInstr* instr);

    Operand* HandleSqrtIntrinsic(CallInstr* instr);
    Operand* HandlePowIntrinsic(CallInstr* instr);

    Operand* CreatePow2(Operand* op);
    Operand* CreatePow4(Operand* op);

    Operand* CreateFmul(Operand* opA, Operand* opB);

    Operand* CreateTwoArgCall(Operand* targetOp, Operand* arg1, Operand* arg2,
                              const Type* resultType);

    Operand* CreateOneArgCall(Operand* targetOp, Operand* argument, const Type* resultType);

    Operand* HandleFmulPowOperandIntr(CallInstr* instr, Operand* op, FloatMode mode);
    Operand* HandleFmulPowPowIntr(CallInstr* instrA, CallInstr* instrB, FloatMode mode);
    Operand* HandleFmulIntrIntr(CallInstr* instrA, CallInstr* instrB, 
                                MathIntrinsic* mathIntrA, MathIntrinsic* mathIntrB,
                                FloatMode mode);

    Operand* HandleFdivPowOperandIntr(CallInstr* instr, Operand* op, FloatMode mode, 
                                      bool reversed);

    Operand* HandleFdivIntrIntr(CallInstr* instrA, CallInstr* instrB, 
                                MathIntrinsic* mathIntrA, MathIntrinsic* mathIntrB,
                                FloatMode mode);

    Operand* HandleCmpIntrinsic(CmpInstrBase* instr);
    Operand* HandleMinMaxIntrinsics(CallInstr* instr);
    Operand* HandleMinMaxIntrCmp(CallInstr* instrA, CallInstr* instrB, 
                                 MathIntrinsic* mathIntrA, MathIntrinsic* mathIntrB,
                                 OrderType order, const Type* resultType,
                                 bool isUnsigned);

    Operand* HandleMinIntrConstCmp(CallInstr* instrA, IntConstant* intConst,
                                   OrderType order, const Type* resultType, 
                                   bool isUnsigned);

    Operand* HandleMinIntrOpCmp(CallInstr* instrA, Operand* opB,
                                OrderType order, const Type* resultType, 
                                bool isUnsigned);

    Operand* HandleMaxIntrConstCmp(CallInstr* instrA, IntConstant* intConst,
                                   OrderType order, const Type* resultType, 
                                   bool isUnsigned);

    Operand* HandleMaxIntrOpCmp(CallInstr* instrA, Operand* opB,
                                OrderType order, const Type* resultType,
                                bool isUnsigned);

    Operand* HandleMinMaxOnQuestion(QuestionInstr* instr, 
                                    IntConstant* intConst, bool isMax);

    Operand* HandleMinMaxOnQuestion(QuestionInstr* instrA, 
                                    QuestionInstr* instrB, bool isMax);

    bool SameMinMaxOperands(CallInstr* instrA, CallInstr* instrB);

    Operand* HandleAbsIntrOpCmp(CallInstr* instrA, Operand* opB, OrderType order, 
                                const Type* resultType, bool isUnsigned);

    Operand* HandleSqrtIntrConstCmp(Operand* a, FloatConstant* C, OrderType order, 
                                    const Type* resultType);

    Operand* HandleByteSwapIntrinsic(CallInstr* instr);
    Operand* HandleRotateIntrinsic(CallInstr* instr);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'quest' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Operand* HandleQuestion(QuestionInstr* instr);

    Operand* HandleOperationOnQuest(Opcode opcode, Operand* opA, Operand* opB);

    Operand* HandleOperationOnQuestAndOther(Opcode opcode, QuestionInstr* instrA,
                                            Operand* op);

    Operand* HandleOperationOnQuestAndQuest(Opcode opcode, QuestionInstr* instr,
                                            QuestionInstr* instrB);

    Operand* HandleQuestionOnBool(QuestionInstr* instr);

    Operand* ExpandQuestToLogical(QuestionInstr* instr, CmpInstr* cmpInstr);

    Operand* HandleQuestionOnBinary(QuestionInstr* instr);

    bool HasOnlyOneOrZeroResult(Operand* op, Block* testBlock, int level = 0);

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'phi' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Operand* HandlePhi(PhiInstr* instr);

    Operand* HandlePhiWithUndef(PhiInstr* instr);

    Operand* HandlePhiOnLoad(PhiInstr* instr);

    Operand* HandlePhiOnInstructions(PhiInstr* instr);

    bool HandlePhiCopyCycle(PhiInstr* instr);

    Operand* HandleInstructionOnPhi(Opcode opcode, Operand* opA, Operand* opB);

    bool ShouldApplyOperatorOnPhi(PhiInstr* instr, Operand* otherOp);

    bool CollectPhiInstructionOps(PhiInstr* instr, InstructionList& instrs,
                                  BlockList& incomingBlock, Opcode& lastOpcode);

    Operand* GetCommonOperand(InstructionList& instrs, OperandList& diffOps,
                              Opcode lastOpcode, bool& commonOnRight);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'load' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleLoad(LoadInstr* instr);

	bool IsLoadFromConstArray(LoadInstr* instr, GlobalVariable*& globalVar,
							  Operand*& indexOp, IndexList& subindexList);

	int ComputeConstRanges(GlobalVariable* globalVar, IntConstant* intConst,
						   OrderType order, RangeList& ranges, IndexList& subindexList,
						   IntConstant* intConstInstr, Opcode opcode);

	void AddToCurrentRange(__int64 value, __int64 requiredValue, __int64 index,
						   ConstantRange*& currentRange, RangeList& ranges,
						   OrderType order, IntConstant* intConstInstr, Opcode opcode);

	bool MatchesRequiredValue(__int64 value, __int64 requiredValue, OrderType order,
							  IntConstant* intConstInstr, Opcode opcode);

	int ComputeRangesCost(RangeList& ranges, bool isNotEquality);

	Operand* CreateRangeTest(ConstantRange& range, Operand* indexOp, bool isEquality,
							 const Type* resultType);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods for simplifying 'store' instructions.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* HandleStore(StoreInstr* instr);

	bool SafeToForwardStore(StoreInstr* instr);

    StoreInstr* FindAvailableStore(Block* block, Operand* addressOp);


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Methods that drive the peephole optimizations.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Operand* Simplify(Instruction* instr);

    Operand* SimplifyBinary(Opcode opcode, Operand* opA, Operand* opB,
                            Block* block, FloatMode mode = FP_Exact,
                            bool constMoveAllowed = true);

	// Removes all instructions from the function that are definitely dead.
	void DeleteDeadInstructions(Function* function);

	// Adds all eligible instructions from the function to the worklist.
    // These are the instructions that are considered for simplification.
    void BuildWorklist(Function* function);

	// Tries to simplify all the instructions in the worklist.
    bool DoOneIteration(Function* function);

    // Tries to simplify a branching instruction.
    bool ProcessBranching(Instruction* instr);

    // Handles an instruction that has been simplified to 'undef'.
    // It handles some special cases like modified calls and deleted stores.
    bool HandleUndefinedResult(Instruction* instr);

    void HandleSpecialCases(Instruction* instr, Operand* result);

	// Simplifies the instructions from the function until
    // there is an iteration when no instruction is simplified anymore.
    // 'maxIterations' can be used to limit the number of iterations
    // (-1 for an unbounded number).
    void IterateToFixedpoint(Function* function, int maxIterations = -1);

public:
	Peephole() : resultOp_(nullptr) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // 'maxIterations' can be used to limit the number of iterations
    // (-1 for an unbounded number).
    void Execute(Function* function, int maxIterations = -1);

    // Tries to simplify the specified instruction using only simple rules.
    // If 'createAllowed' is false no simplification that requires 
    // the creation of new instructions0 is performed.
    Operand* SimplifyInstruction(Instruction* instr, 
                                 bool createAllowed = false);

    Operand* SimplifyInstruction(Opcode opcode, Operand* opA, Operand* opB,
                                 Unit* unit, Block* block = nullptr, 
                                 bool createAllowed = false,
                                 Instruction* instr = nullptr);
};

} // namespace Optimization

#endif