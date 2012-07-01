// ConstantFolder.hpp
// Copyright (c) Lup Gratian
//
// Defines the constant folder, which tries to evaluate instructions that involve
// constant operands at compile time. It is very aggressive, trying to fold
// instructions involving memory addresses, to load data from constant global variables
// and to evaluate math functions called with constant arguments.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_CONSTANT_FOLDER_HPP
#define PC_ANALYSIS_CONSTANT_FOLDER_HPP

#include "IntArithmetic.hpp"
#include "FloatArithmetic.hpp"
#include "PatternMatching.hpp"
#include "StdlibRecognizer.hpp"
#include "OperandInfo.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Constants.hpp"
#include "../IR/Visitor.hpp"
#include "../IR/References.hpp"
#include "../IR/Block.hpp"
#include "../IR/Intrinsic.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Targets/TargetInfo.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include <cmath>
using namespace IR;
using namespace Target;

namespace Analysis {

class ConstantFolder : public Visitor {
private:
	typedef IntArithmetic IA;
	typedef FloatArithmetic FA;
	typedef double (*MathOneParam)(double);
    typedef int (*MathOneParamInt)(int);
    typedef long (*MathOneParamLong)(long);
	typedef float (*MathOneParamFloat)(float);
	typedef double (*MathTwoParams)(double, double);
	typedef float (*MathTwoParamsFloat)(float, float);

	IRGenerator* irGen_;
	TargetInfo* target_;
	Operand* result_;
    bool opInfoDisabled_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	void SwapOperands(Operand*& a, Operand*& b) {
		Operand* temp = a;
		a = b;
		b = temp;
	}

    IntConstant* AsIntConstant(Operand* op, Block* testBlock) {
        if(opInfoDisabled_) {
            return op->As<IntConstant>();
        }

        OperandInfo opInfo(irGen_->GetUnit(), target_);
        return opInfo.GetIntConstant(op, testBlock);
    }

    bool IsNotZero(Operand* op, Block* testBlock) {
        if(opInfoDisabled_) {
            return op->IsIntConstant() && (op->IsZeroInt() == false);
        }

        OperandInfo opInfo(irGen_->GetUnit(), target_);
        return opInfo.IsNotZero(op, testBlock);
    }

    bool IsZero(Operand* op, Block* testBlock) {
        if(opInfoDisabled_) {
            return op->IsIntConstant() && op->IsZeroInt();
        }

        OperandInfo opInfo(irGen_->GetUnit(), target_);
        return opInfo.IsZero(op, testBlock);
    }

	// Methods for obtaining a constant having a specific value.
	Operand* GetZeroInt(const Type* type) {
		return irGen_->GetIntConst(type, 0);
	}

	Operand* GetZeroInt(Operand* base) {
		const IntegerType* type = base->GetType()->As<IntegerType>();
		return GetZeroInt(type);
	}

	Operand* GetOneInt(const Type* type) {
		return irGen_->GetIntConst(type, 1);
	}

	Operand* GetOneInt(Operand* base) {
		const IntegerType* type = base->GetType()->As<IntegerType>();
		return GetOneInt(type);
	}

	Operand* GetBool(bool state, const Type* type = nullptr) {
		// If no type was provided we use 'int32'.
		if(type == nullptr) {
			type = IntegerType::GetInt32();
		}

		Operand* boolOp;
		if(state) boolOp = GetOneInt(type);
		else boolOp = GetZeroInt(type);
		return boolOp;
	}
	
	Operand* GetBool(bool state, Operand* base) {
		return GetBool(state, base->GetType());
	}

	Operand* GetMinusOneInt(const Type* type) {
		return irGen_->GetIntConst(type, -1);
	}

	Operand* GetMinusOneInt(Operand* base) {
		const IntegerType* type = base->GetType()->As<IntegerType>();
		return GetMinusOneInt(type);
	}

	Operand* GetZeroFloat(const Type* type) {
		return irGen_->GetFloatingConst(type, 0.0);
	}

	Operand* GetZeroFloat(Operand* base) {
		const FloatingType* type = base->GetType()->As<FloatingType>();
		return GetZeroFloat(type);
	}

	Operand* GetUndefined(const Type* type) {
		return irGen_->GetUndefinedConst(type);
	}

	Operand* GetUndefined(Operand* base) {
		return irGen_->GetUndefinedConst(base->GetType());
	}

	Operand* GetNullptr(const Type* type) {
		return irGen_->GetNullConst(type);
	}

	Operand* GetNullptr(Operand* base) {
		return irGen_->GetNullConst(base->GetType());
	}

	// Methods that handle arithmetic instructions.
	Operand* HandleArithmetic(Opcode opcode, Operand* opA, 
							  Operand* opB, FloatMode mode = FP_Exact,
                              Block* block = nullptr);
	
	Operand* HandleArithmeticUndef(Opcode opcode, Operand* opA, 
								   Operand* opB, FloatMode mode = FP_Exact);
	
	Operand* HandleArithmeticOneConst(Opcode opcode, Operand* opA, 
									  Operand* opB, FloatMode mode = FP_Exact,
                                      Block* block = nullptr);
	
	Operand* HandlePointerSub(Opcode opcode, Operand* opA, Operand* opB);
	Operand* HandleArithmeticVar(Opcode opcode, Operand* opA, Operand* opB);
	
	// Methods that handle logical instructions.
	Operand* HandleLogical(Opcode opcode, Operand* opA, 
                           Operand* opB, Block* block = nullptr);
	Operand* HandleLogicalUndef(Opcode opcode, Operand* opA, Operand* opB);
	Operand* HandleLogicalOneConst(Opcode opcode, Operand* opA, 
                                   Operand* opB, Block* block = nullptr);
	Operand* HandleAndOnInstr(Operand* opA, IntConstant* intConst);
	
	// Methods that handle comparison instructions.
	Operand* HandleCompare(Opcode opcode, Operand* opA, Operand* opB, 
                           OrderType order, Block* block);

    Operand* HandleCompareInt(Opcode opcode, Operand* opA, Operand* opB, 
                              OrderType order, Block* block);

    Operand* HandleCompareFloat(Opcode opcode, Operand* opA, Operand* opB, 
                                OrderType order, Block* block);

    Operand* HandleCompareUndef(Opcode opcode, Operand* opA, Operand* opB, 
                                OrderType order, Block* block);

    Operand* HandleCompareNullPointer(Opcode opcode, Operand* opA, Operand* opB, 
                                      OrderType order, Block* block);

    Operand* HandleCompareRange(Opcode opcode, Operand* opA, Operand* opB, 
                                OrderType order, Block* block);

    bool DetectQuestion(Operand* opA, Operand* opB, QuestionInstr*& questInstrA,
                        QuestionInstr*& questInstrB, IntConstant*& intConst,
                        OrderType& order);

    Operand* HandleCompareQuestion(Opcode opcode, Operand* opA, Operand* opB, 
                                   OrderType order);

    Operand* HandleCompareQuestionQuestion(Opcode opcode, QuestionInstr* questInstrA,
                                           QuestionInstr* questInstrB, OrderType order);

    // Tests if 'intConstA ORDER intConstB' and 'intConstA ORDER intConstC' are both true.
    bool IsOrderSatisfied(IntConstant* intConstA, IntConstant* intConstB,
                          IntConstant* intConstC, OrderType order, bool isUnsigned);

	Operand* HandleCompareGlobalNull(Opcode opcode, Operand* opA, Operand* opB, 
									 OrderType order);
	
	Operand* HandleCompareVars(Opcode opcode, Operand* opA, Operand* opB, 
                               OrderType order);
	
	Operand* HandleCompareVarAddr(Opcode opcode, VariableReference* variableRef,
								  Operand* otherOp, OrderType order, bool varLeftOp);
	
	Operand* HandleCompareVarAggregate(Opcode opcode, VariableReference* variableRef,
									   Operand* otherOp, OrderType order,bool varLeftOp);
	
	Operand* HandleCompareNullAddr(Opcode opcode, Operand* opA, Operand* opB, 
								   OrderType order);
	
	Operand* HandleCompareAddrAddr(Opcode opcode, Operand* opA, Operand* opB, 
								   OrderType order);
	
	Operand* HandleCompareElemElem(Opcode opcode, Operand* opA, Operand* opB, 
								   OrderType order);

    Operand* HandleCompareLoadLoad(Opcode opcode, Operand* opA, Operand* opB, 
								   OrderType order);

    Operand* HandleComparePhi(Opcode opcode, Operand* opA,  Operand* opB, 
							  OrderType order);

    bool OrderHolds(OrderType order, Operand* opA, Operand* opB, bool isSigned);
	
	Operand* GetResultForOrder(OrderType requiredOrder, OrderType order);
	
	// Method that handle conversion (cast) instructions.
	Operand* HandleConversion(Opcode opcode, Operand* op, 
                              const Type* castType, Block* block);
	
	Operand* HandleConversionPtoi(Opcode opcode, Operand* op, const Type* castType);
	
	// Handles the 'addr' instruction.
	Operand* HandleAddress(Opcode opcode, Operand* baseOp, Operand* indexOp,
                           Block* block);
	
	// Handles the loading of a value.
	Operand* HandleLoad(Operand* sourceOp);
	
	Operand* LoadFromAddress(Operand* op, const Type* loadType, __int64 offset);
	
	Operand* LoadFromGlobal(Operand* op, const Type* loadType, __int64 offset);
	
	bool IsValidOffset(GlobalVariable* globalVar, const Type* loadType, __int64 offset);

	Operand* LoadFromOffset(Initializer* initializer, const Type* sourceType, 
							const Type* loadType, __int64 offset);
	
	Operand* LoadFromInitializer(Initializer* initializer, const Type* loadType,
                                 __int64 offset);
	
	Operand* LoadFromMultipleArray(InitializerList* initList, __int64 childIndex, 
								   __int64 childOffset, const ArrayType* sourceType,
								   const Type* loadType);
	
	Operand* LoadFromMultipleRecord(InitializerList* initList, __int64 childIndex, 
								   __int64 childOffset, const RecordType* sourceType,
								   const Type* loadType);
	
	Operand* ExtractFromOperand(Operand* op, const Type* loadType, __int64 offset);
	
	Operand* LoadFromString(StringConstant* stringConst, const Type* loadType, 
                            __int64 offset);
	
	Operand* GetOperandHavingData(unsigned char* data, __int64 length, const Type* type);

	bool GetOperandData(Operand* op, unsigned char* data, __int64& length);

	bool SameAddressBase(AddressInstr* instrA, AddressInstr* instrB);

	// Handles a 'call' instruction.
	Operand* HandleCall(CallInstr* callInstr);
    Operand* HandleIntrinsic(CallInstr* callInstr, Intrinsic* intrinsic);
    Operand* EvaluateAbs(CallInstr* callInstr);
    Operand* EvaluateMin(CallInstr* callInstr);
    Operand* EvaluateMax(CallInstr* callInstr);

    Operand* HandlePhi(PhiInstr* instr);

	// Verifies if the specified parameters can be used to fold the 'call'.
    // Checks for things like NaN and Infinity.
	bool ParametersValid(CallInstr* callInstr, double& value);
	bool ParametersValid(CallInstr* callInstr, double& value1, double& value2);

	// Methods for evaluating various forms of standard library function calls.
	Operand* EvaluateMathOneParam(CallInstr* callInstr, MathOneParam function);
    Operand* EvaluateMathOneParamInt(CallInstr* callInstr, MathOneParamInt function);
    Operand* EvaluateMathOneParamLong(CallInstr* callInstr, MathOneParamLong function);
	Operand* EvaluateMathOneParamFloat(CallInstr* callInstr, MathOneParamFloat function);
	Operand* EvaluateMathTwoParams(CallInstr* callInstr, MathTwoParams function);
	Operand* EvaluateMathTwoParamsFloat(CallInstr* callInstr, MathTwoParamsFloat function);

    Operand* HandleQuestion(QuestionInstr* instr);

    Operand* HandleOperatorOnQuestion(Opcode opcode, IntConstant* intConst,
                                      QuestionInstr* instr);

    Operand* HandleAbsOnQuestion(QuestionInstr* instr, bool is64Bit);

    BlockReference* HandleIf(IfInstr* instr, IntConstant* conditionOp, 
                             PhiInstr* condPhiInstr, QuestionInstr* condQuestInstr);

    BlockReference* HandleSwitch(SwitchInstr* instr, IntConstant* conditionOp, 
                                 PhiInstr* condPhiInstr, QuestionInstr* condQuestInstr);

    BlockReference* HandleIfOperandInfo(IfInstr* instr);

	// Returns 'true' if both operands are the result of an
    // 'addr' or 'index' instruction.
	bool IsAddressOrIndex(Operand* opA, Operand* opB) {
		return (opA->DefiningInstrIs<AddressInstr>() && 
				  opB->DefiningInstrIs<AddressInstr>()) ||
			   (opA->DefiningInstrIs<IndexInstr>() && 
			      opB->DefiningInstrAs<IndexInstr>());
	}

public:
	ConstantFolder() : irGen_(nullptr), target_(nullptr) {}

	ConstantFolder(IRGenerator* irGen, TargetInfo* target, 
                   bool opInfoDisabled = false) : 
			irGen_(irGen), target_(target), opInfoDisabled_(opInfoDisabled) {
		DebugValidator::IsNotNull(irGen);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Tries to fold the specified instruction. If the instruction could be
	// constant folded/simplified, an operand is returned, else 'nullptr'.
	Operand* Fold(Instruction* instr);

	// Tries to fold the operands by applying the specified operation on them.
	Operand* FoldBinary(Opcode opcode, Operand* opA, Operand* opB, 
						Block* block = nullptr, FloatMode mode = FP_Exact);

	Operand* FoldCompare(Opcode opcode, Operand* opA, Operand* opB, 
                         OrderType order, Block* block = nullptr);

	// Tries to fold the conversion of the operand to the specified type.
	Operand* FoldConversion(Opcode opcode, Operand* op, 
                            const Type* castType, Block* block = nullptr);

	// Tries to fold the specified 'addr' instruction.
	Operand* FoldAddress(AddressInstr* instr, Block* block = nullptr);
    Operand* FoldAddress(Operand* baseOp, Operand* indexOp, 
                         Block* block = nullptr);

	// Tries to fold the specified 'call' instructions. Handles calls to
	// standard library functions with constant arguments ('sin(0.5)', for example).
	Operand* FoldCall(CallInstr* instr);

	// Tries to fold the 'load' from the specified operand to a constant.
	// This can also fold loads from constant arrays/records.
	Operand* FoldLoad(Operand* sourceOp);
	Operand* FoldLoad(LoadInstr* instr);
    Operand* FoldLoad(Operand* op, const Type* loadType, __int64 offset);

	// Tries to fold a branching instruction (if/switch), returning a reference
	// to the block which is selected by the constant condition operand.
	BlockReference* FoldBranching(Instruction* instr);

	// Tries to fold the branching instruction from the specified block.
	BlockReference* FoldBranching(Block* block);

    // Tries to fold the specified branching instruction
    // using a different condition operand.
    BlockReference* FoldBranching(Instruction* instr, Operand* conditionOp);
};

} // namespace Analysis
#endif