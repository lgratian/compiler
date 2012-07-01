// Peephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the Peephole algorithm.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"
#include "../IR/IRPrinter.hpp"
#include "../IR/IRVerifier.hpp"
#include <fstream>
namespace Optimization {

void Peephole::Execute(Function* function, int maxIterations) {
    DebugValidator::IsNotNull(function);

	// Initialize the peephole optimizer.
	irGen_ = IRGenerator(function->ParentUnit());
	folder_ = ConstantFolder(&irGen_, GetTarget());

	// Start execution.
	IterateToFixedpoint(function, maxIterations);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::Simplify(Instruction* instr) {
	// Set the insertion point for new instructions; all new instructions
	// after the current one, in the order they are created.
	irGen_.SetInsertionPoint(instr);

    switch(instr->GetOpcode()) {
        case Instr_Add: {
            return PreserveUSO(instr, HandleAdd(instr->GetSourceOp(0), 
                                                instr->GetSourceOp(1),
                                                instr->ParentBlock()));
        }
        case Instr_Sub: {
            return PreserveUSO(instr, HandleSub(instr->GetSourceOp(0),
                                                instr->GetSourceOp(1),
                                                instr->ParentBlock()));
        }
        case Instr_Mul: {
            return PreserveUSO(instr, HandleMul(instr->GetSourceOp(0), 
                               instr->GetSourceOp(1), instr->ParentBlock()));
        }
        case Instr_Div: {
            auto arithInstr = instr->As<ArithmeticInstr>();
            return PreserveUSO(instr, HandleDiv(arithInstr->LeftOp(),
                                                arithInstr->RightOp(),
                                                instr->ParentBlock(),
                                                arithInstr->HasNoRemainder()));
        }
        case Instr_Udiv: {
            return PreserveUSO(instr, HandleUdiv(instr->GetSourceOp(0),
                                                 instr->GetSourceOp(1),
                                                 instr->ParentBlock()));
        }
        case Instr_Mod:  {
            return PreserveUSO(instr, HandleMod(instr->GetSourceOp(0),
                                                instr->GetSourceOp(1),
                                                instr->ParentBlock()));
        }
        case Instr_Umod: {
            return PreserveUSO(instr, HandleUmod(instr->GetSourceOp(0),
                                                 instr->GetSourceOp(1),
                                                 instr->ParentBlock()));
        }
        case Instr_Fadd: {
            auto arithInstr = instr->As<ArithmeticInstr>();
            return HandleFadd(arithInstr->LeftOp(), 
                              arithInstr->RightOp(),
                              arithInstr->GetFPMode());
        }
        case Instr_Fsub: {
            auto arithInstr = instr->As<ArithmeticInstr>();
            return HandleFsub(arithInstr->LeftOp(), 
                              arithInstr->RightOp(),
                              arithInstr->GetFPMode());
        }
        case Instr_Fmul: {
            auto arithInstr = instr->As<ArithmeticInstr>();
            return HandleFmul(arithInstr->LeftOp(), 
                              arithInstr->RightOp(),
                              arithInstr->GetFPMode());
        }
        case Instr_Fdiv: {
            auto arithInstr = instr->As<ArithmeticInstr>();
            return HandleFdiv(arithInstr->LeftOp(), 
                              arithInstr->RightOp(),
                              arithInstr->GetFPMode());
        }

        case Instr_And:  return HandleAnd(instr->GetSourceOp(0), 
                                          instr->GetSourceOp(1),
                                          instr->ParentBlock());
        case Instr_Or:   return HandleOr(instr->GetSourceOp(0), 
                                         instr->GetSourceOp(1),
                                         instr->ParentBlock());
        case Instr_Xor:  return HandleXor(instr->GetSourceOp(0), 
                                          instr->GetSourceOp(1),
                                          instr->ParentBlock());
        case Instr_Shl:  return HandleShl(instr->As<ShlInstr>());
        case Instr_Shr:  return HandleShr(instr->As<ShrInstr>());
        case Instr_Ushr: return HandleUshr(instr->As<UshrInstr>());

        case Instr_Cmp:  return HandleCmp(instr->As<CmpInstr>());
        case Instr_Ucmp: return HandleUcmp(instr->As<UcmpInstr>());
        case Instr_Fcmp: return HandleFcmp(instr->As<FcmpInstr>());

        case Instr_Trunc:  return HandleTrunc(instr->As<TruncInstr>());
        case Instr_Sext:   return HandleSext(instr->As<SextInstr>());
        case Instr_Zext:   return HandleZext(instr->As<ZextInstr>());
        case Instr_Ftrunc: return HandleFtrunc(instr->As<FtruncInstr>());
        case Instr_Fext:   return HandleFext(instr->As<FextInstr>());
        case Instr_Ftoi:   return HandleFtoi(instr->As<FtoiInstr>());
        case Instr_Ftoui:  return HandleFtoui(instr->As<FtouiInstr>());
        case Instr_Itof:   return HandleItof(instr->As<ItofInstr>());
        case Instr_Itop:   return HandleItop(instr->As<ItopInstr>());
        case Instr_Ptoi:   return HandlePtoi(instr->As<PtoiInstr>());
        case Instr_Ptop:   return HandlePtop(instr->As<PtopInstr>());
        case Instr_Uitof:  return HandleUitof(instr->As<UitofInstr>());

        case Instr_Call:     return HandleCall(instr->As<CallInstr>());
        case Instr_Address:  return HandleAddress(instr->As<AddressInstr>());
        case Instr_Index:    return HandleIndex(instr->As<IndexInstr>());
        case Instr_Element:  return HandleElement(instr->As<ElementInstr>());
		case Instr_Load:     return HandleLoad(instr->As<LoadInstr>());
		case Instr_Store:    return HandleStore(instr->As<StoreInstr>());
        case Instr_Question: return HandleQuestion(instr->As<QuestionInstr>());
        case Instr_Phi:      return HandlePhi(instr->As<PhiInstr>());
    }

    // This is an instruction that is not supported.
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::SimplifyBinary(Opcode opcode, Operand* opA, 
                                  Operand* opB, Block* block,
                                  FloatMode mode, bool constMoveAllowed) {
    // This is used by code that does expression reassociation.
    switch(opcode) {
        case Instr_Add:  return HandleAdd(opA, opB, block, constMoveAllowed);
		case Instr_Sub:  return HandleSub(opA, opB, block);
		case Instr_Mul:  return HandleMul(opA, opB, block, constMoveAllowed);
        case Instr_Div:  return HandleDiv(opA, opB, block, false);
        case Instr_Udiv: return HandleUdiv(opA, opB, block);
		case Instr_Fadd: return HandleFadd(opA, opB, mode);
		case Instr_Fsub: return HandleFsub(opA, opB, mode);
		case Instr_Fmul: return HandleFmul(opA, opB, mode);
		case Instr_Fdiv: return HandleFdiv(opA, opB, mode);
		case Instr_And:  return HandleAnd(opA, opB, block, constMoveAllowed);
		case Instr_Or:   return HandleOr(opA, opB, block,  constMoveAllowed);
		case Instr_Xor:  return HandleXor(opA, opB, block, constMoveAllowed);
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::SimplifyInstruction(Instruction* instr, bool createAllowed) {
    DebugValidator::IsNotNull(instr);

    if(instr->IsArithmetic() || instr->IsLogical()) {
        auto opA = instr->GetSourceOp(0);
        auto opB = instr->GetSourceOp(1);
        auto block = instr->ParentBlock();
        auto unit = instr->ParentFunction()->ParentUnit();

        return SimplifyInstruction(instr->GetOpcode(), opA, opB, unit, 
                                   block, createAllowed, instr);
    }

    // This is an instruction that is not supported.
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::SimplifyInstruction(Opcode opcode, Operand* opA, Operand* opB,
                                       Unit* unit, Block* block,
                                       bool createAllowed, Instruction* instr) {
    DebugValidator::IsNotNull(opA);
    DebugValidator::IsNotNull(opB);
    
    irGen_ = IRGenerator(unit);
    irGen_.SetInsertionPoint(instr);
    folder_ = ConstantFolder(&irGen_, GetTarget());

    switch(opcode) {
        case Instr_Add: {
            return PreserveUSO(instr, HandleAddSimple(opA, opB, block, 
                                                      createAllowed));
        }
        case Instr_Sub: {
            return PreserveUSO(instr, HandleSubSimple(opA, opB, block, 
                                                      createAllowed));
        }
        case Instr_Mul: {
            return PreserveUSO(instr, HandleMulSimple(opA, opB, block, 
                                                      createAllowed));
        }
        case Instr_And:  return HandleAndSimple(opA, opB, block, 
                                                createAllowed);
        case Instr_Or:   return HandleOrSimple(opA, opB, block, 
                                               createAllowed);
        case Instr_Xor:  return HandleXorSimple(opA, opB, block, 
                                                createAllowed);
        case Instr_Shl:  return HandleShiftSimple(opA, opB, true, block, 
                                                  createAllowed);
        case Instr_Shr:  return HandleShiftSimple(opA, opB, false, block, 
                                                  createAllowed);
        case Instr_Ushr: return HandleShiftSimple(opA, opB, false, block, 
                                                  createAllowed);
    }

    // This is an instruction that is not supported.
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Peephole::BuildWorklist(Function* function) {
    // Clear the worklist, it may contain instructions 
    // from the previous iteration.
    worklist_.Clear();
    auto instrEnum = function->GetInstructionEnum();

    while(instrEnum.IsValid()) {
        worklist_.Add(instrEnum.Next());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Peephole::DeleteDeadInstructions(Function* function) {
	// We process the blocks from last to first, and the instructions
    // in the blocks from last to first too, because it allows deleting 
    // more dead instructions than processing from first to last.
	Block* block = function->LastBlock();

	while(block) {
		Instruction* instr = block->LastInstruction();

		while(instr) {
			Instruction* prevInstr = instr->PreviousInstruction();
            DebugValidator::AreNotEqual(prevInstr, instr);

			// Delete the instruction if we're certain it's dead.
			if(GetSafetyInfo()->IsDefinitelyDead(instr)) {
	            block->RemoveInstruction(instr, true /* free */);
			}

			instr = prevInstr;
		}

		block = block->PreviousBlock();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::DoOneIteration(Function* function) {
    bool changed = false;
    int position = 0;
    
    while(position < worklist_.Count()) {
        // Get the instruction at the current position and try to simplify it.
        Instruction* instr = worklist_[position++];

        // Branching instructions don't have any resulting operand,
        // and report a simplification by returning 'true'.
        if(instr->IsBranching()) {
            changed |= ProcessBranching(instr);
            continue;
        }

        // For all other kinds of instructions.
        // If the result is 'nullptr' nothing could be simplified.
        auto result = Simplify(instr);
        if(result == nullptr) continue;

        if(result->IsUndefinedConstant()) {
            if(HandleUndefinedResult(instr)) {
                // The instruction should not be considered below.
                continue;
            }
        }

        // Replace the previous result operand with the new one
        // in all instructions that use it as a source operand.
        instr->GetDestinationOp()->ReplaceWith(result);
        InstructionSimplified(instr, result);
        
        HandleSpecialCases(instr, result);
        changed = true;
#if 1
		IRPrinter(function).Dump();
#endif
    }
	
	// Some instructions may be dead now; delete them here, so that
	// the next iteration doesn't consider them anymore. Note that 
    // this is necessary, else we would enter an infinite loop simplifying
    // the same instructions again and again...
	if(changed) DeleteDeadInstructions(function);
    return changed;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::ProcessBranching(Instruction* instr) {
    switch(instr->GetOpcode()) {
        case Instr_If:     { 
            return HandleIf(instr->As<IfInstr>()); 
        }
        case Instr_Switch: { 
            return HandleSwitch(instr->As<SwitchInstr>());
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::HandleUndefinedResult(Instruction* instr) {
    if(auto callInstr = instr->As<CallInstr>()) {
        // If the instruction is a memory intrinsic it means
        // that it was replaced by a series of instructions, and the
        // intrinsic is now dead and needs to be removed.
        if(auto intrinsic = callInstr->GetIntrinsicAs<MemoryIntrinsic>()) {
            if(intrinsic->IsCopyMemoryIntr() || intrinsic->IsCopyMemoryIntr()) {
                instr->RemoveFromBlock(true /* free */);
                return true;
            }
        }

        // If 'undef' was returned, and we have a 'call' instruction 
        // that has no result operand ('void' return function),
        // we have a signal meaning that the call itself was changed, 
        // but without being replaced.
        if(callInstr->IsVoid()) {
            return true; // No result needs to be propagated.
        }
    }
    else if(instr->IsStore()) {
        // This is a signal that the store is dead, remove it.
        instr->RemoveFromBlock(true /* free */);
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Peephole::HandleSpecialCases(Instruction* instr, Operand* result) {
    // If this was a 'call' that was folded to a constant we removed
    // it now, because 'DeleteDeadInstructions' presumes that 
    // it's still used, and we will get an infinite loop.
    if(instr->IsCall() && result->IsConstant()) {
        instr->RemoveFromBlock(true /* free */);
    }

    // If this is a 'phi' that has no other users beside itself
    // it should be removed. This can happen in case of loops
    // that had one of their predecessors removed, for example.
    if(auto phiInstr = instr->As<PhiInstr>()) {
        if(phiInstr->ResultOp()->HasSingleUser() &&
           (phiInstr->ResultOp()->GetUser(0) == instr)) {
            phiInstr->RemoveFromBlock(true);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Peephole::IterateToFixedpoint(Function* function, int maxIterations) {
    bool limitedIterations = maxIterations != -1;
    bool changed = true;

    // We iterate until no more instructions can be simplified,
    // or until the imposed iteration count is reached.
    while(changed) {
        if(limitedIterations && (maxIterations == 0)) {
            // We stop now, even if more simplifications would have been done.
            return;
        }

        // Build the worklist with the live instructions that need to be processed,
        // then do one iteration that considers the whole worklist.
        BuildWorklist(function);
        changed = DoOneIteration(function);
        maxIterations--;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::NegateInt(Operand* op) {
	// Negate the value by subtracting from 0.
	auto temp = GetTemporary(op);
	auto zeroConst = irGen_.GetIntConst(op->GetType(), 0);
	irGen_.GetSub(zeroConst, op, temp);
	return temp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::NegateFloat(Operand* op) {
	// Negate the value by subtracting from 0.
	auto temp = GetTemporary(op);
	auto zeroConst = irGen_.GetFloatingConst(op->GetType(), 0.0);
	irGen_.GetFsub(zeroConst, op, temp);
	return temp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::MoveConstantToRight(Operand*& opA, Operand*& opB) {
    if(opA->IsConstant() && (opB->IsConstant() == false)) {
		Operand* temp = opA;
		opA = opB;
		opB = temp;
        return true;
	}

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::IsLogicalNot(Operand* op, Operand** result) {
	// ~ is is found as 'a ^ -1'.
	auto xorInstr = op->DefiningInstrAs<XorInstr>();

	if(xorInstr) {
		if(auto intConst = xorInstr->RightOp()->As<IntConstant>()) {
			if(intConst->Value() == -1) {
				if(result) *result = xorInstr->LeftOp();
				return true;
			}
		}
	}
		
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::IsLeftRightMinusOne(IntConstant* a, IntConstant* b) {
	// Test if 'b' equals 'a + 1'.
	auto intKind = a->GetType()->GetSubtype();
	__int64 rightMinusOne = IA::Sub(b->Value(), 1, intKind);
	return IA::AreEqual(a->Value(), rightMinusOne, intKind);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateCompare(Operand* opA, Operand* opB, OrderType order, 
								 const Type* resultType, bool isUcmp) {
	// Create a compare instruction for signed or unsigned numbers.
	auto result = irGen_.GetTemporary(resultType);

    if(opA->IsFloating()) {
        irGen_.GetFcmp(order, opA, opB, result);
    }
	else if(isUcmp) {
        irGen_.GetUcmp(order, opA, opB, result);
    }
	else {
        irGen_.GetUcmp(order, opA, opB, result);
    }

	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateRangeCompare(Operand* op, IntConstant* leftConst,
									  IntConstant* rightConst, const Type* resultType,
									  bool isUcmp, bool noLeftInc) {
	DebugValidator::IsTrue(IA::IsLargerOrEqual(rightConst, leftConst));

	// Create instructions that test if the operand is found in a range:
	// (op > leftConst) & (op < rightConst)
	// Add 1 to the left constant, creates some opportunities
    // by using >= instead of >.
	auto intKind = leftConst->GetType()->GetSubtype();
	__int64 leftPlusOne = noLeftInc ? leftConst->Value() : 
									  IA::Add(leftConst->Value(), 1, intKind);
	leftConst = irGen_.GetIntConst(leftConst->GetType(), leftPlusOne);

    if(IA::AreEqual(leftPlusOne, rightConst->Value(), intKind)) {
		// (op >= 5) & (op < 5) -> always false
		return GetBool(false, resultType);
	}

	// If 'leftConst' is the minimum value, we can eliminate the left part,
	// because 'op >= MIN" will always be true.
	__int64 min = IA::GetMin(leftConst->GetType(), isUcmp == false);

    if(IA::AreEqual(leftPlusOne, min, intKind)) {
		return CreateCompare(op, rightConst, Order_Less, resultType, isUcmp);
	}

	// Else emit a test of the form '(op - leftConst) < (rightConst - leftConst)'.
	// (op >= 4) & (op < 6) -> (op - 4) < 2
	__int64 diff = IA::Sub(rightConst, leftConst);
	auto diffOp = irGen_.GetIntConst(op->GetType(), diff);

	auto subOp = GetTemporary(op);
	irGen_.GetSub(op, leftConst, subOp);
	return CreateCompare(subOp, diffOp, Order_Less, resultType, true);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateOutOfRangeCompare(Operand* op, IntConstant* leftConst,
									       IntConstant* rightConst, 
                                           const Type* resultType,
									       bool isUcmp, bool noLeftInc) {
	DebugValidator::IsTrue(IA::IsLargerOrEqual(rightConst, leftConst));

	// Create instructions that test if the operand is found in a range:
	// (op < leftConst) | (op > rightConst)
	// Add 1 to the left constant, creates some opportunities 
    // by using >= instead of >.
	auto intKind = leftConst->GetType()->GetSubtype();
	__int64 leftPlusOne = noLeftInc ? leftConst->Value() : 
									  IA::Add(leftConst->Value(), 1, intKind);
	leftConst = irGen_.GetIntConst(leftConst->GetType(), leftPlusOne);

    if(IA::AreEqual(leftPlusOne, rightConst->Value(), intKind)) {
		// (op <= 5) | (op | 5) -> always true
		return GetBool(true, resultType);
	}

	// Emit a test of the form '(op - leftConst) > (rightConst - leftConst)'.
	// (op <= 4) | (op > 6) -> (op - 4) > 2
	__int64 diff = IA::Sub(rightConst, leftConst);
	auto diffOp = irGen_.GetIntConst(op->GetType(), diff);

	auto subOp = GetTemporary(op);
	irGen_.GetSub(op, leftConst, subOp);
	return CreateCompare(subOp, diffOp, Order_Greater, resultType, true);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateIntCastIfRequired(Operand* op, const Type* type, 
                                           bool isSigned ) {
    auto intType = type->As<IntegerType>();
    DebugValidator::IsNotNull(intType);

    if(auto opIntType = op->GetType()->As<IntegerType>()) {
        if(opIntType->RankBelow(intType)) {
            // sext or zext
            auto extOp = irGen_.GetTemporary(intType);

            if(isSigned) irGen_.GetSext(op, intType, extOp);
            else irGen_.GetZext(op, intType, extOp);

            return extOp;
        }
        else  if(opIntType->RankBelow(intType)) {
            // trunc
            auto truncOp = irGen_.GetTemporary(intType);
            irGen_.GetTrunc(op, intType, truncOp);
            return truncOp;
        }
    }
    
    return op; // No change.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateBinaryInstruction(Opcode opcode, Operand* leftOp, 
                                           Operand* rightOp, const Type* resultType,
                                           Instruction* insertionPoint,
                                           Instruction* previousInsertionPoint,
                                           FloatMode mode) {
    // Change the insertion point if required.
    if(insertionPoint) {
        irGen_.SetInsertionPoint(insertionPoint);
    }

    auto resultOp = irGen_.GetTemporary(resultType);

    if(Instruction::IsArithmetic(opcode)) {
        auto instr = irGen_.GetArithmetic(opcode, leftOp, rightOp, resultOp);
        instr->SetFPMode(mode);
    }
    else if(Instruction::IsLogical(opcode)) {
        irGen_.GetLogical(opcode, leftOp, rightOp, resultOp);
    }
    else if(opcode == Instr_Address) {
        irGen_.GetAddress(leftOp, rightOp, resultOp);
    }
    else if(opcode == Instr_Index) {
        irGen_.GetIndex(leftOp, rightOp, resultOp);
    }
    else if(opcode == Instr_Element) {
        DebugValidator::IsTrue(rightOp->IsIntConstant());
        irGen_.GetElement(leftOp, rightOp, resultOp);
    }

    if(previousInsertionPoint) {
        irGen_.SetInsertionPoint(previousInsertionPoint);
    }
    return resultOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::PreserveUSO(Instruction* instr, Operand* op) {
    if(instr == nullptr) {
        return op;
    }
    else if(op == nullptr) {
        return nullptr;
    }
    else if(op->HasDefiningInstruction() == false) {
        return op;
    }
    else if(auto arithInstr1 = instr->As<ArithmeticInstr>()) {
        if(auto arithInstr2 = op->DefiningInstrAs<ArithmeticInstr>()) {
            bool hasUSO = arithInstr1->HasUndefinedOverflow();
            arithInstr2->SetHasUndefinedOverflow(hasUSO);
        }
    }

    return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Peephole::InstructionSimplified(Instruction* before, Operand* after) {
#if 1
	auto block = before->ParentBlock();
	auto function = before->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string beforeText = IRPrinter(before).ToString();
	string afterText = after->HasDefiningInstruction() ?
					   IRPrinter(after->DefiningInstruction()).ToString() :
					   IRPrinter(after).ToString();
	Log::Warning("Peephole in " + functionName + ":" + blockName +
				 ": " +  beforeText + " => " + afterText);
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Peephole::LogReplacement(const char* file, int line, const char* value) {
    Log::Info("Replacement in " + string(file) + 
              " on line " + string::Format(L"%d", line) +
              " with " + string(value));
}

} // namespace Optimization