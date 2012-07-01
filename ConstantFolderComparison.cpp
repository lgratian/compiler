// ConstantFolderArithmeticLogical.cpp
// Copyright (c) Lup Gratian
//
// Implements the constant folder for arithmetic and logical instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConstantFolder.hpp"

namespace Analysis {

Operand* ConstantFolder::HandleCompare(Opcode opcode, Operand* opA, Operand* opB, 
									   OrderType order, Block* block) {
	// We handle both integer and floating comparison here, because they
	// have many properties in common. First check for constant operands.
    if(opA->IsInteger()) {
        if(auto result = HandleCompareInt(opcode, opA, opB,
                                          order, block)) {
            return result;
        }
    }
	else if(opA->IsFloatingConstant() && 
            opB->IsFloatingConstant()) {
		return HandleCompareFloat(opcode, opA, opB, order, block);
	}

	if(opA->IsNullConstant() && opB->IsNullConstant()) {
		// nullptr ==/<=/>= nullptr -> 1
		// nullptr !=/</> nullptr -> 0
		switch(order) {
			case Order_Equal:
			case Order_LessOrEqual:
			case Order_GreaterOrEqual: return GetBool(true);
			case Order_NotEqual:
			case Order_Less:
			case Order_Greater: return GetBool(false);
			default: DebugValidator::Unreachable();
		}
	}
	else if(opA->IsUndefinedConstant() || 
            opB->IsUndefinedConstant()) {
		return HandleCompareUndef(opcode, opA, opB, order, block);
	}

    // Test for comparisons involving null pointers.
    if(opA->IsPointer()) {
        if(auto result = HandleCompareNullPointer(opcode, opA, opB,
                                                  order, block)) {
            return result;
        }
    }

    // Test for comparisons involving 'quest' instructions.
    if(auto result = HandleCompareQuestion(opcode, opA, opB, order)) {
        return result;
    }

    // Test for comparisons involving 'phi' instructions.
    if(auto result = HandleComparePhi(opcode, opA, opB, order)) {
        return result;
    }

	// Test for some relationships including global variables.
	if(auto result = HandleCompareGlobalNull(opcode, opA, opB, order)) {
		return result;
	}

    // Test for some relationships including loads
    // from the same/different locations.
	if(auto result = HandleCompareLoadLoad(opcode, opA, opB, order)) {
		return result;
	}

	// Test for cases in which at least one of the operands is a variable
	// or an expression that involves an 'addr'/'index'/'elem' instruction.
	if(auto result = HandleCompareVars(opcode, opA, opB, order)) {
        return result;
    }

    return HandleCompareRange(opcode, opA, opB, order, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareInt(Opcode opcode, Operand* opA, Operand* opB, 
									      OrderType order, Block* block) {
    OperandInfo opInfo(irGen_->GetUnit(), target_);
    auto icA = opInfo.GetIntConstant(opA, block);
    auto icB = opInfo.GetIntConstant(opB, block);

	if(icA && icB) {
		// This is a 'cmp' instruction with both operands constant.
		// Evaluate the relation and return '0' for 'false' and '1' for true.
		// Note that we need to take care about unsigned comparison.
		bool isUnsigned = opcode == Instr_Ucmp;

		switch(order) {
			case Order_Equal: {
                return GetBool(IA::AreEqual(icA, icB));
            }
			case Order_NotEqual: {
                return GetBool(IA::AreNotEqual(icA, icB));
            }
			case Order_Less: {
				if(isUnsigned) return GetBool(IA::IsSmallerUnsigned(icA, icB));
				else return GetBool(IA::IsSmaller(icA, icB));
			}
			case Order_LessOrEqual: {
				if(isUnsigned) return GetBool(IA::IsSmallerOrEqualUnsigned(icA, icB));
				else return GetBool(IA::IsSmallerOrEqual(icA, icB));
			}
			case Order_Greater: {
				if(isUnsigned) return GetBool(IA::IsLargerUnsigned(icA, icB));
				else return GetBool(IA::IsLarger(icA, icB));
			}
			case Order_GreaterOrEqual: {
				if(isUnsigned) return GetBool(IA::IsLargerOrEqualUnsigned(icA, icB));
				else return GetBool(IA::IsLargerOrEqual(icA, icB));
			}
			default: DebugValidator::Unreachable();
		}
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareFloat(Opcode opcode, Operand* opA, Operand* opB, 
									        OrderType order, Block* block) {
    IRFloatingKind kind = opA->GetType()->As<FloatingType>()->GetSubtype();
	double va = opA->As<FloatConstant>()->Value();
	double vb = opB->As<FloatConstant>()->Value();

	switch(order) {
		case Order_Equal:          return GetBool(FA::AreEqual(va, vb, kind));
		case Order_NotEqual:       return GetBool(FA::AreNotEqual(va, vb, kind));
		case Order_Less:           return GetBool(FA::IsSmaller(va, vb, kind));
		case Order_LessOrEqual:    return GetBool(FA::IsSmallerOrEqual(va, vb, kind));
		case Order_Greater:        return GetBool(FA::IsLarger(va, vb, kind));
		case Order_GreaterOrEqual: return GetBool(FA::IsLargerOrEqual(va, vb, kind));
		default: DebugValidator::Unreachable();
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareUndef(Opcode opcode, Operand* opA, Operand* opB, 
									        OrderType order, Block* block) {
    if(opA->IsUndefinedConstant() && opB->IsUndefinedConstant()) {
		// Both operands are undefined, so we can choose 'undef' as the result.
		return GetUndefined(opA);
	}

	// One of the operands may be a constant. 
    // If it is the result is based on it.
	Operand* constantOp = nullptr;

	if(opA->IsIntConstant() || opA->IsFloatingConstant()) {
		constantOp = opA;
	}
	else if(opB->IsIntConstant() || opB->IsFloatingConstant()) {
		constantOp = opB;
	}
	else {
		// Comparing an 'undef' with something that is
        // not a constant, return 'undef'.
		return GetUndefined(opA);
	}

	if(auto intConst = constantOp->As<IntConstant>()) {
		IRIntegerKind kind = intConst->GetType()->GetSubtype();
		return GetBool(IA::AreEqual(intConst->Value(), 0, kind));
	}
	else if(auto floatConst = constantOp->As<FloatConstant>()) {
		IRFloatingKind kind = floatConst->GetType()->GetSubtype();
		return GetBool(FA::AreEqual(floatConst->Value(), 0.0, kind));
	}
	else { 
        DebugValidator::Unreachable();
        return nullptr;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareNullPointer(Opcode opcode, Operand* opA, 
                                                  Operand* opB, OrderType order,
                                                  Block* block) {
    DebugValidator::IsTrue(opA->IsPointer());

    NullConstant* nullConst1 = nullptr;
    NullConstant* nullConst2 = nullptr;
    Operand* other;

    if(auto nullConst = opA->As<NullConstant>()) {
        nullConst1 = nullConst;
    }
    else other = opA;

    if(auto nullConst = opB->As<NullConstant>()) {
        if(nullConst1) nullConst2 = nullConst;
        else nullConst1 = nullConst;
    }
    else other = opB;

    // If we have two 'nullptr' then we definitely know the result.
    if(nullConst1 && nullConst2) {
        switch(order) {
            case Order_Equal:
            case Order_LessOrEqual:
            case Order_GreaterOrEqual: {
                return GetBool(true);
            }
            default: {
                return GetBool(false);
            }
        }
    }
    else if(nullConst1) {
        // Move the null constant on the right so that
        // we have fewer cases to test for.
        if(nullConst1 == opA) {
            order = CmpInstrBase::InvertedOrder(order, false /* invertEquality */);
        }

        // Check if we know anything about the other operand.     
        // p != NULL -> 1, if we know p != NULL
        // p != NULL -> 0, if we know p == NULL
        OperandInfo opInfo(irGen_->GetUnit(), target_);

        if(opInfo.IsPointerNotNull(other, block)) {
            switch(order) {
                case Order_Equal:
                case Order_LessOrEqual:
                case Order_GreaterOrEqual: {
                    return GetBool(false);
                }
                default: return GetBool(true);
            }
        }
        else if(opInfo.IsPointerNull(other, block)) {
            switch(order) {
                case Order_Equal:
                case Order_LessOrEqual:
                case Order_GreaterOrEqual: {
                    return GetBool(true);
                }
                default: return GetBool(false);
            }
        }
    }
    else if((order == Order_Equal) ||
            (order == Order_NotEqual)) {
        OperandInfo opInfo(irGen_->GetUnit(), target_);
        bool isEqual = order == Order_Equal;

        // First result is for 'equal', second for 'not equal'.
        // a == b -> 1/0, if a == NULL and b == NULL
        if(opInfo.IsPointerNull(opA, block) &&
           opInfo.IsPointerNull(opB, block)) {
            return GetBool(isEqual ? true : false);
        }

        // a == b -> 0/1, if a != NULL and b == NULL or a == NULL and b != NULL
        if((opInfo.IsPointerNotNull(opA, block) &&
            opInfo.IsPointerNull(opB, block))
            ||
            (opInfo.IsPointerNull(opA, block) &&
             opInfo.IsPointerNotNull(opB, block))) {
            return GetBool(isEqual ? false : true);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareRange(Opcode opcode, Operand* opA, 
                                            Operand* opB, OrderType order, 
                                            Block* block) {
    // Try so compare the operands using value-range info.
    // Floating-point comparisons can't be handled here.
    if(opcode == Instr_Fcmp) {
        return nullptr;
    }

    OperandInfo opInfo(irGen_->GetUnit(), target_);

    // For pointers we can handle only equality.
    if(opA->IsPointer()) {
        switch(order) {
            case Order_Equal: {
                auto result = opInfo.AreEqual(opA, opB, block);

                if(result == Result_Yes) return GetBool(true);
                else if(result == Result_No) return GetBool(false);
                break;
            }
            case Order_NotEqual: {
                auto result = opInfo.AreNotEqual(opA, opB, block);

                if(result == Result_Yes) return GetBool(true);
                else if(result == Result_No) return GetBool(false);
                break;
            }
        }

        return nullptr;
    }

    switch(order) {
		case Order_Equal: {
            auto result = opInfo.AreEqual(opA, opB, block);

            if(result == Result_Yes) return GetBool(true);
            else if(result == Result_No) return GetBool(false);
            break;
        }
		case Order_NotEqual: {
            auto result = opInfo.AreNotEqual(opA, opB, block);

            if(result == Result_Yes) return GetBool(true);
            else if(result == Result_No) return GetBool(false);
            break;
        }
		case Order_Less: {
            bool isSigned = opcode == Instr_Cmp;
            auto result = opInfo.IsSmaller(opA, opB, isSigned, block);

            if(result == Result_Yes) return GetBool(true);
            else if(result == Result_No) return GetBool(false);
            break;
        }
		case Order_LessOrEqual: {
            bool isSigned = opcode == Instr_Cmp;
            auto result = opInfo.IsSmallerOrEqual(opA, opB, isSigned, block);

            if(result == Result_Yes) return GetBool(true);
            else if(result == Result_No) return GetBool(false);
            break;
        }
		case Order_Greater: {
            bool isSigned = opcode == Instr_Cmp;
            auto result = opInfo.IsGreater(opA, opB, isSigned, block);

            if(result == Result_Yes) return GetBool(true);
            else if(result == Result_No) return GetBool(false);
            break;
        }
		case Order_GreaterOrEqual: {
            bool isSigned = opcode == Instr_Cmp;
            auto result = opInfo.IsGreaterOrEqual(opA, opB, isSigned, block);

            if(result == Result_Yes) return GetBool(true);
            else if(result == Result_No) return GetBool(false);
            break;
        }
		default: DebugValidator::Unreachable();
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleComparePhi(Opcode opcode, Operand* opA,  Operand* opB, 
							              OrderType order) {
    // cmp lt phi {1,2}, 5 -> 1
    // cmp lt phi {1,2}, phi {3,4} -> 1
    // We do this only for integer constants.
    auto phiInstrA = opA->DefiningInstrAs<PhiInstr>();
    auto phiInstrB = opB->DefiningInstrAs<PhiInstr>();

    // All incoming operands should be integer constants.
    if(phiInstrA) {
        if(phiInstrA->ResultOp()->IsInteger() == false) {
            return nullptr;
        }

        if(phiInstrA->HasOnlyConstants() == false) {
            return nullptr;
        }

        // Don't do any check if the 'phi' has many incoming operands,
        // it would be time consuming and the chances to simplify something are small.
        if((phiInstrA->OperandCount() < 2) ||
           (phiInstrA->OperandCount() > 8)) {
            return nullptr;
        }
    }

    if(phiInstrB) {
        if(phiInstrA == nullptr) {
            return nullptr;
        }

        if((phiInstrB->ResultOp()->IsInteger() == false) ||
           (phiInstrB->HasOnlyConstants() == false)) {
            return nullptr;
        }

        if((phiInstrA->OperandCount() < 2) ||
           (phiInstrA->OperandCount() > 8)) {
            return nullptr;
        }
    }

    if(phiInstrA && phiInstrB) {
        bool isSigned = opcode == Instr_Cmp;
        bool first = true;
        bool previous = false;

        // Test each pair.
        for(int i = 0; i < phiInstrA->OperandCount(); i++) {
            for(int j = 0; j < phiInstrB->OperandCount(); j++) {
                auto incomingOpA = phiInstrA->GetOperand(i);
                auto incomingOpB = phiInstrB->GetOperand(j);

                if((incomingOpA && incomingOpB) == false) {
                    return nullptr;
                }

                bool result = OrderHolds(order, incomingOpA, 
                                         incomingOpB, isSigned);
                if(first) {
                    previous = result;
                    first = false;
                }
                else if(result != previous) {
                    // The result for all incoming operands should be the same.
                    return nullptr;
                }
            }
        }

        // We can fold to a constant.
        return GetBool(previous);
    }
    else if(phiInstrA) {
        auto intConst = AsIntConstant(opB, phiInstrA->ParentBlock());

        if(intConst == nullptr) {
            return nullptr;
        }

        bool isSigned = opcode == Instr_Cmp;
        bool first = true;
        bool previous = false;

        for(int i = 0; i < phiInstrA->OperandCount(); i++) {
            auto op = phiInstrA->GetOperand(i);

            if(op->IsIntConstant() == false) {
                return nullptr;
            }

            bool result = OrderHolds(order, op, intConst, isSigned);

            if(first) {
                previous = result;
                first = false;
            }
            else if(result != previous) {
                // The result for all incoming operands should be the same.
                return nullptr;
            }
        }

        // We can fold to a constant.
        return GetBool(previous);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::OrderHolds(OrderType order, Operand* opA, Operand* opB, 
                                bool isSigned) {
    auto intConstA = opA->As<IntConstant>();
    auto intConstB = opB->As<IntConstant>();
    DebugValidator::IsNotNull(intConstA);
    DebugValidator::IsNotNull(intConstB);

    switch(order) {
        case Order_Equal: {
            return IA::AreEqual(intConstA, intConstB);
        }
        case Order_NotEqual: {
            return IA::AreNotEqual(intConstA, intConstB);
        }
        case Order_Less: {
            if(isSigned) return IA::IsSmaller(intConstA, intConstB);
            else return IA::IsSmallerUnsigned(intConstA, intConstB);
        }
        case Order_LessOrEqual: {
            if(isSigned) return IA::IsSmallerOrEqual(intConstA, intConstB);
            else return IA::IsSmallerOrEqualUnsigned(intConstA, intConstB);
        }
        case Order_Greater: {
            if(isSigned) return IA::IsLarger(intConstA, intConstB);
            else return IA::IsLargerUnsigned(intConstA, intConstB);
        }
        case Order_GreaterOrEqual: {
            if(isSigned) return IA::IsLargerOrEqual(intConstA, intConstB);
            else return IA::IsLargerOrEqualUnsigned(intConstA, intConstB);
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareQuestion(Opcode opcode, Operand* opA, 
                                               Operand* opB, OrderType order) {
    DebugValidator::IsFalse(opA->IsConstant() && opB->IsConstant());

    // Test for things like:
    // cmp gt (quest c, 5, 3), 0
    // We don't do this for floating numbers and for equality comparisons.
    if(opcode == Instr_Fcmp) {
        return nullptr;
    }

    if((order == Order_Equal) || (order == Order_NotEqual)) {
       return nullptr;
    }

    // Check if at least one of the operands is a 'quest' instruction.
    QuestionInstr* questInstrA = nullptr;
    QuestionInstr* questInstrB = nullptr;
    IntConstant* intConst = nullptr;
    
    if(DetectQuestion(opA, opB, questInstrA, questInstrB, 
                      intConst, order) == false) {
        return nullptr;
    }

    // Now check for the possible cases.
    if(intConst) {
        // Both values need to met the comparison order
        // to be able to fold the comparison to a constant.
        auto trueConst = questInstrA->TrueOp()->As<IntConstant>();
        auto falseConst = questInstrA->FalseOp()->As<IntConstant>();
        bool isUnsigned = opcode == Instr_Ucmp;

        switch(order) {
        case Order_Less: {
            if(isUnsigned) {
                if(IA::IsSmallerUnsigned(trueConst, intConst) &&
                   IA::IsSmallerUnsigned(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsLargerUnsigned(trueConst, intConst) &&
                        IA::IsLargerUnsigned(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            else {
                if(IA::IsSmaller(trueConst, intConst) &&
                   IA::IsSmaller(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsLarger(trueConst, intConst) &&
                        IA::IsLarger(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            break;
        }
        case Order_LessOrEqual: {
            if(isUnsigned) {
                if(IA::IsSmallerOrEqualUnsigned(trueConst, intConst) &&
                   IA::IsSmallerOrEqualUnsigned(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsLargerUnsigned(trueConst, intConst) &&
                        IA::IsLargerUnsigned(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            else {
                if(IA::IsSmallerOrEqual(trueConst, intConst) &&
                   IA::IsSmallerOrEqual(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsLarger(trueConst, intConst) &&
                        IA::IsLarger(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            break;
        }
        case Order_Greater: {
            if(isUnsigned) {
                if(IA::IsLargerUnsigned(trueConst, intConst) &&
                   IA::IsLargerUnsigned(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsSmallerUnsigned(trueConst, intConst) &&
                        IA::IsSmallerUnsigned(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            else {
                if(IA::IsLarger(trueConst, intConst) &&
                   IA::IsLarger(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsSmaller(trueConst, intConst) &&
                        IA::IsSmaller(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            break;
        }
        case Order_GreaterOrEqual: {
            if(isUnsigned) {
                if(IA::IsLargerOrEqualUnsigned(trueConst, intConst) &&
                   IA::IsLargerOrEqualUnsigned(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsSmallerUnsigned(trueConst, intConst) &&
                        IA::IsSmallerUnsigned(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            else {
                if(IA::IsLargerOrEqual(trueConst, intConst) &&
                   IA::IsLargerOrEqual(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsSmaller(trueConst, intConst) &&
                        IA::IsSmaller(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            break;
        }
        default: DebugValidator::Unreachable();
        }
    }
    else return HandleCompareQuestionQuestion(opcode, questInstrA, 
                                              questInstrB, order);

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::DetectQuestion(Operand* opA, Operand* opB, 
                                    QuestionInstr*& questInstrA, 
                                    QuestionInstr*& questInstrB, 
                                    IntConstant*& intConst, OrderType& order) {
    // Check if we have a 'quest' instructions as one of the operands.
    if(auto questInstr = opA->DefiningInstrAs<QuestionInstr>()) {
        questInstrA = questInstr;
    }
    else intConst = opA->As<IntConstant>();

    if(auto questInstr = opB->DefiningInstrAs<QuestionInstr>()) {
        if(questInstrA) questInstrB = questInstr;
        else questInstrA = questInstr;

        // Invert the order in this case.
        if(intConst) {
            order = CmpInstrBase::InvertedOrder(order, false /* invertEquality */);
        }
    }
    else intConst = opB->As<IntConstant>();

    // If we don't have the right operands return.
    if(questInstrA == nullptr) {
        return false;
    }

    if((intConst == nullptr) && (questInstrB == nullptr)) {
        return false;
    }

    // The operands of the 'quest' instructions need to be constant.
    if((questInstrA->TrueOp()->IsConstant() && 
        questInstrA->FalseOp()->IsConstant()) == false) {
        return false;
    }

    if(questInstrB) {
        if((questInstrB->TrueOp()->IsConstant() && 
            questInstrB->FalseOp()->IsConstant()) == false) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareQuestionQuestion(Opcode opcode, 
                                                       QuestionInstr* questInstrA,
                                                       QuestionInstr* questInstrB,
                                                       OrderType order) {
    // cmp lt (quest c1, 1, 2), (quest c2, 3, 4) -> always true
    // We don't test 'equal' and 'not equal' orders.
    if((order == Order_Equal) || (order == Order_NotEqual)) {
        return nullptr;
    }

    auto trueConstA = questInstrA->TrueOp()->As<IntConstant>();
    auto falseConstA = questInstrA->FalseOp()->As<IntConstant>();
    auto trueConstB = questInstrB->TrueOp()->As<IntConstant>();
    auto falseConstB = questInstrB->FalseOp()->As<IntConstant>();
    bool isUnsigned = opcode == Instr_Ucmp;

    // Check if each constant from the left side satisfies the order
    // when compared with any constant from the right side.
    if(IsOrderSatisfied(trueConstA, trueConstB, falseConstB, order, isUnsigned) && 
       IsOrderSatisfied(falseConstA, trueConstB, falseConstB, order, isUnsigned)) {
        return GetBool(true);
    }

    // Try on the reverse order, so we may return 'false' as the result.
    order = CmpInstrBase::InvertedOrder(order);

    if(IsOrderSatisfied(trueConstA, trueConstB, falseConstB, order, isUnsigned) && 
       IsOrderSatisfied(falseConstA, trueConstB, falseConstB, order, isUnsigned)) {
        return GetBool(false);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::IsOrderSatisfied(IntConstant* intConstA, IntConstant* intConstB,
                                      IntConstant* intConstC, OrderType order,
                                      bool isUnsigned) {
    switch(order) {
        case Order_Less: {
            if(isUnsigned) return IA::IsSmallerUnsigned(intConstA, intConstB) &&
                                  IA::IsSmallerUnsigned(intConstA, intConstC);
            else return IA::IsSmaller(intConstA, intConstB) &&
                        IA::IsSmaller(intConstA, intConstC);
        }
        case Order_LessOrEqual: {
            if(isUnsigned) return IA::IsSmallerOrEqualUnsigned(intConstA, intConstB) &&
                                  IA::IsSmallerOrEqualUnsigned(intConstA, intConstC);
            else return IA::IsSmallerOrEqual(intConstA, intConstB) &&
                        IA::IsSmallerOrEqual(intConstA, intConstC);
        }
        case Order_Greater: {
            if(isUnsigned) return IA::IsLargerUnsigned(intConstA, intConstB) &&
                                  IA::IsLargerUnsigned(intConstA, intConstC);
            else return IA::IsLarger(intConstA, intConstB) &&
                        IA::IsLarger(intConstA, intConstC);
        }
        case Order_GreaterOrEqual: {
            if(isUnsigned) return IA::IsLargerOrEqualUnsigned(intConstA, intConstB) &&
                                  IA::IsLargerOrEqualUnsigned(intConstA, intConstC);
            else return IA::IsLargerOrEqual(intConstA, intConstB) &&
                        IA::IsLargerOrEqual(intConstA, intConstC);
        }
        default: DebugValidator::Unreachable();
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareGlobalNull(Opcode opcode, Operand* opA, 
												 Operand* opB, OrderType order) {
	// Test for the comparison of a global variable reference with 'nullptr'.
	// The address of global variables can never be 'nullptr', so we can evaluate
	// the comparison at compile time.
	VariableReference* globalVariableRef = nullptr;
	Operand* otherOp = nullptr;

	// Also look through 'ptop' conversions, because they don't change bits,
	// and through 'ptoi' conversions (when compared with 0).
	// Handles cases like '(int*)&p != NULL' or '(int)&p == 0'.
	if(auto variableRef = opA->As<VariableReference>()) {
		globalVariableRef = variableRef;
		otherOp = opB;
	}
	else if(auto variableRef = opB->As<VariableReference>()) {
		globalVariableRef = variableRef;
		otherOp = opA;
	}
	else if(auto ptopInstr = opA->DefiningInstrAs<PtopInstr>()) {
		// Strip all other 'ptop' instructions and then check the base.
		while(auto basePtopInstr = ptopInstr->TargetOp()->DefiningInstrAs<PtopInstr>()) {
			ptopInstr = basePtopInstr;
		}

		globalVariableRef = ptopInstr->TargetOp()->As<VariableReference>();
		otherOp = opB;
	}
	else if(auto ptopInstr = opB->DefiningInstrAs<PtopInstr>()) {
		// Strip all other 'ptop' instructions and then check the base.
		while(auto basePtopInstr = ptopInstr->TargetOp()->DefiningInstrAs<PtopInstr>()) {
			ptopInstr = basePtopInstr;
		}

		globalVariableRef = ptopInstr->TargetOp()->As<VariableReference>();
		otherOp = opA;
	}
	else if(auto ptoiInstr = opA->DefiningInstrAs<PtoiInstr>()) {
		globalVariableRef = ptoiInstr->TargetOp()->As<VariableReference>();
		otherOp = opB;
	}
	else if(auto ptoiInstr = opB->DefiningInstrAs<PtoiInstr>()) {
		globalVariableRef = ptoiInstr->TargetOp()->As<VariableReference>();
		otherOp = opA;
	}

	// If we don't have a variable reference, and if the variable 
	// is not global we have nothing to do.
	if((globalVariableRef == nullptr) || 
       (globalVariableRef->IsGlobalVariableRef() == false)) {
		return nullptr;
	}

	// Determine the type of the other operand.
	// Supported are only 'nullptr' and the integer constant '0'.
	if(otherOp->IsNullConstant() || MatchInt(0)(otherOp)) {
		// cmp eq G, nullptr -> false
		// cmp neq G, nullptr -> true
		// For other cases there is nothing can be done
		if(order == Order_Equal) {
			return GetZeroInt(otherOp);
		}
		else if(order == Order_NotEqual) {
			return GetOneInt(otherOp);
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareVars(Opcode opcode, Operand* opA, 
                                           Operand* opB, OrderType order) {
	// Test for comparisons involving references to global variables.
	VariableReference* variableRefA = opA->As<VariableReference>();
	VariableReference* variableRefB = opB->As<VariableReference>();

	if(variableRefA && variableRefB) {
		// Example in C: '&a == &b' or '&c == &c'.
		if(variableRefA->IsGlobalVariableRef() && variableRefB->IsGlobalVariableRef()) {
			if(variableRefA->GetVariable() == variableRefB->GetVariable()) {
				// The same variable is on both sides.
				return GetResultForOrder(order, Order_Equal);
			}
			else {
				// The address of the variables can never be the same.
				return GetResultForOrder(order, Order_NotEqual);
			}
		}
	}
	else if(variableRefA || variableRefB) {
		// One of the operands is a variable reference.
		// See which is the reference and which is the other operand.
		VariableReference* variableRef = nullptr;
		Operand* otherOp = nullptr;

		if(variableRefA && variableRefA->IsGlobalVariableRef()) {
			variableRef = variableRefA;
			otherOp = opB;
		}
		else if(variableRefB->IsGlobalVariableRef()) {
			variableRef = variableRefB;
			otherOp = opA;
		}

		if(variableRef) {
			// We have a reference to a global variable.
			if(auto result = HandleCompareVarAddr(opcode, variableRef, otherOp, 
												  order, variableRef == variableRefA)) {
				return result;
			}
			else if(auto result = HandleCompareVarAggregate(opcode, variableRef, otherOp, 
															order, variableRef == variableRefA)) {
				return result;
			}
		}
	}

	// Test for a comparison between a 'nullptr' and 
    // an 'addr' involving a 'nullptr'.
	if(auto result = HandleCompareNullAddr(opcode, opA, opB, order)) {
		return result;
	}

	// Test for comparisons involving references to functions.
	if(auto functRefA = opA->As<FunctionReference>()) {
		if(auto functRefB = opB->As<FunctionReference>()) {
			if(functRefA->Target() == functRefB->Target()) {
				// The same function is on both sides.
				return GetResultForOrder(order, Order_Equal);
			}
			else {
				// Two functions reside in memory at different addresses.
				return GetResultForOrder(order, Order_NotEqual);
			}
		}
		else if(opB->IsNullConstant()) {
			// A function always has an address.
			return GetResultForOrder(order, Order_NotEqual);
		}
	}
	else if(opA->IsNullConstant() && opB->IsFunctionReference()) {
		// A function always has an address.
		return GetResultForOrder(order, Order_NotEqual);
	}

	// Test for cases involving the 'addr' and 'index' as both operands.
	// Example in C: '&a[4] < &a[6]' or '(p + 5) > 'p + 2'.
	if(IsAddressOrIndex(opA, opB)) {
		if(auto result = HandleCompareAddrAddr(opcode, opA, opB, order)) {
			return result;
		}	
	}
	else if(opA->DefiningInstrIs<ElementInstr>() &&
			opB->DefiningInstrIs<ElementInstr>()) {
		if(auto result = HandleCompareElemElem(opcode, opA, opB, order)) {
			return result;
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareVarAddr(Opcode opcode, VariableReference* variableRef,
											  Operand* otherOp, OrderType order, 
											  bool varLeftOp) {
	// Test for cases in which 'otherOp' is an 'addr' instruction.
	// Example in C: '&a < (&a + 5)' or '&v < (NULL + 3)'.
	auto addrInstr = otherOp->DefiningInstrAs<AddressInstr>();

	if(addrInstr == nullptr) {
        return nullptr;
    }

	// cmp ORDER, variableRef, (addr BASE, INDEX)
	// We can handle here only an index that is a constant.
	auto index = addrInstr->IndexOp()->As<IntConstant>();

	if(index == nullptr) {
        return nullptr;
    }

	// The index is a constant, now check the type of the base.
	// If it's 'nullptr' and 'INDEX' is 0 then we can compute the result now.
	if(addrInstr->BaseOp()->IsNullConstant() && (index->Value() == 0)) {
		if(varLeftOp) {
            return GetResultForOrder(order, Order_Greater);
        }
		else return GetResultForOrder(order, Order_Less);
	}

	// If 'BASE' is 'variableRef', we can compute the result now,
	// based on the value of 'INDEX'.
	if(auto baseVariableRef = addrInstr->BaseOp()->As<VariableReference>()) {
		if(baseVariableRef->GetVariable() == variableRef->GetVariable()) {
			if(index->Value() == 0) {
				return GetResultForOrder(order, Order_Equal);
			}
			else if(index->Value() > 0) {
				// cmp ORDER &g, (addr &g, 2)
				if(varLeftOp) {
                    return GetResultForOrder(order, Order_Less);
                }
				else return GetResultForOrder(order, Order_Greater);
			}
			else {
				// cmp ORDER (addr &g, 2), &g
				if(varLeftOp) {
                    return GetResultForOrder(order, Order_Greater);
                }
				else return GetResultForOrder(order, Order_Less);
			}
		}
		
		// If they are different global variables, and 'INDEX' is 0
		// then they can't be located at the same address.
		if(index->Value() == 0) {
			return GetResultForOrder(order, Order_NotEqual);
		}
	}
	
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareVarAggregate(Opcode opcode, 
                                                   VariableReference* variableRef,
												   Operand* otherOp, 
                                                   OrderType order, 
                                                   bool varLeftOp) {
	// Test for cases in which 'otherOp' is an 'index' or 'elem' instruction.
	// Example in C: '&a < &a[5]' or '&p <= &s.x'.
	if((otherOp->DefiningInstrIs<IndexInstr>() || 
	    otherOp->DefiningInstrIs<ElementInstr>()) == false) {
		return nullptr;
	}

	// cmp ORDER G1, (index/elem BASE, INDEX)
	// If 'BASE' is the same as the referenced variable, we can use
	// 'INDEX' to decide the outcome of the comparison.
	auto addrInstr = static_cast<AddressInstr*>(otherOp->DefiningInstruction());
	bool isElement = addrInstr->Is<ElementInstr>();
	auto baseVariableRef = addrInstr->BaseOp()->As<VariableReference>();

	if(baseVariableRef == nullptr) {
        return nullptr;
    }
	
	// We can handle here only an index that is a constant.
	auto index = addrInstr->IndexOp()->As<IntConstant>();

	if(index == nullptr) {
        return nullptr;
    }

	// If the variables are not the same, and the index is in the bounds
	// of the array (or record, but that is mandatory), we know that the 
	// address returned by the 'index'/'elem' instruction 
    // can't be the one of the global.
	if(baseVariableRef->GetVariable() != variableRef->GetVariable()) {
		if(auto arrayType = baseVariableRef->GetVariable()->GetType()->As<ArrayType>()) {
			if((index->Value() >= 0) && index->Value() < arrayType->Size()) {
				return GetResultForOrder(order, Order_NotEqual);
			}
		}
		else if(baseVariableRef->GetVariable()->GetType()->Is<RecordType>()) {
			// The index is always valid for records.
			return GetResultForOrder(order, Order_NotEqual);
		}

		return nullptr;
	}
	
	// The variables are the same, try to make a decision based on the index.
	if(index->Value() == 0) {
		// The addresses are the same. If the base is a record then the offset
		// of the first element must be 0 for the addresses to be the same.
		const Type* baseType = baseVariableRef->GetVariable()->GetType();

		if(const RecordType* record = baseType->As<RecordType>()) {
			if(record->Fields()[0].FieldOffset == 0) {
				return GetResultForOrder(order, Order_Equal);
			}
									
			// Other cases should not appear here.
		}
		else return GetResultForOrder(order, Order_Equal);
	}
	else if(index->Value() > 0) {
		// The address of the variable is smaller than the one returned by the 
		// 'index'/elem instruction. For records we must test the offset of the field,
		// because in case of unions, the offset can be 0 for more fields.
		const Type* baseType = baseVariableRef->GetVariable()->GetType();
								
		if(const RecordType* record = baseType->As<RecordType>()) {
			if(record->Fields()[index->Value()].FieldOffset == 0) {
				return GetResultForOrder(order, Order_Equal);
			}
		}
								
		if(varLeftOp) {
            return GetResultForOrder(order, Order_Less);
        }
		else return GetResultForOrder(order, Order_Greater);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandleCompareNullAddr(Opcode opcode, Operand* opA, 
                                               Operand* opB, OrderType order) {
	// Test for a comparison between a 'nullptr' and an 'addr' involving a 'nullptr'.
	// Example in C: '(&a + 4) != NULL' or 'NULL == NULL' or 'NULL != (NULL + 4')'.
	AddressInstr* addrInstr = nullptr;
	bool addrOnLeft = true;

	if(opB->IsNullConstant() && 
       (addrInstr = opA->DefiningInstrAs<AddressInstr>())) {
        addrOnLeft = true;
    }
	else if(opA->IsNullConstant() && 
           (addrInstr = opB->DefiningInstrAs<AddressInstr>())) {
		addrOnLeft = false;
	}

	// If there is no address instruction we have nothing to do.
	if(addrInstr == nullptr) {
        return nullptr;
    }

	// Test for 'nullptr' as the base of the 'addr'.
	if(addrInstr->BaseOp()->IsNullConstant()) {
		// If the index is 0 then the result is still 'nullptr'.
		// Else the address returned by 'addr' is greater than
        // 'nullptr' in all cases.
		if(auto index = addrInstr->IndexOp()->As<IntConstant>()) {
			if(index->Value() == 0) {
				return GetResultForOrder(order, Order_Equal);
			}
			else if(addrOnLeft) {
                return GetResultForOrder(order, Order_Greater);
            }
			else return GetResultForOrder(order, Order_Less);
		}
	}
	else if(addrInstr->BaseOp()->IsVariableReference()) {
		// No matter what the offset is, 'nullptr' will never be equal to the
		// address of the referenced variable.
		if(addrOnLeft) {
            return GetResultForOrder(order, Order_Greater);
        }
		else return GetResultForOrder(order, Order_Less);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandleCompareAddrAddr(Opcode opcode, Operand* opA,
                                               Operand* opB, OrderType order) {
	// Compare the index of the first 'addr' with the second one.
	// If it's smaller, set the result 'Less", if it's larger, it's 'Greater'.
	// If it's equal to the other one, we need to test for a 'addr' or 'index' base.
	auto addrInstrA = static_cast<AddressInstr*>(opA->DefiningInstruction());
	auto addrInstrB = static_cast<AddressInstr*>(opB->DefiningInstruction());
		
	while(true) {
		// Note that we can compare the indices only if
        // they are integer constants.
		auto indexA = addrInstrA->IndexOp()->As<IntConstant>();
		auto indexB = addrInstrB->IndexOp()->As<IntConstant>();

		if(indexA == nullptr || indexB == nullptr) {
            break;
        }

		// Compare the values of the indices.
		if(SameAddressBase(addrInstrA, addrInstrB)) {
			if(IA::IsSmaller(indexA, indexB)) {
				// (addr p, 2) < (addr p, 5)
				return GetResultForOrder(order, Order_Less);
			}
			else if(IA::IsLarger(indexA, indexB)) {
				// (addr p, 4) > (addr p, 2)
				return GetResultForOrder(order, Order_Greater);
			}
			else {
				// (addr p, 2) == (addr p, 2)
				return GetResultForOrder(order, Order_Equal);
			}
		}
			
		// The base of the 'addr'/'index' instructions is not the same.
		// Test if we have another level of these instructions.
		Operand* baseA = addrInstrA->BaseOp();
		Operand* baseB = addrInstrB->BaseOp();
							
		if(IsAddressOrIndex(baseA, baseB)) {
			addrInstrA = static_cast<AddressInstr*>(baseA->DefiningInstruction());
			addrInstrB = static_cast<AddressInstr*>(baseB->DefiningInstruction());
		}
		else {
			// The bases differ, so no relationship can be obtained.
			break;
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandleCompareElemElem(Opcode opcode, Operand* opA, 
                                               Operand* opB, OrderType order) {
	// Test for comparisons involving two 'elem' instructions.
	// Example in C: '&a.x < &a.y'.
	auto elemA = opA->DefiningInstrAs<ElementInstr>();
	auto elemB = opB->DefiningInstrAs<ElementInstr>();
	
	// The base for both must be a global variable reference.
	auto variableRefA = elemA->BaseOp()->As<VariableReference>();
	auto variableRefB = elemB->BaseOp()->As<VariableReference>();

	if(((variableRefA && variableRefA->IsGlobalVariableRef()) &&
		(variableRefB && variableRefB->IsGlobalVariableRef())) == false) {
		return nullptr;
	}

	if(variableRefA->GetVariable() != variableRefB->GetVariable()) {
		// We can't determine the relationship between different variables.
		return nullptr;
	}

	// Make a decision based on the index.
	auto indexA = elemA->IndexOp()->As<IntConstant>();
	auto indexB = elemB->IndexOp()->As<IntConstant>();
	auto recordType = variableRefA->GetVariable()->GetType()->As<RecordType>();

	if(IA::AreEqual(indexA, indexB)) {
		// Both 'elem' instructions return the same address.
		return GetResultForOrder(order, Order_Equal);
	}
	else {
		// Because more fields of the record can have the same offset
		// the decision is based on the offset, and not on the member index.
		__int64 offsetA = recordType->Fields()[indexA->Value()].FieldOffset;
		__int64 offsetB = recordType->Fields()[indexB->Value()].FieldOffset;
		
		if(offsetA < offsetB) {
            return GetResultForOrder(order, Order_Less);
        }
		else if(offsetA > offsetB) {
            return GetResultForOrder(order, Order_Greater);
        }
		else return GetResultForOrder(order, Order_Equal);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareLoadLoad(Opcode opcode, Operand* opA,    
                                               Operand* opB, OrderType order) {
    // *p !=/</>   *p -> 0
    // *p ==/<=/>= *p -> 1
    // Note that this is 100% true only if the loads are
    // in the same block, else there could be a path where
    // 'p' is assigned a value.
    auto loadA = opA->DefiningInstrAs<LoadInstr>();
    auto loadB = opB->DefiningInstrAs<LoadInstr>();

    if((loadA && loadB) == false) {
        return nullptr;
    }

    if(loadA->IsVolatile() || loadB->IsVolatile()) {
        return nullptr;
    }

    if((loadA->SourceOp() == loadB->SourceOp()) &&
       (loadA->ParentBlock() == loadB->ParentBlock())) {
        switch(order) {
            case Order_Equal:
            case Order_LessOrEqual:
            case Order_GreaterOrEqual: {
                return GetBool(true);
            }
            case Order_NotEqual:
            case Order_Less:
            case Order_Greater: {
                return GetBool(false);
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::SameAddressBase(AddressInstr* instrA, AddressInstr* instrB) {
	// Test for a base that is a variable.
	if(auto variableRefA = instrA->BaseOp()->As<VariableReference>()) {
		if(auto variableRefB = instrB->BaseOp()->As<VariableReference>()) {
			return variableRefA->GetVariable() == variableRefB->GetVariable();
		}
	}

	// Test for a base that is an integer converted to pointer.
	// (addr (itop 0, int32*), 5), (addr (itop 0, int32*), 6)
	if(auto itopInstrA = instrA->BaseOp()->DefiningInstrAs<ItopInstr>()) {
		if(auto itopInstrB = instrB->BaseOp()->DefiningInstrAs<ItopInstr>()) {
			// The pointer type to which we cast must be the same.
			if(itopInstrA->CastType() != itopInstrB->CastType()) {
                return false;
            }

			// The converted values must be the same constants.
			if(auto intConstA = itopInstrA->TargetOp()->As<IntConstant>()) {
				if(auto intConstB = itopInstrB->TargetOp()->As<IntConstant>()) {
					return IA::AreEqual(intConstA, intConstB);
				}
			}
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::GetResultForOrder(OrderType requiredOrder, 
                                           OrderType order) {
	// We know the order established between the instruction operands,
	// now we need to combine it with the required order.
	switch(order) {
		case Order_Equal: {
			if((requiredOrder == Order_Equal)       || 
               (requiredOrder == Order_LessOrEqual) ||
			   (requiredOrder == Order_GreaterOrEqual)) {
				// The result is 'true'.
				return GetBool(true);
			}
			else {
				// The result is 'false' for all other cases.
				return GetBool(false);
			}
			break;
		}
		case Order_NotEqual: {
			if(requiredOrder == Order_NotEqual) {
				return GetBool(true);
			}
			else if(requiredOrder == Order_Equal) {
				return GetBool(false);
			}
			break;
		}
		case Order_Less: {
			if((requiredOrder == Order_Less)        || 
               (requiredOrder == Order_LessOrEqual) ||
			   (requiredOrder == Order_NotEqual)) {
				return GetBool(true);
			}
			else if((requiredOrder == Order_Greater) || 
                    (requiredOrder == Order_Equal)   ||
				    (requiredOrder == Order_GreaterOrEqual)) {
				return GetBool(false);
			}
			break;
		}
		case Order_LessOrEqual: {
			if((requiredOrder == Order_Less) || 
               (requiredOrder == Order_LessOrEqual)) {
				return GetBool(true);
			}
			else if(requiredOrder == Order_Greater) {
				return GetBool(false);
			}
			break;
		}
		case Order_Greater: {
			if((requiredOrder == Order_Greater)        || 
               (requiredOrder == Order_GreaterOrEqual) ||
			   (requiredOrder == Order_NotEqual)) {
				return GetBool(true);
			}
			else if((requiredOrder == Order_Less)        || 
                    (requiredOrder == Order_LessOrEqual) ||
					(requiredOrder == Order_Equal)) {
				return GetBool(false);
			}
			break;
		}
		case Order_GreaterOrEqual: {
			if((requiredOrder == Order_Greater) || 
               (requiredOrder == Order_GreaterOrEqual)) {
				return GetBool(true);
			}
			else if(requiredOrder == Order_Less) {
				return GetBool(false);
			}
			break;
		}
	}

	// Nothing could be decided.
	return nullptr;
}

} // namespace Analysis