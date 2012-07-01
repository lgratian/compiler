// AdditivePeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 
// 'mul', 'fmul', 'div', 'udiv', 'fdiv', 'mod' and 'umod' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

Operand* Peephole::HandleMul(Operand* opA, Operand* opB, Block* block,
                             bool constMoveAllowed) {
	// If both operands are constant we can fold the operation.
	if(auto result = folder_.FoldBinary(Instr_Mul, opA, opB, block)) {
        return LOG(result);
	}

	// If one of the operands is a constant make it the the right operand,
	// so that we need to test much fewer cases.
	bool moved = MoveConstantToRight(opA, opB);

	// Test for simple cases first, like 'a * 1 -> a'.
	if(auto resultOp = HandleMulSimple(opA, opB, block)) {
		return resultOp;
	}

    // Try to simplify using associativity/commutativity rules.
    if(auto result = HandleAssociative(Instr_Mul, opA, opB, block)) {
        return result;
    }

    // Try to simplify using distributivity rules.
    if(auto result = HandleDistributive(Instr_Mul, opA, opB, block)) {
        return result;
    }

	// Test for more complicated expressions.
	if(auto resultOp = HandleMulComplex(opA, opB, block)) {
		return resultOp;
	}

	// Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_Mul, opA, opB)) {
        return result;
    }

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_Mul, opA, opB)) {
        return result;
    }

    // Returns a new instruction if the constant was moved to the right.
    if(moved && constMoveAllowed) {
        auto result = GetTemporary(opA);
        irGen_.GetMul(opA, opB, result);
        return LOG(result);
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleMulSimple(Operand* opA, Operand* opB, Block* block,
                                   bool createAllowed) {
    Operand* a;
	Operand* b;
	Operand* x;
	IntConstant* C;
	IntConstant* C1;
	IntConstant* C2;

	// All the simplifications in this method don't create any new instructions.
	// a * 0 -> 0
	if(IsZeroInt(opB, block)) {
		return LOG(opB);
	}

	// a * 1 -> a
	if(IsOneInt(opB, block)) {
		return LOG(opA);
	}

    // (a / b) * b -> a, if no remainder is produced
    if(Match<DivInstr>(MatchAny(&a), MatchAny(&b))(opA) && 
       (b == opB)) {
        auto divInstr = opA->DefiningInstrAs<DivInstr>();
       
        if(divInstr->HasNoRemainder()) {
            return LOG(a);
        }
    }

    // b * (a / b) -> a, if no remainder is produced
    if(Match<DivInstr>(MatchAny(&a), MatchAny(&b))(opB) &&
       (b == opA)) {
        auto divInstr = opA->DefiningInstrAs<DivInstr>();
       
        if(divInstr->HasNoRemainder()) {
            return LOG(a);
        }
    }

    // NOTE: All the following simplification are done only if
    // we're allowed to create new instructions.
    if(createAllowed == false) {
        return nullptr;
    }

    // a * -1 -> -a
    if(IsMinusOneInt(opB, block)) {
        auto temp = GetTemporary(opA);
        irGen_.GetSub(GetZeroInt(opA), opA, temp);
        return LOG(temp);
    }

	// (-a) * (-b) -> a * b
	if(Match<SubInstr>(MatchInt(0, block), MatchAny(&a))(opA) &&
	   Match<SubInstr>(MatchInt(0, block), MatchAny(&b))(opB)) {
		auto temp = GetTemporary(opA);
		irGen_.GetMul(a, b, temp);
		return LOG(temp);
	}

	// (-a) * b -> -(a * b)
	// a * (-b) -> -(a * b)
	if((Match<SubInstr>(MatchInt(0, block), MatchAny(&a))(opA) && 
        MatchAny(&b)(opB)) ||
	   (Match<SubInstr>(MatchInt(0, block), MatchAny(&b))(opB) && 
        MatchAny(&a)(opA))) {
		auto mulOp = GetTemporary(opA);
		irGen_.GetMul(a, b, mulOp);
		
        auto subOp = GetTemporary(opA);
		irGen_.GetSub(GetZeroInt(opA), mulOp, subOp);
		return LOG(subOp);
	}

	// (-a) * C -> a * -C
	if(Match<SubInstr>(MatchInt(0, block), MatchAny(&a))(opA) &&
       MatchIC(&C, block)(opB)) {
		auto constantOp = irGen_.GetIntConst(opA->GetType(), 
                                             IA::Sub(GetZeroInt(opA), C));
		auto temp = GetTemporary(opA);
		irGen_.GetMul(a, constantOp, temp);
		return LOG(temp);
	}

    // (a + a) * C -> a * (2 * C)
    if(Match<AddInstr>(MatchAny(&a), MatchOp(a))(opA) && 
       MatchIC(&C, block)(opB)) {
        __int64 value = IA::Mul(C->Value(), 2, C->GetType()->GetSubtype());
        auto constantOp = irGen_.GetIntConst(opA->GetType(), value);
		
        auto temp = GetTemporary(opA);
		irGen_.GetMul(a, constantOp, temp);
		return LOG(temp);
    }

	// (a + C1) * C2 -> (a * C2) + (C1 * C2)
	// This can allow for better expression reassociation.
	if(Match<AddInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) && 
       MatchIC(&C2, block)(opB)) {
		auto mulOp1 = GetTemporary(opA);
		irGen_.GetMul(a, C2, mulOp1);
		
        auto mulOp2 = irGen_.GetIntConst(opA->GetType(), IA::Mul(C1, C2));
		
        auto addOp = GetTemporary(opA);
		irGen_.GetAdd(mulOp1, mulOp2, addOp);
		return LOG(addOp);
	}

    // These are special cases of the above ones, when one 
    // of the operands is guaranteed to be a power of two.
	// a * (1 << b) -> a << b
	if(Match<ShlInstr>(MatchInt(1, block), MatchAny(&b))(opB)) {
		auto temp = GetTemporary(opA);
		irGen_.GetShl(opA, b, temp);
		return LOG(temp);
	}

	// (1 << a) * b -> b << a
	if(Match<ShlInstr>(MatchInt(1, block), MatchAny(&a))(opA)) {
		auto temp = GetTemporary(opA);
		irGen_.GetShl(opB, a, temp);
		return LOG(temp);
	}
	
	// (a << C1) * C2 -> a * (C2 << C1)
	if(Match<ShlInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) && 
       MatchIC(&C2, block)(opB)) {
		auto constantOp = irGen_.GetIntConst(opA->GetType(), IA::Shl(C2, C1));
		
        auto temp = GetTemporary(opA);
		irGen_.GetMul(a, constantOp, temp);
		return LOG(temp);
	}

    // (-a) * (b - x) -> -a*b + a*x -> a * (x - b)
	if(Match<SubInstr>(MatchInt(0, block), MatchAny(&a))(opA) &&
	   Match<SubInstr>(MatchAny(&b), MatchAny(&x))(opB)) {
		auto subOp = GetTemporary(opA);
		irGen_.GetSub(x, b, subOp);
		
        auto mulOp = GetTemporary(opA);
		irGen_.GetMul(a, subOp, mulOp);
		return LOG(mulOp);
	}

	// (a - b) * (-x) -> (b - a) * x
	if(Match<SubInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
	   Match<SubInstr>(MatchInt(0, block), MatchAny(&x))(opB)) {
		auto subOp = GetTemporary(opA);
		irGen_.GetSub(b, a, subOp);
		
        auto mulOp = GetTemporary(opA);
		irGen_.GetMul(subOp, x, mulOp);
		return LOG(mulOp);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleMulComplex(Operand* opA, Operand* opB, Block* block) {
	Operand* a;
	Operand* b;
	IntConstant* C1;
	IntConstant* C2;

    // (a / b) * -b -> -a, if no remainder is produced
    if(Match<DivInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
	   Match<SubInstr>(MatchInt(0, block), MatchOp(b))(opB)) {
        auto divInstr = opA->DefiningInstrAs<DivInstr>();

        if(divInstr->HasNoRemainder()) {
            return LOG(NegateInt(a));
        }
    }

    // -b * (a / b) -> -a, if no remainder is produced
    if(Match<DivInstr>(MatchAny(&a), MatchAny(&b))(opB) &&
	   Match<SubInstr>(MatchInt(0, block), MatchOp(b))(opA)) {
        auto divInstr = opA->DefiningInstrAs<DivInstr>();

        if(divInstr->HasNoRemainder()) {
            return LOG(NegateInt(a));
        }
    }

	// (a / b) * b = a - (a % b), for both signed and unsigned numbers.
    // If 'b' is a power of two the operation masks the lowest log2(b)
    // bits, and can be replaced by 'a & ~(b - 1)'. For example,
    // '(a / 32) * 32' can be replaced by 'a & 0xFFFFFFE0' (for int32).
	if((Match<DivInstr>(MatchAny(&a), MatchAny(&b))(opA) ||
        Match<UdivInstr>(MatchAny(&a), MatchAny(&b))(opA)) &&
       (b == opB)) {
        if(auto intConst = AsIntConstant(b, block)) {
            if(IA::IsPowerOfTwo(intConst)) {
                auto intKind = intConst->GetType()->GetSubtype();
                __int64 mask = IA::Not(intConst->Value() - 1, intKind);
                auto maskConst = irGen_.GetIntConst(intConst->GetType(), mask);
                
                auto andOp = GetTemporary(a);
                irGen_.GetAnd(a, maskConst, andOp);
                return LOG(andOp);
            }
        }

		auto modOp = GetTemporary(opA);
		irGen_.GetMod(a, b, modOp);
		
        auto subOp = GetTemporary(opA);
		irGen_.GetSub(a, modOp, subOp);
		return LOG(subOp);
	}
	
	// (a / b) * -b = (a % b) - a, for both signed and unsigned numbers.
	if(Match<DivInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
	   Match<SubInstr>(MatchInt(0, block), MatchOp(b))(opB)) {
		auto modOp = GetTemporary(opA);
		irGen_.GetMod(a, b, modOp);

		auto subOp = GetTemporary(opA);
		irGen_.GetSub(modOp, a, subOp);
		return LOG(subOp);
	}
	else if(Match<UdivInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
			Match<SubInstr>(MatchInt(0, block), MatchOp(b))(opB)) {
		auto modOp = GetTemporary(opA);
		irGen_.GetUmod(a, b, modOp);

		auto subOp = GetTemporary(opA);
		irGen_.GetSub(modOp, a, subOp);
		return LOG(subOp);
	}

	// Test for a multiply that involves boolean operands.
	// If the second operand is a boolean (it's value is either 0 or 1),
	// then the multiply behaves exactly like applying masks 0 or -1, respectively.
	// a * b -> a & (0 - b), if 'b' is 'bool'
	// a * b -> (0 - a) & b, if 'a' is 'bool'	
	if(opB->IsBoolean()) {
		auto subOp = GetTemporary(opA);
		irGen_.GetSub(GetZeroInt(opA), opB, subOp);
		subOp->SetIsBoolean(true);

		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(opA, subOp, andOp);
		return LOG(andOp);
	}
	else if(opA->IsBoolean()) {
		auto subOp = GetTemporary(opA);
		irGen_.GetSub(GetZeroInt(opA), opA, subOp);
		subOp->SetIsBoolean(true);

		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(subOp, opB, andOp);
		return LOG(andOp);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFmul(Operand* opA, Operand* opB, FloatMode mode) {
	// We need to take care when simplifying operations involving floating operands,
	// because the semantics could change in ways that are not possible for integers.
	Operand* a;
	Operand* b;
	FloatConstant* C1;
	FloatConstant* C2;

	// If both operands are constant we can fold the operation.
	if(opA->IsFloatingConstant() && opB->IsFloatingConstant()) {
		return LOG(folder_.FoldBinary(Instr_Fmul, opA, opB, nullptr));
	}

    // Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_Fmul, opA, opB)) {
        return result;
    }

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_Fmul, opA, opB)) {
        return result;
    }

	// ALL the following simplifications are valid only if 
	// the 'FP_Exact' mode is NOT activated for the instruction.
	if(mode == FP_Exact) return nullptr;

	// a * 1.0 -> a.
	if(MatchFloat(1.0)(opB)) {
		return LOG(opA);
	}

	// 2.0 * a -> a + a
	// a * 2.0 -> a + a
	if(MatchFloat(2.0)(opA)) {
		return LOG(opB);
	}
	else if(MatchFloat(2.0)(opB)) {
		return LOG(opA);
	}

	// (C1 / a) * C2 -> (C1 * C2) / a, 
    // if 'C1' and 'C2' are not NaN or Infinity.
	if(Match<FdivInstr>(MatchFC(&C1), MatchAny(&a))(opA) &&
	   MatchFC(&C2)(opB) &&
	   ((FA::IsNaN(C1) || FA::IsInfinity(C1)) == false) &&
	   ((FA::IsNaN(C1) || FA::IsInfinity(C1)) == false)) {
		auto constantOp = irGen_.GetFloatingConst(opA->GetType(), 
                                                  FA::Fmul(C1, C2));
		auto fdivOp = GetTemporary(opA);
		irGen_.GetFdiv(constantOp, a, fdivOp);
		return LOG(fdivOp);
	}

	// ALL the following simplifications are valid only if
	// the 'FP_Fast' mode is activated for the instruction.
	if(mode != FP_Fast) {
        return nullptr;
    }

	// -a * -b = a * b
	if(Match<FsubInstr>(MatchFloat(0.0), MatchAny(&a))(opA) && 
	   Match<FsubInstr>(MatchFloat(0.0), MatchAny(&b))(opB)) {
		auto temp = GetTemporary(opA);
		irGen_.GetFmul(a, b, temp);
		return LOG(temp);
	}

    // Test for expressions that involve math intrinsics.
    // For example 'sqrt(x) * sqrt(x) -> x'.
    if(auto result = HandleFmulIntrinsics(opA, opB, mode)) {
        return result;
    }

	// Try to simplify using associativity/commutativity rules.
    if(auto result = HandleAssociative(Instr_Fmul, opA, opB, nullptr, mode)) {
        return result;
    }

    // Try to simplify using distributivity rules.
    if(auto result = HandleDistributive(Instr_Fmul, opA, opB, nullptr, mode)) {
        return result;
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleDiv(Operand* opA, Operand* opB, Block* block,
                             bool hasNoRemainder) {
	Operand* a;
	IntConstant* C;

    // Try to constant-fold.
    if(auto result = folder_.FoldBinary(Instr_Div, opA, opB, block)) {
        return LOG(result);
    }

    // Try to simplify using distributivity rules.
    if(auto result = HandleDistributive(Instr_Div, opA, opB, block)) {
        return result;
    }

	// Handle operations that are common to both signed 
    // and unsigned division.
	if(auto result = HandleDivCommon(opA, opB, block, true /* isDiv */)) {
		return result;
	}

	// a / -1 -> -a
	// We replace an expensive division by a subtraction.
	if(IsMinusOneInt(opB, block)) {
		auto temp = GetTemporary(opA);
		irGen_.GetSub(GetZeroInt(opA), opA, temp);
		return LOG(temp);
	}

	// a / C -> a >> log2(C), if 'C' is a power of two.
    // Sadly, this simplification can be done only if we know that the division
    // doesn't produce any remainder. The problem is that the language standards
    // state that division should round towards zero, while using a shift would 
    // round towards minus infinity. For example, -3 / 2 = -1, but -3 >> 1 = -2.
    // An exception is when we know that the operand can't be negative.
    if(MatchIC(&C, block)(opB) && IA::IsPowerOfTwo(C)) {
        if(hasNoRemainder || opInfo_.IsPositive(opA)) {
		    auto amountOp = irGen_.GetIntConst(C->GetType(), IA::Log2(C));

		    auto temp = GetTemporary(opA);
		    irGen_.GetShr(opA, amountOp, temp);
		    return LOG(temp);
        }
	}
	
	// (-a) / C -> a / (-C), if the negation doesn't overflow.
    // Same reasons as above for not doing it if we could have a remainder.
	if(hasNoRemainder && Match<SubInstr>(MatchInt(0, block), 
                                         MatchAny(&a))(opA) && 
       MatchIC(&C, block)(opB)) {
		if(IA::SubOverflows(GetZeroInt(opA), C) == false) {
            auto constantOp = irGen_.GetIntConst(C->GetType(), -C->Value());
			auto temp = GetTemporary(opA);

			if(opA->DefiningInstrIs<DivInstr>()) {
				irGen_.GetDiv(a, constantOp, temp);
			}
			else irGen_.GetUdiv(a, constantOp, temp);

			return LOG(temp);
		}
	}

	// -a / -b -> a / b, if the type is signed and overflow is undefined.
	if(auto subInstrA = opA->DefiningInstrAs<SubInstr>()) {
		if(auto subInstrB = opB->DefiningInstrAs<SubInstr>()) {
			if(IsZeroInt(subInstrA->LeftOp(), block) &&
			   IsZeroInt(subInstrB->LeftOp(), block) &&
			   subInstrA->HasUndefinedOverflow() &&
			   subInstrB->HasUndefinedOverflow()) {
				auto a = subInstrA->RightOp();
				auto b = subInstrB->RightOp();

				auto divOp = GetTemporary(opA);
				irGen_.GetDiv(a, b, divOp);
				return LOG(divOp);
			}
		}
	}
	
    // Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_Div, opA, opB)) {
        return result;
    }

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_Div, opA, opB)) {
        return result;
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleUdiv(Operand* opA, Operand* opB, Block* block) {
	Operand* a;
    Operand* b;
	IntConstant* C;
	IntConstant* C1;
	IntConstant* C2;

    // Try to constant-fold.
    if(auto result = folder_.FoldBinary(Instr_Udiv, opA, opB, block)) {
        return LOG(result);
    }

    // Try to simplify using distributivity rules.
    if(auto result = HandleDistributive(Instr_Udiv, opA, opB, block)) {
        return result;
    }

	// Handle operations that are common to both signed and unsigned division.
	if(auto result = HandleDivCommon(opA, opB, block, false /* isDiv */)) {
		return result;
	}

	// a / C -> a (u)>> log2(C), if 'C' is a power of two.
	if(MatchIC(&C, block)(opB) && IA::IsPowerOfTwo(C)) {
		auto amountOp = irGen_.GetIntConst(C->GetType(), IA::Log2(C));
		auto temp = GetTemporary(opA);
		irGen_.GetUshr(opA, amountOp, temp);
		return LOG(temp);
	}

	// a / (C1 << C2) -> a >> (log2(C1) + C2), if 'C1' is a power of two.
	if(Match<ShlInstr>(MatchIC(&C1, block), 
                       MatchIC(&C2, block))(opB) &&
	   IA::IsPowerOfTwo(C1)) {
		auto amountOp = irGen_.GetIntConst(C1->GetType(), IA::Log2(C1));
		auto constantOp = irGen_.GetIntConst(C1->GetType(), IA::Add(amountOp, C2));
		
        auto temp = GetTemporary(opA);
		irGen_.GetUshr(opA, constantOp, temp);
		return LOG(temp);
	}

    // a / (C1 << b) -> a >> (log2(C1) + b), if 'C1' is a power of two.
    if(Match<ShlInstr>(MatchIC(&C1, block), MatchAny(&b))(opB) && 
       IA::IsPowerOfTwo(C1)) {
		auto amountOp = irGen_.GetIntConst(C1->GetType(), IA::Log2(C1));
		auto addOp = GetTemporary(C1);
        irGen_.GetAdd(amountOp, b, addOp);
		auto temp = GetTemporary(opA);

		irGen_.GetUshr(opA, addOp, temp);
		return LOG(temp);
	}

    // (a & (-C)) / C -> a (u)>> log2(C), if 'C' is a power of two.
    if(Match<AndInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) &&
       MatchIC(&C2, block)(opB) && 
       (C1->Value() == -C2->Value())) {
        auto amountOp = irGen_.GetIntConst(C2->GetType(), IA::Log2(C2));
		
        auto temp = GetTemporary(opA);
		irGen_.GetUshr(a, amountOp, temp);
		return LOG(temp);
    }

    // Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_Udiv, opA, opB)) {
        return result;
    }

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_Udiv, opA, opB)) {
        return result;
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleDivCommon(Operand* opA, Operand* opB, 
                                   Block* block, bool isDiv) {
	Operand* a;
	Operand* b;
	Operand* x;
	IntConstant* C1;
	IntConstant* C2;

	// a / 1 -> a
	if(IsOneInt(opB, block)) {
		return LOG(opA);
	}

    // (a / C1) / C2  -> a / (C1 * C2), for both signed and unsigned numbers.
	// Note that we must check that the multiply doesn't overflow
	// in order to preserve the semantics of the application.
	if((Match<DivInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) ||
        Match<UdivInstr>(MatchAny(&a), MatchIC(&C1, block))(opA)) && 
       MatchIC(&C2, block)(opB)) {
		if(IA::MulOverflows(C1, C2) == false) {
			// We can safely do the transformation.
			auto constantOp = irGen_.GetIntConst(opA->GetType(), 
                                                 IA::Mul(C1, C2));
			auto temp = GetTemporary(opA);
			irGen_.GetDiv(a, constantOp, temp);
			return LOG(temp);
		}
	}

	// (a / C1) / C2  -> a / (C1 * C2), if there is no overflow when multiplying.
	if((Match<DivInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) && 
        MatchIC(&C2, block)(opB)) ||
	   (Match<UdivInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) && 
        MatchIC(&C2, block)(opB))) {
		if(IA::MulOverflows(C1, C2) == false) {
			auto constantOp = irGen_.GetIntConst(opA->GetType(), IA::Mul(C1, C2));
			auto temp = GetTemporary(opA);

			if(isDiv) irGen_.GetDiv(a, constantOp, temp);
			else irGen_.GetUdiv(a, constantOp, temp);

			return LOG(temp);
		}
	}

	// C1 / (a / C2) -> (C1 * C2) / a, if there is no overflow when multiplying.
	if((MatchIC(&C1, block)(opA) && 
        Match<DivInstr>(MatchAny(&a), MatchIC(&C2, block))(opB)) ||
	   (MatchIC(&C1, block)(opA) && 
        Match<UdivInstr>(MatchAny(&a), MatchIC(&C2, block))(opB))) {
		if(IA::MulOverflows(C1, C2) == false) {
			auto constantOp = irGen_.GetIntConst(opA->GetType(), IA::Mul(C1, C2));
			auto temp = GetTemporary(opA);

			if(isDiv) irGen_.GetDiv(constantOp, a, temp);
			else irGen_.GetUdiv(constantOp, a, temp);

			return LOG(temp);
		}
	}
    
    // C1 / (a * C2) -> (C1 / C2) / a
    if((MatchIC(&C1, block)(opA) && 
        Match<MulInstr>(MatchAny(&a), MatchIC(&C2, block))(opB))) {
        __int64 value = isDiv ? IA::Div(C1, C2) : 
                                IA::Udiv(C1, C2);
		auto constantOp = irGen_.GetIntConst(opA->GetType(), value);
        auto divOp = GetTemporary(opA);

		if(isDiv) irGen_.GetDiv(constantOp, a, divOp);
		else irGen_.GetUdiv(constantOp, a, divOp);

		return LOG(divOp);
	}
    
	// a / (b / x) -> (a / b) * x
	if(Match<DivInstr>(MatchAny(&b), MatchAny(&x))(opB) ||
	   Match<UdivInstr>(MatchAny(&b), MatchAny(&x))(opB)) {		
        auto divOp = GetTemporary(opA);

		if(isDiv) irGen_.GetDiv(opA, b, divOp);
		else irGen_.GetUdiv(opA, b, divOp);

        auto mulOp = GetTemporary(opA);
		irGen_.GetMul(divOp, x, mulOp);
		
        return LOG(mulOp);
	}

	// (a - (a % b)) / b -> a / b
	if(Match<SubInstr>(MatchAny(&a), 
                       Match<SubInstr>(MatchOp(a), MatchAny(&b)))(opA) &&
	   (b == opB)) {
		auto temp = GetTemporary(opA);

		if(isDiv) irGen_.GetDiv(a, b, temp);
		else irGen_.GetUdiv(a, b, temp);
		
        return LOG(temp);
	}

    // (&a[i] - &a[j]) / C -> i-j, if 'C' is the array element size,
    // or a multiple of it.
    if(auto result = HandleIndexDifference(opA, opB)) {
        return result;
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIndexDifference(Operand* opA, Operand* opB) {
    // (&a[i] - &a[j]) / C -> i-j, if 'C' is the array element size, 
    // or a multiple of it. For example,
    // div (sub (ptoi (index a, i), intX), (ptoi (index a, j), intX)), C
    auto C = opB->As<IntConstant>();
    if((C == nullptr) || C->IsZero()) {
        return nullptr;
    }

    auto subInstr = opA->DefiningInstrAs<SubInstr>();
    if(subInstr == nullptr) {
        return nullptr;
    }

    auto ptoiInstrA = subInstr->LeftOp()->DefiningInstrAs<PtoiInstr>();
    auto ptoiInstrB = subInstr->RightOp()->DefiningInstrAs<PtoiInstr>();
    if((ptoiInstrA && ptoiInstrB) == false) {
        return nullptr;
    }

    // The index instructions must target the same variable.
    auto indexInstrA = ptoiInstrA->TargetOp()->DefiningInstrAs<IndexInstr>();
    auto indexInstrB = ptoiInstrB->TargetOp()->DefiningInstrAs<IndexInstr>();
    if((indexInstrA && indexInstrB) == false) {
        return nullptr;
    }

    if(indexInstrA->BaseOp() != indexInstrA->BaseOp()) {
        return nullptr;
    }

    // The constant must be a multiple of the size of the element type.
    // We allow a multiple because the simplification we get it's still cheaper
    // than the original instructions (for example, if 'a' is "array of int" 
    // and 'C = 8', we simplify to '(i - j) / 2' -> '(i - j) >> 1').
    auto elementType = indexInstrA->GetArrayType()->ElementType();
    auto castType = ptoiInstrA->CastType();

    __int64 elemSize = TI::GetSize(elementType, GetTarget());
    __int64 constValue = IA::LimitToType(C);

    if(constValue == elemSize) {
        // This is the most common case, we need just to subtract the index operands.
        return LOG(SubtractIndexOps(indexInstrA->IndexOp(), 
                                    indexInstrB->IndexOp(), castType));
    }
    else if(constValue > elemSize) {
        if((constValue % elemSize) != 0) {
            return nullptr;
        }

        // We need to divide (shift right) the subtraction result, because
        // we're dividing by a constant larger than the element size.
        auto subOp = SubtractIndexOps(indexInstrA->IndexOp(), 
                                      indexInstrB->IndexOp(), 
                                      castType);
        auto shrConst = irGen_.GetIntConst(castType, IA::Log2(constValue / elemSize));

        auto shrOp = irGen_.GetTemporary(castType);
        irGen_.GetShr(subOp, shrConst, shrOp);
        return LOG(shrOp);
    }
    else {
        if((elemSize % constValue) != 0) {
            return nullptr;
        }

        // We need to multiply (shift left) the subtraction result, because
        // we're dividing by a constant smaller than the element size.
        auto subOp = SubtractIndexOps(indexInstrA->IndexOp(), 
                                      indexInstrB->IndexOp(), castType);
        auto shlConst = irGen_.GetIntConst(castType, IA::Log2(elemSize / constValue));

        auto shlOp = irGen_.GetTemporary(castType);
        irGen_.GetShl(subOp, shlConst, shlOp);
        return LOG(shlOp);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::SubtractIndexOps(Operand* opA, Operand* opB, 
                                    const Type* resultType) {
    opA = CreateIntCastIfRequired(opA, resultType);
    opB = CreateIntCastIfRequired(opB, resultType);
    auto subOp = irGen_.GetTemporary(resultType);
    irGen_.GetSub(opA, opB, subOp);
    return subOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleMod(Operand* opA, Operand* opB, Block* block) {
    IntConstant* C;
	Operand* b;

    // Try to constant-fold.
    if(auto result = folder_.FoldBinary(Instr_Mod, opA, opB, block)) {
        return LOG(result);
    }

	// Handle operations that are common to both signed 
    // and unsigned modulus.
	if(auto result = HandleModCommon(opA, opB, block, true /* isMod */)) {
		return result;
	}

    // An operand modulus a power of two is equivalent with applying a bit mask.
	// a % C -> a & (C - 1), if 'C' is a power of two.
    // For singed numbers we can do this only if we're certain
    // that the number is always positive.
	if(MatchIC(&C, block)(opB) && IA::IsPowerOfTwo(C)) {
        if(opInfo_.IsPositive(opA)) {
		    auto constantOp = irGen_.GetIntConst(opA->GetType(), 
                                                 IA::Sub(C, GetOneInt(opA)));
		    auto temp = GetTemporary(opA);
		    irGen_.GetAnd(opA, constantOp, temp);
		    return LOG(temp);
        }
	}

	// a % -b -> a % b
	if(Match<SubInstr>(MatchInt(0, block), MatchAny(&b))(opB)) {
		auto temp = GetTemporary(opA);
		irGen_.GetMod(opA, b, temp);
		return LOG(temp);
	}

    // Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_Mod, opA, opB)) {
        return result;
    }

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_Mod, opA, opB)) {
        return result;
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleUmod(Operand* opA, Operand* opB, Block* block) {
	IntConstant* C;
	IntConstant* C1;
	IntConstant* C2;

    // Try to constant-fold.
    if(auto result = folder_.FoldBinary(Instr_Umod, opA, opB, block)) {
        return LOG(result);
    }

	// Handle operations that are common to both signed 
    // and unsigned modulus.
	if(auto result = HandleModCommon(opA, opB, block, true /* isUmod */)) {
		return result;
	}

    // An operand modulus a power of two is equivalent with applying a bit mask.
	// a % C -> a & (C - 1), if 'C' is a power of two.
	if(MatchIC(&C, block)(opB) && IA::IsPowerOfTwo(C)) {
		auto constantOp = irGen_.GetIntConst(opA->GetType(), 
                                             IA::Sub(C, GetOneInt(opA)));
		auto temp = GetTemporary(opA);
		irGen_.GetAnd(opA, constantOp, temp);
		return LOG(temp);
	}

	// a % (C1 << C2) -> a & ((C1 << C2) - 1), if 'C1' is a power of two.
	if(Match<ShlInstr>(MatchIC(&C1, block), 
                       MatchIC(&C2, block))(opB) &&
	   IA::IsPowerOfTwo(C1)) {
		auto constOp1 = irGen_.GetIntConst(opA->GetType(), IA::Shl(C1, C2));
		auto constOp2 = irGen_.GetIntConst(opA->GetType(), 
											IA::Sub(constOp1, GetOneInt(opA)));
		auto temp = GetTemporary(opA);
		irGen_.GetAnd(opA, constOp2, temp);
		return LOG(temp);
	}

    // Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_Umod, opA, opB)) {
        return result;
    }

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_Umod, opA, opB)) {
        return result;
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleModCommon(Operand* opA, Operand* opB, 
                                   Block* block, bool isMod) {
	// a % a -> 0. Note that we don't care about 'a' possibly being zero,
	// because the behavior is undefined anyway.
	if(AreEqual(opA, opB, block)) {
		return LOG(GetZeroInt(opA));
	}

	// a % 1 -> 0
	if(IsOneInt(opB, block)) {
		return LOG(GetZeroInt(opA));
	}

	// a % -1 -> 0
	if(IsMinusOneInt(opB, block)) {
		return LOG(GetZeroInt(opA));
	}

	// 0 % a -> 0. Note that we don't care about 'a' possibly being zero,
	// because the behavior is undefined anyway.
	if(IsZeroInt(opA, block)) {
		return LOG(GetZeroInt(opA));
	}

	// a % 0 -> undef, same reason as above.
	if(IsZeroInt(opB, block)) {
		return LOG(GetUndefined(opA));
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFdiv(Operand* opA, Operand* opB, FloatMode mode) {
	// We need to take care when simplifying operations involving 
    // floating operands, because the semantics could change in ways 
    // that are not possible for integers.
	Operand* a;
	Operand* b;
	Operand* x;
	FloatConstant* C;
	FloatConstant* C1;
	FloatConstant* C2;

	// If both operands are constant we can fold the operation.
	if(opA->IsFloatingConstant() && opB->IsFloatingConstant()) {
		return LOG(folder_.FoldBinary(Instr_Fdiv, opA, opB, nullptr));
	}

    // Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_Fdiv, opA, opB)) {
        return result;
    }

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_Fdiv, opA, opB)) {
        return result;
    }
	
	// ALL the following simplifications are valid only if 
	// the 'FP_Exact' mode is NOT activated for the instruction.
	if(mode == FP_Exact) {
        return nullptr;
    }

	// a / 1.0 -> a
	if(MatchFloat(1.0)(opB)) {
		return LOG(opA);
	}

	// a / C -> a * (1.0 / C), if 'C' is not NaN or Infinity
	// This replaces the very expensive division by 
    // a multiplication by the reciprocal.
	if(MatchFC(&C)(opB) && (FA::IsNaN(C) || 
       FA::IsInfinity(C)) == false) {
		auto floatKind = C->GetType()->GetSubtype();
		auto reciprocal = FA::Fdiv(1.0, C->Value(), floatKind);
		auto reciprocalConst = irGen_.GetFloatingConst(C->GetType(), reciprocal);

		auto mulOp = GetTemporary(opA);
		irGen_.GetFmul(opA, reciprocalConst, mulOp);
		return LOG(mulOp);
	}

	// (a / C1) / C2  -> a / (C1 * C2), 
    // if 'C1' and 'C2' are not NaN or Infinity
	if((Match<FdivInstr>(MatchAny(&a), MatchFC(&C1))(opA) &&
		MatchFC(&C2)(opB)) &&
		((FA::IsNaN(C1) || FA::IsInfinity(C1)) == false) &&
		((FA::IsNaN(C1) || FA::IsInfinity(C1)) == false)) {
		auto constantOp = irGen_.GetFloatingConst(opA->GetType(), FA::Fmul(C1, C2));

		auto temp = GetTemporary(opA);
		irGen_.GetFdiv(a, constantOp, temp);
		return LOG(temp);
	}

	// C1 / (a / C2) -> (C1 * C2) / a
	if(MatchFC(&C1)(opA) && 
	   Match<FdivInstr>(MatchAny(&a), MatchFC(&C2))(opB) &&
	   ((FA::IsNaN(C1) || FA::IsInfinity(C1)) == false) &&
	   ((FA::IsNaN(C1) || FA::IsInfinity(C1)) == false)) {
		auto constantOp = irGen_.GetFloatingConst(opA->GetType(), FA::Fmul(C1, C2));

		auto temp = GetTemporary(opA);
		irGen_.GetFdiv(constantOp, a, temp);
		return LOG(temp);
	}

	// ALL the following simplifications are valid only if
	// the 'FP_Fast' mode is activated for the instruction.
	if(mode != FP_Fast) {
        return nullptr;
    }

	// a / a -> 1.0
	if(opA == opB) {
		return LOG(irGen_.GetFloatingConst(opA->GetType(), 1.0));
	}

	// a / -a -> -1
	// -a / a -> -1
	if((Match<FsubInstr>(MatchFloat(0.0), MatchAny(&a))(opB) &&
	   (a == opA)) ||
	   (Match<FsubInstr>(MatchFloat(0.0), MatchAny(&a))(opA) &&
	   (a == opB))) {
		return LOG(irGen_.GetFloatingConst(opA->GetType(), -1.0));
	}

	// a / (b / x) -> (a * x) / b
	if(Match<FdivInstr>(MatchAny(&b), MatchAny(&x))(opB) ||
	   Match<UdivInstr>(MatchAny(&b), MatchAny(&x))(opB)) {
		auto mulOp = GetTemporary(opA);
		irGen_.GetFmul(opA, x, mulOp);

		auto divOp = GetTemporary(opA);
		irGen_.GetFdiv(mulOp, b, divOp);
		return LOG(divOp);
	}

	// a / b -> a * (1 / b). Note that this is beneficial only if
	// there is more than one instruction that divides by 'b', allowing
	// common subexpression elimination to be performed. We don't know here
	// how many divisions by 'b' we have, but we do the transformation anyway,
	// relaying on the code generator to transform back if it slows things down.
	if(b->HasSingleUser() == false) {
		// We presume all users are divisions.
		auto oneConst = irGen_.GetFloatingConst(opA->GetType(), 1.0);

        auto divOp = GetTemporary(opA);
		irGen_.GetFdiv(oneConst, opB, divOp);

		auto mulOp = GetTemporary(opA);
		irGen_.GetFmul(opA, divOp, mulOp);
		return LOG(mulOp);
	}

    // Test for expressions that involve math intrinsics.
    // For example 'sin(x) / tan(x) -> cos(x)'.
    if(auto result = HandleFdivIntrinsics(opA, opB, mode)) {
        return result;
    }

	return nullptr;
}

} // namespace Optimization