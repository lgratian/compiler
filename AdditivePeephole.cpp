// AdditivePeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 
// 'add', 'sub', 'fadd' and 'fsub' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

Operand* Peephole::HandleAdd(Operand* opA, Operand* opB, Block* block,
                             bool constMoveAllowed) {
	// Try to constant-fold the instruction.
	if(auto result = folder_.FoldBinary(Instr_Add, opA, opB, block)) {
        return result;
	}

	// If one of the operands is a constant make it the the right operand,
	// so that we need test much fewer cases.
	bool moved = MoveConstantToRight(opA, opB);

	// Test for simple cases first, like 'a + 0 -> a'.
	if(auto result = HandleAddSimple(opA, opB, block)) {
		return result;
	}

    // Try to simplify using associativity/commutativity rules.
    if(auto result = HandleAssociative(Instr_Add, opA, opB, block)) {
        return result;
    }

    // Try to simplify using distributivity rules.
    if(auto result = HandleDistributive(Instr_Add, opA, opB, block)) {
        return result;
    }

	// Test for more complicated expressions.
	if(auto result= HandleAddComplex(opA, opB, block)) {
		return result;
	}

    // Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_Add, opA, opB)) {
        return result;
    }

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_Add, opA, opB)) {
        return result;
    }

    // Returns a new instruction if the constant was moved to the right.
    if(moved && constMoveAllowed) {
        auto result = GetTemporary(opA);
        irGen_.GetAdd(opA, opB, result);
        return result;
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddSimple(Operand* opA, Operand* opB, Block* block,
                                   bool createAllowed) {
    Operand* a;
    Operand* b;
    IntConstant* C;
    IntConstant* C1;
    IntConstant* C2;

	// All the simplifications in this method don't create any new instructions.
	// a + 0 -> a
	if(IsZeroInt(opB, block)) {
		return LOG(opA);
	}

	// a + undef -> undef
	// undef + a -> undef
	if(MatchUndef()(opA)) {
		return LOG(opA);
	}
	else if(MatchUndef()(opB)) {
		return LOG(opB);
	}

	// (-a) + a -> 0
	if(Match<SubInstr>(MatchInt(0, block), MatchOp(opB))(opA)) {
		return LOG(irGen_.GetIntConst(opA->GetType(), 0));
	}

	// a + (-a) -> 0
	if(Match<SubInstr>(MatchInt(0, block), MatchOp(opA))(opB)) {
		return LOG(irGen_.GetIntConst(opB->GetType(), 0));
	}

	// (b - a) + a -> b
	if(Match<SubInstr>(MatchAny(&b), MatchOp(opB))(opA)) {
		return LOG(b);
	}

	// a + (b - a) -> b
	if(Match<SubInstr>(MatchAny(&b), MatchOp(opA))(opB)) {
		return LOG(b);
	}

	// a + ~a -> -1, because ~a = -a - 1 => a - a - 1 -> -1
	// The complement is written as: a ^ -1
	if(Match<XorInstr>(MatchOp(opA), MatchInt(-1, block))(opB) ||
	   Match<XorInstr>(MatchOp(opB), MatchInt(-1, block))(opA)) {
		return LOG(irGen_.GetIntConst(opA->GetType(), -1));
	}

    // NOTE: All the following simplification are done only if
    // we're allowed to create new instructions.
    if(createAllowed == false) {
        return nullptr;
    }

    // a + a -> a * 2
	// We use 'a * 2' instead of the expected 'a << 1', because this may create
	// more simplification opportunities if the result is multiplied by constants;
	// if another multiplication is not found, it will be converted to a shift anyway.
	if(opA == opB) {
		auto temp = GetTemporary(opA);
		auto twoConst = irGen_.GetIntConst(opA->GetType(), 2);
		irGen_.GetMul(opA, twoConst, temp);
		return LOG(temp);
	}

	// a + (-b) -> a - b
	if(Match<SubInstr>(MatchInt(0, block), MatchAny(&b))(opB) && MatchAny(&a)(opA)) {
		auto temp = GetTemporary(a);
		irGen_.GetSub(a, b, temp);
		return LOG(temp);
	}

	// (-a) + b -> b - a
	if(Match<SubInstr>(MatchInt(0, block), MatchAny(&a))(opA) && MatchAny(&b)(opB)) {
		auto temp = GetTemporary(a);
		irGen_.GetSub(b, a, temp);
		return LOG(temp);
	}

    // (a - C1) + C2 = a + (C2 - C1)
    if(Match<SubInstr>(MatchAny(&a), MatchIC(&C1))(opA) && MatchIC(&C2)(opB)) {
        auto constantOp = folder_.FoldBinary(Instr_Sub, C2, C1, block);
        auto resultOp = GetTemporary(a);
        irGen_.GetAdd(a, constantOp, resultOp);
        return LOG(resultOp);
    }

    // a + (a * C) -> a * (C + 1)
    // (a * C) + a -> a * (C + 1)
    if((Match<MulInstr>(MatchAny(&a), MatchIC(&C))(opB) && (opA == a)) ||
       (Match<MulInstr>(MatchAny(&a), MatchIC(&C))(opA) && (opB == a))) {
        auto oneConst = irGen_.GetIntConst(a->GetType(), 1);
		auto constantOp = folder_.FoldBinary(Instr_Add, C, oneConst, block);

		auto temp = GetTemporary(a);
		irGen_.GetMul(a, constantOp, temp);
		return LOG(temp);
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddComplex(Operand* opA, Operand* opB, Block* block) {
    Operand* a;
    Operand* b;
    IntConstant* C;
    IntConstant* C1;
    IntConstant* C2;

	// Test for things like '(a * b) + (a * d)'.
	if(auto result = HandleAddDistributive(opA, opB, block)) {
        return result;
    }

	// (-a) + (-b) -> -(a + b)
	if(Match<SubInstr>(MatchInt(0, block), MatchAny())(opA) &&
	   Match<SubInstr>(MatchInt(0, block), MatchAny())(opB)) {
		auto temp = GetTemporary(opA);
		irGen_.GetAdd(opA, opB, temp);

		// Now negate the value.
		return LOG(NegateInt(temp));
	}

	// ~a + C -> (C - 1) - a. The complement is written as: a ^ -1
	if(IsLogicalNot(opA, &a) && MatchIC(&C, block)(opB)) {
		auto oneConst = irGen_.GetIntConst(a->GetType(), 1);
		auto constantOp = folder_.FoldBinary(Instr_Sub, C, oneConst, block);

		auto temp = irGen_.GetTemporary(a->GetType());
		irGen_.GetSub(constantOp, a, temp);
		return LOG(temp);
	}

	// ~a + a = -1. The complement is written as: a ^ -1
	if(IsLogicalNot(opA, &a) && (a == opB)) {
		return LOG(GetMinusOneInt(opA));
	}

    // ~a + 1 -> -a
    if(IsLogicalNot(opA, &a) && MatchInt(1, block)(opB)) {
        return LOG(NegateInt(a));
    }

    // (a + 1) + b -> (a + b) + 1.
    // This exposes the constant for further simplifications.
    if((Match<AddInstr>(MatchAny(&a), MatchIC(&C, block))(opA) && 
       (b = opB))) {
        // The 'add' should have a single user, else we will increase
        // the register pressure.
        if(opA->HasSingleUser()) {
            auto addOp = GetTemporary(opA);
            irGen_.GetAdd(a, b, addOp);

            auto resultOp = GetTemporary(opA);
            irGen_.GetAdd(addOp, C, resultOp);
            return LOG(resultOp);
        }
    }

	// zext(C1<bool>) + C -> 'C + 1', if 'C1 == 1', else 'C'
	// sext(C1<bool>) + C -> 'C + 1', if 'C1 == 1', else 'C'
	if((MatchConversion<ZextInstr>(MatchIC(&C1, block))(opA) && 
        MatchIC(&C2)(opB)) ||
	   (MatchConversion<SextInstr>(MatchIC(&C1, block))(opA) && 
        MatchIC(&C2)(opB))) {
		if(C1->Value() == 0) {
            return C2;
        }

		auto oneConst = GetOneInt(C2);
		auto constantOp = folder_.FoldBinary(Instr_Add, C2, oneConst, block);
		return LOG(constantOp);
	}

	// Try to simplify the addition of a constant with a value that is masked.
	// If the zero bits in the low part of the mask match with the ones
	// in the constant, we can do the following:
	// (a & C1) + C2 -> (a + C2) & C1
	// (a & 0xFF...00) + 0xXX...00 -> (a + 0xXX...00) & 0xFF...000
    // This may expose simplification opportunities, and has the same cost as before.
	if(((Match<AndInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) && 
         MatchIC(&C2, block)(opB))) ||
	   ((Match<AndInstr>(MatchIC(&C1, block), MatchAny(&a))(opB) && 
         MatchIC(&C2, block)(opB)))) {
        // C1: 0xFFF4                     1111111111110100
        // C2: 0x8    0000000000001000 -> 1111111111110000
        // The above situation is valid; it guarantees that doing the 'and' last
        // will not reset any bits introduced by the 'add'.
		int zeroBits1 = IA::LeftmostZeroBits(C1);
		int zeroBits2 = IA::LeftmostZeroBits(C2);

		if((zeroBits1 != 0) && (zeroBits1 >= zeroBits2)) {
			// Make the transformation.
			auto addOp = GetTemporary(a);
			irGen_.GetAdd(a, C2, addOp);

			auto andOp = GetTemporary(a);
			irGen_.GetAnd(addOp, C1, andOp);
			return LOG(andOp);
		}
	}

	// If we add an operand that is marked as 'bool' with a number that has
	// the leftmost bit set we can replace the 'add' by an 'xor'.
	// a<bool> + C -> a<bool> ^ C
	if(opA->IsBoolean() && MatchIC(&C, block)(opB)) {
		if(IA::IsBitSet(0, C)) {
			auto temp = GetTemporary(opA);
			temp->SetIsBoolean(true);
			irGen_.GetXor(opA, C, temp);
			return LOG(temp);
		}
	}
	
	// Try to simplify sign extension (very important, because frontends
	// generate code that does this often in a very naive way). 
	// Note that the type of 'a' and 'b' must be the same, and the 'add'
	// shouldn't overflow after we make the change.
	// sext(a) + sext(b) -> sext(a + b)
	if((MatchConversion<SextInstr>(MatchIC(&C1, block))(opA) &&
		MatchConversion<SextInstr>(MatchIC(&C2, block))(opB)) || 
		// zext(a) + zext(b) -> zext(a + b)
	   (MatchConversion<SextInstr>(MatchIC(&C1, block))(opA) &&
		MatchConversion<SextInstr>(MatchIC(&C2, block))(opB)) ||
		// sext(a) + b -> sext(a + b)
		(MatchConversion<SextInstr>(MatchIC(&C1, block))(opA) && 
		 MatchIC(&C2, block)(opB)) ||
		// zext(a) + b -> zext(a + b)
	   (MatchConversion<ZextInstr>(MatchIC(&C1, block))(opA) && 
	    MatchIC(&C2, block)(opB))) {
		if((C1->GetType() == C2->GetType()) &&
		   (opInfo_.CanAddOverflow(C1, C2) == false)) {
			// It's safe to perform the transformation, fold the constants.
			auto constantOp = folder_.FoldBinary(Instr_Add, C1, C2, block);
			auto temp = GetTemporary(opA);

			if(opA->DefiningInstrIs<SextInstr>()) {
				irGen_.GetSext(constantOp, opA->GetType(), temp);
			}
			else irGen_.GetZext(constantOp, opA->GetType(), temp);

			return LOG(temp);
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleSub(Operand* opA, Operand* opB, Block* block) {
	// If both operands are constant we can fold the operation.
	if(auto result = folder_.FoldBinary(Instr_Sub, opA, opB, block)) {
        return result;
	}

	// Test for simple cases first, like 'a - 0 -> a'.
	if(auto result = HandleSubSimple(opA, opB, block)) {
		return result;
	}

    // Try to simplify using associativity/commutativity rules.
    if(auto result = HandleAssociative(Instr_Sub, opA, opB, block)) {
        return result;
    }

    // Try to simplify using distributivity rules.
    if(auto result = HandleDistributive(Instr_Sub, opA, opB, block)) {
        return result;
    }

	// Test for more complicated expressions.
	if(auto result = HandleSubComplex(opA, opB, block)) {
		return result;
	}

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_Sub, opA, opB)) {
        return result;
    }

    // Check if we are adding at least one 'quest' instruction result.
    return HandleOperationOnQuest(Instr_Sub, opA, opB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleSubSimple(Operand* opA, Operand* opB, Block* block,
                                   bool createAllowed) {
	// a - 0 -> a
	if(IsZeroInt(opB, block)) {
		return LOG(opA);
	}

	// a - undef -> undef
	// undef - a -> undef
	if(MatchUndef()(opB)) {
		return LOG(opB);
	}
	else if(MatchUndef()(opA)) {
		return LOG(opA);
	}

	// a - a -> 0
	if(AreEqual(opA, opB, block)) {
		return LOG(irGen_.GetIntConst(opA->GetType(), 0));
	}

	// -(-a) -> a (represented as '0 - (0 - a)').	
	Operand* a;
    Operand* b;
	IntConstant* C;

	if(IsZeroInt(opA, block) && 
       Match<SubInstr>(MatchInt(0, block), MatchAny(&a))(opB)) {
		return LOG(a);
	}

    // a - (a - b) -> b
	if(Match<SubInstr>(MatchAny(&a), MatchAny(&b))(opB) && 
       (a == opA)) {
		return LOG(b);
	}

    // (a + b) - b -> a
	if(Match<AddInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
       (b == opB)) {
		return LOG(a);
	}

	// (a * 2) - a  -> a
	// (a << 1) - a -> a
	if((Match<MulInstr>(MatchOp(opB), MatchInt(2, block))(opA)) ||
	   (Match<ShlInstr>(MatchOp(opB), MatchInt(1, block))(opA))) {
		return LOG(opB);
	}

    // NOTE: All the following simplification are done only if
    // we're allowed to create new instructions.
    if(createAllowed == false) {
        return nullptr;
    }

    // (-a) - (-b) -> b - a
	if((Match<SubInstr>(MatchInt(0, block), MatchAny(&a))(opA)) &&
	   (Match<SubInstr>(MatchInt(0, block), MatchAny(&b))(opB))) {
		auto temp = GetTemporary(opA);
		irGen_.GetSub(a, b, temp);
		return LOG(temp);
	}

	// a - (-b) -> a + b
	if(Match<SubInstr>(MatchInt(0, block), MatchAny(&b))(opB)) {
		auto temp = GetTemporary(opA);
		irGen_.GetAdd(opA, b, temp);
		return LOG(temp);
	}

    // a - C -> a + -C
    // By promoting an 'sub' to 'add' we expose more simplification opportunities.
	if(MatchIC(&C, block)(opB)) {
		auto addOp = GetTemporary(opA);
		auto constantOp = irGen_.GetIntConst(C->GetType(), -C->Value());
		irGen_.GetAdd(opA, constantOp, addOp);
		return LOG(addOp);
	}

	// (-a) - b -> -(a + b)
	if(Match<SubInstr>(MatchInt(0, block), MatchAny(&a))(opA)) {
		auto temp = GetTemporary(opA);
		irGen_.GetAdd(a, opB, temp);

		// Negate the value.
		return LOG(NegateInt(temp));
	}

	// -(a - b) -> b - a (represented as 0 - (a - b))
	if(IsZeroInt(opA, block)&& 
       Match<SubInstr>(MatchAny(&a), MatchAny(&b))(opB)) {
		auto temp = GetTemporary(opA);
		irGen_.GetSub(b, a, temp);
		return LOG(temp);
	}

	// a - (b - a) -> -b
	if(Match<SubInstr>(MatchAny(&b), MatchAny(&a))(opB) && 
       (a == opA)) {
		return LOG(NegateInt(b));
	}

	// (a - b) - a -> -b
	if(Match<SubInstr>(MatchAny(&a), MatchAny(&b))(opA) && 
       (a == opB)) {
		return LOG(NegateInt(b));
	}

    // (a * C) - a -> a * (C - 1), if 'C - 1' is a power of two.
	if(Match<MulInstr>(MatchOp(opB), MatchIC(&C, block))(opA) && 
       (C->Value() > 2)) {
		if(IA::IsPowerOfTwo(C->Value() - 1)) {
			auto constantOp = irGen_.GetIntConst(C->GetType(), C->Value() - 1);
			auto mulOp = GetTemporary(opB);
			irGen_.GetMul(opB, constantOp, mulOp);
			return LOG(mulOp);
		}
	}

    // a - (a & b) -> ~b & a, can expose further simplification opportunities.
    if(Match<AndInstr>(MatchAny(&a), MatchAny(&b))(opB) &&
       (a == opA)) {
        auto xorOp = GetTemporary(opA);
        irGen_.GetXor(b, GetMinusOneInt(opA), xorOp);
        auto andOp = GetTemporary(opA);
        irGen_.GetAnd(xorOp, a, andOp);
        return LOG(andOp);
    }

    // -1 - a -> ~a, can expose further simplification opportunities.
    // -a - 1 -> ~a
    if(MatchInt(-1, block)(opA) ||
       (Match<SubInstr>(MatchInt(0, block), MatchAny(&a))(opA) && 
        MatchInt(0, block)(opB))) {
        auto xorOp = GetTemporary(opA);
        irGen_.GetXor(opB, GetMinusOneInt(opA), xorOp);
        return LOG(xorOp);
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleSubComplex(Operand* opA, Operand* opB, Block* block) {
	Operand* a;
	Operand* b;
	IntConstant* C;
	IntConstant* C1;
	IntConstant* C2;

    // Try to reassociate the expression.
	if(auto result = HandleSubAssociative(opA, opB, block)) {
		return result;
	}
	
	// 0 - (a / C)  -> (a / -C), for signed division.
	if(MatchInt(0, block)(opA) && 
       Match<DivInstr>(MatchAny(&a), MatchIC(&C, block))(opB)) {
		auto constantOp = folder_.FoldBinary(Instr_Sub, GetZeroInt(opA), C, block);
		auto temp = GetTemporary(opA);
		irGen_.GetDiv(a, constantOp, temp);
		return LOG(temp);
	}

	// a - (a * C) -> a * (1 - C)
	if(Match<MulInstr>(MatchAny(&a), MatchIC(&C, block))(opB) &&
       (a == opA)) {
		auto constantOp = folder_.FoldBinary(Instr_Sub, GetOneInt(opA), C, block);
		auto temp = GetTemporary(opA);
		irGen_.GetMul(a, constantOp, temp);
		return LOG(temp);
	}

	// (a * b) - a -> a * (b - 1). This could simplify further if 'b' is a constant.
	if(Match<MulInstr>(MatchAny(&a), MatchAny(&b))(opA) && 
       (a == opB)) {
		auto subOp = GetTemporary(opA);
		irGen_.GetSub(b, GetOneInt(opA), subOp);

		auto mulOp = GetTemporary(opA);
		irGen_.GetMul(a, subOp, mulOp);
		return LOG(mulOp);
	}

	// (C1 - a) - C2 -> (C1 - C2) - a
	if(Match<SubInstr>(MatchIC(&C1, block), MatchAny(&a))(opA) && 
       MatchIC(&C2, block)(opB)) {
		auto constantOp = folder_.FoldBinary(Instr_Sub, C1, C2, block);
		auto subOp = GetTemporary(opA);
		irGen_.GetSub(constantOp, a, subOp);
		return LOG(subOp);
	}

	// (a - C1) - C2 -> a - (C1 + C2)
	if(Match<SubInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) && 
       MatchIC(&C2, block)(opB)) {
		auto constantOp = folder_.FoldBinary(Instr_Add, C1, C2, block);
		auto subOp = GetTemporary(opA);
		irGen_.GetSub(a, constantOp, subOp);
		return LOG(subOp);
	}

	// a - (a << C) -> a * (1 - (1 << C))
	if(Match<ShlInstr>(MatchAny(&a), MatchIC(&C, block))(opB) &&
       (a == opA)) {
		auto oneConst = GetOneInt(opA);
		auto constantOp = folder_.FoldBinary(Instr_Sub, oneConst,
										  folder_.FoldBinary(Instr_Shl, 
                                          oneConst, C, block), block);
		auto temp = GetTemporary(opA);
		irGen_.GetMul(a, constantOp, temp);
		return LOG(temp);
	}

	// a - (a * -b) -> a - -(a * b) -> a + (a * b)
	// a - (-a * b) -> a - -(a * b) -> a + (a * b)
	if(((Match<MulInstr>(MatchAny(&a), 
                         Match<SubInstr>(MatchInt(0, block), MatchAny(&b)))(opB) ||
	    (Match<MulInstr>(Match<SubInstr>(MatchInt(0, block), MatchAny(&a)), 
                         MatchAny(&b))(opB)))) &&
	   (a == opA)) {
		auto mulOp = GetTemporary(opA);
		irGen_.GetMul(a, b, mulOp);

		auto addOp = GetTemporary(opA);
		irGen_.GetAdd(a, mulOp, addOp);
        return LOG(addOp);
	}

    // Convert subtraction to addition because more
    // simplification rules are defined for addition.
	// a - (a * C) -> a + (a * -C)
	// a - (C * a) -> a + (a * -C)
	if((Match<MulInstr>(MatchAny(&a), MatchIC(&C, block))(opB) && 
       (a == opA)) ||
	   (Match<MulInstr>(MatchAny(&a), MatchIC(&C, block))(opB) && 
       (a == opA))) {
		auto constantOp = folder_.FoldBinary(Instr_Sub, GetOneInt(opA), 
                                             C, block);
		auto mulOp = GetTemporary(opA);
		irGen_.GetMul(a, constantOp, mulOp);

		auto addOp = GetTemporary(opA);
		irGen_.GetAdd(a, mulOp, addOp);
		return LOG(addOp);
	}

	// (a * C) - a -> a * (C - 1)
	if(Match<MulInstr>(MatchAny(&a), MatchIC(&C, block))(opA) && 
       (a == opB)) {
		auto constantOp = folder_.FoldBinary(Instr_Sub, C, 
                                             GetOneInt(opA), block);
		auto temp = GetTemporary(opA);
		irGen_.GetMul(a, constantOp, temp);
		return LOG(temp);
	}

	// (a * C1) - (a * C2) -> a * (C1 - C2)
	if(Match<MulInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) &&
	   Match<MulInstr>(MatchAny(&b), MatchIC(&C2, block))(opB) && 
       (a == b)) {
		auto constantOp = folder_.FoldBinary(Instr_Sub, C1, C2, block);
		auto temp = GetTemporary(opA);
		irGen_.GetMul(a, constantOp, temp);
		return LOG(temp);
	}

    // a - ((a / C) * C) -> a % C
    if(Match<MulInstr>(Match<DivInstr>(MatchAny(&a), MatchIC(&C1, block)), 
                       MatchIC(&C2, block))(opB) && (a == opA) && 
       (C1 == C2)) {
        auto modOp = GetTemporary(opA);
        irGen_.GetMod(a, C1, modOp);
        return LOG(modOp);
    }
    else if(Match<MulInstr>(Match<UdivInstr>(MatchAny(&a), MatchIC(&C1, block)), 
                            MatchIC(&C2, block))(opB) && (a == opA) && 
            (C1 == C2)) {
        auto umodOp = GetTemporary(opA);
        irGen_.GetUmod(a, C1, umodOp);
        return LOG(umodOp);
    }

    // sub 1, (cmp eq a, b) -> cmp neq a, b
    // sub 1, (ucmp eq a, b) -> ucmp neq a, b
    if(opA->IsOneInt() && 
       (opB->DefiningInstrIs<CmpInstr>() ||
        opB->DefiningInstrIs<UcmpInstr>())) {
        auto cmpInstr = opB->DefiningInstrAs<CmpInstrBase>();

        return CreateCompare(cmpInstr->LeftOp(), cmpInstr->RightOp(),
                             CmpInstrBase::InvertedOrder(cmpInstr->Order()),
                             cmpInstr->ResultOp()->GetType(), cmpInstr->IsUcmp());
    }

	// trunc(a) - trunc(b) -> trunc(a - b)
	// This transformation is safe because no overflow (that wasn't 
    // in the original code) can occur after we eliminate the truncations.
	if(MatchConversion<TruncInstr>(MatchAny(&a))(opA) &&
	   MatchConversion<TruncInstr>(MatchAny(&b))(opB)) {
		auto subOp = GetTemporary(a);
		irGen_.GetSub(a, b, subOp);

		auto truncOp = GetTemporary(opA);
		irGen_.GetTrunc(subOp, opA->GetType(), truncOp);
		return LOG(truncOp);
	}

	// C1 - (a + C2) -> (C1 - C2) - a
	if(MatchIC(&C1)(opA) && Match<AddInstr>(MatchAny(&a), 
                                            MatchIC(&C2, block))(opB)) {
		auto constantOp = folder_.FoldBinary(Instr_Sub, C1, C2, block);
		auto temp = GetTemporary(opA);
		irGen_.GetSub(constantOp, a, temp);
		return LOG(temp);
	}

	// (a + C1) - C2 -> a + (C1 - C2)
	if(Match<AddInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) && 
       MatchIC(&C2, block)(opB)) {
		auto constantOp = folder_.FoldBinary(Instr_Sub, C1, C2, block);
		auto temp = GetTemporary(opA);
		irGen_.GetAdd(a, constantOp, temp);
		return LOG(temp);
	}

	// C1 - sext(C2(bool)) -> 'C1 - 1', if 'C2 = 1', else 'C1'
	// C1 - zext(C2(bool)) -> 'C1 - 1', if 'C2 = 1', else 'C1'
	if((MatchIC(&C1, block)(opA) && 
        MatchConversion<SextInstr>(MatchIC(&C2, block))(opB)) ||
	   (MatchIC(&C1, block)(opA) && 
        MatchConversion<SextInstr>(MatchIC(&C2, block))(opB))) {
		if(C2->Value() == 1) {
			return LOG(folder_.FoldBinary(Instr_Sub, C1, GetOneInt(opB), block));
		}
		else return LOG(C1);
	}

	// sext(C1(bool)) - C2 -> '-C2', if 'C1 = 1', else '1 - C2'
	// zext(C1(bool)) - C2 -> '-C2', if 'C1 = 1', else '1 - C2'
	if(((MatchConversion<SextInstr>(MatchIC(&C1, block))(opA) && 
         MatchIC(&C2, block)(opB)) ||
		(MatchConversion<ZextInstr>(MatchIC(&C1, block))(opA) && 
         MatchIC(&C2, block)(opB))) && C1->IsBoolean()) {
		if(C1->Value() == 1) {
			return LOG(folder_.FoldBinary(Instr_Sub, GetZeroInt(opB), C2, block));
		}
		else return LOG(folder_.FoldBinary(Instr_Sub, GetOneInt(opB), C2, block));
	}

	// C - ~a -> C - (-1 - a) -> a + (C + 1)
	// The complement is written as: a ^ -1
	if(MatchIC(&C)(opA) && 
       Match<XorInstr>(MatchAny(&a), MatchInt(-1, block))(opB)) {
		auto constantOp = folder_.FoldBinary(Instr_Add, C, 
                                             GetOneInt(opA), block);
		auto temp = GetTemporary(opA);
		irGen_.GetAdd(a, constantOp, temp);
		return LOG(temp);
	}

    // (a & ~C) - (a & C) -> (a ^ C) - C, if 'C' is any power of 2 minus 1.
    if(Match<AndInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) &&
       Match<AndInstr>(MatchOp(a), MatchIC(&C2, block))(opB)) {
        // We have 2^N - 1 if the number of bits set is equal to the number
        // of rightmost bits set. 7: 0000...111, 3 bits on the right, 3 bits total
        if(IA::OneBits(C2) == (IA::TypeBits(C2->GetType() - 
                               IA::LeftmostZeroBits(C1)))) {
            if(C2->Value() == IA::Not(C1)) {
                auto xorOp = GetTemporary(opA);
                irGen_.GetXor(a, C2, xorOp);

                auto subOp = GetTemporary(opA);
                irGen_.GetSub(xorOp, C2, subOp);
                return LOG(subOp);
            }
        }
    }

	// -(a >> C) -> a (u)>> C, if C = sizeof(IntX) - 1
	// This means that all bits, except the leftmost one, become the sign bit,
	// which is then negated, so we replace the arithmetic shift with a logical one.
	if(IsZeroInt(opA, block) &&
       Match<ShrInstr>(MatchAny(&a), MatchIC(&C, block))(opB)) {
		int bits = opA->GetType()->As<IntegerType>()->SizeInBits() - 1;

		if(C->Value() == bits) {
			// The transformation can be applied.
			auto temp = GetTemporary(opA);
			irGen_.GetUshr(a, C, temp);
			return LOG(temp);
		}
	}

	// -(a (u)>> C) -> a >> C, if 'C = sizeof(IntX) - 1'
	// This means that only the sign bit remains, which is then negated,
	// so we replace the logical shift with an arithmetic one.
	if(IsZeroInt(opA, block) && 
       Match<UshrInstr>(MatchAny(&a), MatchIC(&C, block))(opB)) {
		int bits = opA->GetType()->As<IntegerType>()->SizeInBits() - 1;

		if(C->Value() == bits) {
			// The transformation can be applied.
			auto temp = GetTemporary(opA);
			irGen_.GetShr(a, C, temp);
			return LOG(temp);
		}
	}

    // Test for the difference of pointers converted to integers.
    if(auto result = HandleSubPointers(opA, opB)) {
        return result;
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleSubPointers(Operand* opA, Operand* opB) {
    // Try to simplify the difference of two addresses converted to integers.
    // ptoi(ptr1) - ptoi(ptr2)
    auto ptoiInstrA = opA->DefiningInstrAs<PtoiInstr>();
    auto ptoiInstrB = opB->DefiningInstrAs<PtoiInstr>();
    if((ptoiInstrA && ptoiInstrB) == false) {
        return nullptr;
    }

    auto targetOpA = ptoiInstrA->TargetOp();
    auto targetOpB = ptoiInstrB->TargetOp();

    if(auto addrInstrA = targetOpA->DefiningInstrAs<AddressInstr>()) {
        // ptoi(addr ptr1, C1) - ptoi(addr ptr2, C2) -> 
        //      (ptoi(ptr1) - ptoi(ptr2)) + (C1 * size(*ptr1) - C2 * size(*ptr2))
        if(auto addrInstrB = targetOpB->DefiningInstrAs<AddressInstr>()) {
            return LOG(HandleSubPointersAddrIndex(addrInstrA, addrInstrB,
                                                  ptoiInstrA->CastType()));
        }
        // ptoi(addr ptr, a) - ptoi(ptr) ->  a * size(*ptr)
        else if(addrInstrA->BaseOp() == targetOpB) {
            return LOG(CreateMultipliedOffset(addrInstrA->GetPointeeType(),
                                             addrInstrA->IndexOp(), 
                                             ptoiInstrA->CastType()));
        }
    }
    else if(auto indexInstrA = targetOpA->DefiningInstrAs<IndexInstr>()) {
        // ptoi(index ptr1, C1) - ptoi(index ptr2, C2) -> 
        //      (ptoi(ptr1) - ptoi(ptr2)) + (C1 * size(element1) - C2 * size(element2))
        if(auto indexInstrB = targetOpB->DefiningInstrAs<IndexInstr>()) {
            return LOG(HandleSubPointersAddrIndex(indexInstrA, indexInstrB,
                                                  ptoiInstrA->CastType()));
        }
        // ptoi(index ptr, a) - ptoi(ptr) ->  a * size(element)
        else if(indexInstrA->BaseOp() == targetOpB) {
            return LOG(CreateMultipliedOffset(indexInstrA->GetElementType(),
                                              indexInstrA->IndexOp(), 
                                              ptoiInstrA->CastType()));
        }
    }
    else if(auto elemInstrA = targetOpA->DefiningInstrAs<ElementInstr>()) {
        // ptoi(elem ptr1, C1) - ptoi(elem ptr2, C2) -> 
        //      (ptoi(ptr1) - ptoi(ptr2)) + (offset(C1) - offset(C2))
        if(auto elemInstrB = targetOpB->DefiningInstrAs<ElementInstr>()) {
            auto C1 = elemInstrA->IndexOp()->As<IntConstant>();
            auto C2 = elemInstrB->IndexOp()->As<IntConstant>();
            DebugValidator::IsNotNull(C1);
            DebugValidator::IsNotNull(C2);

            auto castType = ptoiInstrA->CastType();
            bool sameBase = elemInstrA->BaseOp() == elemInstrB->BaseOp();
            Operand* baseDiffOp = nullptr;

            // If we have the same base we don't need to emit the base difference.
            if(sameBase == false) {
                baseDiffOp = CreateBaseDifference(elemInstrA->BaseOp(), 
                                                  elemInstrB->BaseOp(), 
                                                  castType);
            }

            // Compute the constant 'C1 * size(*ptr1) - C2 * size(*ptr2)'.
            auto recordTypeA = elemInstrA->GetRecordType();
            __int64 offsetA = recordTypeA->GetFieldOffset(C1->Value());

            auto recordTypeB = elemInstrB->GetRecordType();
            __int64 offsetB = recordTypeB->GetFieldOffset(C2->Value());
            auto constantOp = irGen_.GetIntConst(castType, offsetA - offsetB);

            if(sameBase) {
                return LOG(constantOp);
            }
            else {
                // Add the constant to the base pointer difference.
                auto addOp = irGen_.GetTemporary(castType);
                irGen_.GetAdd(baseDiffOp, constantOp, addOp);
                return LOG(addOp);
            }
        }
        // ptoi(elem ptr, C) - ptoi(ptr) ->  offset(C)
        else if(elemInstrA->BaseOp() == ptoiInstrB->TargetOp()) {
            return LOG(CreateFieldOffset(elemInstrA, ptoiInstrA->CastType()));
        }
    }
    else if(auto addrInstrB = targetOpB->DefiningInstrAs<AddressInstr>()) {
        // ptoi(ptr) - ptoi(addr ptr, a) -> -a * size(*ptr)
        if(addrInstrB->BaseOp() == targetOpA) {
            return LOG(CreateMultipliedOffset(addrInstrB->GetPointeeType(),
                                              addrInstrB->IndexOp(), 
                                              ptoiInstrB->CastType(),
                                              true /* negatedOffsetOp */));
        }
    }
    else if(auto indexInstrB = targetOpB->DefiningInstrAs<IndexInstr>()) {
        // ptoi(ptr) - ptoi(index ptr, a) -> -a * size(element)
        if(indexInstrB->BaseOp() == targetOpA) {
            return LOG(CreateMultipliedOffset(indexInstrB->GetElementType(),
                                              indexInstrB->IndexOp(), 
                                              ptoiInstrB->CastType(),
                                              true /* negatedOffsetOp */));
        }
    }
    else if(auto elemInstrB = targetOpB->DefiningInstrAs<ElementInstr>()) {
        // ptoi(ptr) - ptoi(elem ptr, C) -> -offset(C)
        if(elemInstrB->BaseOp() == targetOpA) {
            return LOG(CreateFieldOffset(elemInstrA, ptoiInstrA->CastType(), 
                                         true /* negateOffset */));
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleSubPointersAddrIndex(AddressInstr* addrInstrA, 
                                              AddressInstr* addrInstrB, 
                                              const Type* castType) {
    // It doesn't pay off to perform this simplification if the index
    // operands are not constants (because 'ptoi' is a no-op anyway).
    auto C1 = AsIntConstant(addrInstrA->IndexOp(),
                            addrInstrA->ParentBlock());
    auto C2 = AsIntConstant(addrInstrB->IndexOp(), 
                            addrInstrB->ParentBlock());
    if((C1 && C2) == false) {
        return nullptr;
    }

    bool sameBase = addrInstrA->BaseOp() == addrInstrB->BaseOp();
    Operand* baseDiffOp = nullptr;

    // If we have the same base we don't need to emit the base difference.
    if(sameBase == false) {
        baseDiffOp = CreateBaseDifference(addrInstrA->BaseOp(), 
                                          addrInstrB->BaseOp(), castType);
    }

    // Compute the constant 'C1 * size(*ptr1) - C2 * size(*ptr2)'.
    __int64 sizeA = TI::GetSize(addrInstrA->GetPointeeType(), GetTarget());
    __int64 sizeB = TI::GetSize(addrInstrB->GetPointeeType(), GetTarget());

    __int64 constValue = (C1->Value() * sizeA) - (C2->Value() * sizeB);
    auto constantOp = irGen_.GetIntConst(castType, constValue);

    if(sameBase)  {
        return LOG(constantOp);
    }
    else {
        // Add the constant to the base pointer difference.
        auto addOp = irGen_.GetTemporary(castType);
        irGen_.GetAdd(baseDiffOp, constantOp, addOp);
        return LOG(addOp);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateBaseDifference(Operand* baseOpA, Operand* baseOpB,
                                        const Type* castType) {
    auto ptoiOpA = irGen_.GetTemporary(castType);
    irGen_.GetPtoi(baseOpA, castType, ptoiOpA);

    auto ptoiOpB = irGen_.GetTemporary(castType);
    irGen_.GetPtoi(baseOpB, castType, ptoiOpB);

    auto baseDiffOp = irGen_.GetTemporary(castType);
    irGen_.GetSub(ptoiOpA, ptoiOpB, baseDiffOp);
    return baseDiffOp;
}                                    

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateMultipliedOffset(const Type* offsetType, Operand* offsetOp, 
                                          const Type* resultType, bool negatedOffsetOp) {
    // Create 'offsetOp * size(offsetType)', or 
    // '(-offsetOp) * size(offsetType)' if 'negatedOffsetOp' is set.
    auto sizeOp = irGen_.GetIntConst(resultType, TI::GetSize(offsetType, GetTarget()));
    auto mulOp = irGen_.GetTemporary(resultType);
    irGen_.GetMul(offsetOp, sizeOp, mulOp);
    return offsetOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateFieldOffset(ElementInstr* elemInstr, const Type* resultType,
                                     bool negateOffset) {
    auto C = elemInstr->IndexOp()->As<IntConstant>();
    DebugValidator::IsNotNull(C);

    auto recordType = elemInstr->GetRecordType();
    __int64 offset = recordType->GetFieldOffset(C->Value());
    return irGen_.GetIntConst(resultType, negateOffset ? -offset : offset);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFadd(Operand* opA, Operand* opB, FloatMode mode) {
	// We need to take care when simplifying operations
    // involving floating operands, // because the semantics could 
    // change in ways that are not possible for integers.
	Operand* a;
	Operand* b;

	// If both operands are constant we can fold the operation.
	if(opA->IsFloatingConstant() && opB->IsFloatingConstant()) {
		return LOG(folder_.FoldBinary(Instr_Fadd, opA, opB, nullptr));
	}

    // Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_Fadd, opA, opB)) {
        return result;
    }

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_Fadd, opA, opB)) {
        return result;
    }

	// ALL the following simplifications are valid only if 
	// the 'FP_Exact' mode is NOT activated for the instruction.
	if(mode == FP_Exact) {
        return nullptr;
    }

	// a + 0.0 -> a, if 0.0 is "positive zero".
	if(auto floatConst = opB->As<FloatConstant>()) {
        auto floatKind = floatConst->GetType()->GetSubtype();

		if(FA::AreEqual(floatConst->Value(), 0.0, floatKind) &&
		   FA::IsPositiveZero(floatConst)) {
			// It's safe to perform the simplification.
			return LOG(opA);
		}
	}

	// -a + b -> b - a
	if(Match<FsubInstr>(MatchFloat(0.0), MatchAny(&a))(opA)) {
		auto temp = GetTemporary(opA);
		irGen_.GetFsub(opB, a, temp);
		return LOG(temp);
	}

	// ALL the following simplifications are valid only if
	// the 'FP_Fast' mode is activated for the instruction.
	if(mode != FP_Fast) {
        return nullptr;
    }

	// -a + -b -> -(a + b)
	if(Match<FsubInstr>(MatchFloat(0.0), MatchAny(&a))(opA) &&
	   Match<FsubInstr>(MatchFloat(0.0), MatchAny(&b))(opB)) {
		auto addOp = GetTemporary(opA);
		irGen_.GetFadd(a, b, addOp);

		auto subOp = GetTemporary(opA);
		irGen_.GetFsub(GetZeroFloat(opA), addOp, subOp);
		return LOG(subOp);
	}

	// Try to simplify using associativity/commutativity rules.
    if(auto result = HandleAssociative(Instr_Fadd, opA, opB, nullptr)) {
        return result;
    }

    // Try to simplify using distributivity rules.
    if(auto result = HandleDistributive(Instr_Fadd, opA, opB, nullptr)) {
        return result;
    }
	   
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFsub(Operand* opA, Operand* opB, FloatMode mode) {
	// We need to take care when simplifying operations 
    // involving floating operands, because the semantics could 
    // change in ways that are not possible for integers.
	Operand* a;
	Operand* b;

	// If both operands are constant we can fold the operation.
	if(opA->IsFloatingConstant() && opB->IsFloatingConstant()) {
		return LOG(folder_.FoldBinary(Instr_Fsub, opA, opB, nullptr));
	}

    // Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_Fsub, opA, opB)) {
        return result;
    }

	// ALL the following simplifications are valid only if 
	// the 'FP_Exact' mode is NOT activated for the instruction.
	if(mode == FP_Exact) {
        return nullptr;	
    }

	// a - 0 -> a
	if(MatchFloat(0.0)(opB)) {
		return LOG(opA);
	}

	// ALL the following simplifications are valid only if
	// the 'FP_Fast' mode is activated for the instruction.
	if(mode != FP_Fast) {
        return nullptr;
    }

	// a - a -> 0
	if(opA == opB) {
		return LOG(GetZeroFloat(opA));
	}

	// -(-a) -> a
	if(MatchFloat(0.0)(opA) && 
       Match<FsubInstr>(MatchFloat(0.0), MatchAny(&a))(opB)) {
		return LOG(a);
	}

	// a - (-b) -> a + b
	if(Match<FsubInstr>(MatchFloat(0.0), MatchAny(&b))(opB)) {
		auto temp = GetTemporary(opA);
		irGen_.GetFadd(opA, b, temp);
		return LOG(temp);
	}

	// Try to simplify using associativity/commutativity rules.
    if(auto result = HandleAssociative(Instr_Fsub, opA, opB, nullptr)) {
        return result;
    }

    // Try to simplify using distributivity rules.
    if(auto result = HandleDistributive(Instr_Fsub, opA, opB, nullptr)) {
        return result;
    }
	
	return nullptr;
}

} // namespace Optimization