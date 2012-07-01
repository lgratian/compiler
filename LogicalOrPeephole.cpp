// LogicalOrPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 'or'.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

Operand* Peephole::HandleOr(Operand* opA, Operand* opB, Block* block, 
                            bool constMoveAllowed) {
	Operand* a;
	Operand* b;
	IntConstant* C1;
	IntConstant* C2;
	bool match = false;

    // Try to constant-fold.
	if(auto result = folder_.FoldBinary(Instr_Or, opA, opB, block)) {
		return LOG(result);
	}

	// If one of the operands is a constant make it the the right operand.
	// This is also an improvement for CPUs that don't support 
    // a constant on the left.
	bool moved = MoveConstantToRight(opA, opB);

	// Try to simplify using information about the bits 
	// that are definitely zero or one.
	if(auto result = HandleOrOperandInfo(opA, opB)) {
		return result;
	}

	// Try to simplify using associativity/commutativity rules.
	if(auto result = HandleAssociative(Instr_Or, opA, opB, block)) {
		return result;
	}

	// Try to simplify using distributivity rules.
	if(auto result = HandleDistributive(Instr_Or, opA, opB, block)) {
		return result;
	}

	if(auto result = HandleOrSimple(opA, opB, block)) {
		return result;
	}

	// (a & C1) | C2 -> (a | C2) & (C1 | C2), if 'C1 & C2 == 0'.
	if(Match<AndInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) && 
       MatchIC(&C2, block)(opB)) {
		if(IA::And(C1, C2) == 0) {
			auto constantOp = folder_.FoldBinary(Instr_Or, C1, C2, block);
			auto orOp = GetTemporary(opA);
			irGen_.GetOr(a, C2, orOp);
			
            auto andOp = GetTemporary(opA);
			irGen_.GetAnd(orOp, constantOp, andOp);
			return LOG(andOp);
		}
	}
	
	// (a ^ C1) | C2 -> (a | C2) ^ (C1 & ~C2)
	if(Match<XorInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) &&
       MatchIC(&C2, block)(opB)) {
		auto intKind = C1->GetType()->GetSubtype();
		__int64 result = IA::Xor(C2->Value(), -1, intKind);
		result = IA::And(C1->Value(), result, intKind);

		auto constantOp = irGen_.GetIntConst(opA->GetType(), result);
		auto orOp = GetTemporary(opA);
		irGen_.GetOr(a, C2, orOp);
		
        auto xorOp = GetTemporary(opA);
		irGen_.GetXor(orOp, constantOp, xorOp);
		return LOG(xorOp);
	}

	// ((a | b) == 0) | ((a | d) == 0) -> (a | (b | d)) == 0
	// This reduces the number of operations, and 'b' and 'd' are often
	// constants, leading to further simplifications.
	if(opA->DefiningInstruction() && 
       opA->DefiningInstruction()->IsComparison() &&
	   opB->DefiningInstruction() && 
       opB->DefiningInstruction()->IsComparison()) {
		if(auto result = HandleOrCmpCmp(opA, opB, block)) {
			return result;
		}
	}

	// zext(a) | zext(b) -> zext(a | b)
	// sext(a) | sext(b) -> sext(a | b)
	if((MatchConversion<SextInstr>(MatchAny(&a))(opA) && 
		MatchConversion<SextInstr>(MatchAny(&b))(opB)) 
        ||
	   (MatchConversion<ZextInstr>(MatchAny(&a))(opA) && 
		MatchConversion<ZextInstr>(MatchAny(&b))(opB))) {
		// Do the transformation only if the source type
        // is the same for both operands.
		if(a->GetType() == b->GetType()) {
			auto orOp = GetTemporary(a);
			irGen_.GetOr(a, b, orOp);
			auto extOp = GetTemporary(opA);

			if(opA->DefiningInstrIs<SextInstr>()) {
				irGen_.GetSext(orOp, opA->GetType(), extOp);
			}
			else irGen_.GetZext(orOp, opA->GetType(), extOp);

			return LOG(extOp);
		}
	}

	// trunc(a | C1) | C2 -> ((trunc a) | trunc(C1)) | C2
	if(MatchConversion<TruncInstr>(Match<OrInstr>(MatchAny(&a), 
                                            MatchIC(&C1, block)))(opA) &&
	   MatchIC(&C2)(opB)) {
		auto castType = opA->GetType()->As<IntegerType>();
		auto constantOp = irGen_.GetIntConst(castType, IA::Trunc(C1, castType));
		
        auto truncOp = irGen_.GetTemporary(castType);
		irGen_.GetTrunc(a, castType, truncOp);
		
		auto orOp1 = irGen_.GetTemporary(castType);
		irGen_.GetOr(truncOp, constantOp, orOp1);
		
        auto orOp2 = irGen_.GetTemporary(castType);
		irGen_.GetOr(orOp1, C2, orOp2);
		return LOG(orOp2);
	}

	// Try to simplify 'or's that involve integer compare instructions. 
	if(auto result = HandleOrIntCmp(opA, opB, block)) {
		return result;
	}

	// ((a | b) & C1) | (b & C2) -> (a & C1) | B, 
    // but only when 'C1 ^ C2 == -1'. This means that all bits 
    // from 'b' will be used, regardless of the constants.
	if(Match<AndInstr>(Match<OrInstr>(MatchAny(&a), MatchAny(&b)), 
                       MatchIC(&C1, block))(opA) &&
	   Match<AndInstr>(MatchOp(b), MatchIC(&C2, block))(opB)) {
		if(IA::Xor(C1, C2) == -1) {
			// The transformation can be done.
			auto andOp = GetTemporary(opA);
			irGen_.GetAnd(a, C1, andOp);
			
            auto orOp = GetTemporary(opA);
			irGen_.GetOr(andOp, b, orOp);
			return LOG(orOp);
		}
	}

	// Check if we are adding at least one 'quest' instruction result.
	if(auto result = HandleOperationOnQuest(Instr_Or, opA, opB)) {
		return result;
	}

	// Check if we can simplify by applying the operation
	// one each incoming value of a 'phi' instruction.
	if(auto result = HandleInstructionOnPhi(Instr_Or, opA, opB)) {
		return result;
	}

	// Returns a new instruction if the constant was moved to the right.
	if(moved && constMoveAllowed) {
		auto result = GetTemporary(opA);
		irGen_.GetOr(opA, opB, result);
		return LOG(result);
	}
	
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleOrOperandInfo(Operand* opA, Operand* opB) {
	// Try to simplify using information about the bits 
	// that are definitely zero and one.
	unsigned __int64 opAZeroBits;
	unsigned __int64 opBZeroBits;
	unsigned __int64 opAOneBits;
	unsigned __int64 opBOneBits;
	
	// Test if one of the operands is definitely zero.
	__int64 mask = IA::GetMinusOneMask(opA->GetType()->As<IntegerType>());
	opInfo_.EstimateZeroBits(opA, opAZeroBits);
	
	if(opAZeroBits == mask) {
		// 0 | b -> b
		return LOG(opB);
	}
	
	opInfo_.EstimateZeroBits(opB, opBZeroBits);

	if(opBZeroBits == mask) {
		// a | 0 -> a
		return LOG(opA);
	}

	// Check if 'opA' contributes with any bits to 'opB'.
	// It doesn't contribute if all one bits in 'opA' are also found in 'opB',
	// and the bits that are not one in 'opA' must be definitely be zero.
	opInfo_.EstimateOneBits(opA, opAOneBits);
	opInfo_.EstimateOneBits(opB, opBOneBits);

	if(((opAOneBits & opBOneBits) == opAOneBits) &&
	   ((opAZeroBits & ~opAOneBits) == ~opAOneBits)) {
		return LOG(opB);
	}

	// Now do the same for 'opB'.
	if(((opBOneBits & opAOneBits) == opBOneBits) &&
	   ((opBZeroBits & ~opBOneBits) == ~opBOneBits)) {
		return LOG(opA);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleOrSimple(Operand* opA, Operand* opB, 
                                  Block* block, bool createAllowed)  {
	Operand* a;
	Operand* b;
    Operand* x;
	IntConstant* C;
    bool match = false;

	// If one of the operands is a constant make it the the right operand,
	// so that we need to test much fewer cases.
	MoveConstantToRight(opA, opB);

	// a | 0 -> a
	if(IsZeroInt(opB, block)) {
		return LOG(opA);
	}

	// a | -1 -> -1
	if(IsMinusOneInt(opB, block)) {
		return LOG(GetMinusOneInt(opA));
	}

	// a | ~a -> 1
	// ~a | a -> 1
	if((Match<XorInstr>(MatchAny(&a), MatchInt(-1, block))(opB) && 
       (a == opA))
       ||
	   (Match<XorInstr>(MatchAny(&a), MatchInt(-1, block))(opB) && 
       (a == opA))) {
		return LOG(GetOneInt(opA));
	}

	// a | a -> a
	if(AreEqual(opA, opB, block)) {
		return LOG(opA);
	}

	// (a & b) | a -> a
	// (b & a) | a -> a
	if(Match<AndInstr>(MatchAny(&a), MatchAny(&b))(opA) && 
	   ((a == opB) || (b == opB))) {
		return LOG(opB);
	}

	// a | (a & b) -> a
	// a | (b & a) -> a
	if(Match<AndInstr>(MatchAny(&a), MatchAny(&b))(opB) &&
	   ((a == opA) || (b == opA))) {
		return LOG(opA);
	}

	// a | 1 -> 1, if 'a' has boolean type
	// a | 0 -> a, if 'a' has boolean type
	if(IsZeroInt(opB, block) && opA->IsBoolean()) {
		return LOG(opA);
	}
	else if(IsOneInt(opB, block) && opA->IsBoolean()) {
		return LOG(GetBool(true, opA));
	}

	// ~(a & b) | a -> -1
	// ~(b & a) | a -> -1
	if(Match<XorInstr>(Match<AndInstr>(MatchAny(&a), MatchAny(&b)),
                       MatchInt(-1, block))(opA) &&
	   ((a == opB) || (b == opB))) {
		return LOG(GetMinusOneInt(opA));
	}

	// a | ~(a & b) -> -1
	// a | ~(b & a) -> -1
	if(Match<XorInstr>(Match<AndInstr>(MatchAny(&a), MatchAny(&b)), 
                       MatchInt(-1, block))(opB) &&
	   ((a == opA) || (b == opA))) {
		return LOG(GetMinusOneInt(opA));
	}

    // NOTE: All the following simplification are done only if
    // we're allowed to create new instructions.
    if(createAllowed == false) {
        return nullptr;
    }

    // ((a & ~b) | (~a & b)) -> a ^ b
	if(Match<AndInstr>(MatchAny(&a), 
                       Match<XorInstr>(MatchAny(&b), MatchInt(-1, block)))(opA) 
       &&
	   Match<AndInstr>(Match<XorInstr>(MatchOp(a), MatchInt(-1, block)), 
                       MatchOp(b))(opB)) {
		match = true;
	}
	// ((~b & a) | (~a & b)) -> a ^ b
	else if(Match<AndInstr>(Match<XorInstr>(MatchAny(&b), MatchInt(-1, block)), 
                            MatchAny(&a))(opA) 
            &&
			Match<AndInstr>(Match<XorInstr>(MatchOp(a), MatchInt(-1, block)), 
                            MatchOp(b))(opB)) {
		match = true;
	}
	// ((a & ~b) | (b & ~a)) -> a ^ b
	else if(Match<AndInstr>(MatchAny(&a), 
                            Match<XorInstr>(MatchAny(&b), MatchInt(-1, block)))(opA) 
            &&
			Match<AndInstr>(MatchOp(b), 
                            Match<XorInstr>(MatchOp(a), MatchInt(-1, block)))(opB)) {
		match = true;
	}
	// ((~b & a) | (b & ~a)) -> a ^ b
	else if(Match<AndInstr>(Match<XorInstr>(MatchAny(&b), MatchInt(-1, block)), 
                            MatchAny(&a))(opA) 
            &&
			Match<AndInstr>(MatchOp(b), 
                            Match<XorInstr>(MatchOp(a), MatchInt(-1, block)))(opB)) {
		match = true;
	}

	if(match) {
		auto xorOp = GetTemporary(opA);
		irGen_.GetXor(a, b, xorOp);
		return LOG(xorOp);
	}

    // ~a | ~b -> ~(a & b) (De Morgan)
	if(IsLogicalNot(opA, &a) && IsLogicalNot(opB, &b)) {
		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(a, b, andOp);
		
        auto xorOp = GetTemporary(opA);
		irGen_.GetXor(andOp, GetMinusOneInt(opA), xorOp);
		return LOG(xorOp);
	}
	
	// (a & b) | (a ^ b) -> a | b
	if(Match<AndInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
	   Match<XorInstr>(MatchOp(a), MatchOp(b))(opB)) {
		auto orOp = GetTemporary(opA);
		irGen_.GetOr(a, b, orOp);
		return LOG(orOp);
	}

    // To make the following tests simpler we move 'xor' to the right.
	// a | ( a ^ b) -> a |  b
	// a | (~a ^ b) -> a | ~b
	if(Match<AndInstr>(MatchAny(&a), 
                       Match<XorInstr>(MatchAny(&b), MatchInt(-1, block)))(opA) 
       &&
	   Match<AndInstr>(Match<XorInstr>(MatchOp(a), MatchInt(-1, block)), 
                       MatchOp(b))(opB)) {
		auto xorOp = GetTemporary(opA);
		irGen_.GetXor(a, b, xorOp);
		return LOG(xorOp);
	}

	// a | ~(a | b) -> a | ~b
	// a | ~(a ^ b) -> a | ~b
	if((Match<XorInstr>(Match<OrInstr>(MatchAny(&a), MatchAny(&b)), 
                        MatchInt(-1, block))(opA) 
        ||
		Match<XorInstr>(Match<XorInstr>(MatchAny(&a), MatchAny(&b)), 
                        MatchInt(-1, block))(opA)) &&
	   (a == opA)) {
		auto xorOp = GetTemporary(opA);
		irGen_.GetXor(b, GetMinusOneInt(opA), xorOp);
		
        auto orOp = GetTemporary(opA);
		irGen_.GetOr(a, xorOp, orOp);
		return LOG(orOp);
	}

	// (a >> x) | (b >> x) -> (a | b) >> x,  for both 'shr' and 'ushr'.
	if((Match<ShrInstr>(MatchAny(&a), MatchAny(&x))(opA) &&
		Match<ShrInstr>(MatchAny(&b), MatchOp(x))(opB)) 
        ||
	   (Match<UshrInstr>(MatchAny(&a), MatchAny(&x))(opA) &&
		Match<UshrInstr>(MatchAny(&b), MatchOp(x))(opB))) {
        // We have a match.
		auto orOp = GetTemporary(opA);
		irGen_.GetOr(a, b, orOp);
		auto temp = GetTemporary(opA);

		if(opA->DefiningInstrIs<ShrInstr>()) {
			irGen_.GetShr(orOp, x, temp);
		}
		else irGen_.GetUshr(orOp, x, temp);

		return LOG(temp);
	}

    // Try to move out a constant from inside, improves loop invariant motion.
	// (a | C) | b -> (a | b) | C
	// b | (a | C) -> (a | b) | C
	if((Match<OrInstr>(MatchAny(&a), MatchIC(&C, block))(opA) && 
        MatchAny(&b)(opB) && (opB->IsConstant() == false)) 
        ||
	   (Match<OrInstr>(MatchAny(&a), MatchIC(&C, block))(opB) &&
        MatchAny(&b)(opA)) && (opA->IsConstant() == false)) {
		auto orOp1 = GetTemporary(opA);
		irGen_.GetOr(a, b, orOp1);
		
        auto orOp2 = GetTemporary(opA);
		irGen_.GetOr(orOp1, C, orOp2);
		return LOG(orOp2);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleOrCmpCmp(Operand* opA, Operand* opB, Block* block) {
	// ((a | b) == 0) | ((a | d) == 0) -> (a | (b | d)) == 0
	// This reduces the number of operations, and 'b' and 'd' are often
	// constants, leading to further simplifications.
	auto cmpInstrA = static_cast<CmpInstrBase*>(opA->DefiningInstruction());
	auto cmpInstrB = static_cast<CmpInstrBase*>(opB->DefiningInstruction());

	// We can't handle floating comparisons.
	if(cmpInstrA->IsFcmp() || cmpInstrB->IsFcmp()) {
        return nullptr;
    }

	// Only an 'equal' comparison to 0 can be optimized here.
	if((cmpInstrA->IsEqual() &&
        cmpInstrB->IsEqual()) == false) return nullptr; {
        return nullptr;
    }

	if((IsZeroInt(cmpInstrA->RightOp(), block) &&
		IsZeroInt(cmpInstrB->RightOp(), block)) == false) {
        return nullptr;
    }

	// The left operands must be 'or' instructions.
	auto orInstrA = cmpInstrA->LeftOp()->DefiningInstrAs<OrInstr>();
	auto orInstrB = cmpInstrB->LeftOp()->DefiningInstrAs<OrInstr>();
	if((orInstrA && orInstrB) == false) return nullptr;

	// Try to identify 'a', 'b', and 'd'.
	// Make sure that the equal operands are on the left side.
	if(orInstrA->LeftOp() == orInstrB->RightOp()) {
		orInstrB->SwapOperands();
	}
	else if(orInstrA->RightOp() == orInstrB->LeftOp()) {
		orInstrA->SwapOperands();
	}
	else if(orInstrA->RightOp() == orInstrB->RightOp()) {
		orInstrA->SwapOperands();
		orInstrB->RightOp();
	}

	if(orInstrA->LeftOp() != orInstrB->RightOp()) {
		// The conditions are not met.
		return nullptr;
	}

	Operand* a = orInstrA->LeftOp();
	Operand* b = orInstrA->RightOp();
	Operand* d = orInstrB->RightOp();

	// Now create the new sequence of instructions '(a | (b | d)) == 0'.
	auto orOp1 = GetTemporary(a);
	irGen_.GetOr(b, d, orOp1);

	auto orOp2 = GetTemporary(a);
	irGen_.GetOr(a, orOp1, orOp2);
	return LOG(CreateCompare(orOp2, GetZeroInt(a), Order_Equal, 
						     cmpInstrA->ResultOp()->GetType(), 
                             cmpInstrA->IsUcmp()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleOrIntCmp(Operand* opA, Operand* opB, Block* block) {
	// Test for cases where both operands are integer compare instructions,
	// like '(a < 4) | (a == 4)'.
	if(((opA->DefiningInstrIs<CmpInstr>() && 
		 opB->DefiningInstrIs<CmpInstr>()) == false) &&
	   ((opA->DefiningInstrIs<UcmpInstr>() && 
		 opB->DefiningInstrIs<UcmpInstr>()) == false) &&
	   ((opA->DefiningInstrIs<FcmpInstr>() && 
		 opB->DefiningInstrIs<FcmpInstr>()) == false)) {
		// None of the operands is a comparison instruction.
		return nullptr;
	}
	
	CmpInstrBase* leftCmp = static_cast<CmpInstrBase*>(opA->DefiningInstruction());
	CmpInstrBase* rightCmp = static_cast<CmpInstrBase*>(opB->DefiningInstruction());

	// Test for the case in which the operands of both compares are the same,
	// the (possible) difference being only the comparison order. Some examples:
	// (a < 5)  | (a == 5)  -> a <= 5
	// (a >= 7) | (a <= 7)  -> a == 7
	if((leftCmp->LeftOp() == rightCmp->RightOp()) && 
	   (leftCmp->RightOp() == rightCmp->LeftOp())) {
		// We have something like '(a < b) | (b > a); 
        // change the order for the right part.
		rightCmp->InvertOrder(true, false /* invertEquality */);
	}
	
	if((leftCmp->LeftOp() == rightCmp->LeftOp()) && 
	   (leftCmp->RightOp() == rightCmp->RightOp())) {
		// Combine the orders of the two compares and replace them with a compare
		// having the new order. If the result of the test is always 0, use it
		// instead of doing an useless comparison.
		bool orderTrue = false;
		OrderType order = CombineOrdersOr(leftCmp->Order(), 
                                          rightCmp->Order(), orderTrue);
		
		if(orderTrue) {
			return LOG(GetBool(true, opA));
		}
		else return LOG(CreateCompare(leftCmp->LeftOp(), rightCmp->RightOp(), 
                                      order, opA->GetType(), leftCmp->IsUcmp() && 
                                                             rightCmp->IsUcmp()));
	}

	// Test for comparisons that involve the same right operand
	// and the same comparison order.
	if((leftCmp->Order() == rightCmp->Order()) && 
	   (leftCmp->RightOp() == rightCmp->RightOp())) {
		if((leftCmp->IsNotEqual() || 
           (leftCmp->IsCmp() && leftCmp->IsLess())) &&
		   leftCmp->RightOp()->IsZeroInt()) {
			// (a != 0) | (b != 0) -> (a | b) != 0
			// (a < 0)  | (b < 0)  -> (a | b) < 0
			auto orOp = GetTemporary(leftCmp->LeftOp());
			irGen_.GetOr(leftCmp->LeftOp(), rightCmp->LeftOp(), orOp);

			return LOG(CreateCompare(orOp, leftCmp->RightOp(), leftCmp->Order(),
								     opA->GetType(), true));
		}
		else if(leftCmp->IsCmp() && leftCmp->IsGreater() && 
				leftCmp->RightOp()->IsMinusOneInt()) {
			// (a > -1) | (b > -1) -> (a & b) > -1
			auto andOp = GetTemporary(leftCmp->LeftOp());
			irGen_.GetAnd(leftCmp->LeftOp(), rightCmp->LeftOp(), andOp);

			return LOG(CreateCompare(andOp, leftCmp->RightOp(), leftCmp->Order(),
								     opA->GetType(), true));
		}
	}

	// Test for comparisons that target the same operand, but using a different constant.
	// (a < 4) | (a < 2) -> a < 4
	if(leftCmp->LeftOp() == rightCmp->LeftOp()) {
        auto leftConst = AsIntConstant(leftCmp->RightOp(), block);
        auto rightConst = AsIntConstant(rightCmp->RightOp(), block);

        if(leftConst && rightConst) {
            return HandleOrIntCmpConst(leftCmp, rightCmp, 
                                       leftConst, rightConst);
        }
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleOrIntCmpConst(CmpInstrBase* leftCmp, CmpInstrBase* rightCmp,
									   IntConstant* leftConst, IntConstant* rightConst) {
	// To make testing a bit easier, move the largest constant on the right.
	// (a < 5) | (a < 3) -> (a < 3) | (a < 5)
	if(IA::IsLarger(leftConst, rightConst)) {
        std::swap(leftConst, rightConst);
        std::swap(leftCmp, rightCmp);
	}

	switch(leftCmp->Order()) {
	case Order_Equal: {
		switch(rightCmp->Order()) {
		case Order_NotEqual: { // (a == 8) | (a != 9) -> a != 9
			return LOG(rightCmp->ResultOp());
		}
		case Order_Less: {   // (a == 8) | (a < 9)  -> a < 9
			return LOG(rightCmp->ResultOp());
		}
		case Order_Equal: {  // (a == 8) | (a == 9) -> (a - 8) < 2
			// This transformation is safe only if the difference between
			// the two constants is 1.
			if(IsLeftRightMinusOne(leftConst, rightConst) == false) {
				return nullptr;
			}

			// Note that we must use an 'ucmp', even if the original comparisons
			// use the signed 'cmp'. Not doing so would not enable
			// the "wrap around" behavior, which is mandatory in this case.
			auto leftOp = leftCmp->LeftOp();
			auto subOp = GetTemporary(leftOp);
			irGen_.GetSub(leftOp, leftConst, subOp);
			auto twoConst = irGen_.GetIntConst(leftOp->GetType(), 2);
			return LOG(CreateCompare(subOp, twoConst, Order_Less,
									 leftCmp->ResultOp()->GetType(), true /* isUcmp */));
		}
		case Order_Greater: { // (a == 8) | (a >= 9) -> a >= 8
			// This transformation is safe only if the difference between
			// the two constants is 1.
			if(IsLeftRightMinusOne(leftConst, rightConst) == false) {
				return nullptr;
			}

			return LOG(CreateCompare(leftCmp->LeftOp(), leftCmp->RightOp(),
									 Order_GreaterOrEqual, leftCmp->ResultOp()->GetType(),
									 leftCmp->IsUcmp()));
		}
		}
		break;
	}
	case Order_NotEqual: {
		switch(rightCmp->Order()) {
		case Order_NotEqual:  // (a != 8) | (a != 9) -> true
		case Order_Less: {    // (a != 8) | (a < 9) -> true
			return LOG(GetBool(true, leftCmp->ResultOp()));
		}
		case Order_Equal:     // (a != 8) | (a == 10) -> a != 8
		case Order_Greater: { // (a != 8) | (a > 10)  -> a != 8
			return LOG(leftCmp->ResultOp());
		}
		}
		break;
	}
	case Order_Less: {
		switch(rightCmp->Order()) {
		case Order_NotEqual:  // (a < 8) | (a != 9) -> a != 9
		case Order_Less: {    // (a < 8) | (a < 10) -> a < 10
			// This is true only if both comparisons are of the same type.
			if(leftCmp->IsUcmp() == leftCmp->IsUcmp()) {
				return LOG(rightCmp->ResultOp());
			}
		}
        case Order_Greater: { // (a < 8) | (a > 10) -> (a - 8) > 2
            // This is true only if both comparisons are of the same type.
			if(leftCmp->IsUcmp() == leftCmp->IsUcmp()) {
				return LOG(CreateOutOfRangeCompare(leftCmp->LeftOp(), leftConst, rightConst,
											       leftCmp->ResultOp()->GetType(), 
											       leftCmp->IsUcmp()));
			}
            break;
        }
		}
		break;
	}
	case Order_Greater: {
		switch(rightCmp->Order()) {
		case Order_Equal: {   // (a > 8) | (a == 9) -> a > 8
			return LOG(leftCmp->ResultOp());
		}
		case Order_Greater: { // (a > 8) | (a > 9)  -> a > 8
			// This is true only if both comparisons are of the same type.
			if(leftCmp->IsUcmp() == leftCmp->IsUcmp()) {
				return LOG(leftCmp->ResultOp());
			}
		}
		case Order_Less: {     // (a > 8) | (a < 10) -> true
			// This is true only if both comparisons are of the same type.
			if(leftCmp->IsUcmp() == leftCmp->IsUcmp()) {
				return LOG(GetBool(true, leftCmp->ResultOp()));
			}
		}
		case Order_NotEqual: { // (a > 8) | (a != 9) -> true
			return LOG(GetBool(true, leftCmp->ResultOp()));
		}
		}
		break;
	}
	} // switch

	return nullptr;
}

} // namespace Optimization