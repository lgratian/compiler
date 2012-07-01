// LogicalPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 
// 'xor', 'shl', 'shr' and 'ushr' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

OrderType Peephole::CombineOrdersAnd(OrderType a, OrderType b, bool& orderFalse) {
	switch(a) {
	case Order_Equal: {
		switch(b) {
			case Order_Equal: return Order_Equal;          // (a == 5) & (a == 5)
			case Order_LessOrEqual: return Order_Equal;    // (a == 5) & (a <= 5)
			case Order_GreaterOrEqual: return Order_Equal; // (a == 5) & (a >= 5)
			default: orderFalse = true; break;
		}
		break;
	}
	case Order_NotEqual: {
		switch(b) {
			case Order_NotEqual: return Order_NotEqual;      // (a != 5) & (a != 5)
			case Order_Less: return Order_Less;		         // (a != 5) & (a < 5)
			case Order_Greater: return Order_Greater;		 // (a != 5) & (a > 5)
			case Order_LessOrEqual: return Order_Less;		 // (a != 5) & (a <= 5)
			case Order_GreaterOrEqual: return Order_Greater; // (a != 5) & (a >= 5)
			default: orderFalse = true; break;
		}
		break;
	}
	case Order_Less: {
		switch(b) {
			case Order_Less: return Order_Less;        // (a < 5) & (a < 5)
			case Order_LessOrEqual: return Order_Less; // (a < 5) & (a <= 5)
			case Order_NotEqual: return Order_Less;    // (a < 5) & (a != 5)
			default: orderFalse = true; break;
		}
		break;
	}
	case Order_LessOrEqual: {
		switch(b) {
			case Order_LessOrEqual: return Order_LessOrEqual; // (a <= 5) & (a <= 5)
			case Order_Equal: return Order_Equal;             // (a <= 5) & (a == 5)
			case Order_NotEqual: return Order_Less;           // (a <= 5) & (a != 5)
			case Order_Less: return Order_Less;               // (a <= 5) & (a < 5)
			case Order_GreaterOrEqual: return Order_Equal;    // (a <= 5) & (a >= 5)
			default: orderFalse = true; break;
		}
		break;
	}
	case Order_Greater: {
		switch(b) {
			case Order_Greater: return Order_Greater;        // (a > 5) & (a > 5)
			case Order_GreaterOrEqual: return Order_Greater; // (a > 5) & (a >= 5)
			case Order_NotEqual: return Order_Greater;       // (a > 5) & (a != 5)
			default: orderFalse = true; break;
		}
		break;
	}
	case Order_GreaterOrEqual: {
		switch(b) {
			case Order_GreaterOrEqual: return Order_GreaterOrEqual;
			case Order_Greater: return Order_Greater;   // (a >= 5) & (a > 5)
			case Order_Equal: return Order_Equal;       // (a >= 5) & (a == 5)
			case Order_NotEqual: return Order_Greater;  // (a >= 5) & (a > 5)
			case Order_LessOrEqual: return Order_Equal; // (a >= 5) & (a <= 5)
			default: orderFalse = true; break;
		}
		break;
	}
	}

	// Doesn't matter what it's returned here, shouldn't be checked anyway.
	return Order_Equal;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
OrderType Peephole::CombineOrdersOr(OrderType a, OrderType b, bool& orderTrue) {
	switch(a) {
	case Order_Equal: {
		switch(b) {
			case Order_Equal: return Order_Equal;                   // (a == 5) | (a == 5)
			case Order_LessOrEqual: return Order_LessOrEqual;       // (a == 5) | (a <= 5)
			case Order_GreaterOrEqual: return Order_GreaterOrEqual; // (a == 5) | (a >= 5)
			case Order_Greater: return Order_Greater;               // (a == 5) | (a > 5)
			case Order_Less: return Order_LessOrEqual;              // (a == 5) | (a < 5)
			default: orderTrue = true; break;
		}
		break;
	}
	case Order_NotEqual: {
		switch(b) {
			case Order_NotEqual: return Order_NotEqual; // (a != 5) | (a != 5)
			case Order_Less: return Order_NotEqual;		// (a != 5) | (a < 5)
			case Order_Greater: return Order_NotEqual;	// (a != 5) | (a > 5)
			default: orderTrue = true; break;
		}
		break;
	}
	case Order_Less: {
		switch(b) {
			case Order_Less: return Order_Less;               // (a < 5) | (a < 5)
			case Order_LessOrEqual: return Order_LessOrEqual; // (a < 5) | (a <= 5)
			case Order_NotEqual: return Order_NotEqual;       // (a < 5) | (a != 5)
			case Order_Greater: return Order_NotEqual;        // (a < 5) | (a > 5)
			case Order_Equal: return Order_LessOrEqual;       // (a < 5) | (a == 5)
			default: orderTrue = true; break;
		}
		break;
	}
	case Order_LessOrEqual: {
		switch(b) {
			case Order_LessOrEqual: return Order_LessOrEqual; // (a <= 5) | (a <= 5)
			case Order_Equal: return Order_LessOrEqual;       // (a <= 5) | (a == 5)
			case Order_Less: return Order_LessOrEqual;        // (a <= 5) | (a < 5)
			default: orderTrue = true; break;
		}
		break;
	}
	case Order_Greater: {
		switch(b) {
			case Order_Greater: return Order_Greater;               // (a > 5) | (a == 5)
			case Order_GreaterOrEqual: return Order_GreaterOrEqual; // (a > 5) | (a >= 5)
			case Order_NotEqual: return Order_NotEqual;             // (a > 5) | (a != 5)
			case Order_Equal: return Order_GreaterOrEqual;          // (a > 5) | (a == 5)
			case Order_Less: return Order_NotEqual;                 // (a > 5) | (a < 5)
			default: orderTrue = true; break;
		}
		break;
	}
	case Order_GreaterOrEqual: {
		switch(b) {
			case Order_GreaterOrEqual: return Order_GreaterOrEqual; // (a >= 5) | (a >= 5)
			case Order_Greater: return Order_GreaterOrEqual;        // (a >= 5) | (a > 5)
			case Order_Equal: return Order_GreaterOrEqual;          // (a >= 5) | (a == 5)
			default: orderTrue = true; break;
		}
		break;
	}
	}

	// Doesn't matter what it's returned here, shouldn't be checked anyway.
	return Order_Equal;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
OrderType Peephole::CombineOrdersXor(OrderType a, OrderType b, bool& orderFalse,
									 bool& orderTrue) {
	switch(a) {
	case Order_Equal: {
		switch(b) {
			case Order_LessOrEqual: return Order_Less;       // (a == 5) ^ (a <= 5)
			case Order_GreaterOrEqual: return Order_Greater; // (a == 5) ^ (a >= 5)
			case Order_Greater: return Order_GreaterOrEqual; // (a == 5) ^ (a > 5)
			case Order_Less: return Order_LessOrEqual;       // (a == 5) ^ (a < 5)
			case Order_Equal: orderFalse = true; break;
			case Order_NotEqual: orderTrue = true; break;
		}
		break;
	}
	case Order_NotEqual: {
		switch(b) {
			case Order_Greater: return Order_Less;		         // (a != 5) ^ (a < 5)
			case Order_GreaterOrEqual: return Order_LessOrEqual; // (a != 5) ^ (a <= 5)
			case Order_Less: return Order_Greater;		         // (a != 5) ^ (a > 5)
			case Order_LessOrEqual: return Order_GreaterOrEqual; // (a != 5) ^ (a >= 5)
			case Order_Equal: orderTrue = true; break;
			case Order_NotEqual: orderFalse = true; break;
		}
		break;
	}
	case Order_Less: {
		switch(b) {
			case Order_Greater: return Order_NotEqual;	// (a < 5) ^ (a > 5)
			case Order_Equal: return Order_LessOrEqual;	// (a < 5) ^ (a == 5)
			case Order_NotEqual: return Order_Greater;	// (a < 5) ^ (a != 5)
			case Order_LessOrEqual: return Order_Equal; // (a < 5) ^ (a <= 5)
			case Order_Less: orderFalse = true; break;
			case Order_GreaterOrEqual: orderTrue = true; break;
		}
		break;
	}
	case Order_LessOrEqual: {
		switch(b) {
			case Order_Equal: return Order_Less;              // (a <= 5) ^ (a == 5)
			case Order_GreaterOrEqual: return Order_NotEqual; // (a <= 5) ^ (a >= 5)
			case Order_Less: return Order_Equal;		      // (a <= 5) ^ (a < 5)
			case Order_NotEqual: return Order_GreaterOrEqual; // (a <= 5) ^ (a != 5)
			case Order_LessOrEqual: orderFalse = true; break;
			case Order_Greater: orderTrue = true; break;
		}
		break;
	}
	case Order_Greater: {
		switch(b) {
			case Order_Equal: return Order_GreaterOrEqual;   // (a > 5) ^ (a == 5)
			case Order_GreaterOrEqual: return Order_Equal;   // (a > 5) ^ (a >= 5)
			case Order_Less: return Order_NotEqual;          // (a > 5) ^ (a < 5)
			case Order_NotEqual: return Order_Less;          // (a > 5) ^ (a != 5)
			case Order_LessOrEqual: orderTrue = true; break;
			case Order_Greater: orderFalse = true; break;
		}
		break;
	}
	case Order_GreaterOrEqual: {
		switch(b) {
			case Order_Greater: return Order_Equal;        // (a > 5) ^ (a >= 5)
			case Order_Equal: return Order_Greater;        // (a > 5) ^ (a == 5)
			case Order_NotEqual: return Order_LessOrEqual; // (a > 5) ^ (a != 5)
			case Order_LessOrEqual: return Order_NotEqual; // (a > 5) ^ (a <= 5)
			case Order_GreaterOrEqual: orderFalse = true; break;
			case Order_Less: orderTrue = true; break;
		}
		break;
	}
	}

	// Doesn't matter what it's returned here, shouldn't be checked anyway.
	return Order_Equal;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleXor(Operand* opA, Operand* opB, Block* block,
                             bool constMoveAllowed) {
	Operand* a;
	Operand* b;
	Operand* x;
	bool match = false;

	// Try to constant-fold.
	if(auto result = folder_.FoldBinary(Instr_Xor, opA, opB, block)) {
		return LOG(result);
	}

    // If one of the operands is a constant make it the the right operand.
    // This is also an improvement for CPUs that don't support
    // a constant on the left side by default.
	bool moved = MoveConstantToRight(opA, opB);

    // Try to simplify using associativity/commutativity rules.
    if(auto result = HandleAssociative(Instr_Xor, opA, opB, block)) {
        return result;
    }

    // Try to simplify using distributivity rules.
    if(auto result = HandleDistributive(Instr_Xor, opA, opB, block)) {
        return result;
    }

    if(auto result = HandleXorSimple(opA, opB, block)) {
        return result;
    }

	// (a >> x) ^ (b >> x)  -> (a ^ b) >> x, for both 'shr' and 'ushr'.
	if(Match<ShrInstr>(MatchAny(&a), MatchAny(&x))(opA) &&
	   Match<ShrInstr>(MatchAny(&b), MatchOp(x))(opB)) {
		auto xorOp = GetTemporary(opA);
		irGen_.GetXor(a, b, xorOp);
		auto shrOp = GetTemporary(opA);

		if(opA->DefiningInstrIs<ShrInstr>()) {
			irGen_.GetShr(xorOp, x, shrOp);
		}
		else irGen_.GetUshr(xorOp, x, shrOp);
		
        return LOG(shrOp);
	}

	// Try to simplify an 'xor' that targets compare instructions.
	if(auto result = HandleXorIntCmp(opA, opB, block)) {
		return result;
	}

	// Try to simplify an 'xor' that involves integer constants.
	if(auto result = HandleXorConst(opA, opB, block)) {
		return result;
	}

	// Try to eliminate useless casts, which are frequently
    // generated by the frontend.
	// zext(a) ^ zext(b) -> zext(a ^ b)
	// sext(a) ^ sext(b) -> sext(a ^ b)
	if((MatchConversion<SextInstr>(MatchAny(&a))(opA) && 
		MatchConversion<SextInstr>(MatchAny(&b))(opB))
        ||
	   (MatchConversion<ZextInstr>(MatchAny(&a))(opA) && 
		MatchConversion<ZextInstr>(MatchAny(&b))(opB))) {
		// Do the transformation only if the source type 
        // is the same for both operands.
		if(a->GetType() == b->GetType()) {
			auto xorOp = GetTemporary(a);
			irGen_.GetXor(a, b, xorOp);
			auto extOp = GetTemporary(opA);

			if(opA->DefiningInstrIs<SextInstr>()) {
				irGen_.GetSext(xorOp, opA->GetType(), extOp);
			}
			else irGen_.GetZext(xorOp, opA->GetType(), extOp);
			
            return LOG(extOp);
		}
	}

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_Xor, opA, opB)) {
        return result;
    }

	// Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_Xor, opA, opB)) {
        return result;
    }

    // Returns a new instruction if the constant was moved to the right.
    if(moved && constMoveAllowed) {
        auto result = GetTemporary(opA);
        irGen_.GetAnd(opA, opB, result);
        return LOG(result);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleXorSimple(Operand* opA, Operand* opB, 
                                   Block* block, bool createAllowed) {
    Operand* a;
    Operand* b;
	IntConstant* C;

    // If one of the operands is a constant make it 
    // the right operand, so that we need to test much fewer cases.
    MoveConstantToRight(opA, opB);

    // a ^ 0 -> a
    if(IsZeroInt(opB, block)) {
        return LOG(opA);
    }

    // a ^ a -> 0
    if(AreEqual(opA, opB, block)) {
        return LOG(GetZeroInt(opA));
    }

    // a ^ ~a -> -1
    // ~a ^ a -> -1
    if((IsLogicalNot(opB, &a) && (a == opA)) ||
       (IsLogicalNot(opA, &a) && (a == opB))) {
        return LOG(GetMinusOneInt(opA));
    }

    // NOTE: All the following simplification are done only if
    // we're allowed to create new instructions.
    if(createAllowed == false) {
        return nullptr;
    }

     // ~a ^ C -> a ^ ~C
    if(IsLogicalNot(opA, &a) && MatchIC(&C, block)(opB)) {
        auto constantOp = irGen_.GetIntConst(C->GetType(), IA::Not(C));

        auto xorOp = GetTemporary(opA);
        irGen_.GetXor(a, constantOp, xorOp);
        return LOG(xorOp);
    }

	// ~a ^ ~b -> a ^ b
	if(IsLogicalNot(opA, &a) && IsLogicalNot(opB, &b)) {
		auto xorOp = GetTemporary(opA);
		irGen_.GetXor(a, b, xorOp);
		return LOG(xorOp);
	}

	// ~(~a & b) -> (a | ~b) (De Morgan)
	if(Match<AndInstr>(Match<XorInstr>(MatchAny(&a), 
                                       MatchInt(-1, block)), 
                       MatchAny(&b))(opA) && 
       MatchInt(-1, block)(opB)) {
		auto xorOp = GetNegated(b);
		auto orOp = GetTemporary(opA);
		irGen_.GetOr(a, xorOp, orOp);
		return LOG(orOp);
	}

	// ~(~a | b) -> (a & ~b) (De Morgan)
	if(Match<OrInstr>(Match<XorInstr>(MatchAny(&a), 
                                      MatchInt(-1, block)), 
                      MatchAny(&b))(opA) && 
       MatchInt(-1, block)(opB)) {
		auto xorOp = GetNegated(b);
		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(a, xorOp, andOp);
		return LOG(andOp);
	}

	// ~(a | b) -> (~a & ~b) (De Morgan)
	if(Match<OrInstr>(MatchAny(&a), MatchAny(&b))(opA) && 
       MatchInt(-1, block)(opB)) {
		auto xorOp1 = GetNegated(a);
		auto xorOp2 = GetNegated(b);

		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(xorOp1, xorOp2, andOp);
		return LOG(andOp);
	}

    // a ^ (~a & b) -> a | b
	// a ^ (b & ~a) -> a | b
	if((Match<AndInstr>(Match<XorInstr>(MatchAny(&a), MatchInt(-1, block)), 
                        MatchAny(&b))(opB)
        ||
		Match<AndInstr>(MatchAny(&b), 
                        Match<XorInstr>(MatchAny(&a), MatchInt(-1, block)))(opB)) &&
	   (a == opA)) {
		auto orOp = GetTemporary(opA);
		irGen_.GetOr(a, b, orOp);
		return LOG(orOp);
	}

	// a ^ (a & ~b) -> a & b
	// a ^ (~b & a) -> a & b
	if((Match<AndInstr>(MatchAny(&a), 
                        Match<XorInstr>(MatchAny(&b), MatchInt(-1, block)))(opB) 
        ||
		Match<AndInstr>(Match<XorInstr>(MatchAny(&b), MatchInt(-1, block)), 
                        MatchAny(&a))(opB)) &&
	   (a == opA)) {
		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(a, b, andOp);
		return LOG(andOp);
	}

	// a ^ (a | b) -> ~a & b
	// a ^ (b | a) -> ~a & b
	if((Match<OrInstr>(MatchAny(&a), MatchAny(&b))(opB) ||
	   Match<OrInstr>(MatchAny(&b), MatchAny(&a))(opB)) && 
       (a == opA)) {
		auto xorOp = GetNegated(a);

		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(xorOp, b, andOp);
		return LOG(andOp);
	}

	// ~(~a >> b) -> (a >> b)
	if(Match<ShrInstr>(Match<XorInstr>(MatchAny(&a), 
                                       MatchInt(-1, block)), 
                       MatchAny(&b))(opA) &&
	   MatchInt(-1)(opB)) {
		auto shrOp = GetTemporary(opA);
		irGen_.GetShr(a, b, shrOp);
		return LOG(shrOp);
	}

    // (a & b) ^ (a | b) -> a ^ b
	// (a & b) ^ (b | a) -> a ^ b
	if((Match<AndInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
		(Match<OrInstr>(MatchOp(a), MatchOp(b))(opB) ||
		 Match<OrInstr>(MatchOp(b), MatchOp(a))(opB))) 
         ||
		// (a | b) ^ (a & b) -> a ^ b
		// (a | b) ^ (b & a) -> a ^ b
	   (Match<OrInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
		(Match<AndInstr>(MatchOp(a), MatchOp(b))(opB) ||
		 Match<AndInstr>(MatchOp(b), MatchOp(a))(opB)))) {
		auto xorOp = GetTemporary(opA);
		irGen_.GetXor(a, b, xorOp);
		return LOG(xorOp);
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleXorIntCmp(Operand* opA, Operand* opB, Block* block) {
	IntConstant* C;

	// If we have a comparison followed by an Xor we can invert
	// the condition in the comparison. For example,
	// (a < b) ^ C -> ~(a < b) -> a >= b, if 'C' has the rightmost bit set.
	if(MatchIC(&C, block)(opB) && IA::IsBitSet(0, C) &&
	   opA->HasDefiningInstruction() && 
       opA->DefiningInstruction()->IsComparison() &&
       opA->As<Temporary>()->HasSingleUser()) {
		auto cmpInstr = static_cast<CmpInstrBase*>(opA->DefiningInstruction());
		cmpInstr->NegateOrder();
		return LOG(cmpInstr->ResultOp());
	}

	// zext(a < b) ^ 1 -> zext(~(a < b)) -> zext(a >= b)
	// sext(a < b) ^ 1 -> sext(~(a < b)) -> sext(a >= b)
	// This kind of code appears when the result of 
    // a comparison is added to a number.
	if(IsOneInt(opB, block) && opA->HasDefiningInstruction()) {
		CmpInstrBase* cmpInstr = nullptr;

		if(auto sextInstr = opA->DefiningInstrAs<SextInstr>()) {
			auto targetInstr = sextInstr->TargetOp()->DefiningInstruction();

			if(cmpInstr->IsComparison()) {
				cmpInstr = static_cast<CmpInstrBase*>(cmpInstr);
			}
		}
		else if(auto zextInstr = opB->DefiningInstrAs<ZextInstr>()) {
			auto targetInstr = zextInstr->TargetOp()->DefiningInstruction();

			if(cmpInstr->IsComparison()) {
				cmpInstr = static_cast<CmpInstrBase*>(cmpInstr);
			}
		}

        if(cmpInstr && cmpInstr->GetDestinationOp()->HasSingleUser()) {
			cmpInstr->NegateOrder();
			return LOG(cmpInstr->ResultOp());
		}
	}

	// Try to simplify an 'xor' on two compare instructions.
	// (a ORDER1 b) ^ (a ORDER2) -> a ORDER3 b
    auto leftCmp = opA->DefiningInstrAs<CmpInstrBase>();
    auto rightCmp = opB->DefiningInstrAs<CmpInstrBase>();

    if((leftCmp && rightCmp) == false) {
        return nullptr;
    }

	if((leftCmp->LeftOp() == rightCmp->RightOp()) && 
	   (leftCmp->RightOp() == rightCmp->LeftOp()) &&
       rightCmp->GetDestinationOp()->HasSingleUser()) {
		// We have something like '(a < b) ^ (b > a); 
        // change the order for the right part.
		rightCmp->InvertOrder();
	}
	
	if((leftCmp->LeftOp() == rightCmp->RightOp()) && 
	   (leftCmp->RightOp() == rightCmp->RightOp())) {
		// Combine the orders of the two compares and replace them
        // with a compare having the new order. If the result of the test 
        // is always 0, use it instead of doing an useless comparison.
		bool orderFalse = false;
		bool orderTrue = false;
		OrderType order = CombineOrdersXor(leftCmp->Order(), 
                                           rightCmp->Order(), 
										   orderFalse, orderTrue);
		
		if(orderFalse) {
            return LOG(GetBool(false, opA));
        }
		else if(orderTrue) {
            return LOG(GetBool(true, opA));
        }
		else return LOG(CreateCompare(leftCmp->LeftOp(), rightCmp->RightOp(), 
                                      order, opA->GetType(), leftCmp->IsUcmp()));
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleXorConst(Operand* opA, Operand* opB, Block* block) {
	Operand* a;
	IntConstant* C;
	IntConstant* C1;
	IntConstant* C2;

	// ~(C-a) -> a - C - 1 -> a + (-C - 1)
	if(Match<SubInstr>(MatchIC(&C, block), MatchAny(&a))(opA) && 
       MatchInt(-1, block)(opB)) {
        // We have a match.
		auto intKind = C->GetType()->GetSubtype();
		__int64 result = IA::Sub(IA::Sub(0, C->Value(), intKind), 
                                 1, intKind);
		auto constantOp = irGen_.GetIntConst(C->GetType(), result);

		auto addOp = GetTemporary(opA);
		irGen_.GetAdd(a, constantOp, addOp);
		return LOG(addOp);
	}

	// ~(a - C) -> (-C - 1) - a
	if(Match<SubInstr>(MatchAny(&a), MatchIC(&C, block))(opA) && 
       MatchInt(-1, block)(opB)) {
        // We have a match.
		auto intKind = C->GetType()->GetSubtype();
		__int64 result = IA::Sub(IA::Sub(0, C->Value(), intKind), 
                                 1, intKind);
		auto constantOp = irGen_.GetIntConst(C->GetType(), result);
		
        auto subOp = GetTemporary(opA);
		irGen_.GetSub(constantOp, a, subOp);
		return LOG(subOp);
	}

	// (a + C1) ^ C2 -> a + (C1 + C2), if 'C2' has the value of the sign bit.
	if(Match<AddInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) && 
       MatchIC(&C2, block)(opB)) {
		if(C2->Value() == IA::GetSignBitMask(C2->GetType())) {
			__int64 result = IA::Add(C1, C2);
			auto constantOp = irGen_.GetIntConst(opA->GetType(), result);
			
            auto addOp = GetTemporary(opA);
			irGen_.GetAdd(a, constantOp, addOp);
			return LOG(addOp);
		}
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleShl(ShlInstr* instr) {
	// We use the constant evaluator even if not both operands are constant,
	// because it also eliminates very simple cases.
	if(auto result = folder_.FoldBinary(Instr_Shl, instr->LeftOp(), 
                                        instr->RightOp(), 
                                        instr->ParentBlock())) {
		return LOG(result);
	}

	if(auto result = HandleShiftCommon(instr)) {
		return result;
	}

	// Check if we are adding at least one 'quest' instruction result.
    return HandleOperationOnQuest(Instr_Shl, instr->LeftOp(), 
                                  instr->RightOp());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleShr(ShrInstr* instr) {
	// We use the constant evaluator even if not both operands are constant,
	// because it also eliminates very simple cases.
	if(auto result = folder_.FoldBinary(Instr_Shr, instr->LeftOp(), 
                                        instr->RightOp(), 
                                        instr->ParentBlock())) {
		return LOG(result);
	}

	if(auto result = HandleShiftCommon(instr)) {
		return result;
	}

	// Test for a sequence of instructions that does a a sign extension
	// without using the 'sext' instruction. For example:
	// t1 = zext a, int32       t2 = shl t1, 16       t3 = shr t2, 16
	// "sign extends" an 'int16', and can be replaced by 't1 = sext a, int32'.
	IntConstant* C1;
	IntConstant* C2;
	const Type* destType;
	Operand* a;

	if(Match<ShlInstr>(MatchConversion<ZextInstr>(MatchAny(&a), nullptr, &destType), 
                       MatchIC(&C1, instr->ParentBlock()))(instr->LeftOp()) &&
       MatchIC(&C2, instr->ParentBlock())(instr->RightOp())) {
		if(IA::AreEqual(C1, C2)) {
			// The shift amount must be the difference between the bitwidth
			// of the destination and source types.
			int sourceBitwidth = a->GetType()->As<IntegerType>()->SizeInBits();
			int destBitwidth = destType->As<IntegerType>()->SizeInBits();
			auto intKind = destType->As<IntegerType>()->GetSubtype();

			if(IA::AreEqual(C1->Value(), destBitwidth - sourceBitwidth, intKind)) {
				// We can do the simplification.
				auto sextOp = GetTemporary(instr->ResultOp());
				irGen_.GetSext(a, destType, sextOp);
				return LOG(sextOp);
			}
		}
	}

	// Check if we are adding at least one 'quest' instruction result.
    return HandleOperationOnQuest(Instr_Shr, instr->LeftOp(), 
                                  instr->RightOp());
}
   
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleUshr(UshrInstr* instr) {
	// We use the constant evaluator even if not both operands are constant,
	// because it also eliminates very simple cases.
	if(auto result = folder_.FoldBinary(Instr_Ushr, instr->LeftOp(), 
                                        instr->RightOp(), 
                                        instr->ParentBlock())) {
		return LOG(result);
	}

	if(auto result = HandleShiftCommon(instr)) {
		return result;
	}

	// Check if we are adding at least one 'quest' instruction result.
    return HandleOperationOnQuest(Instr_Ushr, instr->LeftOp(), 
                                  instr->RightOp());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleShiftCommon(LogicalInstr* instr) {
	Operand* a;
	IntConstant* C;
	IntConstant* C1;
	IntConstant* C2;

    Operand* opA = instr->LeftOp();
	Operand* opB = instr->RightOp();
    auto block = instr->ParentBlock();

    // Check for some simple cases first.
    if(auto result = HandleShiftSimple(opA, opB, instr->IsShl(), block)) {
        return result;
    }
    
	auto type = instr->ResultOp()->GetType()->As<IntegerType>();
	auto intKind = type->GetSubtype();
	int bits = type->SizeInBits();

	// Eliminate shifts that always generate 0.
	// a >> 33 -> 0, for 'a' having 'int32' type.
	if(MatchIC(&C, block)(opB) && 
       IA::IsLargerOrEqual(C->Value(), bits, intKind)) {
		// We can replace with zero only for logical shifts; we can't do it
		// for arithmetic shift because we don't know the value of the sign.
        // An exception is when Operand Info can determine that
        // the sign bit is always zero or one.
		if(instr->IsShr() == false) {
			return LOG(GetZeroInt(opA));
		}
        else if(opInfo_.IsPositive(opA)) {
            return LOG(GetZeroInt(opA));
        }
        else if(opInfo_.IsNegative(opA)) {
            return LOG(GetMinusOneInt(opA));
        }
		else {
			// If it's 'shr' we can still improve the result a bit
			// by setting the shift amount to 'bits - 1'.
			instr->SetLeftOp(irGen_.GetIntConst(type, bits - 1));
			return LOG(instr->ResultOp());
		}
	}

	// Try to simplify shifts that target other shifts and logical operations
	// that use the same constant amount. This is done in two steps:
	// 1. Verify that the simplifications can be applied and the cost
	//    is at least equal that the current one, if not smaller.
	// 2. Apply the transformations. For example:
	//    (a << 16) >> 16 -> a & 0xFFFF
	//    ((a << 8) | (b << 8)) >> 8 -> ((a & 0xFF) | (b & 0xFF)) -> (a | b) & 0xFF
	if(CanSimplifyShiftWithConst(instr)) {
		auto shiftAmount = instr->RightOp()->As<IntConstant>();
		return SimplifyShiftWithConst(instr->LeftOp(), 
                                      shiftAmount, instr->IsShl());
	}

	// (a & C1) >> C2 -> 0, if LSB(C1) <= C2. For example,
	// (a & 0xFB) >> 16 -> 0, because all the top bits are guaranteed to be 0.
	// Can be applied only for unsigned numbers.
	if(Match<AndInstr>(MatchAny(&a), 
                       MatchIC(&C1, block))(opA) &&
	   MatchIC(&C2, block)(opB) && 
       instr->IsUshr()) {
		// Test the position of the leftmost bit; if it's smaller or equal
		// to the shift amount we know the result will be always 0.
		int bitPos = IA::LeftmostSetBit(C1);

		if(bitPos <= C2->Value()) {
			return LOG(GetZeroInt(opA));
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleShiftSimple(Operand* opA, Operand* opB, bool isShl,
                                     Block* block, bool createAllowed) {
    Operand* a;
	IntConstant* C1;
	IntConstant* C2;

    // a << 0 -> a
	// a >> 0 -> a
	if(auto intConst = AsIntConstant(opB, block)) {
        if(intConst->IsZero()) {
            return LOG(opA);
        }
	}

    // NOTE: All the following simplification are done only if
    // we're allowed to create new instructions.
    if(createAllowed == false) return nullptr;

    auto type = opA->GetType()->As<IntegerType>();
    int bits = type->SizeInBits();

    // (a << C1) << C2 -> a << (C1 + C2)
	if(isShl && Match<ShlInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) &&
	   MatchIC(&C2, block)(opB)) {
		__int64 result = IA::Add(C1, C2);
		
		// If the sum is greater than the bitwidth return 0 directly.
		if(result >= bits) {
			return LOG(GetZeroInt(opA));
		}
		else {
			auto constantOp = irGen_.GetIntConst(type, result);
			auto shlOp = GetTemporary(opA);
			irGen_.GetShl(a, constantOp, shlOp);
			return LOG(shlOp);
		}
	}

	// (a * C1) << C2) -> a * (C1 << C2)
	if(isShl && Match<MulInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) &&
	   MatchIC(&C2, block)(opB)) {
		auto constantOp = irGen_.GetIntConst(type, IA::Add(C1, C2));
		auto mulOp = GetTemporary(opA);
		irGen_.GetMul(a, constantOp, mulOp);
		return LOG(mulOp);
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleShiftConst(Operand* opA, Operand* opB, 
                                    LogicalInstr* instr) {
	Operand* a;
	IntConstant* C1;
	IntConstant* C2;
	auto type = instr->ResultOp()->GetType()->As<IntegerType>();
	auto intKind = type->GetSubtype();

	// Give up if the conditions don't match.
	// The left operand should be a shift instruction.
	auto leftInstr = opA->DefiningInstrAs<LogicalInstr>();
	
    if(leftInstr = nullptr) {
        return nullptr;
    }

	// The right operand should be a constant integer.
    C2 = AsIntConstant(opB, instr->ParentBlock());

	if(C2 == nullptr) {
        return nullptr;
    }

	// The instruction on the left should be a shift too.
	if(leftInstr->IsShift() == false) {
        return nullptr;
    }

    C1 = AsIntConstant(leftInstr->RightOp(), instr->ParentBlock());

    if(C1 == nullptr) {
        return nullptr;
    }

	// See in which category the instructions are and try to simplify.
	// Even if some simplifications don't seem to make sense, they could
	// allow other simplifications, and the const isn't higher anyhow.
	a = leftInstr->LeftOp();

	if(IA::AreEqual(C1, C2)) {
		if((leftInstr->IsUshr() || 
            leftInstr->IsShr()) && instr->IsShl()) {
			// (a >> C) << C -> a & (-1 << C).
			__int64 result = IA::Shl(-1, C1->Value(), intKind);
			auto constantOp = irGen_.GetIntConst(type, result);
			
            auto andOp = GetTemporary(opA);
			irGen_.GetAnd(a, constantOp, andOp);
			return LOG(andOp);
		}
		else if(leftInstr->IsShl() && instr->IsUshr()) {
			// (a << C) >> C -> a & (-1 >> C), for unsigned right shift only.
			__int64 result = IA::Ushr(-1, C1->Value(), intKind);
			auto constantOp = irGen_.GetIntConst(type, result);
			
            auto andOp = GetTemporary(opA);
			irGen_.GetAnd(a, constantOp, andOp);
			return LOG(andOp);
		}
	}
	else if(IA::IsSmaller(C1, C2)) {
		// (a >> C1) << C2 -> (a << (C2 - C1)) & (-1 << C2)
		if((leftInstr->IsShr() || 
            leftInstr->IsUshr()) && instr->IsShl()) {
			// a << (C2 - C1)
			auto constOp1 = irGen_.GetIntConst(type, IA::Sub(C2, C1));
			auto shlOp = GetTemporary(opA);
			irGen_.GetShl(a, constOp1, shlOp);
					
			// (-1 << C2)
			__int64 result = IA::Shl(-1, C1->Value(), intKind);
			auto constOp2 = irGen_.GetIntConst(type, result);
			
            auto andOp = GetTemporary(opA);
			irGen_.GetAnd(shlOp, constOp2, andOp);
			return LOG(andOp);
		}

		// (a << C1) >> C2 -> (a >> (C2-C1)) & (-1 >> C2),
		// Note that this it valid for unsigned right shift only.
		if(leftInstr->IsShl() && instr->IsUshr()) {
			// a >> (C2 - C1)
			auto constOp1 = irGen_.GetIntConst(type, IA::Sub(C2, C1));
			auto shlOp = GetTemporary(opA);
			irGen_.GetUshr(a, constOp1, shlOp);
					
			// (-1 >> C2)
			__int64 result = IA::Ushr(-1, C1->Value(), intKind);
			auto constOp2 = irGen_.GetIntConst(type, result);
			
            auto andOp = GetTemporary(opA);
			irGen_.GetAnd(shlOp, constOp2, andOp);
			return LOG(andOp);
		}
	}
	else {
		// (a >> C1) << C2 -> (a >> (C1 - C2)) & (-1 << C2)
		if((leftInstr->IsShr() || 
            leftInstr->IsUshr()) && instr->IsShl()) {
			// a >> (C1 - C2)
			auto constOp1 = irGen_.GetIntConst(type, IA::Sub(C1, C2));
			auto shlOp = GetTemporary(opA);
			
            if(leftInstr->IsShr()) {
				irGen_.GetShr(a, constOp1, shlOp);
			}
			else irGen_.GetUshr(a, constOp1, shlOp);
					
			// (-1 << C2)
			__int64 result = IA::Shl(-1, C1->Value(), intKind);
			auto constOp2 = irGen_.GetIntConst(type, result);
			
            auto andOp = GetTemporary(opA);
			irGen_.GetAnd(shlOp, constOp2, andOp);
			return LOG(andOp);
		}

		// (a << C1) >> C2 -> (a << (C1 - C2)) & (-1 >> C2)
		// Note that this it valid for unsigned right shift only.
		if(leftInstr->IsShl() && instr->IsUshr()) {
			// a << (C1 - C2)
			auto constOp1 = irGen_.GetIntConst(type, IA::Sub(C1, C2));
			auto shlOp = GetTemporary(opA);
			irGen_.GetShl(a, constOp1, shlOp);
					
			// (-1 >> C2)
			__int64 result = IA::Ushr(-1, C1->Value(), intKind);
			auto constOp2 = irGen_.GetIntConst(type, result);
			
            auto andOp = GetTemporary(opA);
			irGen_.GetAnd(shlOp, constOp2, andOp);
			return LOG(andOp);
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::CanSimplifyShiftWithConst(LogicalInstr* instr) {
	// Note that these optimizations can be done only if the first operation
	// is not an arithmetic right shift, which would introduce the unknown sign bit.
	if(instr->IsShr()) {
        // Using Operand Info we may know that the sign bit is zero,
        // in which case we can continue.
        if(opInfo_.IsPositive(instr->LeftOp()) == false) {
            return nullptr;
        }
    }
	
	// The shift amount needs to be a constant.
	auto shiftAmount = instr->RightOp()->As<IntConstant>();

	if(shiftAmount == nullptr) {
        return nullptr;
    }

	return CanSimplifyShiftWithConstImpl(instr->LeftOp(), 
                                         shiftAmount, instr->IsShl());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::CanSimplifyShiftWithConstImpl(Operand* op, IntConstant* shiftAmount, 
											 bool isLeft) {
	// If the operand is a constant integer we can always simplify it.
	if(op->IsIntConstant()) {
		return true;
	}

	// The operand should be the temporary result of an instruction otherwise.
	auto definingInstr = op->DefiningInstrAs<LogicalInstr>();

	if(definingInstr == nullptr) {
        return false;
    }

	// Else it must be an instruction whose result is used a single time.
	// Because this transformation changes the instructions directly, we can't
	// afford to create new instructions for the other uses.
	if(op->HasSingleUser() == false) {
		return false;
	}

	// Based on the type of the defining instruction try to take a decision.
	switch(definingInstr->GetOpcode()) {
		case Instr_Shl: {
			// The shift amount needs to be a constant.
			auto intConst = definingInstr->RightOp()->As<IntConstant>();

			if(intConst == nullptr) {
                return false;
            }

			// (a << C1) << C2 can always be transformed.
			if(isLeft) {
                return true;
            }
		
			// (a << C1) >> C2 can be transformed if 'C1 == C2'.
			if(IA::AreEqual(intConst, shiftAmount)) {
				return true;
			}
			break;
		}
		case Instr_Ushr: {
			// Can be applied only to logical right shift.
			// The shift amount needs to be a constant.
			auto intConst = definingInstr->RightOp()->As<IntConstant>();

			if(intConst == nullptr) {
                return false;
            }

			// (a >> C1) >> C2 can always be transformed.
			if(isLeft == false) {
                return true;
            }
			
			// (a >> C1) << C2 can be transformed if 'C1 == C2'.
			if(IA::AreEqual(intConst, shiftAmount)) {
				return true;
			}
			break;
		}
		case Instr_Or:
		case Instr_And:
		case Instr_Xor: {
			// (a | b) << C -> (a << C) | (b << C)
			// The constant always distributes over the logical operation.
			// Both parts must be available for simplification.
			return CanSimplifyShiftWithConstImpl(definingInstr->LeftOp(), 
                                                 shiftAmount, isLeft) &&
				   CanSimplifyShiftWithConstImpl(definingInstr->RightOp(), 
                                                 shiftAmount, isLeft);
		}
	}

	// No simplification can be safely done.
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::SimplifyShiftWithConst(Operand* op, IntConstant* shiftAmount,
										  bool isLeft) {
	// If the operand is a constant we can compute the result now.
	if(auto intConst = op->As<IntConstant>()) {
		__int64 result = isLeft ? IA::Shl(intConst, shiftAmount) :
								  IA::Ushr(intConst, shiftAmount);
		return irGen_.GetIntConst(op->GetType(), result);
	}

	// Based on the type of the instruction decide what to do.
	auto definingInstr = op->DefiningInstrAs<LogicalInstr>();
	DebugValidator::IsNotNull(definingInstr);

	switch(definingInstr->GetOpcode()) {
		case Instr_Shl: {
			auto intConst = definingInstr->RightOp()->As<IntConstant>();
			DebugValidator::IsNotNull(intConst);
			int bitwidth = intConst->GetType()->SizeInBits();
			
			if(isLeft) {
				// (a << C1) << C2 -> a << (C1 + C2)
				// If the amount is larger than the bitwidth we can return 0 directly.
				__int64 result = IA::Add(intConst, shiftAmount);

				if(result >= bitwidth) {
					return LOG(GetZeroInt(intConst));
				}
				else {
					// We modify the instruction directly, by replacing 'C1'.
					auto amountOp = irGen_.GetIntConst(op->GetType(), result);
					definingInstr->SetRightOp(amountOp);
					return LOG(definingInstr->ResultOp());
				}
			}
			else {
				// (a << C) >> C -> a & (1 << ((BITWIDTH - C) - 1))
				// We need to apply the mask because the last 'C' bits need to be 0.
				auto intKind = intConst->GetType()->GetSubtype();
				int maskBits = IA::Sub(bitwidth, shiftAmount->Value(), intKind);
				
                __int64 mask = IA::ValueFromBitCount(maskBits);
				auto maskOp = irGen_.GetIntConst(intConst->GetType(), mask);

				auto andOp = GetTemporary(op);
				irGen_.GetAnd(definingInstr->LeftOp(), maskOp, andOp);
				return LOG(andOp);
			}
		}
		case Instr_Ushr: {
			auto intConst = definingInstr->RightOp()->As<IntConstant>();
			DebugValidator::IsNotNull(intConst);
			int bitwidth = intConst->GetType()->SizeInBits();

			if(isLeft) {
				// (a >> C) << C -> a & (-1 << C)
				// We need to apply the mask because the first 'C' bits need to be 0.
				auto intKind = intConst->GetType()->GetSubtype();
				int maskBits = IA::Sub(-1, shiftAmount->Value(), intKind);
				
                __int64 mask = IA::ValueFromBitCount(maskBits);
				auto maskOp = irGen_.GetIntConst(intConst->GetType(), mask);

				auto andOp = GetTemporary(op);
				irGen_.GetAnd(definingInstr->LeftOp(), maskOp, andOp);
				return LOG(andOp);
			}
			else {
				// (a >> C1) >> C2 -> a >> (C1 + C2).
				// If the amount is larger than the bitwidth we can return 0 directly.
				__int64 result = IA::Add(intConst, shiftAmount);

				if(result >= bitwidth) {
					return LOG(GetZeroInt(intConst));
				}
				else {
					// We modify the instruction directly, by replacing 'C1'.
					auto amountOp = irGen_.GetIntConst(op->GetType(), result);
					definingInstr->SetRightOp(amountOp);
					return LOG(definingInstr->ResultOp());
				}
			}
		}
		case Instr_And:
		case Instr_Or:
		case Instr_Xor: {
			// We modify the instruction directly, by replacing both operands.
			definingInstr->SetLeftOp(SimplifyShiftWithConst(definingInstr->LeftOp(), 
													        shiftAmount, isLeft));
			definingInstr->SetRightOp(SimplifyShiftWithConst(definingInstr->RightOp(), 
													         shiftAmount, isLeft));
			return LOG(definingInstr->ResultOp());						   
		}
		default: {
			// If this is reached there is an error in 'CanSimplifyShiftWithConst'.
			DebugValidator::Unreachable();
			return nullptr;
		}
	}

    return nullptr;
}

} // namespace Optimization