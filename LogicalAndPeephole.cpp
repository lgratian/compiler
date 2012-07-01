// LogicalAndPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 'and'.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

Operand* Peephole::HandleAnd(Operand* opA, Operand* opB, Block* block,
                             bool constMoveAllowed) {
	Operand* a;
	Operand* b;
	Operand* x;
	IntConstant* C;
	IntConstant* C1;
	IntConstant* C2;

	// We use the constant evaluator even if not both operands are constant,
	// because it also eliminates very simple cases, like 'a & 0 -> 0'.
	if(auto result = folder_.FoldBinary(Instr_And, opA, opB, block)) {
		return LOG(result);
	}

	// If one of the operands is a constant make it the the right operand.
    // This is also an improvement for CPUs that don't support a constant on the left.
	bool moved = MoveConstantToRight(opA, opB);

    // Try to simplify using information about the bits 
    // that are definitely zero or one.
    // (a << 4) & 2 -> 0
    if(auto result = HandleAndOperandInfo(opA, opB, block)) {
        return result;
    }

    // Try to simplify using associativity/commutativity laws.
    if(auto result = HandleAssociative(Instr_And, opA, opB, block)) {
        return result;
    }

    // Try to simplify using distributivity laws.
    if(auto result = HandleDistributive(Instr_And, opA, opB, block)) {
        return result;
    }

	if(auto result = HandleAndSimple(opA, opB, block)) {
        return result;
    }

	// The following two cases may be tested in an 'if' or 'switch',
	// and some CPU's use a flag to test if a result is 0, so the transformed
	// sequence will be cheaper for them.
	// (a ^ 1) & 1 -> (a & 1) == 0
	// ~a & 1 -> (a & 1) == 0
	// Note that this works for any power of two, not just 1.
	if(Match<XorInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) && 
	   MatchIC(&C2, block)(opB) &&
       (C1 == C2) && IA::IsPowerOfTwo(C1)) {
		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(a, C1, andOp);

		return LOG(CreateCompare(andOp, C1, Order_Equal, 
                                 opA->GetType(), true));
	}

	// Note that this works for any power of two, not just 1.
	if(IsLogicalNot(opA, &a) && MatchIC(&C, block)(opB) && 
       IA::IsPowerOfTwo(C)) {
		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(a, C, andOp);

		return LOG(CreateCompare(andOp, C, Order_Equal, 
                                 opA->GetType(), true));
	}

	// Handle cases which involve constants.
	if(auto result = HandleAndOneConst(opA, opB, block)) {
		return result;
	}

	if(auto result = HandleAndTwoConst(opA, opB, block)) {
		return result;
	}

	// (a >> x) & (b >> x)  -> (a & b) >> x, for both 'shr' and 'ushr'.
	if((Match<ShrInstr>(MatchAny(&a), MatchAny(&x))(opA) &&
		Match<ShrInstr>(MatchAny(&b), MatchOp(x))(opB))  
        ||
	   (Match<UshrInstr>(MatchAny(&a), MatchAny(&x))(opA) &&
		Match<UshrInstr>(MatchAny(&b), MatchOp(x))(opB))) {
        // We have a match.
		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(a, b, andOp);
		auto shrOp = GetTemporary(opA);

		if(opA->DefiningInstrIs<ShrInstr>()) {
			irGen_.GetShr(andOp, x, shrOp);
		}
		else irGen_.GetUshr(andOp, x, shrOp);
		
        return LOG(shrOp);
	}

	// Try to eliminate useless casts, which are introduced 
    // by the frontend frequently.
	// sext(a) & sext(b) -> sext(a & b)
	// zext(a) & zext(b) -> zext(a & b)
	if((MatchConversion<SextInstr>(MatchAny(&a))(opA) && 
		MatchConversion<SextInstr>(MatchAny(&b))(opB)) 
        ||
	   (MatchConversion<ZextInstr>(MatchAny(&a))(opA) && 
		MatchConversion<ZextInstr>(MatchAny(&b))(opB))) {
		// Do the transformation only if the source type
        // is the same for both operands.
		if(a->GetType() == b->GetType()) {
			auto andOp = GetTemporary(a);
			irGen_.GetAnd(a, b, andOp);
			auto extOp = GetTemporary(opA);

			if(opA->DefiningInstrIs<SextInstr>()) {
				irGen_.GetSext(andOp, opA->GetType(), extOp);
			}
			else irGen_.GetZext(andOp, opA->GetType(), extOp);
			
            return LOG(extOp);
		}
	}

	// Try to simplify 'and's that involve integer compare instructions.
	if(auto result = HandleAndIntCmp(opA, opB, block)) {
		return result;
	}

	// Try to simplify 'and's that involve float compare instructions.
	if(auto result = HandleAndFloatCmp(opA, opB)) {
		return result;
	}

    // trunc(a & C1) & C2 -> ((trunc a) & trunc(C1)) & C2
    if(MatchConversion<TruncInstr>(Match<AndInstr>(MatchAny(&a), 
                                             MatchIC(&C1)))(opA) &&
       MatchIC(&C2)(opB)) {
        auto castType = opA->GetType()->As<IntegerType>();
        auto constantOp = irGen_.GetIntConst(castType, IA::Trunc(C1, castType));

        auto truncOp = irGen_.GetTemporary(castType);
        irGen_.GetTrunc(a, castType, truncOp);
        
        auto andOp1 = irGen_.GetTemporary(castType);
        irGen_.GetAnd(truncOp, constantOp, andOp1);

        auto andOp2 = irGen_.GetTemporary(castType);
        irGen_.GetAnd(andOp1, C2, andOp2);
        return LOG(andOp2);
    }

    // Try to simplify an 'and' of two integer comparisons like the following:
    // ((a & b) == 0) & ((a & d) == 0) -> (a & (b | d)) == 0
    // This reduces the number of operations, and 'b' and 'd' are often
    // constants, leading to further simplifications.
    if(opA->DefiningInstruction() && 
       opA->DefiningInstruction()->IsComparison() &&
       opB->DefiningInstruction() && 
       opB->DefiningInstruction()->IsComparison()) {
        return HandleAndCmpCmp(opA, opB, block);
    }

    // Try to simplify '((a & 8) != 0) & ((a & 4) != 0) -> (a & (8|4)) != 0'.
	if(auto result = HandleAndBit(opA, opB, block)) {
		return result;
	}

	// Check if we are adding at least one 'quest' instruction result.
    if(auto result = HandleOperationOnQuest(Instr_And, opA, opB)) {
        return result;
    }

    // Check if we can simplify by applying the operation
    // one each incoming value of a 'phi' instruction.
    if(auto result = HandleInstructionOnPhi(Instr_And, opA, opB)) {
        return result;
    }

    // Returns a new instruction if the constant was moved to the right.
    if(moved && constMoveAllowed) {
        auto result = GetTemporary(opA);
        irGen_.GetAnd(opA, opB, result);
        return LOG(result);
    }

    // If we couldn't do any simplification try at least to move
    // the constant "outside" (to the right):
    // (a & C) & b -> (a & b) & C
    if(Match<AndInstr>(MatchAny(&a), MatchIC(&C, block))(opA) && 
       (b = opB)) {
        auto andOp1 = GetTemporary(a);
        irGen_.GetAnd(a, b, andOp1);

        auto andOp2 = GetTemporary(a);
        irGen_.GetAnd(andOp1, C, andOp2);
        return LOG(andOp2);
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAndOperandInfo(Operand* opA, Operand* opB, Block* block) {
    // Try to simplify using information about the bits 
    // that are definitely zero and one.
    // (a << 4) & 2 -> 0
    unsigned __int64 opAZeroBits;
    unsigned __int64 opBZeroBits;
    opInfo_.EstimateZeroBits(opA, opAZeroBits);
    opInfo_.EstimateZeroBits(opB, opBZeroBits);

    // Test if all bits that may be one in 'opB' 
    // have a correspondent in the bits that may be one in 'opA'.
    if((~opAZeroBits & ~opBZeroBits) == 0) {
        return LOG(GetZeroInt(opA));
    }

    // If all bits in one of the operands are one
    // the second operand can be chosen.
    __int64 mask = IA::GetMinusOneMask(opA->GetType()->As<IntegerType>());
    unsigned __int64 opAOneBits;
    unsigned __int64 opBOneBits;

    opInfo_.EstimateOneBits(opA, opAOneBits);
    opInfo_.EstimateOneBits(opB, opBOneBits);

    if((opAOneBits == mask) && (opBOneBits == mask)) {
        // -1 & -1 -> -1
        return LOG(GetMinusOneInt(opA));
    }
    else if(opAOneBits == mask) {
        // -1 & b -> b
        opInfo_.EstimateOneBits(opA, opAOneBits);
        return LOG(opB);
    }
    else if(opBOneBits == mask) {
        // a & -1 -> a
        opInfo_.EstimateOneBits(opB, opBOneBits);
        return LOG(opA);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAndSimple(Operand* opA, Operand* opB, Block* block,
                                   bool createAllowed) {
    Operand* a;
    Operand* b;

    // a & a -> a
    if(AreEqual(opA, opB, block)) {
        return LOG(opA);
    }

    // a & ~a -> 0
    // ~a & a -> 0
    if((IsLogicalNot(opB, &a) && (a == opA)) ||
       (IsLogicalNot(opA, &a) && (a == opB))) {
        return LOG(GetZeroInt(opA));
    }

    // a & -1 -> a
    if(IsMinusOneInt(opB, block)) {
        return LOG(opA);
    }

    // a & (a | b) -> a
    // a & (b | a) -> a
    if(Match<OrInstr>(MatchAny(&a), MatchAny(&b))(opB) &&
        ((a == opA) || (b == opA))) {
        return LOG(opA);
    }

    // (a | b) & a -> a
    // (b | a) & a -> a
    if(Match<OrInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
        ((a == opB) || (b == opB))) {
        return LOG(opB);
    }

    // a & 1 -> a, if 'a' has boolean type
    // a & 0 -> 0, if 'a' has boolean type
    if(IsOneInt(opB, block) && opA->IsBoolean()) {
        return LOG(opA);
    }
    else if(IsZeroInt(opB, block) && opA->IsBoolean()) {
        return LOG(GetBool(0, opA));
    }

    // NOTE: All the following simplification are done only if
    // we're allowed to create new instructions.
    if(createAllowed == false) {
        return nullptr;
    }

    // ~a & ~b -> ~(a | b) (De Morgan's law).
	if(IsLogicalNot(opA) && IsLogicalNot(opB)) {
		auto orOp = GetTemporary(opA);
		irGen_.GetOr(opA, opB, orOp);
		
        auto xorOp = GetTemporary(opA);
		irGen_.GetXor(orOp, GetMinusOneInt(opA), xorOp);
		return LOG(xorOp);
	}

	// ~a && (a | b) -> ~a && b
    // (a || b) && ~a -> ~a && b
	if((IsLogicalNot(opA) && 
        Match<OrInstr>(MatchOp(a), MatchAny(&b))(opB)) 
        ||
	   (Match<OrInstr>(MatchOp(a), MatchAny(&b))(opB) &&
	    Match<XorInstr>(MatchAny(&a), MatchInt(-1, block))(opA))) {
		auto xorOp = GetTemporary(opA);
		irGen_.GetXor(a, GetMinusOneInt(opA), xorOp);
		
        auto andOp = GetTemporary(opA);
		irGen_.GetAnd(xorOp, b, andOp);
		return LOG(andOp);
	}

	// a && (~a | b) -> a && b
    // (~a | b) && a
	if((Match<OrInstr>(Match<XorInstr>(MatchAny(&a), MatchInt(-1, block)), 
                       MatchAny(&b))(opB) && 
	   (a == opA)) 
       ||
	   (Match<OrInstr>(Match<XorInstr>(MatchAny(&a), MatchInt(-1, block)),
                       MatchAny(&b))(opA) &&
	   (a == opB))) {
		auto temp = GetTemporary(opA);
		irGen_.GetAnd(a, b, temp);
		return LOG(temp);
	}

    // ~(a & b) & (a | b) -> a ^ b
	if(Match<XorInstr>(Match<AndInstr>(MatchAny(&a), MatchAny(&b)), 
                       MatchInt(-1, block))(opA) &&
	   (Match<OrInstr>(MatchOp(a), MatchOp(b))(opB) || 
		Match<OrInstr>(MatchOp(a), MatchOp(b))(opB))) {
		auto xorOp = GetTemporary(opA);
		irGen_.GetXor(a, b, xorOp);
		return LOG(xorOp);
	}

	// (a | b) & ~(a & b) -> a ^ b
	if(Match<OrInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
	   (Match<XorInstr>(Match<AndInstr>(MatchOp(a), MatchOp(b)), 
                        MatchInt(-1, block))(opB) ||
		Match<XorInstr>(Match<AndInstr>(MatchOp(a), MatchOp(b)), 
                        MatchInt(-1, block))(opB))) {
		auto xorOp = GetTemporary(opA);
		irGen_.GetXor(a, b, xorOp);
		return LOG(xorOp);
	}

	// a & (~a | b) -> a & b
	if(Match<OrInstr>(Match<XorInstr>(MatchAny(&a), MatchInt(-1, block)), 
                      MatchAny(&b))(opB) &&
	   (a == opA)) {
		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(a, b, andOp);
		return LOG(andOp);
	}

	// a & (a ^ b) -> a & ~b. This is a simplification for most targets.
	if(Match<XorInstr>(MatchAny(&a), MatchAny(&b))(opB) && (a == opA)) {
		auto xorOp = GetTemporary(opA);
		irGen_.GetXor(b, GetMinusOneInt(opB), xorOp);

		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(a, xorOp, andOp);
		return LOG(andOp);
	}

	// ~a & (a | ~b) -> a | b
	if(IsLogicalNot(opA, &a) && Match<OrInstr>(MatchOp(a), 
	   Match<XorInstr>(MatchAny(&b), MatchInt(-1)))(opB)) {
		auto orOp = GetTemporary(opA);
		irGen_.GetOr(a, b, orOp);
		return LOG(orOp);
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAndCmpCmp(Operand* opA, Operand* opB, Block* block) {
    Operand* a;
    Operand* b;
    Operand* d;
    auto cmpInstrA = static_cast<CmpInstrBase*>(opA->DefiningInstruction());
    auto cmpInstrB = static_cast<CmpInstrBase*>(opB->DefiningInstruction());

    // We can't handle floating comparisons.
    if(cmpInstrA->IsFcmp() || cmpInstrB->IsFcmp()) {
        return nullptr;
    }

    // ((a & b) == 0) & ((a & d) == 0) -> (a & (b | d)) == 0
    if(MatchCmpCmpOperands(cmpInstrA, cmpInstrB, CmpCmp_Zero, 
                           true /* expectAnd */, a, b, d)) {
        auto orOp = GetTemporary(a);
        irGen_.GetOr(b, d, orOp);

        auto andOp = GetTemporary(a);
        irGen_.GetAnd(a, orOp, andOp);
        
        return LOG(CreateCompare(andOp, GetZeroInt(a), Order_Equal, 
                                 cmpInstrA->ResultOp()->GetType(), 
                                 cmpInstrA->IsUcmp()));
    }

	// ((a & b) == b) & ((a & d) == d) -> (a & (b | d)) == (b | d)
    if(MatchCmpCmpOperands(cmpInstrA, cmpInstrB, CmpCmp_Right, 
                           true /* expectAnd */, a, b, d)) {
        auto orOp = GetTemporary(a);
        irGen_.GetOr(b, d, orOp);

        auto andOp = GetTemporary(a);
        irGen_.GetAnd(a, orOp, andOp);
        
        return LOG(CreateCompare(andOp, orOp, Order_Equal, 
                                 cmpInstrA->ResultOp()->GetType(), 
                                 cmpInstrA->IsUcmp()));
    }

	// ((a & b) == a) & ((a & d) == a) -> (a & (b & d)) == a
    if(MatchCmpCmpOperands(cmpInstrA, cmpInstrB, CmpCmp_Left, 
                           true /* expectAnd */, a, b, d)) {
        auto andOp1 = GetTemporary(a);
        irGen_.GetAnd(b, d, andOp1);
        
        auto andOp2 = GetTemporary(a);
        irGen_.GetAnd(a, andOp1, andOp2);
        
        return LOG(CreateCompare(andOp2, a, Order_Equal, 
                                 cmpInstrA->ResultOp()->GetType(), 
                                 cmpInstrA->IsUcmp()));
    }
    
    // ((a | b) == b) & ((a | d) == d) -> (a | (b & d)) == (b & d)
    if(MatchCmpCmpOperands(cmpInstrA, cmpInstrB, CmpCmp_Right, 
                           false /* expectAnd */, a, b, d)) {
        auto andOp = GetTemporary(a);
        irGen_.GetAnd(b, d, andOp);
        
        auto orOp = GetTemporary(a);
        irGen_.GetOr(a, andOp, orOp);
        
        return LOG(CreateCompare(orOp, andOp, Order_Equal, 
                                 cmpInstrA->ResultOp()->GetType(), 
                                 cmpInstrA->IsUcmp()));
    }

    // ((a | b) == a) & ((a | d) == a) -> (a | (b | d)) == a
    if(MatchCmpCmpOperands(cmpInstrA, cmpInstrB, CmpCmp_Left, 
                           false /* expectAnd */, a, b, d)) {
        auto orOp1 = GetTemporary(a);
        irGen_.GetOr(b, d, orOp1);
        
        auto orOp2 = GetTemporary(a);
        irGen_.GetOr(a, orOp1, orOp2);
        
        return LOG(CreateCompare(orOp2, a, Order_Equal, 
                                 cmpInstrA->ResultOp()->GetType(), 
                                 cmpInstrA->IsUcmp()));
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::MatchCmpCmpOperands(CmpInstrBase* cmpInstrA, CmpInstrBase* cmpInstrB,
                                   CmpCmpComparand comp, bool expectAnd,
                                   Operand*& a, Operand*& b, Operand*& d) {
    // Verify if one of the following patterns are matched.
    // ((a & b) == 0/a) & ((a & d) == 0/d)
    // ((a | b) == 0/a) & ((a | d) == 0/d) (when 'expectAnd' is false)
    // We can't handle floating comparisons.
    if(cmpInstrA->IsFcmp() || cmpInstrB->IsFcmp()) {
        return false;
    }
    
    // Only an 'equal' comparison to 0 can be optimized here.
    if((cmpInstrA->IsEqual() && 
        cmpInstrB->IsEqual()) == false) {
        return false;
    }

    if(expectAnd) {
        // The left operands must be 'and' instructions.
        auto andInstrA = cmpInstrA->LeftOp()->DefiningInstrAs<AndInstr>();
        auto andInstrB = cmpInstrB->LeftOp()->DefiningInstrAs<AndInstr>();
        if((andInstrA && andInstrB) == false) {
            return false;
        }

        // Try to identify 'a', 'b', and 'd'.
        // Make sure that the equal operands are on the left side.
        if(andInstrA->LeftOp() == andInstrB->RightOp()) {
            andInstrB->SwapOperands();
        }
        else if(andInstrA->RightOp() == andInstrB->LeftOp()) {
            andInstrA->SwapOperands();
        }
        else if(andInstrA->RightOp() == andInstrB->RightOp()) {
            andInstrA->SwapOperands();
            andInstrB->RightOp();
        }

        if(andInstrA->LeftOp() != andInstrB->RightOp()) {
            // The conditions are not met.
            return false;
        }

        a = andInstrA->LeftOp();
        b = andInstrA->RightOp();
        d = andInstrB->RightOp();
    }
    else {
        // The left operands must be 'or' instructions.
        auto orInstrA = cmpInstrA->LeftOp()->DefiningInstrAs<OrInstr>();
        auto orInstrB = cmpInstrB->LeftOp()->DefiningInstrAs<OrInstr>();
        if((orInstrA && orInstrB) == false) {
            return false;
        }

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
            return false;
        }

        a = orInstrA->LeftOp();
        b = orInstrA->RightOp();
        d = orInstrB->RightOp();
    }

    // Now test the right operands of the comparisons.
    if(comp == CmpCmp_Zero) {
        if((cmpInstrA->RightOp()->IsZeroInt() &&
            cmpInstrB->RightOp()->IsZeroInt()) == false) {
            return false;
        }
    }
    else if(comp == CmpCmp_Left) {
        // 'a' should be used by both comparisons.
        if(((cmpInstrA->RightOp() == a) &&
            (cmpInstrB->RightOp() == a)) == false) {
            return false;
        }
    }
    else  {
        // 'b' should be used by the left comparison, 'd' by the right one.
        if(((cmpInstrA->RightOp() == b) &&
            (cmpInstrB->RightOp() == d)) == false) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAndTwoConst(Operand* opA, Operand* opB, Block* block) {
	// Test for cases like '(a OP C1) & C2'.
	Operand* a;
	IntConstant* C1;
	IntConstant* C2;

	// If the second operand is not an integer constant we have nothing to do.
	C2 = opB->As<IntConstant>();
	if(C2 == nullptr) {
        return nullptr;
    }

	// Test for '(a | C1) & C2'.
	if(Match<OrInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) &&
	   opA->HasSingleUser()) {
		__int64 C1C2 = IA::And(C1, C2);

		// Try to distribute 'C2' over the left side; this could create more 
		// opportunities, because the '&' in 'C1 & C2' could "kill" some bits.
		// (a | C1) & C2 -> (a | (C1 & C2)) & C2, if '(C1 & C2) != C1'.
		if(C1C2 != C1->Value()) {
			auto constantOp = folder_.FoldBinary(Instr_And, C1, C2, block);
			auto orOp = GetTemporary(opA);
			irGen_.GetOr(a, constantOp, orOp);

			auto andOp = GetTemporary(opA);
			irGen_.GetAnd(orOp, C2, andOp);
			return LOG(andOp);
		} 
	}

	// Test for '(a ^ C1) & C2'.
	if(Match<XorInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) &&
	   opA->HasSingleUser()) {
		// (a ^ C1) & C2 -> (a & C2) ^ (C1 & C2)
		// This reduces the number of set bits in the right part.
		auto andOp1 = GetTemporary(opA);
		irGen_.GetAnd(a, C2, andOp1);
		auto andOp2 = folder_.FoldBinary(Instr_And, C1, C2, block);

		auto xorOp = GetTemporary(opA);
		irGen_.GetXor(andOp1, andOp2, xorOp);
		return LOG(xorOp);
	}

	// Test for '(a << C1) & C2'. If shifting by 'C1' sets to 0 all the bits
	// that are tested by the mask 'C2' then we can eliminate the test.
	if(Match<ShlInstr>(MatchAny(&a), MatchIC(&C1, block))(opA)) {
		// Consider '(a << 3) & 0xFFF8'. 0xFFF8 masks the lowest 3 bits,
		// but they are always set to 0, because of the left shift.
		auto intKind = C1->GetType()->GetSubtype();
		__int64 shlMask = IA::ValueFromBitCount(C1->Value());
		shlMask = IA::Xor(shlMask, -1, intKind);

		if(IA::AreEqual(shlMask, C2->Value(), intKind)) {
			return LOG(opA);
		}
	}

	// Test for '(a >> C1) & C2'. If shifting by 'C1' sets to 0 all the bits
	// that are tested by the mask 'C2' then we can eliminate the test.
	if(Match<UshrInstr>(MatchAny(&a), MatchIC(&C1, block))(opA)) {
		// Consider '(a >> 8) & 0xFFF'. 0xFFF masks the highest 8 bits,
		// but they are always set to 0, because of the right shift
        // by the same amount.
		auto intKind = C1->GetType()->GetSubtype();
		int intBits = C1->GetType()->SizeInBits();
		__int64 shlMask = IA::ValueFromBitCount(intBits - C1->Value());

		if(IA::AreEqual(shlMask, C2->Value(), intKind)) {
			return LOG(opA);
		}
	}  

	// (a + C1) & C2 -> a & C2, if 'C2' has a single bit set and
    // all bits below it in 'C1' are zero (it means that 'C1' can't
    // add any bits to 'a', so we can test 'a' directly). 
    // This happens often in bitfield accesses.
	if(Match<AddInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) && 
       opA->HasSingleUser()) {
		// C2 is a single bit mask only if it's a power of two.
		auto intKind = C2->GetType()->GetSubtype();
		__int64 mask = IA::Sub(C2->Value(), 1, intKind);

		// We also need to make sure that no bits below the one in the mask
		// is set in the added constant.
		if(IA::IsPowerOfTwo(C2) && (IA::And(C1->Value(), mask, intKind) == 0)) {
			// If the bit in the added constant is not set then we can
			// eliminate the addition completely.
			int bitPos = IA::Log2(C2);

			if(IA::IsBitSet(bitPos, C1) == false) {
				// (a + C1) & C2 -> a & C2
				auto temp = GetTemporary(opA);
				irGen_.GetAnd(a, C2, temp);
				return LOG(temp);
			}
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAndOneConst(Operand* opA, Operand* opB, Block* block) {
	// Test for cases like '((a OP C1) OP b) & C2'.
	Operand* a;
	Operand* b;
	IntConstant* C1;
	IntConstant* C2;
	bool match = false;

	// If the second operand is not an integer constant we have nothing to do.
	C2 = opB->As<IntConstant>();
	if(C2 == nullptr) {
        return nullptr;
    }
	
	// ((a & C1) + b) & C2 -> (a + b) & C2, if 'C1 & C2 == C2'.
	// This means that 'C2' is "included" in 'C1',
    // so it's enough to mask only with 'C2'.
	if(Match<AddInstr>(Match<AndInstr>(MatchAny(&a), MatchIC(&C1, block)), 
					   MatchAny(&b))(opA) && 
       (IA::And(C1, C2) == 0)) {
		match = true;
	}
	// ((a | C1) + b) & C2 -> (a + b) & C2, if 'C1 & C2 == 0'.
	// This means that any bit added by 'C1' is eliminated by 'C2'.
	else if(Match<AddInstr>(Match<OrInstr>(MatchAny(&a), MatchIC(&C1, block)), 
							MatchAny(&b))(opA) && 
            (IA::And(C1, C2) == 0)) {
		match = true;
	}
	// ((a ^ C1) + b) & C2 -> (a + b) & C2, if 'C1 & C2 == 0'.
	// This means that any bit added by 'C1' is eliminated by 'C2'.
	else if(Match<AddInstr>(Match<XorInstr>(MatchAny(&a), MatchIC(&C1, block)), 
							MatchAny(&b))(opA) && 
            (IA::And(C1, C2) == 0)) {
		match = true;
	}

	if(match) {
		auto sumOp = GetTemporary(opA);
		irGen_.GetAdd(a, b, sumOp);
		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(sumOp, C2, andOp);
		match = false;
		return andOp;
	}

	// The same as above, but for subtraction.
	// ((a & C1) - b) & C2 -> (a - b) & C2, if 'C1 & C2 == C2'.
	if(Match<SubInstr>(Match<AndInstr>(MatchAny(&a), MatchIC(&C1, block)), 
	                   MatchAny(&b))(opA) && 
       IA::AreEqual(IA::And(C1, C2), C2->Value(), C2->GetType()->GetSubtype())) {
		match = true;
	}
	// ((a | C1) - b) & C2 -> (a - b) & C2, if 'C1 & C2 == 0'.
	else if(Match<SubInstr>(Match<OrInstr>(MatchAny(&a), MatchIC(&C1, block)), 
			                MatchAny(&b))(opA) && 
            IA::AreEqual(IA::And(C1, C2), 0, C2->GetType()->GetSubtype())) {
		match = true;
	}
	// ((a ^ C1) - b) & C2 -> (a - b) & C2, if 'C1 & C2 == 0'.
	else if(Match<SubInstr>(Match<XorInstr>(MatchAny(&a), MatchIC(&C1, block)), 
			                MatchAny(&b))(opA) && 
            IA::AreEqual(IA::And(C1, C2), 0, C2->GetType()->GetSubtype())) {
		match = true;
	}

	if(match) {
		auto subOp = GetTemporary(opA);
		irGen_.GetSub(a, b, subOp);
		auto andOp = GetTemporary(opA);
		irGen_.GetAnd(subOp, C2, andOp);
		return LOG(andOp);
	}

	// If the mask tests only the least significant bit we can do some simplifications.
	// (1 << a) & 1 -> a == 0
	// (1 >> a) & 1 -> a == 0
	if((Match<ShlInstr>(MatchInt(1, block), MatchAny(&a))(opA) ||
		Match<ShrInstr>(MatchInt(1, block), MatchAny(&a))(opA)) &&
        MatchInt(1, block)(opB)) {
		auto cmpOp = GetTemporary(opA);
		irGen_.GetUcmp(Order_Equal, a, GetZeroInt(opA), cmpOp);
		return LOG(cmpOp);
	}

	// Test for a zero extension that is masked. If the mask covers the
	// range from which we extend completely, it's unnecessary. For example,
	// (zext int16, int32) & 0xFFFF -> zext int16, int32
	if(auto zextInstr = opA->DefiningInstrAs<ZextInstr>()) {
		auto fromType = zextInstr->TargetOp()->GetType()->As<IntegerType>();
		auto toType = zextInstr->CastType()->As<IntegerType>();
		
        int fromBits = IA::TypeBits(fromType->GetSubtype());
		int fromBitsValue = IA::ValueFromBitCount(fromBits);

		// Note that we allow values that are larger than 'fromBitsValue',
		// as long as they have the right number of set bits.
		if((fromBitsValue & C2->Value()) == fromBitsValue) {
			// We can make the simplification.
			return LOG(opA);
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAndIntCmp(Operand* opA, Operand* opB, Block* block) {
	// Test for cases where both operands are integer compare instructions,
	// like '(a < 4) & (b > 5)'.
	if(((opA->DefiningInstrIs<CmpInstr>() &&
		 opB->DefiningInstrIs<CmpInstr>()) == false) &&
	   ((opA->DefiningInstrIs<UcmpInstr>() && 
	     opB->DefiningInstrIs<UcmpInstr>()) == false) &&
	   ((opA->DefiningInstrIs<FcmpInstr>() && 
	     opB->DefiningInstrIs<FcmpInstr>()) == false)) {
		// None of the operands is a comparison instruction.
		return nullptr;
	}

	CmpInstrBase* leftCmp = static_cast<CmpInstrBase*>
                                (opA->DefiningInstruction());
	CmpInstrBase* rightCmp = static_cast<CmpInstrBase*>
                                (opB->DefiningInstruction());

	// Test for the case in which the operands of both compares 
    // are the same, the (possible) difference being only the comparison order.
	// (a < 5)  & (a < 5)  -> a < 5
	// (a >= 7) & (a <= 7) -> a == 7
	if((leftCmp->LeftOp() == rightCmp->RightOp()) && 
	   (leftCmp->RightOp() == rightCmp->LeftOp())) {
		// We have something like '(a < b) & (b > a); 
        // change the order for the right part.
		rightCmp->InvertOrder(true, false /* invertEquality */);
	}
	
	if((leftCmp->LeftOp() == rightCmp->RightOp()) && 
	   (leftCmp->RightOp() == rightCmp->RightOp())) {
		// Combine the orders of the two compares and replace them with a compare
		// having the new order. If the result of the test is always 0, use it
		// instead of doing an useless comparison.
		bool orderFalse = false;
		OrderType order = CombineOrdersAnd(leftCmp->Order(), 
                                           rightCmp->Order(), orderFalse);
		
		if(orderFalse) {
			return LOG(GetBool(false, opA));
		}
		else return LOG(CreateCompare(leftCmp->LeftOp(), rightCmp->RightOp(), 
                                      order, opA->GetType(), leftCmp->IsUcmp()));
	}

	// Test for comparisons that involve the same right operand.
	if((leftCmp->Order() == rightCmp->Order()) && 
	   (leftCmp->RightOp() == rightCmp->RightOp())) {
		return HandleAndIntCmpSameConst(leftCmp, rightCmp, block);
	}

	// Test for comparisons that target the same operand, 
    // but using a different constant.
	// (a < 2) & (a < 4) -> a < 2
	// (a < 4) & (a > 7) -> 0 (false)
	if(leftCmp->LeftOp() == rightCmp->LeftOp()) {
        auto leftConst = AsIntConstant(leftCmp->RightOp(), block);
        auto rightConst = AsIntConstant(rightCmp->RightOp(), block);

        if(leftConst && rightConst) {
            return HandleAndIntCmpConst(leftCmp, rightCmp, 
                                        leftConst, rightConst);
        }
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAndIntCmpSameConst(CmpInstrBase* leftCmp, 
											CmpInstrBase* rightCmp, Block* block) {
    IntConstant* C;

    if(leftCmp->IsEqual() && 
       leftCmp->RightOp()->IsZeroInt()) {
        // (a == 0) & (b == 0) -> (a | b) == 0
        auto orOp = GetTemporary(leftCmp->LeftOp());
		irGen_.GetOr(leftCmp->LeftOp(), rightCmp->LeftOp(), orOp);

        return LOG(CreateCompare(orOp, leftCmp->RightOp(), leftCmp->Order(),
							     leftCmp->ResultOp()->GetType(), true));
    }
    else if(leftCmp->IsCmp() && leftCmp->IsLess() && 
            leftCmp->RightOp()->IsZeroInt()) {
        // (a < 0) & (b < 0) -> (a & b) < 0
        auto andOp = GetTemporary(leftCmp->LeftOp());
		irGen_.GetAnd(leftCmp->LeftOp(), rightCmp->LeftOp(), andOp);

		return LOG(CreateCompare(andOp, leftCmp->RightOp(), leftCmp->Order(),
							     leftCmp->ResultOp()->GetType(), true));
    }
    else if(leftCmp->IsCmp() && leftCmp->IsGreater() && 
            leftCmp->RightOp()->IsMinusOneInt()) {
        // (a > -1) && (b > -1) -> (a | b) > -1
        auto orOp = GetTemporary(leftCmp->LeftOp());
		irGen_.GetOr(leftCmp->LeftOp(), rightCmp->LeftOp(), orOp);

        return LOG(CreateCompare(orOp, leftCmp->RightOp(), leftCmp->Order(),
							     leftCmp->ResultOp()->GetType(), true));
    }
    else if(leftCmp->IsUcmp() && leftCmp->IsLess() && 
            MatchIC(&C, block)(leftCmp->RightOp())) {
	    // (a < 8) & (b < 8) -> (a | b) < 8
		// The constant needs to be a power of two.
		if(IA::IsPowerOfTwo(C)) {
			auto orOp = GetTemporary(leftCmp->LeftOp());
			irGen_.GetOr(leftCmp->LeftOp(), rightCmp->LeftOp(), orOp);

			return LOG(CreateCompare(orOp, C, leftCmp->Order(),
							         leftCmp->ResultOp()->GetType(), true));
		}
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAndIntCmpConst(CmpInstrBase* leftCmp, CmpInstrBase* rightCmp,
										IntConstant* leftConst, IntConstant* rightConst) {
	// To make testing a bit easier, move the largest constant on the right.
	// (a < 5) & (a < 3) -> (a < 3) & (a < 5)
	if(IA::IsLarger(leftConst, rightConst)) {
        std::swap(leftCmp, rightCmp);
        std::swap(leftConst, rightConst);
	}

	switch(leftCmp->Order()) {
	case Order_Equal: {
		switch(rightCmp->Order()) {
		case Order_NotEqual: // (a == 8) & (a != 9) -> a == 8
		case Order_Less: {   // (a == 8) & (a < 9)  -> a == 8
			return LOG(leftCmp->ResultOp());
		}
		case Order_Equal:     // (a == 8) & (a == 9) -> false
        case Order_Greater: { // (a == 8) & (a  > 9) -> false
			return LOG(GetBool(false, leftCmp->LeftOp()));
		}
		}
		break;
	}
	case Order_NotEqual: {
		switch(rightCmp->Order()) {
		case Order_Less: { // (a != 8) & (a < 9) -> a < 8
			// We can to this transformation only if the right value
			// is the left one plus 1.
			if(IsLeftRightMinusOne(leftConst, rightConst)) {
				return LOG(CreateCompare(leftCmp->LeftOp(), leftConst, 
                                         Order_Less, leftCmp->ResultOp()->GetType(), 
									     leftCmp->IsUcmp()));
			}
            break;
		}
		case Order_Equal:     // (a != 8) & (a == 10) -> a == 10
		case Order_Greater: { // (a != 8) & (a  > 10) -> a > 10
			return LOG(rightCmp->ResultOp());
		}
		case Order_NotEqual: {
			// (a != 8) & (a != 9) -> (a - 8) > 1
			// This transformation is safe only if the difference between
			// the two constants is 1. If 'a' is larger than 9, then 
			// '(a - 8) > 1' is true. If it's smaller than 8, 'a - 8' will wrap
			// around 0 and become a very large number, so the result is still 'true'.
			if(IsLeftRightMinusOne(leftConst, rightConst) == false) {
				return nullptr;
			}

			// Note that we must use an 'ucmp', even if the original comparisons
			// use the signed 'cmp'. Not doing so would not enable
			// the "wrap around" behavior, which is mandatory in this case.
			auto leftOp = leftCmp->LeftOp();
			auto subOp = GetTemporary(leftOp);
			irGen_.GetSub(leftOp, leftConst, subOp);

			return LOG(CreateCompare(subOp, GetOneInt(leftOp), Order_Greater,
							         leftCmp->ResultOp()->GetType(), 
                                     true /* isUcmp */));
		}
		}
		break;
	}
	case Order_Less: {
		switch(rightCmp->Order()) {
		case Order_Equal: { // (a < 8) & (a == 9) -> false
            return LOG(GetBool(false, leftCmp->ResultOp()));
		}
		case Order_Greater: { // (a < 8) & (a > 9)  -> false
			// This is true only if both comparisons are of the same type.
			if(leftCmp->IsUcmp() == leftCmp->IsUcmp()) {
				return LOG(GetBool(false, leftCmp->ResultOp()));
			}
            break;
		}
		case Order_NotEqual: { // (a < 8) & (a != 10) -> a < 8
			return LOG(leftCmp->ResultOp());
		}
		case Order_Less: { // (a < 8) & (a < 10) -> a < 8
			// This is true only if both comparisons are of the same type.
			if(leftCmp->IsUcmp() == leftCmp->IsUcmp()) {
				return LOG(leftCmp->ResultOp());
			}
            break;
		}
		}
		break;
	}
	case Order_Greater: {
		switch(rightCmp->Order()) {
		case Order_Equal: { // (a > 8) & (a == 9) -> a == 9
			return LOG(rightCmp->ResultOp());
		}
		case Order_Greater: { // (a > 8) & (a > 9)  -> a > 9
			// This is true only if both comparisons are of the same type.
			if(leftCmp->IsUcmp() == leftCmp->IsUcmp()) {
				return LOG(rightCmp->ResultOp());
			}
            break;
		}
		case Order_Less: { // (a > 8) & (a < 10) -> (a - 8) < 2
			// This is true only if both comparisons are of the same type.
			if(leftCmp->IsUcmp() == leftCmp->IsUcmp()) {
				return LOG(CreateRangeCompare(leftCmp->LeftOp(), leftConst, rightConst,
											 leftCmp->ResultOp()->GetType(), 
											 leftCmp->IsUcmp()));
			}
            break;
		}
		case Order_NotEqual: { // (a > 8) & (a != 9) -> a > 9
			if(IsLeftRightMinusOne(leftConst, rightConst)) {
				return LOG(CreateCompare(leftCmp->LeftOp(), rightConst, 
										 Order_Greater, leftCmp->ResultOp()->GetType(), 
										 leftCmp->IsUcmp()));
			}
            break;
		}
		}
		break;
	}
	} // switch

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAndBit(Operand* opA, Operand* opB, Block* block) {
	// Test for the 'and' between two compare instructions that test a single
	// bit from the same operand. A single bit means that the constants
	// are powers or two, and can be merged into a single constant.
	// ((a & 8) !== 0) & ((a & 4) == 0) -> (a & (8|4)) == 0
	if((opA->DefiningInstrIs<CmpInstr>() || 
        opA->DefiningInstrIs<UcmpInstr>()) 
        &&
	   (opB->DefiningInstrIs<CmpInstr>() ||
        opB->DefiningInstrIs<UcmpInstr>())) {
		// Both operands are comparisons.
		auto leftCmp = static_cast<CmpInstrBase*> 
                            (opA->DefiningInstruction());
		auto rightCmp = static_cast<CmpInstrBase*>
                            (opB->DefiningInstruction());
		OrderType order;

		if(leftCmp->Order() == rightCmp->Order()) {
			order = leftCmp->Order();
			if(order != Order_Equal) {
				// We support only compares with == and != order.
				return nullptr;
			}
		}
		else {
			// The compares don't have the same order.
			return nullptr;
		}

		// Move the constant to the right, so we test fewer cases.
		if(leftCmp->LeftOp()->IsIntConstant()) {
			leftCmp->InvertOrder(true, false /* invertEquality */);
		}
		if(rightCmp->LeftOp()->IsIntConstant()) {
			rightCmp->InvertOrder(true, false /* invertEquality */);
		}

		// The constant on the right must be 0 for both compares.
		if((leftCmp->RightOp()->IsZeroInt() && 
			rightCmp->RightOp()->IsZeroInt()) == false) {
            return nullptr;
        }

		// Get the operands, and give up if they don't match.
		auto leftAnd = leftCmp->LeftOp()->DefiningInstrAs<AndInstr>();
		auto rightAnd = rightCmp->LeftOp()->DefiningInstrAs<AndInstr>();

		if((leftAnd && rightAnd) == false) {
            return nullptr;
        }

		// Move the mask to the right, if not already there, 
        // so we have fewer cases to test for.
		if(leftAnd->LeftOp()->IsIntConstant()) {
			leftAnd->SwapOperands();
		}
		if(rightAnd->LeftOp()->IsIntConstant()) {
			rightAnd->SwapOperands();
		}

		// The operand that is masked must be the same for both sides.
		if(leftAnd->LeftOp() != rightAnd->LeftOp()) {
            return nullptr;
        }

		auto leftConst = AsIntConstant(leftAnd->RightOp(), block);
		auto rightConst = AsIntConstant(rightAnd->RightOp(), block);

		if((leftConst && rightConst) == false) {
            return nullptr;
        }

		// Make sure the constants are power of two, and then merge them.
		if((IA::IsPowerOfTwo(leftConst) && 
            IA::IsPowerOfTwo(rightConst)) == false) {
			return nullptr;
		}

		// Create the simplified instruction sequence.
		__int64 merged = IA::Or(leftConst, rightConst);
		auto mergedOp = irGen_.GetIntConst(leftConst->GetType(), merged);

		auto andOp = GetTemporary(leftAnd->ResultOp());
		irGen_.GetAnd(leftAnd->LeftOp(), mergedOp, andOp);

		return LOG(CreateCompare(andOp, GetZeroInt(andOp), order, 
							     opA->GetType(), leftCmp->IsUcmp()));
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAndFloatCmp(Operand* opA, Operand* opB) {
	Operand* a;
	Operand* b;

	// Test for a redundant check.
	// (a < b) & (a < b) -> a < b
	if(Match<FcmpInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
	   Match<FcmpInstr>(MatchOp(a), MatchOp(b))(opB)) {
		auto leftCmp = opA->DefiningInstrAs<FcmpInstr>();
		auto rightCmp = opB->DefiningInstrAs<FcmpInstr>();

		if(leftCmp->Order() == rightCmp->Order()) {
			return LOG(opA);
		}
	}

	return nullptr;
}

} // namespace Optimization