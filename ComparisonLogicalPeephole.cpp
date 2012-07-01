// ComparisonLogicalPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 
// 'cmp', 'ucmp' and 'fcmp' instructions that involve logical operations.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

Operand* Peephole::HandleIntegerCmpLogical(LogicalInstr* logicalInstrA, 
										   LogicalInstr* logicalInstrB, 
										   Operand* resultOp, OrderType order, 
                                           bool isSigned, Block* block) {
	switch(logicalInstrA->GetOpcode()) {
		case Instr_And: {
            if(AreEqual(logicalInstrA->RightOp(),
                        logicalInstrB->RightOp(), block) == false) {
				return nullptr;
			}

			// (a & m) == (b & m) -> a == b, same for !=
			// This can be done only if we can prove (using Operand Info)
			// that the mask doesn't effect any existing bits in 'a' and 'b'.
			if((order == Order_Equal) || (order == Order_NotEqual)) {
                // All zero bits from 'm' should be found in both 'a' and 'b'.
                unsigned __int64 oneBitsMask;
                opInfo_.EstimateOneBits(logicalInstrA->RightOp(), oneBitsMask);

                // All the bits that are not 0 and not 1 are considered to be 0
                // (worst-case assumption).
                unsigned __int64 zeroBitsMask;
                zeroBitsMask = ~oneBitsMask;

                // Check operand 'a'.
                unsigned __int64 zeroBitsA;
                opInfo_.EstimateZeroBits(logicalInstrA->LeftOp(), zeroBitsA);
                if((zeroBitsA & zeroBitsMask) != zeroBitsMask) {
                    return nullptr;
                }

                // Check operand 'b'.
                unsigned __int64 zeroBitsB;
                opInfo_.EstimateZeroBits(logicalInstrB->LeftOp(), zeroBitsB);
                if((zeroBitsB & zeroBitsMask) != zeroBitsMask) {
                    return nullptr;
                }

				return LOG(CreateCompare(logicalInstrA->LeftOp(), logicalInstrB->LeftOp(),
								         order, resultOp->GetType(), isSigned == false));
			}
			break;
        }
		case Instr_Or: {
			if(logicalInstrA->RightOp() != logicalInstrB->RightOp()) {
				return nullptr;
			}

            // (a | m) == (b | m) -> a == b
			// This can be done only if we can prove (using Operand Info)
			// that the mask doesn't effect any existing bits in 'a' and 'b'.
			if((order == Order_Equal) || (order == Order_NotEqual)) {
                // All one bits from 'm' should be found in both 'a' and 'b'.
				unsigned __int64 zeroBitsMask;
                opInfo_.EstimateOneBits(logicalInstrA->RightOp(), zeroBitsMask);

                // All the bits that are not 1 and not 0 are considered to be 1
                // (worst-case assumption).
                unsigned __int64 oneBitsMask;
                oneBitsMask = ~zeroBitsMask;

                // Check operand 'a'.
                unsigned __int64 oneBitsA;
                opInfo_.EstimateZeroBits(logicalInstrA->LeftOp(), oneBitsA);
                if((oneBitsA & oneBitsMask) != oneBitsMask) {
                    return nullptr;
                }

                // Check operand 'b'.
                unsigned __int64 oneBitsB;
                opInfo_.EstimateZeroBits(logicalInstrB->LeftOp(), oneBitsB);
                if((oneBitsB & oneBitsMask) != oneBitsMask) {
                    return nullptr;
                }

                return LOG(CreateCompare(logicalInstrA->LeftOp(), logicalInstrB->LeftOp(),
								         order, resultOp->GetType(), isSigned == false));
            }
			break;
		}
		case Instr_Xor: {
			if(logicalInstrA->RightOp() != logicalInstrB->RightOp()) {
				return nullptr;
			}

			if((order == Order_Equal) || (order == Order_NotEqual)) {
				// (a ^ m) == (b ^ m) -> a == b, same for !=
				return LOG(CreateCompare(logicalInstrA->LeftOp(), logicalInstrB->LeftOp(),
									     order, resultOp->GetType(), isSigned == false));
			}

			// Test for the case in which 'm' is a constant
			// that inverts the sign bit using an 'xor'.
			if(auto intConst = AsIntConstant(logicalInstrA->RightOp(), block)) {
				auto intKind = intConst->GetType()->GetSubtype();
				__int64 signBitMask = IA::GetSignBitMask(intKind);

				if(IA::AreEqual(intConst->Value(), signBitMask, intKind)) {
					// Consider the following example:
					// (a ^ 16) < (b ^ 16) -> a < b
					// 10101    <  00111   -> result: false 
					// 00101    <  10111   -> result: true (the right one)
					// If we want to eliminate the 'xor' from both sides, we need to
					// invert the result of 'a < b' to get the right value. This can
					// be done by changing from the comparison of signed numbers
					// to unsigned ones (and the same in the opposite direction).
					return LOG(CreateCompare(logicalInstrA->LeftOp(),
										     logicalInstrB->LeftOp(), order,
										     resultOp->GetType(), isSigned));
				}
			}
			break;
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpXor(XorInstr* xorInstr, IntConstant* intConst, 
									   Operand* resultOp, OrderType order, 
                                       bool isSigned, Block* block) {
	// We can do some simplifications if the 'xor' involves a constant.
	auto xorConst = xorInstr->RightOp()->As<IntConstant>();

	if(xorConst == nullptr) {
        return nullptr;
    }

	// If we compare to 0 or -1 (effectively testing the sign bit) we can eliminate
	// the 'xor' if it doesn't invert the sign bit. The 'xor' constant inverts the
	// sign bit if it has only one bit set, which is the leftmost bit.
	if(isSigned &&
      ((order == Order_Less) && intConst->IsZero()) ||
	  ((order == Order_Greater) && intConst->IsMinusOne())) {
		// (a ^ 5) < 0 -> a < 0 (the 'xor' has no effect on the sign)
		if((IA::IsPowerOfTwo(xorConst) &&
		   (IA::RightmostZeroBits(xorConst) == 0)) == false) {
			return LOG(CreateCompare(xorInstr->LeftOp(), xorConst, 
                                     order, resultOp->GetType(),
                                     isSigned == false));
		}

		// The 'xor' inverts the sign of the operand, so we can eliminate it
		// if we change the order of the comparison.
		if(order == Order_Less) {
			// (a ^ 0x80) < 0 -> a >= 0, for 'a' having type 'int16'
			return LOG(CreateCompare(xorInstr->LeftOp(), xorConst, 
                                     Order_GreaterOrEqual, resultOp->GetType(),
                                     isSigned == false));
		}
		else {
			// (a ^ 0x80) > -1 -> a <= -1
			return LOG(CreateCompare(xorInstr->LeftOp(), xorConst, 
                                     Order_LessOrEqual, resultOp->GetType(),
                                     isSigned == false));
		}
	}

	// If we have a generic constant we can still do simplifications
	// if the 'xor' constant is the sign bit.
	// Note that we don't simplify if it would lead to increased register pressure.
	auto intKind = xorConst->GetType()->GetSubtype();
	__int64 signBitMask = IA::GetSignBitMask(intKind);

	bool isEquality = (order == Order_Equal) || 
                      (order == Order_NotEqual);

	if(IA::AreEqual(xorConst->Value(), signBitMask, intKind) &&
	   (isEquality == false)) {
		if(xorInstr->ResultOp()->HasSingleUser() == false) {
			return nullptr; // Don't increase register pressure.
		}

		// (a ^ S) < C -> a < (S ^ C)
		// We change from signed to unsigned comparison, and from
		// unsigned to signed, in order to invert the result.
		__int64 result = IA::Xor(signBitMask, intConst->Value(), intKind);
		auto constantOp = irGen_.GetIntConst(intConst->GetType(), result);

		return LOG(CreateCompare(xorInstr->LeftOp(), constantOp, 
                                 order, resultOp->GetType(), isSigned));
	}

	// If the 'xor' constant is the inverse of the sign bit we can simplify.
	__int64 invSignBitBask = IA::Not(signBitMask, intKind);
	
	if(IA::AreEqual(xorConst->Value(), invSignBitBask, intKind) &&
	   (isEquality == false)) {
		if(xorInstr->ResultOp()->HasSingleUser() == false) {
			return nullptr; // Don't increase register pressure.
		}

		// (a ^ ~S) < C -> (~S ^ C) < a 
		// We change from signed to unsigned comparison, and from
		// unsigned to signed, in order to invert the result.
		__int64 result = IA::Xor(invSignBitBask, intConst->Value(), intKind);
		auto constantOp = irGen_.GetIntConst(intConst->GetType(), result);

		return LOG(CreateCompare(constantOp, xorInstr->LeftOp(), 
                                 order, resultOp->GetType(), isSigned));
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpAnd(AndInstr* andInstr, IntConstant* intConst, 
									   Operand* resultOp, OrderType order, 
                                       bool isSigned, Block* block) {
	// We can do some simplifications if the 'xor' involves a constant.
	auto andConst = AsIntConstant(andInstr->RightOp(), block);

	if(andConst == nullptr) {
        return nullptr;
    }

	// (trunc(a, intX) & C1) ORDER C2 -> (a & C1) ORDER C2
	// We can do this simplification only if both constants don't have
	// the sign bit set or if the order is 'equal' or 'not equal'.
	if(auto truncInstr = andInstr->LeftOp()->DefiningInstrAs<TruncInstr>()) {
		auto intKind = andConst->GetType()->GetSubtype();

		if(((IA::GetSignBit(andConst) == 0) && (IA::GetSignBit(intConst) == 0)) ||
		   ((order == Order_Equal) || (order == Order_NotEqual))) {
			// Don't increase the register pressure.
			if((andInstr->LeftOp()->HasSingleUser() &&
				andInstr->ResultOp()->HasSingleUser()) == false) {
				return nullptr;
			}

			auto targetType = truncInstr->TargetOp()->GetType();
			auto newAndConst = irGen_.GetIntConst(targetType, andConst->Value());
			auto newIntConst = irGen_.GetIntConst(targetType, intConst->Value());

			auto andOp = irGen_.GetTemporary(targetType);
			irGen_.GetAnd(truncInstr->TargetOp(), newAndConst, andOp);

			return LOG(CreateCompare(andOp, newIntConst, order, 
                                     resultOp->GetType(), isSigned == false));
		}
	}

	// The following two simplifications are valid only for signed numbers.
	// (a >> C1) & C2) == 0 -> a >= 0
	// (a >> C1) & C2) != 0 -> a < 0
	// Example:
	// (a >> 24) & 0x8000000 -> a < 0. This tests if bit 27 is set,
	// and this is possible only if the number was negative to begin with.
	// We generate a mask '111111110000...000' and test if 'C2' has any bit
	// set in this mask. If it does, we can do the simplifications.
	if(auto shrInstr = andInstr->LeftOp()->DefiningInstrAs<ShrInstr>()) {
		if(intConst->IsZero() &&
           ((order == Order_Equal) || (order == Order_NotEqual))) {
			if(auto shrConst = AsIntConstant(shrInstr->RightOp(), block)) {
				int bits = shrConst->GetType()->SizeInBits();

				// If we shift more than the bitwidth we give up;
				// it will be handled later by another part.
				if(shrConst->Value() > bits) {
					return nullptr;
				}
				
				auto intKind = shrConst->GetType()->GetSubtype();
				__int64 mask = IA::Not(IA::ValueFromBitCount(shrConst->Value()), intKind);

				if(IA::And(mask, intConst->Value(), intKind) != 0) {
					// We can do the simplification.
					OrderType newOrder = order == Order_Equal ?
                                                  Order_GreaterOrEqual : Order_Less;

					return LOG(CreateCompare(shrInstr->LeftOp(), GetZeroInt(shrConst), 
										     newOrder, andInstr->LeftOp()->GetType(), true));
				}
			}
		}
	}

	// The following appears frequently in code that uses bitfields.
	// ((a >> C1) & C2) ORDER C3 -> (a & (C2 << C1)) ORDER (C3 << C1)
	// ((a << C1) & C2) ORDER C3 -> (a & (C2 >> C1)) ORDER (C3 >> C1)
	// We try to handle both right and left shifts here.
	if(auto logicalInstr = andInstr->LeftOp()->DefiningInstrAs<LogicalInstr>()) {
		if(logicalInstr->IsShift() == false) {
			return nullptr;
		}

		auto shlConst = AsIntConstant(logicalInstr->RightOp(), block);
		if(shlConst == nullptr) {
            return nullptr;
        }

		auto intKind = intConst->GetType()->GetSubtype();

		// If this is an arithmetic right shift we need to make sure that
		// no bits that are unknown remain after applying the mask.
		if(logicalInstr->IsShr()) {
			// The position of the leftmost bit in 'C2' must be smaller
			// than the size of the type - 'C1' (meaning that all bits
			// introduced by the shift are masked with 0).
			if((intConst->GetType()->SizeInBits() + shlConst->Value()) >=
			   IA::LeftmostSetBit(andConst)) {
				return nullptr;
			}
		}

		// Compute the value of the new constant to which we compare.
		__int64 newIntConst = logicalInstr->IsShl() ?
							  IA::Ushr(intConst, shlConst) :
							  IA::Shl(intConst, shlConst);

		// We also need to make sure that we don't shift "out" any of the bits
		// from 'C3'. We do this by shifting back and checking that the values are equal.
		__int64 shiftedBack = logicalInstr->IsShl() ?
							  IA::Shl(newIntConst, shlConst->Value(), intKind) :
							  IA::Ushr(newIntConst, shlConst->Value(), intKind);

		if(shiftedBack != newIntConst) {
			// We can still simplify if the order is 'equal' or 'not equal',
			// because even with the transformation the sides couldn't be equal.
			if(order == Order_Equal) {
				return LOG(GetBool(false, resultOp));
			}
			else if(order == Order_NotEqual) {
				return LOG(GetBool(true, resultOp));
			}
			else return nullptr;
		}

		// Now shift the mask; no other check needs to be performed.
		__int64 newMask = logicalInstr->IsShl() ?
						  IA::Ushr(andConst, shlConst) :
						  IA::Shr(andConst, shlConst);
		
		// (a & (C2 << C1)) ORDER (C3 << C1)
		auto andOp = GetTemporary(andConst);
		auto newMaskOp = irGen_.GetIntConst(shlConst->GetType(), newMask);
		irGen_.GetAnd(logicalInstr->LeftOp(), newMaskOp, andOp);

		auto constantOp = irGen_.GetIntConst(intConst->GetType(), newIntConst);
		return LOG(CreateCompare(andOp, constantOp, order,
                                 resultOp->GetType(), isSigned == false));
	}

	// Test for a simplification which could enhance loop-invariant code motion.
	// ((a >> y) & C) ==/!= 0 -> (a & (C << y)) ==/!= 0
	// ((a << y) & C) ==/!= 0 -> (a & (C >> y)) ==/!= 0
	if(auto logicalInstr = andInstr->LeftOp()->DefiningInstrAs<LogicalInstr>()) {
		// The shift must not be an arithmetic one, it can't be handled.
		if((logicalInstr->IsShift() == false) || 
            logicalInstr->IsShr()) {
			return nullptr;
		}

		// 'a' should not be a constant, otherwise the transformation has no reason.
		// This is valid only for 'equal' and 'not equal' orders, with the constant 0.
		if(logicalInstr->LeftOp()->IsConstant() || 
           (intConst->IsZero() == false) ||
		   (((order == Order_Equal) || (order == Order_NotEqual)) == false)) {
			return nullptr;
		}

		// Try not to increase the register pressure.
		if(andInstr->LeftOp()->HasSingleUser() == false) {
			return nullptr;
		}

		// Now do the transformation.
		auto maskOp = GetTemporary(andConst);

		if(logicalInstr->IsShl()) {
			irGen_.GetUshr(andConst, logicalInstr->RightOp(), maskOp);
		}
		else irGen_.GetShl(andConst, logicalInstr->RightOp(), maskOp);

		auto andOp = GetTemporary(andConst);
		irGen_.GetAnd(logicalInstr->LeftOp(), maskOp, andOp);

		return LOG(CreateCompare(andOp, intConst, order, 
                                 resultOp->GetType(), isSigned == false));
	}

	// Handle situations when we know how many bits are set to 0.
	return HandleIntegerCmpAndZeroBits(andInstr, intConst, resultOp, 
                                       order, isSigned, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpAndZeroBits(AndInstr* andInstr, IntConstant* intConst, 
											   Operand* resultOp, OrderType order, 
											   bool isSigned, Block* block) {
	auto maskOp = AsIntConstant(andInstr->RightOp(), block);

	if(maskOp == nullptr) {
        return nullptr;
    }

	auto intKind = intConst->GetType()->GetSubtype();
	bool isEquality = (order == Order_Equal) ||
                      (order == Order_NotEqual);

	// Test for a masked 'shl'.
	if(auto shlInstr = andInstr->LeftOp()->DefiningInstrAs<ShlInstr>()) {
		// These simplifications can be performed only on 'equal'
        // and 'not equal' orders.
		auto shlConst = shlInstr->RightOp()->As<IntConstant>();

		if((shlConst && isEquality) == false) {
			return nullptr;
		}

		// (a << 16) & 0xFFFF == 5 -> 0
		// If the shift amount is larger than the leftmost bit in 'intConst'
		// then the left side can't be equal to the right one.
		// (a << 16) == 4 -> 0
		if(shlConst->Value() > IA::LeftmostSetBit(intConst)) {
			if(order == Order_Equal) {
				return LOG(GetBool(false, resultOp));
			}
			else return LOG(GetBool(true, resultOp));
		}

		// (a << 2) & 0xFF00 == 5 -> 0
		// If the mask doesn't include some bits from 'intConst'
		// then the left and right side can never be equal.
		if(IA::And(intConst, maskOp) == 0) {
			if(order == Order_Equal) {
				return LOG(GetBool(false, resultOp));
			}
			else return LOG(GetBool(true, resultOp));
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpOr(OrInstr* orInstr, IntConstant* intConst, 
									  Operand* resultOp, OrderType order, 
                                      bool isSigned, Block* block) {
	// Try to apply De Morgan's law to 'ptoi' instruction operands.
	// ptoi(a, X) | ptoi(b, X) == 0 -> (a == nullptr) & (b == nullptr)
	// ptoi(a, X) | ptoi(b, X) != 0 -> (a == nullptr) | (b == nullptr)
	if(auto ptoiA = orInstr->LeftOp()->DefiningInstrAs<PtoiInstr>()) {
		if(auto ptoiB = orInstr->RightOp()->DefiningInstrAs<PtoiInstr>()) {
			// This can be done only for 'equal' and 'not equal' orders.
			if((order == Order_Equal) || (order == Order_NotEqual)) {
				return nullptr;
			}

			// It's valid only if we compare to 0.
			if(intConst->IsZero() == false) {
				return nullptr;
			}

			auto targetTypeA = ptoiA->TargetOp()->GetType();
			auto cmpOp1 = CreateCompare(ptoiA->TargetOp(), 
                                        GetNullptr(targetTypeA),
										Order_Equal, resultOp->GetType(),
										isSigned == false);

			auto targetTypeB = ptoiB->TargetOp()->GetType();
			auto cmpOp2 = CreateCompare(ptoiB->TargetOp(), 
                                        GetNullptr(targetTypeB),
										Order_Equal, resultOp->GetType(),
										isSigned == false);

			// If it's 'equal' we combine the results 
            // using an 'and', else an 'or'.
			if(order == Order_Equal) {
				auto andOp = GetTemporary(resultOp);
				irGen_.GetAnd(cmpOp1, cmpOp2, andOp);
				return LOG(andOp);
			}
			else {
				auto orOp = GetTemporary(resultOp);
				irGen_.GetOr(cmpOp1, cmpOp2, orOp);
				return LOG(orOp);
			}
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpShl(ShlInstr* shlInstr, IntConstant* intConst, 
									   Operand* resultOp, OrderType order, 
                                       bool isSigned, Block* block) {
	// We can do some simplifications if the 'shl' has a constant shift amount.
	auto shlConst = AsIntConstant(shlInstr->RightOp(), block);

	if(shlConst == nullptr) {
        return nullptr;
    }

	if((order == Order_Equal) || (order == Order_NotEqual)) {
		// If the shift amount is larger than the leftmost bit in 'intConst'
		// the left side can't be equal to the right one.
		// (a << 16) == 4 -> 0
		if(shlConst->Value() > IA::LeftmostSetBit(intConst)) {
			if(order == Order_Equal) {
				return LOG(GetBool(false, resultOp));
			}
			else return LOG(GetBool(true, resultOp));
		}

		// The following simplification is not profitable
        // if it increases register pressure.
		if(shlInstr->ResultOp()->HasSingleUser() == false) {
			return nullptr;
		}

		// (a << 8) == 0x2300 -> (a & 0xFF) == 0x23, for 'a' having type 'int16'. 
		// This transformation is profitable because there are more opportunities 
		// that involve 'and' instructions, while the cost remains the same.

        // Note that we do this only if 'intConst' is a multiple of '2 ^ shlConst'.
        __int64 pow2ShlConst = IA::ValueFromBitCount(shlConst->Value()) + 1;

        if((intConst->Value() % pow2ShlConst) == 0) {
            __int64 newIntConst = IA::Ushr(intConst, shlConst);
		    auto constantOp = irGen_.GetIntConst(intConst->GetType(), newIntConst);

		    // The leftmost bits in the mask need to be 0.
		    int maskBits = intConst->GetType()->SizeInBits() - 
                           shlConst->Value();
		    __int64 newMask = IA::ValueFromBitCount(maskBits);
		    auto maskOp = irGen_.GetIntConst(intConst->GetType(), newMask);

		    auto andOp = GetTemporary(maskOp);
		    irGen_.GetAnd(shlInstr->LeftOp(), maskOp, andOp);

		    return LOG(CreateCompare(andOp, constantOp, order, 
                                     resultOp->GetType(), isSigned == false));
        }
        else {
            // It is not a multiple, then we know that there can't be
            // any integer multiplied by '2 ^ shlConst' that equals 'intConst'.
            if(order == Order_Equal) {
				return LOG(GetBool(false, resultOp));
			}
			else return LOG(GetBool(true, resultOp));
        }
	}

	// Test for a sequence that compares the sign. Something like this:
	// (a << 16) < 0 -> (a & 1) != 0, for 'a' having 'int16' type.
    // We test if the bit that will become the sign bit after the shift is 1 or not.
	// This works also for shift amounts that are not powers of two.
	// (a << 12) < 0 -> (a & 4) != 0
	if(isSigned && intConst->IsZero() && 
       (order == Order_Less)) {
		// It is not profitable if it increases register pressure.
		if(shlInstr->ResultOp()->HasSingleUser() == false) {
			return nullptr;
		}

		__int64 testPosition = shlConst->GetType()->SizeInBits() - 
                          shlConst->Value() - 1;
		__int64 mask = 1LL << testPosition;
		auto maskOp = irGen_.GetIntConst(shlConst->GetType(), mask);
		
        auto andOp = GetTemporary(maskOp);
		irGen_.GetAnd(shlInstr->LeftOp(), maskOp, andOp);

		return LOG(CreateCompare(andOp, intConst, Order_NotEqual,
							     resultOp->GetType(), isSigned == false));
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpShr(LogicalInstr* shrInstr, IntConstant* intConst, 
									   Operand* resultOp, OrderType order, 
                                       bool isSigned, Block* block) {
	// We can do some simplifications if the 'shl' has a constant shift amount.
	auto shrConst = AsIntConstant(shrInstr->RightOp(), block);

	if(shrConst == nullptr) {
        return nullptr;
    }

	// Don't do anything if we shift with 0 or more than the size of the type.
	if(shrConst->IsZero() || 
       (shrConst->Value() > shrConst->GetType()->SizeInBits())) {
		return nullptr;
	}

	bool isEquality = (order == Order_Equal) || 
                      (order == Order_NotEqual);
	auto intKind = shrConst->GetType()->GetSubtype();

	// If we are testing for 'equal' or 'not equal', and the value on the right side
	// is larger than the one on the left side can ever be, we can return the result.
	if(isEquality) {
		__int64 newIntConst = IA::Shl(intConst, shrConst);
		__int64 shiftedBack = shrInstr->IsShr() ? 
							  IA::Shr(newIntConst, shrConst->Value(), intKind) :
							  IA::Ushr(newIntConst, shrConst->Value(), intKind);

		if(shiftedBack != newIntConst) {
			if(order == Order_Equal) {
				return LOG(GetBool(false, resultOp));
			}
			else return LOG(GetBool(true, resultOp));
		}

		// Convert shift to an 'and' if this doesn't increase the register pressure.
		// This transformation is profitable because there are more opportunities 
		// that involve 'and' instructions, while the cost remains the same.
		if(shrInstr->ResultOp()->HasSingleUser()) {
			// The mask has 0 bits in the right part.
			int maskBits = shrConst->GetType()->SizeInBits() - 
                           shrConst->Value();
			__int64 mask = IA::ValueFromBitCount(maskBits);

			mask = IA::Shl(mask, shrConst->Value(), intKind);
			auto maskOp = irGen_.GetIntConst(intConst->GetType(), mask);

			__int64 newIntConst = IA::Shl(intConst, shrConst);
			auto constantOp = irGen_.GetIntConst(intConst->GetType(), 
                                                 newIntConst);

			auto andOp = GetTemporary(intConst);
			irGen_.GetAnd(shrInstr->LeftOp(), maskOp, andOp);
			return LOG(CreateCompare(andOp, constantOp, order, 
                                     resultOp->GetType(), shrInstr->IsUshr()));
		}
	}
	else {
		// a >> 2 == 4 -> a == 12, for both signed and unsigned numbers.
		// We can do this transformation only if the constant doesn't overflow.
		int count = shrConst->Value();
		__int64 newIntConst = intConst->Value();

		while(count) {
			if(IA::MulOverflows(newIntConst, 2, intKind, isSigned)) {
				return nullptr;
			}

			newIntConst = IA::Mul(newIntConst, 2, intKind);
			count--;
		}

		auto constantOp = irGen_.GetIntConst(intConst->GetType(), newIntConst);

		return LOG(CreateCompare(shrInstr->LeftOp(), constantOp, order,
							     resultOp->GetType(), shrInstr->IsUshr()));
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpEqualityAnd(AndInstr* andInstr, IntConstant* intConst, 
											   Operand* resultOp, OrderType order, 
											   bool isSigned, Block* block) {
	Operand* a;
	IntConstant* C;
	auto andConst = AsIntConstant(andInstr->RightOp(), block);

	if(andConst == nullptr) {
        return nullptr;
    }

	// a & C1 == C2 -> 0, if 'C1' doesn't have all the bits of 'C2'.
	// All bits from 'C2' are in 'C1' if after we perform an
	// 'and' between the two we obtain 'C2'.
	auto intKind = intConst->GetType()->GetSubtype();
	__int64 andResult = IA::And(andConst, intConst);

	if(IA::AreEqual(andResult, intConst->Value(), intKind) == false) {
		if(order == Order_Equal) {
			return LOG(GetBool(false, resultOp));
		}
		else return LOG(GetBool(true, resultOp));
	}

	if(intConst->IsZero()) {
		// (a & ~15) == 0 -> a < 16
		// To test the condition we invert the number (~15 -> 14),
		// which should be 'sizeof(a) - 2'.
		int bits = intConst->GetType()->SizeInBits();
		__int64 invertedAndConst = IA::Not(andConst);

		if(invertedAndConst == (bits - 2)) {
			auto constantOp = irGen_.GetIntConst(intConst->GetType(), bits);

			return LOG(CreateCompare(andInstr->LeftOp(), constantOp, 
                                     Order_Less, resultOp->GetType(), false));
		}

		// (~a & C) == 0 -> (a & C) != 0
		// (~a & C) != 0 -> (a & C) == 0, if 'C' is a power of two.
		// (a ^ C) & C == 0 -> (a & C) != 0
		// (a ^ C) & C != 0 -> (a & C) == 0, if 'C' is a power of two.
		if((IsLogicalNot(andInstr->LeftOp(), &a) ||
			Match<XorInstr>(MatchAny(&a), MatchIC(&C, block))(andInstr->LeftOp())) &&
			MatchIC(&C, block)(andInstr->RightOp()) && IA::IsPowerOfTwo(C)) {
            // We have a match.
			order = CmpInstrBase::InvertedOrder(order);
			auto andOp = GetTemporary(andInstr->LeftOp());
			irGen_.GetAnd(a, C, andOp);

			return LOG(CreateCompare(andOp, intConst, order, 
                                     resultOp->GetType(), isSigned == false));
		}
	}

	// (a & C) == C -> (a & C) != 0
	// This could create more simplification opportunities.
	if(andConst == intConst) { // Valid because constants are unique.
		OrderType newOrder = order == Order_Equal ? Order_NotEqual : Order_Equal;

		return LOG(CreateCompare(andInstr->LeftOp(), andConst, newOrder,
							     resultOp->GetType(), isSigned == false));
	}
	
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpEqualityXor(XorInstr* xorInstr, IntConstant* intConst, 
											   Operand* resultOp, OrderType order, 
											   bool isSigned, Block* block) {
	Operand* a;
	IntConstant* C;

	// a ^ C1 == C2 -> a == C1 ^ C2
	if(auto xorConst = AsIntConstant(xorInstr->RightOp(), block)) {
		__int64 result = IA::Xor(xorConst, intConst);
		auto constantOp = irGen_.GetIntConst(intConst->GetType(), result);

		return LOG(CreateCompare(xorInstr->LeftOp(), constantOp, order,
						         resultOp->GetType(), isSigned == false));
	}

	if(intConst->IsZero()) {
		// (a & C) ^ C == 0 -> (a & C) != 0
		// (a & C) ^ C != 0 -> (a & C) == 0, if 'C' is a power of two.
		if(Match<AndInstr>(MatchAny(&a), MatchIC(&C, block))(xorInstr->LeftOp()) &&
			((C == xorInstr->RightOp()) && IA::IsPowerOfTwo(C))) {
            // We have a match.
			order = CmpInstrBase::InvertedOrder(order);
			auto andOp = GetTemporary(xorInstr->LeftOp());
			irGen_.GetAnd(a, C, andOp);
			return LOG(CreateCompare(andOp, GetZeroInt(a), order, 
							         resultOp->GetType(), isSigned == false));
		}

		// (a >> C) != 0 -> x < 0
		// (a >> C) == 0 -> x >= 0, if 'C' is equal to the bitwidth - 1.
		if(Match<ShrInstr>(MatchAny(&a), MatchIC(&C, block))(xorInstr)) {
			int bits = C->GetType()->SizeInBits();
			auto intKind = C->GetType()->GetSubtype();

			if(IA::AreEqual(C->Value(), bits - 1, intKind)) {
				order = order == Order_NotEqual ? Order_Less : 
												  Order_GreaterOrEqual;

				return LOG(CreateCompare(a, GetZeroInt(a), order,
									     resultOp->GetType(), isSigned == false));
			}
		}

		// a ^ b == 0 -> a == b
        // a ^ b != 0 -> a != b
		return LOG(CreateCompare(xorInstr->LeftOp(), xorInstr->RightOp(), order,
						         resultOp->GetType(), isSigned == false));
	}

	return nullptr;
}

} // namespace Optimization