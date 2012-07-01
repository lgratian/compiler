// ComparisonPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 
// 'cmp', 'ucmp' and 'fcmp' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"
#include "../Analysis/RangeTag.hpp"

namespace Optimization {

Operand* Peephole::HandleCmp(CmpInstr* instr) {
	// We use the constant evaluator even if not both operands are constant,
	// because it also eliminates very simple cases, like 'a & 0 -> 0'.
	if(auto result = folder_.FoldCompare(Instr_Cmp, instr->LeftOp(), 
										 instr->RightOp(), instr->Order(),
                                         instr->ParentBlock())) {
		return result;
	}

	// Try to reduce the magnitude of the involved constants.
    ShrinkIntegerCmpConstants(instr);

	// If the left operand is a constant move it to the right,
	// reduces the cases we need to test for.
	if(instr->LeftOp()->IsConstant()) {
		instr->InvertOrder(true /* invertOperands */, 
                           false /* invertEquality */);
	}

    // Try to replace <=/>= with </> in order to have fewer cases to test for.
    CanonicalizeIntegerCmp(instr);

	// Test for patterns that are common for both signed 
    // and unsigned comparisons.
	if(auto result = HandleIntegerCmp(instr)) {
		return result;
	}

    // Try to simplify using Value Range information.
    if(auto tag = instr->ParentBlock()->GetTag<RangeTag>()) {
        if(instr->IsLess()) {
            auto result = tag->IsSmaller(instr->LeftOp(), instr->RightOp(),
                                         instr->IsCmp());
            if(result != Result_Maybe) {
                return LOG(GetBool(result == Result_Yes, instr->ResultOp()));
            }
        }

        if(instr->IsGreater()) {
            auto result = tag->IsGreater(instr->LeftOp(), instr->RightOp(),
                                         instr->IsCmp());
            if(result != Result_Maybe) {
                return LOG(GetBool(result == Result_Yes, instr->ResultOp()));
            }
        }

        if(instr->IsLessOrEqual()) {
            auto result = tag->IsSmallerOrEqual(instr->LeftOp(), instr->RightOp(),
                                                instr->IsCmp());
            if(result != Result_Maybe) {
                return LOG(GetBool(result == Result_Yes, instr->ResultOp()));
            }
        }

        if(instr->IsEqual()) {
            auto result = tag->AreEqual(instr->LeftOp(), instr->RightOp());
            if(result != Result_Maybe) {
                return LOG(GetBool(result == Result_Yes, instr->ResultOp()));
            }
        }

        if(instr->IsNotEqual()) {
            auto result = tag->AreNotEqual(instr->LeftOp(), instr->RightOp());
            if(result != Result_Maybe) {
                return LOG(GetBool(result == Result_Yes, instr->ResultOp()));
            }
        }
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleUcmp(UcmpInstr* instr) {
	// We use the constant evaluator even if not both operands are constant,
	// because it also eliminates very simple cases, like 'a & 0 -> 0'.
	if(auto result = folder_.FoldCompare(Instr_Ucmp, instr->LeftOp(), 
										 instr->RightOp(), instr->Order(),
                                         instr->ParentBlock())) {
		return LOG(result);
	}

    // Try to reduce the magnitude of the involved constants.
    ShrinkIntegerCmpConstants(instr);

	// If the left operand is a constant move it to the right.
	if(instr->LeftOp()->IsConstant()) {
		instr->InvertOrder(true /* invertOperands */, 
                           false /* invertEquality */);
	}

    // Try to replace <=/>= with </> in order to have fewer cases to test for.
    CanonicalizeIntegerCmp(instr);

	// Test for patterns that are common for both signed 
    // and unsigned comparisons.
	if(auto result = HandleIntegerCmp(instr)) {
		return result;
	}

    // Test for the comparison of memory addresses.
    if(auto result = HandleUcmpAddress(instr)) {
        return result;
    }

    // a < (1 << b) -> (a >> b) == 0
    // a >= (1 << b) -> (a >> b) != 0
    Operand* b;

    if(instr->IsEquality() && 
       Match<ShlInstr>(MatchInt(1, instr->ParentBlock()),
                       MatchAny(&b))(instr->RightOp())) {
        auto ushrOp = GetTemporary(b);
        irGen_.GetUshr(instr->LeftOp(), b, ushrOp);

        return LOG(CreateCompare(ushrOp, GetZeroInt(b), instr->Order(),
                                 instr->ResultOp()->GetType(), true));
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleUcmpAddress(UcmpInstr* instr) {
    // These transformation may simplify the computations, but the biggest
    // advantage is that they exposes loop invariant expressions.
	auto addrInstrA = instr->LeftOp()->DefiningInstrAs<AddressInstr>();
    auto addrInstrB = instr->RightOp()->DefiningInstrAs<AddressInstr>();

    if((addrInstrA || addrInstrB) == false) {
        return nullptr;
    }

    // (addr p1, a) ORDER (addr p2, b) -> (p1 ORDER p2) & (a ORDER b)
    // If 'p1 == p2' -> only 'a ORDER b'
    // If 'a == b' -> only 'p1 ORDER p2'
    if(addrInstrA && addrInstrB) {
        Operand* ptrCmpOp = nullptr;
        Operand* offsetCmpOp = nullptr;

        if(addrInstrA->BaseOp() != addrInstrB->BaseOp()) {
            ptrCmpOp = CreateCompare(addrInstrA->BaseOp(), 
                                     addrInstrB->BaseOp(),
                                     instr->Order(), 
                                     instr->ResultOp()->GetType(), true);
        }

        if(addrInstrA->IndexOp() != addrInstrB->IndexOp()) {
            offsetCmpOp = CreateCompare(addrInstrA->IndexOp(), 
                                        addrInstrB->IndexOp(),
                                        instr->Order(), 
                                        instr->ResultOp()->GetType(), true);
        }

        if(ptrCmpOp && offsetCmpOp) {
            // Both conditions must hold.
            auto andOp = irGen_.GetTemporary(instr->ResultOp()->GetType());
            irGen_.GetAnd(ptrCmpOp, offsetCmpOp, andOp);
            return LOG(andOp);
        }
        else if(ptrCmpOp) {
            return LOG(ptrCmpOp);
        }
        else if(offsetCmpOp) {
            return LOG(offsetCmpOp);
        }
        else {
            // The same expression is on both sides; this will be handled
            // later by the constant folder later.
            return nullptr;
        }
    }

    // (addr p1, a) ORDER p2 -> p1 ORDER (addr p2, -a)
    // We presume that 'p2' may be invariant, while 'p1' is not; 
    // this allows the hoisting of the address computation outside of the loop.
    // Note that this can be done easily only if 'p1' and 'p2' have the same type.
    if(addrInstrA && 
       (addrInstrA->BaseOp()->GetType() == instr->RightOp()->GetType())) {
        // If 'a' is a constant we always perform the simplifications.
        // If not, we do it only if the 'addr' has a single user (the 'ucmp').
        if((addrInstrA->IndexOp()->IsConstant() == false) &&
           (addrInstrA->ResultOp()->HasSingleUser() == false)) {
            return nullptr;
        }

        // We don't care now if it's a constant, it will be folded later.
        auto baseOp = addrInstrA->BaseOp();
        auto indexOp = addrInstrA->IndexOp();

        auto subOp = GetTemporary(indexOp);
        irGen_.GetSub(GetZeroInt(indexOp), indexOp, subOp);

        auto addrOp = GetTemporary(baseOp);
        irGen_.GetAddress(instr->RightOp(), subOp, addrOp);
        
        return LOG(CreateCompare(baseOp, addrOp, instr->Order(), 
                                 instr->ResultOp()->GetType(), true));
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFcmp(FcmpInstr* instr) {
	// We use the constant evaluator even if not both operands are constant,
	// because it also eliminates very simple cases, like 'a & 0 -> 0'.
	if(auto result = folder_.FoldCompare(Instr_Fcmp, instr->LeftOp(), 
										 instr->RightOp(), instr->Order(),
                                         instr->ParentBlock())) {
		return LOG(result);
	}
	
	// If the left operand is a constant move it to the right,
	// reduces the cases we need to test for.
	if(instr->LeftOp()->IsConstant()) {
		instr->InvertOrder(true /* invertOperands */, 
                           false /* invertEquality */);
	}

    // Test for expressions that involve intrinsics.
    if(auto result = HandleCmpIntrinsic(instr)) {
        return result;
    }
	
    Operand* opA = instr->LeftOp();
    Operand* opB = instr->RightOp();
    Operand* a;
    Operand* b;
    FloatConstant* C;

    // fcmp (fext a), (fext b) -> fcmp a, b
    if(MatchConversion<FextInstr>(MatchAny(&a))(opA) &&
       MatchConversion<FextInstr>(MatchAny(&b))(opB)) {
        return LOG(CreateCompare(a, b, instr->Order(), 
                                 instr->ResultOp()->GetType()));
    }

    // fcmp ORDER (-a), C -> fcmp invert(ORDER) a, -C
    if(Match<FsubInstr>(MatchFloat(0.0), MatchAny(&a))(opA) &&
       MatchFC(&C)(opB)) {
        OrderType invertedOrder = instr->InvertedOrder(instr->Order());
        auto minusC = irGen_.GetFloatingConst(C->GetType(), -C->Value());

        return LOG(CreateCompare(a, minusC, invertedOrder, 
                                 instr->ResultOp()->GetType()));
    }
	
    // fcmp ORDER (-a), (-b) -> fcmp invert(ORDER) a, b
    if(Match<FsubInstr>(MatchFloat(0.0), MatchAny(&a))(opA) &&
       Match<FsubInstr>(MatchFloat(0.0), MatchAny(&b))(opB)) {
        OrderType invertedOrder = instr->InvertedOrder(instr->Order());

        return LOG(CreateCompare(a, b, invertedOrder, 
                                 instr->ResultOp()->GetType()));
    }

	// Try to simplify a comparison between an integer converted to a float
	// and a constant float. In most cases we can compare the integer
	// (before the conversion) with the constant converted to an integer.
	// itof(a, float) < 5.0 -> a < 5
	auto floatConst = instr->RightOp()->As<FloatConstant>();

	if((floatConst == nullptr) || FA::IsNaN(floatConst)) {
        return nullptr;
    }
	
	if((instr->LeftOp()->DefiningInstrIs<ItofInstr>() ||
		instr->LeftOp()->DefiningInstrIs<UitofInstr>()) == false) {
		return nullptr;
	}
	
	auto itofInstr = instr->LeftOp()->DefiningInstrAs<ConversionInstr>();
	bool isSigned = itofInstr->IsItof();

	// We need to make sure that the number doesn't lose precision when converted
	// to a floating-point number; if it does we can't do the simplification.
	// IEEE 754 defines a 52 bit fraction for double-precision and
	// a 23 bit fraction for single-precision. The integer shouldn't 
    // have more bits than these values!
	auto floatKind = floatConst->GetType()->GetSubtype();
	auto intType = itofInstr->TargetOp()->GetType()->As<IntegerType>();
	auto intKind = intType->GetSubtype();
	int intBits = intType->SizeInBits();
	OrderType order = instr->Order();

	if(intBits > FA::FractionSizeInBits(floatConst->GetType())) {
		return nullptr;
	}

	// First check for comparisons for which we can determine the result directly.
	if(auto result = TestItofRange(instr, itofInstr, floatConst)) {
		return result;
	}

	// Now we have two situations: comparisons with and without a fraction.
	// Without: itof(a, double) == 5.0 -> a == 5
	// With:    itof(a, double) == 5.6 -> false
	// We test for the fraction by converting the float constant to an integer,
	// then back to a floating number. If it is unchanged it means that
	// no fraction is involved.
	// (A) 5.0 --int--> 5 --float--> 5.0 (B) => A == B, no fraction
	// (A) 5.6 --int--> 5 --float--> 5.0 (B) => A != B, fraction
	__int64 floatToInt = FA::Ftoi(floatConst, intType);
	double intToFloat = isSigned ? IA::Itof(floatToInt, intKind, floatKind) :
								   IA::Uitof(floatToInt, intKind, floatKind);

	if(FA::AreEqual(intToFloat, floatConst->Value(), floatKind)) {
		// No fraction, we can simplify without any further tests.
		auto constantOp = irGen_.GetIntConst(intType, floatToInt);

		return LOG(CreateCompare(itofInstr->TargetOp(), constantOp, order,
							     instr->ResultOp()->GetType(), isSigned == false));
	}

	// There is a fraction, determine the result based on the order.
	return HandleFcmpFraction(instr, itofInstr, floatConst, floatToInt);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::TestItofRange(FcmpInstr* instr, ConversionInstr* itofInstr,
								 FloatConstant* floatConst) {
	auto floatKind = floatConst->GetType()->GetSubtype();
	auto intType = itofInstr->TargetOp()->GetType()->As<IntegerType>();
	bool isSigned = itofInstr->IsItof();
	auto order = instr->Order();

	if(isSigned) {
		// itof(a, double) > 1000 -> false, if 'a' has type 'int8'.
		// itof(a, double) < -500 -> false, if 'a' has type 'int8'.
		__int64 max = IA::GetMax(intType, true /* isSigned */);
		__int64 min = IA::GetMin(intType, true /* isSigned */);

		if(FA::IsSmaller((double)max, floatConst->Value(), floatKind) ||
		   FA::IsLarger((double)min, floatConst->Value(), floatKind)) {
			// The comparison can never be true.
			if((order == Order_Equal) || (order == Order_Greater) ||
			   (order == Order_GreaterOrEqual)) {
				return GetBool(false, instr->ResultOp());
			}
			else {
				// For !=, <, and <=
				return LOG(GetBool(true, instr->ResultOp()));
			}
		}
	}
	else {
		// uitof(a, double) > 1000 ->  false, if 'a' has type 'int8'.
		__int64 max = IA::GetMax(intType, false /* isSigned */);

		if(FA::IsSmaller((double)max, floatConst->Value(), floatKind)) {
			// The comparison can never be true.
			if((order == Order_Equal) || (order == Order_Greater) ||
			   (order == Order_GreaterOrEqual)) {
				return LOG(GetBool(false, instr->ResultOp()));
			}
			else {
				// For !=, <, and <=
				return LOG(GetBool(true, instr->ResultOp()));
			}
		}
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFcmpFraction(FcmpInstr* instr, ConversionInstr* itofInstr,
									  FloatConstant* floatConst, __int64 floatToInt) {
	auto intType = itofInstr->TargetOp()->GetType()->As<IntegerType>();
	bool isSigned = itofInstr->IsItof();
	auto order = instr->Order();

	switch(order) {
		case Order_Equal: {
			// itof(a, double) == 5.6 -> false
			return LOG(GetBool(false, instr->ResultOp()));
		}
		case Order_NotEqual: {
			// itof(a, double) != 5.6 -> true
			return LOG(GetBool(true, instr->ResultOp()));
		}
		case Order_Less:
		case Order_LessOrEqual: {
			// itof(a, double)  <  5.6 -> a <= 5
			// itof(a, double)  < -5.6 -> a < 5
			// uitof(a, double) < -5.6 -> false
			if((isSigned == false) && FA::IsNegative(floatConst)) {
				return LOG(GetBool(false, instr->ResultOp()));
			}
			else {
				auto constantOp = irGen_.GetIntConst(intType, floatToInt);
				OrderType order = FA::IsNegative(floatConst) ?
                                  Order_Less : Order_LessOrEqual;

				return LOG(CreateCompare(itofInstr->TargetOp(), constantOp, order,
									     instr->ResultOp()->GetType(), 
                                         isSigned == false));
			}
		}
		case Order_Greater:
		case Order_GreaterOrEqual: {
			// itof(a, double) > 5.6 -> a > 5
			// itof(a, double) > -5.6 -> a >= 5
			// uitof(a, double) > -5.6 -> true
			if((isSigned == false) && FA::IsNegative(floatConst)) {
				return LOG(GetBool(true, instr->ResultOp()));
			}
			else {
				auto constantOp = irGen_.GetIntConst(intType, floatToInt);
				OrderType order = FA::IsNegative(floatConst) ?
                                  Order_GreaterOrEqual : Order_Greater;

				return LOG(CreateCompare(itofInstr->TargetOp(), constantOp, 
                                         order, instr->ResultOp()->GetType(), 
                                         isSigned == false));
			}
		}
		default: DebugValidator::Unreachable();
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Peephole::ShrinkIntegerCmpConstants(CmpInstrBase* instr) {
    // These transformations try to reduce the involved constant by changing
    // the order of the comparison. This may help in certain situations.
    // Note that we do this only if signed overflow is undefined.
    if(instr->HasUndefinedOverflow() == false) {
        return;
    }

    Operand* opA = instr->LeftOp();
    Operand* opB = instr->RightOp();

    if(auto C = AsIntConstant(opA, instr->ParentBlock())) {
        if(instr->IsLessOrEqual()) {
            // C <= a -> (C - 1) < a
            auto constantOp = irGen_.GetIntConst(C->GetType(), 
                                                 IA::Sub(C, GetOneInt(C)));
            instr->SetLeftOp(constantOp);
            instr->SetOrder(Order_Less);
        }
        else if(instr->IsLess() && C->IsNegative()) {
            // -C < a -> (-C-1) <= a 
            auto constantOp = irGen_.GetIntConst(C->GetType(), 
                                                 IA::Sub(C, GetOneInt(C)));
            instr->SetLeftOp(constantOp);
            instr->SetOrder(Order_LessOrEqual);
        }
        else if(instr->IsGreater()) {
            // C > a  -> (C-1) >= a  
            auto constantOp = irGen_.GetIntConst(C->GetType(), 
                                                 IA::Sub(C, GetOneInt(C)));
            instr->SetLeftOp(constantOp);
            instr->SetOrder(Order_GreaterOrEqual);
        }
        else if(instr->IsGreaterOrEqual() && C->IsNegative()) {
            // -C >= a -> (-C-1) > a
            auto constantOp = irGen_.GetIntConst(C->GetType(), 
                                                 IA::Sub(C, GetOneInt(C)));
            instr->SetLeftOp(constantOp);
            instr->SetOrder(Order_Greater);
        }
    }
    else if(auto subInstr = opA->DefiningInstrAs<SubInstr>()) {
        if(subInstr->ResultOp()->HasSingleUser() == false) {
            return;
        }

        auto C = AsIntConstant(subInstr->RightOp(), 
                               instr->ParentBlock());
        if(C == nullptr) {
            return;
        }

        if(instr->IsLess()) {
            // a - C < b -> a - (C-1) <= b  
            auto constantOp = irGen_.GetIntConst(C->GetType(), 
                                                 IA::Sub(C, GetOneInt(C)));
            subInstr->SetRightOp(constantOp);
            instr->SetOrder(Order_LessOrEqual);
        }
        else if(instr->IsGreaterOrEqual()) {
            // a - C >= b -> a - (C-1) > b
            auto constantOp = irGen_.GetIntConst(C->GetType(), 
                                                 IA::Sub(C, GetOneInt(C)));
            subInstr->SetRightOp(constantOp);
            instr->SetOrder(Order_Greater);
        }
    }
    else if(auto addInstr = opA->DefiningInstrAs<SubInstr>()) {
        if(addInstr->ResultOp()->HasSingleUser() == false) {
            return;
        }

        auto C = AsIntConstant(addInstr->RightOp(), 
                               instr->ParentBlock());
        if(C == nullptr) {
            return;
        }

        if(instr->IsGreater()) {
            // a + C > b -> a + (C-1) >= b  
            auto constantOp = irGen_.GetIntConst(C->GetType(), 
                                                 IA::Sub(C, GetOneInt(C)));
            addInstr->SetRightOp(constantOp);
            instr->SetOrder(Order_GreaterOrEqual);
        }
        else if(instr->IsLessOrEqual()) {
            // a + C <= b -> a + (C-1) < b  
            auto constantOp = irGen_.GetIntConst(C->GetType(), 
                                                 IA::Sub(C, GetOneInt(C)));
            addInstr->SetRightOp(constantOp);
            instr->SetOrder(Order_Less);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Peephole::CanonicalizeIntegerCmp(CmpInstrBase* instr) {
    // This helps reducing the number of cases that need to be tested.
	// We don't do it if the constant is 0, because this optimizes some cases.
    // Note that 'ucmp' can be used to compare pointers, 
    // in which case we give up early.
    if(instr->IsUcmp() && instr->LeftOp()->IsPointer()) {
        return;
    }

    auto C = AsIntConstant(instr->RightOp(), instr->ParentBlock());

    if(C == nullptr) {
        return;
    }

    // Comparisons with zero are usually implemented
    // by smaller/faster target CPU instructions, so we leave them.
	if(C->IsZero()) {
        return;
    }

    // a <= C -> a < (C + 1)
    if(instr->IsLessOrEqual()) {
        auto constantOp = irGen_.GetIntConst(C->GetType(), 
                                             IA::Add(C, GetOneInt(C)));
        instr->SetRightOp(constantOp);
        instr->SetOrder(Order_Less);
    }
    // a >= C -> a > (C - 1)
    else if(instr->IsGreaterOrEqual()) {
        auto constantOp = irGen_.GetIntConst(C->GetType(), 
                                             IA::Sub(C, GetOneInt(C)));
        instr->SetRightOp(constantOp);
        instr->SetOrder(Order_Greater);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmp(CmpInstrBase* instr) {
	Operand* opA = instr->LeftOp();
	Operand* opB = instr->RightOp();
	Operand* resultOp = instr->ResultOp();
	OrderType order = instr->Order();
	bool isSigned = instr->IsCmp();
	bool isUnsigned = isSigned == false;
    auto block = instr->ParentBlock();
	Operand* a;
	Operand* b;
	IntConstant* C;
    IntConstant* C1;
    IntConstant* C2;

	// a ORDER a -> 1 / 0
	if(AreEqual(opA, opB, block)) {
		switch(order) {
			case Order_Equal:
			case Order_LessOrEqual:
			case Order_GreaterOrEqual: {
				return LOG(GetBool(true, resultOp));
			}
			case Order_NotEqual:
			case Order_Less:
			case Order_Greater: {
				return LOG(GetBool(false, resultOp));
			}
			default: DebugValidator::Unreachable();
		}
	}

	// If the left operand is not an instruction we move it to the right,
	// so that we have fewer cases to test for in other places.
	if(opA->IsConstant()) {
		instr->InvertOrder(true /* invertOperands */, 
                           false /* invertEquality */);
		return LOG(instr->ResultOp());
	}

	// Try to simplify comparisons with 0, one of the most common cases.
	if(IsZeroInt(opB, block)) {
        if(auto result = HandleIntegerCmpWithZero(opA, opB, resultOp,
                                                  order, isUnsigned, block)) {
            return result;
        }
	}

	// Now test for the more general case in which we have any constant integer.
	if(auto intConst = opB->As<IntConstant>()) {
		if(auto result = HandleIntegerCmpConstGeneric(opA, intConst, resultOp, 
													  order, isSigned, block)) {
			return result;
		}
	}

	// The following situation is pretty common.
	// (a - b) == 0 -> a == b
	// (a - b) != 0 -> a != b
	if(Match<SubInstr>(MatchAny(&a), MatchAny(&b))(opA) && 
       instr->IsEquality() && opB->IsZeroInt()) {
		return CreateCompare(a, b, order, resultOp->GetType(), isUnsigned);
	}

	// Test for the comparison of a casted operand with a constant.
	if(auto result = HandleIntegerCmpExtension(opA, opB, resultOp, 
                                               order, isSigned, block)) {
		return result;
	}

	// Test for comparisons that involve pointers.
	if(auto result = HandleIntegerCmpPtr(opA, opB, resultOp, order)) {
		return result;
	}

	// Test for comparisons that involve an arithmetic instructions
	// at least on one of the sides.
	if(auto result = HandleIntegerCmpBin(opA, opB, resultOp, 
                                         order, isSigned, block)) {
		return result;
	}

	// Test for the comparison of a boolean temporary with a integer constant.
	// Only for some special constants we can do the simplifications.
	if(opA->IsBoolean()) {
		if(auto result = HandleIntegerCmpBool(opA, opB, resultOp, 
                                              order, isSigned, block)) {
			return result;
		}
	}

    // (a +- C1) ORDER (b +- C2) -> a ORDER (b +- C2 -+ C1)
    // a + 5 < a + 12 -> a < b + 7
    if(auto result = HandleIntegerCmpAddSubConst(opA, opB, resultOp, 
                                                 order, isSigned, block)) {
        return result;
    }

    // cmp neq (cmp ORDER a, b), 0 -> cmp ORDER a, b
    // Remove useless comparisons, sometimes they are introduced by the optimizer.
    // This works for any instruction on the inside, not just comparisons.
    if(instr->IsNotEqual() && IsZeroInt(opB, block) && 
       opA->HasDefiningInstruction()) {
        auto destOp = opA->DefiningInstruction()->GetDestinationOp();

        if(destOp->GetType() == resultOp->GetType()) {
            return LOG(opA);
        }
    }

	// ~a ORDER ~b -> b ORDER a
	if(IsLogicalNot(opA, &a) && 
       IsLogicalNot(opB, &b)) {
		return LOG(CreateCompare(b, a, order, resultOp->GetType(), isUnsigned));
	}

	// ~a ORDER C -> ~C ORDER a
	if(IsLogicalNot(opA, &a) && 
       MatchIC(&C, block)(opB)) {
		__int64 result = IA::Not(C);
		auto constantOp = irGen_.GetIntConst(C->GetType(), result);
		return LOG(CreateCompare(constantOp, a, order, 
                                 resultOp->GetType(), isUnsigned));
	}

	// Test for cases that involve 'xor' with operands that are not constants.
	if((order == Order_Equal) || 
       (order == Order_NotEqual)) {
		if(auto result = HandleIntegerCmpXorEquality(opA, opB, resultOp, 
													 order, isSigned, block)) {
			return result;
		}
	}

    // (a & 1) != 0 -> a & 1, if the same type is used
    if(instr->IsNotEqual() && IsZeroInt(opB, block) && 
       (opA->GetType() == resultOp->GetType()) &&
       Match<AndInstr>(MatchAny(&a), MatchInt(1, block))(opA)) {
        return LOG(opA);
    }

    // (a * C1) ==/!= C2 -> a ==/!= (C2 / C1), 
    // if 'C1' divides 'C2' exactly.
    if(instr->IsEquality() && MatchIC(&C2, block)(opB) &&
       Match<MulInstr>(MatchAny(&a), MatchIC(&C1, block))(opA)) {
        if(IA::Mod(C2, C1) == 0) {
            auto consOp = irGen_.GetIntConst(C1->GetType(), 
                                             IA::Div(C2, C1));
            auto mulOp = GetTemporary(opA);

            return LOG(CreateCompare(a, consOp, order, 
                                     resultOp->GetType(), false));
        }
    }

    // (1 << a) & b ==/!= 0 -> (b >> a) & 1 ==/!= 0
    // b & (1 << a) ==/!= 0 -> (b >> a) & 1 ==/!= 0
    // The cost is basically the same, but it's an improvement for CPUs
    // which can't have a constant as the right operand (x86, for example).
    if(instr->IsEquality() && IsZeroInt(opB, block) && isUnsigned 
        &&
       (Match<AndInstr>(Match<ShlInstr>(MatchInt(1, block), MatchAny(&a)), 
                        MatchAny(&b))(opA) 
            ||
        Match<AndInstr>(MatchAny(&b), 
                        Match<ShlInstr>(MatchInt(1, block), MatchAny(&a)))(opA))) {
            auto ushrOp = GetTemporary(a);
            irGen_.GetUshr(b, a, ushrOp);
            
            auto andOp = GetTemporary(opA);
            irGen_.GetAnd(ushrOp, GetOneInt(opA), andOp);
            
            return LOG(CreateCompare(andOp, GetZeroInt(opA), order, 
                                     resultOp->GetType(), true));
    }

	if(auto loadInstr = opA->DefiningInstrAs<LoadInstr>()) {
		auto intConst = opB->As<IntConstant>();

		if(intConst == nullptr) {
            return nullptr;
        }

		return HandleIntegerCmpLoad(loadInstr, intConst, resultOp, 
                                    order, isSigned);
	}

    // Test for expressions that involve intrinsics.
    if(auto result = HandleCmpIntrinsic(instr)) {
        return result;
    }

	// Test for the comparison of two variable references. All variables on the stack
	// have a different address, so we can decide now on the result of the comparison.
	// Note that both references can't point to the same variable, because it was
	// caught by the test above, and references are unique in a module.
	if(opA->IsVariableReference() &&
       opB->IsVariableReference()) {
		switch(order) {
			case Order_Equal:
			case Order_LessOrEqual:
			case Order_GreaterOrEqual: {
				return LOG(GetBool(false, resultOp));
			}
			case Order_NotEqual:
			case Order_Less:
			case Order_Greater: {
				return LOG(GetBool(true, resultOp));
			}
			default: DebugValidator::Unreachable();
		}
	}

	// Test for a pretty uncommon case when an operand is compared to itself,
	// but with an added constant. This could be used to check for overflow.
	if(auto result = HandleIntegerCmpSelf(opA, opB, resultOp, 
                                          order, isSigned)) {
		return result;
	}

    return HandleIntegerCmpNegated(opA, opB, resultOp, 
                                   order, isSigned, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpWithZero(Operand* opA, Operand* opB, 
                                            Operand* resultOp, OrderType order, 
                                            bool isUnsigned, Block* block) {
    // a * C ORDER 0 -> a ORDER 0, if 'C >= 0'
	// a * C ORDER 0 -> a inverted(ORDER) 0, if 'C < 0'
    Operand* a;
    IntConstant* C;

    if(Match<MulInstr>(MatchAny(&a), MatchIC(&C, block))(opA)) {
		if(C->IsPositive()) {
			return LOG(CreateCompare(a, opB, order, 
                                     resultOp->GetType(), isUnsigned));
		}
		else {
			return LOG(CreateCompare(a, opB, CmpInstrBase::InvertedOrder(order), 
                                     resultOp->GetType(), isUnsigned));
		}
    }

	switch(order) {
		case Order_Less: {
			// If we compare unsigned numbers this can never be true,
			// because the integer wraps around and becomes very large.
			if(isUnsigned) {
				return LOG(GetBool(false, resultOp));
			}
            else if(opInfo_.IsNegative(opA, block)) {
                // The sign bit is definitely set.
                return LOG(GetBool(true, resultOp));
            }
			break;
		}
		case Order_GreaterOrEqual: {
			// For unsigned numbers this is always true, because this
			// covers the entire representable range.
			if(isUnsigned) {
				return LOG(GetBool(true, resultOp));
			}
            else if(opInfo_.IsNegative(opA, block)) {
                // The sign bit is definitely set.
                return LOG(GetBool(false, resultOp));
            }
			break;
		}
		case Order_Greater: {
            // Operand Info is needed for this case.
            if(isUnsigned) {
                if(opInfo_.IsNotZero(opA, block)) {
                    return LOG(GetBool(true, resultOp));
                }
            }
            else if(opInfo_.IsNotZero(opA, block) && 
                    (opInfo_.IsNegative(opA, block) == false)) {
				return LOG(GetBool(true, resultOp));
            }
			break;
		}
        case Order_Equal: {
            // Operand Info is needed for this case.
            if(opInfo_.IsZero(opA, block)) {
                return LOG(GetBool(true, resultOp));
            }
            else if(opInfo_.IsNotZero(opA, block)) {
                return LOG(GetBool(false, resultOp));
            }
            break;
        }
        case Order_NotEqual: {
            // Operand Info is needed for this case.
            if(opInfo_.IsNotZero(opA, block)) {
                return LOG(GetBool(true, resultOp));
            }
            else if(opInfo_.IsZero(opA, block)) {
                return LOG(GetBool(false, resultOp));
            }
            break;
        }
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpAddSubConst(Operand* opA, Operand* opB, 
                                               Operand* resultOp, OrderType order, 
                                               bool isSigned, Block* block) {
    // It's safe to do this simplification only for signed numbers.
    if(isSigned == false) {
        return nullptr;
    }

    // (a +- C1) ORDER (b +- C2) -> a ORDER (b +- C2 -+ C1)
    // Note that we can do this only if the addition or
    // subtraction doesn't overflow.
	auto arithInstrA = opA->DefiningInstrAs<ArithmeticInstr>();

	if((arithInstrA == nullptr) ||
       (arithInstrA->HasUndefinedOverflow() == false)) {
        return nullptr;
    }

    if((arithInstrA->IsAdd() || arithInstrA->IsSub()) == false) {
        return nullptr;
    }

    // Check for the constant on the right.
    auto C1 = AsIntConstant(arithInstrA->RightOp(), block);

    if(C1 == nullptr) {
        return nullptr;
    }

    // Validate the second arithmetic instruction.
    auto arithInstrB = opB->DefiningInstrAs<ArithmeticInstr>();

	if((arithInstrB == nullptr) ||
       (arithInstrB->HasUndefinedOverflow() == false)) {
        return nullptr;
    }

    if((arithInstrB->IsAdd() || 
        arithInstrB->IsSub()) == false) {
        return nullptr;
    }

    // Check for the constant on the right.
    auto C2 = AsIntConstant(arithInstrB->RightOp(), block);

    if(C2 == nullptr) {
        return nullptr;
    }

    // We try to move the constant to the side where overflow doesn't occur.
    // a ORDER (b +- C2 -+ C1). How we compute the constant:
    // +  +  ->  C2 - C1                 -
    // +  -  -> -C2 - C1 -> -(C2 + C1)   +
    // -  +  ->  C2 + C1                 +
    // -  -  -> -C2 + C1 -> -(C2 - C1)   -
    auto a = arithInstrA->LeftOp();
    auto b = arithInstrB->LeftOp();
    bool leftAdd = arithInstrA->IsAdd();
    bool rightAdd = arithInstrB->IsAdd();

    bool sameOpcode = arithInstrA->GetOpcode() == 
                      arithInstrB->GetOpcode();
    bool overflow = sameOpcode ? IA::SubOverflows(C2, C1) : 
                                 IA::AddOverflows(C2, C1);

    if(overflow == false) {
        // A second condition is that the absolute value of the new constant
        // should be smaller than the previous right one.
        __int64 value = sameOpcode ? IA::Sub(C2, C1) : IA::Add(C2, C1);
        __int64 valueAbs = value < 0 ? -value : value;

        if(valueAbs < IA::LimitToType(C2)) {
            // We can perform the simplification.
            auto resultOp = GetTemporary(C1);
            auto constantOp = irGen_.GetIntConst(C1->GetType(), value);

            if(rightAdd) irGen_.GetAdd(b, constantOp, resultOp);
            else irGen_.GetSub(b, constantOp, resultOp);
            
            return LOG(CreateCompare(a, resultOp, order, 
                                     resultOp->GetType(), false));
        }
    }
        
    // Try to move the right constant to the left otherwise.
    // (a +- C1 -+ C2) ORDER b
    overflow = sameOpcode ? IA::SubOverflows(C1, C2) :
                            IA::AddOverflows(C1, C2);

    if(overflow == false) {
        // A second condition is that the absolute value of the new constant
        // should be smaller than the previous left one.
        __int64 value = sameOpcode ? IA::Sub(C1, C2) : 
                                     IA::Add(C1, C2);
        value = value < 0 ? -value : value;

        if(value < IA::LimitToType(C1)) {
            // We can perform the simplification.
            auto resultOp = GetTemporary(C1);
            auto constantOp = irGen_.GetIntConst(C1->GetType(), value);

            if(leftAdd) irGen_.GetAdd(a, constantOp, resultOp);
            else irGen_.GetSub(a, constantOp, resultOp);
           
            return LOG(CreateCompare(resultOp, b, order, 
                                     resultOp->GetType(), false));
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpConstGeneric(Operand* opA, IntConstant* intConst, 
												Operand* resultOp, OrderType order,
												bool isSigned, Block* block) {
	// The first test tries to determine a range for the non-constant operand.
	if(auto result = HandleIntegerCmpConst(opA, intConst, resultOp, 
                                           order, block)) {
		return result;
	}
	
	// Other simplifications can still be performed.
	Operand* result = nullptr;
	auto definingInstr = opA->DefiningInstruction();

	if(definingInstr == nullptr) {
        return nullptr;
    }

	switch(definingInstr->GetOpcode()) {
		case Instr_And: {
			auto andInstr = static_cast<AndInstr*>(definingInstr);
			result = HandleIntegerCmpAnd(andInstr, intConst, resultOp, 
                                         order, isSigned, block);
			break;
		}
		case Instr_Or: {
			auto orInstr = static_cast<OrInstr*>(definingInstr);
			result = HandleIntegerCmpOr(orInstr, intConst, resultOp, 
                                        order, isSigned, block);
			break;
		}
		case Instr_Xor: {
			auto xorInstr = static_cast<XorInstr*>(definingInstr);
			result = HandleIntegerCmpXor(xorInstr, intConst, resultOp, 
                                         order, isSigned, block);
			break;
		}
		case Instr_Shl: {
			auto shlInstr = static_cast<ShlInstr*>(definingInstr);
			result = HandleIntegerCmpShl(shlInstr, intConst, resultOp, 
                                         order, isSigned, block);
			break;
		}
		case Instr_Shr:
		case Instr_Ushr: {
			auto shrInstr = static_cast<LogicalInstr*>(definingInstr);
			result = HandleIntegerCmpShr(shrInstr, intConst, resultOp, 
                                         order, isSigned, block);
			break;
		}
		case Instr_Div:
		case Instr_Udiv: {
			auto divInstr = static_cast<ArithmeticInstr*>(definingInstr);
			result = HandleIntegerCmpDiv(divInstr, intConst, resultOp, 
                                         order, isSigned, block);
			break;
		}
	}

	if(result) {
		// One of the cases above succeeded.
		return result;
	}

	// If the order is 'equal' or 'not equal' some more tests can be performed.
	if((order == Order_Equal) ||
       (order == Order_NotEqual)) {
		if(auto result = HandleIntegerCmpEquality(definingInstr, intConst, resultOp,
												  order, isSigned, block)) {
			return result;
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpConst(Operand* opA, IntConstant* intConst, 
										 Operand* resultOp, OrderType order,
                                         Block* block) {
	// Try to deduce a range for the left operand. For some binary operators
	// it can be deduced without knowing anything about the temporary,
	// with the condition that it involves a constant operand.
	__int64 value = intConst->Value();
	bool foundRange = false;
	bool minInclusive = false;
	bool maxInclusive = false;
	__int64 rangeMin;
	__int64 rangeMax;

	// Try to estimate a conservative range for the operand. This is possible for
	// operations like modulus, division, shifts, etc.
	if(EstimateOperandRange(opA, intConst, minInclusive, maxInclusive,
							rangeMin, rangeMax, block) == false) {
		// No range could be determined, give up.
		return nullptr;
	}

	// Determine the result of the comparison based on the found range.
	// Note that only in a few cases this can be safely done.
	switch(order) {
		case Order_Equal: {
			// The range limits must be the same in order to be sure of the result.
			if(rangeMin == rangeMax) {
				return LOG(GetBool(value == rangeMin, resultOp));
			}
			break;
		}
		case Order_NotEqual: {
			// The range limits must be the same in order to be sure of the result.
			if(rangeMin == rangeMax) {
				return LOG(GetBool(value != rangeMin, resultOp));
			}
			break;
		}
		case Order_Less: {
			// If the range is completely below the value -> true.
			if((maxInclusive && (rangeMax < value)) || 
               (rangeMax < (value + 1))) {
				return LOG(GetBool(true, resultOp));
			}

			// If the range is above the value -> false.
			if((minInclusive && (rangeMin >= value)) || 
               (rangeMin >= (value + 1))) {
				return LOG(GetBool(false, resultOp));
			}
			break;
		}
		case Order_LessOrEqual: {
			// If the range is below the value -> true.
			if((maxInclusive && (rangeMax <= value)) || 
               (rangeMax <= (value + 1))) {
				return LOG(GetBool(true, resultOp));
			}

			// If the range is completely above the value -> false.
			if((minInclusive && (rangeMin > value)) || 
               (rangeMin > (value - 1))) {
				return LOG(GetBool(false, resultOp));
			}
			break;
		}
		case Order_Greater: {
			// If the range is completely above the value -> true.
			if((minInclusive && (rangeMin > value)) || 
               (rangeMin > (value - 1))) {
				return LOG(GetBool(true, resultOp));
			}
			// If the range is completely below the value -> false.
			if((maxInclusive && (rangeMax <= value)) || 
               (rangeMax <= (value + 1))) {
				return LOG(GetBool(false, resultOp));
			}
			break;
		}
		case Order_GreaterOrEqual: {
			// If the range is completely above the value -> true.
			if((minInclusive && (rangeMin >= value)) || 
               (rangeMin >= (value - 1))) {
				return LOG(GetBool(true, resultOp));
			}
			// If the range is completely below the value -> false.
			if((maxInclusive && (rangeMax < value)) || 
               (rangeMax < (value + 1))) {
				return LOG(GetBool(false, resultOp));
			}
			break;
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::EstimateOperandRange(Operand* opA, IntConstant* intConst, 
									bool& minInclusive, bool& maxInclusive, 
									__int64& rangeMin, __int64& rangeMax,
                                    Block* block) {
	IntConstant* C;
    Operand* other;
    OperandInfo opInfo(irGen_.GetUnit());

	// Try to determine a range for the specified operand. 
    // For some operations this can be done if we consider 
    // the minimum and maximum representable values.
	// a % C -> (-C, C), for signed numbers.
	if(Match<UmodInstr>(MatchAny(&other), MatchIC(&C))(opA)) {
		if(intConst->IsZero()) {
            return false;
        }

        __int64 otherMin = opInfo.GetMinimumValue(other, true, block);
        __int64 otherMax = opInfo.GetMaximumValue(other, false, block);

        rangeMin = std::max(otherMin, -C->Value());
		rangeMax = std::min(otherMax, C->Value());
		return true;
	}

	// a % C -> [0, C), for unsigned numbers.
	if(Match<UmodInstr>(MatchAny(&other), MatchIC(&C))(opA)) {
		if(intConst->IsZero()) {
            return false;
        }
		
        rangeMin = 0;
        __int64 otherMax = opInfo.GetMaximumValue(other, false, block);

        rangeMax = std::min(otherMax, C->Value());
		minInclusive = true;
		return true;
	}

	// a / C -> [INT_MIN / C, INT_MAX / C], for signed numbers.
	if(Match<DivInstr>(MatchAny(&other), MatchIC(&C))(opA)) {
		if(intConst->IsZero()) {
            return false;
        }
		
        __int64 otherMin = opInfo.GetMinimumValue(other, true, block);
        __int64 otherMax = opInfo.GetMaximumValue(other, false, block);

        rangeMin = otherMin / C->Value();
		rangeMax = otherMax / C->Value();
		minInclusive = true;
		maxInclusive = true;
		return true;
	}
		
	// a / C -> [0, UINT_MAX / C], for unsigned numbers.
	if(Match<UdivInstr>(MatchAny(&other), MatchIC(&C))(opA)) {
		if(intConst->IsZero()) {
            return false;
        }

		rangeMin = 0;
        __int64 otherMax = opInfo.GetMaximumValue(other, false, block);

		rangeMax = otherMax / C->Value();
		minInclusive = true;
		maxInclusive = true;
		return true;
	}

	// a >> C -> [0, UINT_MAX >> C], for unsigned numbers.
	if(Match<UshrInstr>(MatchAny(&other), MatchIC(&C))(opA)) {
		rangeMin = 0;
        __int64 otherMax = opInfo.GetMaximumValue(other, false, block);

		rangeMax = otherMax >> C->Value();
		minInclusive = true;
		maxInclusive = true;
		return true;
	}

	// a >> C -> [INT_MIN >> C, INT_MAX >> C], for signed numbers.
	if(Match<ShrInstr>(MatchAny(&other), MatchIC(&C))(opA)) {
        __int64 otherMin = opInfo.GetMinimumValue(other, true, block);
        __int64 otherMax = opInfo.GetMaximumValue(other, false, block);

		rangeMin = otherMin >> C->Value();
		rangeMax = otherMax >> C->Value();
		minInclusive = true;
		maxInclusive = true;
		return true;
	}

	// a | C -> [C, UINT_MAX], because at least the bits from 'C' are set.
	if(Match<OrInstr>(MatchAny(&other), MatchIC(&C))(opA)) {
        __int64 otherMax = opInfo.GetMaximumValue(other, true, block);

		rangeMin = C->Value();
        rangeMax = IA::GetMax(C->GetType(), false);
		minInclusive = true;
		maxInclusive = true;
		return true;
	}

	// a & C -> [0, C], because no more bits than the ones in 'C' can be set.
	if(Match<AndInstr>(MatchAny(&other), MatchIC(&C))(opA)) {
        __int64 otherMin = opInfo.GetMinimumValue(other, true, block);
        __int64 otherMax = opInfo.GetMaximumValue(other, false, block);

        rangeMin = std::min(otherMin, 0LL);
        rangeMax = std::max(otherMax, C->Value());
		minInclusive = true;
		maxInclusive = true;
		return true;
	}

	// The range cannot be determined.
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpExtension(Operand* opA, Operand* opB, 
                                             Operand* resultOp, OrderType order, 
                                             bool isSigned, Block* block) {
	// Test for the comparison of a casted operand with a constant.
	// zext(a, int32) == 0 -> a == 0 and other ones.
	if(auto zextInstrA = opA->DefiningInstrAs<ZextInstr>()) {
		// zext(a, int32) ORDER zext(b, int32) -> a ORDER b
		// Note that the simplification is valid only if 
        // the source types are the same.
		if(auto zextInstrB = opB->DefiningInstrAs<ZextInstr>()) {
			if(zextInstrA->TargetOp()->GetType() == 
               zextInstrB->TargetOp()->GetType()) {
				return LOG(CreateCompare(zextInstrA->TargetOp(), 
                                         zextInstrB->TargetOp(), 
									     order, resultOp->GetType(),
                                         isSigned == false));
			}
		}

		// If we compare the extended operand with a constant, try to convert
		// the constant to the type of the operand before the extension.
		// If no precision is lost we can eliminate the cast.
		// zext(a, int32) ORDER C -> a ORDER C
        // zext(a, int32) == 0 -> a == 0, for example.
		if(auto intConst = AsIntConstant(opB, block)) {
			auto sourceType = zextInstrA->TargetOp()->GetType()->As<IntegerType>();
			__int64 truncValue = IA::Trunc(intConst, sourceType);

			if(IA::AreEqual(truncValue, intConst->Value(), 
                            sourceType->GetSubtype())) {
				// No precision was lost, do the transformation.
				// We need a new constant operand having the right type.
				auto constantOp = irGen_.GetIntConst(sourceType, truncValue);
				return LOG(CreateCompare(zextInstrA->TargetOp(), constantOp, 
                                         order, resultOp->GetType(),
                                         isSigned == false));
			}

			// Even if the constant can't be represented, in some cases
			// we can determine the result taking notice of the following facts:
			// 1. because the left part is zero extended from a smaller to a larger
			//    type, the upper half is definitely zero.
			// 2. the constant is larger than the source type of the target,
			//    meaning that it has at least one bit set in the upper half.
			// 3. from (1) and (2) it results that the constant is always larger
			//    than the extended value (for unsigned numbers).
			switch(order) {
				case Order_Equal: {
					return LOG(GetBool(false, resultOp));
				}
				case Order_NotEqual: {
					return LOG(GetBool(true, resultOp));						
				}
				case Order_Less:
				case Order_LessOrEqual: {
					return LOG(GetBool(true, resultOp));
				}
				case Order_Greater:
				case Order_GreaterOrEqual: {
					return LOG(GetBool(false, resultOp));
				}
			}
		}
	}
	else if(auto sextInstr = opA->DefiningInstrAs<SextInstr>()) {
		// sext(a, int32) ORDER sext(b, int32) -> a ORDER b
		// Note that the simplification is valid only if 
        // the source types are the same.
		if(auto sextInstrB = opB->DefiningInstrAs<SextInstr>()) {
			if(sextInstr->TargetOp()->GetType() == 
               sextInstrB->TargetOp()->GetType()) {
				return LOG(CreateCompare(sextInstr->TargetOp(), 
                                         sextInstrB->TargetOp(), 
									     order, resultOp->GetType(), 
                                         isSigned == false));
			}
		}

		// If we compare the extended operand with a constant, try to convert
		// the constant to the type of the operand before the extension.
		// If no precision is lost we can eliminate the cast.
		// sext(a, int32) ORDER C -> a ORDER C
		if(auto intConst = AsIntConstant(opB, block)) {
			auto sourceType = sextInstr->TargetOp()->GetType()->As<IntegerType>();
			__int64 truncValue = IA::Trunc(intConst, sourceType);

			if(IA::AreEqual(truncValue, intConst->Value(), 
                            sourceType->GetSubtype())) {
				auto constantOp = irGen_.GetIntConst(sourceType, truncValue);
				return LOG(CreateCompare(sextInstr->TargetOp(), constantOp, 
									     order, resultOp->GetType(), 
                                         isSigned == false));
			}

			// See the comment from the 'zext' case for an explanation.
			// The difference is that the left part is not always smaller than
			// the right one (the constant); this depends on the sign of the constant.
			switch(order) {
				case Order_Equal: {
					return LOG(GetBool(false, resultOp));
				}
				case Order_NotEqual: {
					return LOG(GetBool(true, resultOp));						
				}
				case Order_Less:
				case Order_LessOrEqual: {
					if(IA::IsPositive(intConst)) {
						return LOG(GetBool(true, resultOp));
					}
					else return LOG(GetBool(false, resultOp));
				}
				case Order_Greater:
				case Order_GreaterOrEqual: {
					if(IA::IsPositive(intConst)) {
						return LOG(GetBool(false, resultOp));
					}
					else return LOG(GetBool(true, resultOp));
				}
			}
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpPtr(Operand* opA, Operand* opB, Operand* resultOp, 
									   OrderType order) {
	// Test for the comparison of two pointers converted to integers.
	// If the type to which we convert has the same number of bits as the pointer
	// type then we can compare the pointers directly, because no bits are lost.
	// ptoi(a, int64) ORDER ptoi(b, int64) -> a ORDER b
	if(auto ptoiInstrA = opA->DefiningInstrAs<PtoiInstr>()) {
		if(auto ptoiInstrB = opB->DefiningInstrAs<PtoiInstr>()) {
			int ptrBits = GetTarget()->GetPointerSizeInBits();
			int intBits = ptoiInstrA->CastType()->As<IntegerType>()->SizeInBits();

			if(ptrBits == intBits) {
				// Compare the pointers directly, using only 'ucmp'.
				// If the pointers don't have the same type we introduce a 'ptop'
				// to convert one to the type of the other. This will not penalize us,
				// because 'ptop' will not be transformed in a target instruction.
				auto targetA = ptoiInstrA->TargetOp();
				auto targetB = ptoiInstrB->TargetOp();
				auto targetTypeA = targetA->GetType();
				auto targetTypeB = targetB->GetType();

				if(targetTypeA != targetTypeB) {
					// Convert 'A' to 'B'.
					auto targetA = irGen_.GetTemporary(targetTypeB);
					irGen_.GetPtop(ptoiInstrA->TargetOp(), 
                                   targetTypeB, targetA);
				}

				return LOG(CreateCompare(targetA, targetB, order,
									     resultOp->GetType(), true /* isUcmp */));
			}
		}
		else if(MatchInt(0)(opB)) {
			// (ptoi p, int64) ORDER 0 -> p ORDER nullptr
			// This transformation can be done only if the size of the pointer
			// matches the size of the integer to which it's converted.
			int pointerSize = TI::GetSize(ptoiInstrA->CastType(), GetTarget());
			int intSize = TI::GetSize(ptoiInstrA->TargetOp()->GetType(), GetTarget());

			if(pointerSize == intSize) {
				auto nullConst = GetNullptr(ptoiInstrA->TargetOp());
				return LOG(CreateCompare(ptoiInstrA->TargetOp(), nullConst, order, 
									     resultOp->GetType(), true));
			}
		}
	}

	// Test for the comparison of two pointers that are converted to another pointer.
	// If the original types are the same we can eliminate the cast. Actually we could
	// always eliminate the casts (because we compare the address of the pointers,
	// which doesn't depend on their type) but don't do it because it interferes
	// with the rules of the IR language, and these casts don't generate code anyway.
	if(auto ptopInstrA = opA->DefiningInstrAs<PtopInstr>()) {
		if(auto ptopInstrB = opB->DefiningInstrAs<PtopInstr>()) {
			auto targetA = ptopInstrA->TargetOp();
			auto targetB = ptopInstrB->TargetOp();

			if(targetA->GetType() == targetB->GetType()) {
				return LOG(CreateCompare(targetA, targetB, order,
									     resultOp->GetType(), true /* isUcmp */));
			}
		}
	}

	// Test for the comparison of two integers converted to pointers.
	if(auto itopInstrA = opA->DefiningInstrAs<ItopInstr>()) {
		if(auto itopInstrB = opB->DefiningInstrAs<ItopInstr>()) {
			// If we don't cast from the same type give up.
			auto targetOpA = itopInstrA->TargetOp();
			auto targetOpB = itopInstrB->TargetOp();

			if(targetOpA->GetType() != targetOpB->GetType()) {
				return nullptr;
			}

			// (itop a, X*) ORDER (itop b, X*) -> a ORDER b
			// Note that we need to emulate the extension/truncation
			// generated by the 'itop' in order to preserve the semantics.
			int pointerSize = TI::GetSize(itopInstrA->CastType(), GetTarget());
			int intSize = TI::GetSize(targetOpA->GetType(), GetTarget());

			if(pointerSize == intSize) {
				// Nothing needs to be changed.
				return LOG(CreateCompare(targetOpA, targetOpB, order, 
									     resultOp->GetType(), true));
			}
			else if(pointerSize > intSize) {
				// 'zext' the integers.
				auto zextType = IntegerType::GetHavingSize(pointerSize);
				auto zext1 = irGen_.GetTemporary(zextType);
				irGen_.GetZext(targetOpA, zextType, zext1);

				auto zext2 = irGen_.GetTemporary(zextType);
				irGen_.GetZext(targetOpB, zextType, zext2);
				return LOG(CreateCompare(zext1, zext2, order, 
                                         resultOp->GetType(), true));
			}
			else {
				// 'trunc' the integers.
				// This should actually not happen on most targets.
				auto truncType = IntegerType::GetHavingSize(pointerSize);
				auto trunc1 = irGen_.GetTemporary(truncType);
				irGen_.GetTrunc(targetOpA, truncType, trunc1);

				auto trunc2 = irGen_.GetTemporary(truncType);
				irGen_.GetTrunc(targetOpB, truncType, trunc2);
				return LOG(CreateCompare(trunc1, trunc2, order, 
                                         resultOp->GetType(), true));
			}
		}
		else if(opB->IsNullConstant()) {
			// (itop a, X*) ORDER nullptr -> a ORDER 0
			// This transformation is possible only if the pointer size
			// is larger or equal to the integer type (true for most targets).
			int pointerSize = TI::GetSize(itopInstrA->CastType(), GetTarget());
			auto targetType = itopInstrA->TargetOp()->GetType();
			int intSize = TI::GetSize(targetType, GetTarget());

			if(pointerSize >= intSize) {
				auto zeroConst = GetZeroInt(itopInstrA->TargetOp());
				return LOG(CreateCompare(itopInstrA->TargetOp(), zeroConst, order,
									     resultOp->GetType(), true));
			}
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpBin(Operand* opA, Operand* opB, Operand* resultOp, 
									   OrderType order, bool isSigned, Block* block) {
	// Test for comparisons that involve binary operations, like 'add'.
	// At lest one of the operands should be an arithmetic instruction;
	if((opA->DefiningInstrIs<ArithmeticInstr>() ||
		opB->DefiningInstrIs<ArithmeticInstr>()) == false) {
		return nullptr;
	}
	
	Operand* a;
	Operand* b;
	Operand* c;
	Operand* d;

	// While doing the simplifications we try not to increase register pressure
	// by introducing a new computation, while existing one is needed by
	// instructions in other places.
	if(Match<AddInstr>(MatchAny(&a), MatchAny(&b))(opA)) {
		if(((order == Order_Equal) || 
            (order == Order_NotEqual)) == false) {
			return nullptr;
		}

		// (a + b) == b -> a == 0
		// (a + b) != b -> a != 0
		if(b == opB) { 
            return LOG(CreateCompare(a, GetZeroInt(opA), order, 
                                     resultOp->GetType(), true));
        }

		// (a + b) == a -> b == 0
		// (a + b) != a -> b != 0
		if(a == opB) {
            return LOG(CreateCompare(b, GetZeroInt(opA), order, 
                                     resultOp->GetType(), true));
        }
	}

	if(Match<AddInstr>(MatchAny(&a), MatchAny(&b))(opB)) {
		if(((order == Order_Equal) || 
            (order == Order_NotEqual)) == false) {
			return nullptr;
		}

		// b == (a + b) -> a == 0
		// b != (a + b) -> a != 0
		if(b == opA) {
            return LOG(CreateCompare(a, GetZeroInt(opA), order, 
                                     resultOp->GetType(), true));
        }

		// a == (a + b) -> b == 0
		// a != (a + b) -> b != 0
		if(a == opA) {
            return LOG(CreateCompare(b, GetZeroInt(opA), order, 
                                     resultOp->GetType(), true));
        }
	}

	if(Match<SubInstr>(MatchAny(&a), MatchAny(&b))(opA)) {
		if(((order == Order_Equal) || 
            (order == Order_NotEqual)) == false) {
			return nullptr;
		}

		// (a - b) == a -> b == 0
		// (a - b) != a -> b != 0
		if(a == opB) {
            return LOG(CreateCompare(b, GetZeroInt(opA), order,
                                     resultOp->GetType(), true));
        }
	}

	// Test for cases when both operands are instructions.
	if(Match<AddInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
	   Match<AddInstr>(MatchAny(&c), MatchAny(&d))(opB)) {
		if((opA->HasSingleUser() && opB->HasSingleUser()) == false) {
			// Don't increase register pressure.
			return nullptr;
		}

		if(((order == Order_Equal) || 
            (order == Order_NotEqual)) == false) {
			return nullptr;
		}

		//  a   b      c   d
		// (a + b) == (a + d) -> b == d
		// (a + b) != (a + d) -> b != d
		if(a == c) {
			return LOG(CreateCompare(b, d, order, resultOp->GetType(), true));
		}
		else if(a == d) {
			return LOG(CreateCompare(b, c, order, resultOp->GetType(), true));
		}
	}

	if(Match<SubInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
	   Match<SubInstr>(MatchAny(&c), MatchAny(&d))(opB)) {
		if((opA->HasSingleUser() && opB->HasSingleUser()) == false) {
			// Don't increase register pressure.
			return nullptr;
		}

		if(((order == Order_Equal) || 
            (order == Order_NotEqual)) == false) {
			return nullptr;
		}

		//  a   b      c   d
		// (a - b) == (a - d) -> b == d
		// (a - b) != (a - d) -> b != d
		if(a == c) {
			return LOG(CreateCompare(b, d, order, resultOp->GetType(), true));
		}
		else if(a == d) {
			return LOG(CreateCompare(b, c, order, resultOp->GetType(), true));
		}
	}

	// Test for the comparison between a 'mod'/'umod' instruction 
	// that uses the same operand as the one to which we compare.
	// (a % b) ORDER b. We don't need to test for the 'b == (a % b)' situation
	// because it is guaranteed that the instruction will be always on the left.
	// For example, '(a % b) == b -> always false'.
	if(Match<UmodInstr>(MatchAny(&a), MatchAny(&b))(opA) && 
       (b == opB)) {
		switch(order) {
			case Order_Equal: {
				// (a % b) == b -> 0
				return LOG(GetBool(false, resultOp));
			}
			case Order_NotEqual: {
				// (a % b) != b -> 1
				return LOG(GetBool(true, resultOp));
			}
			case Order_Less:
			case Order_LessOrEqual: {
				// (a % b) < b -> 1
				// (a % b) <= b -> 1
				return LOG(GetBool(true, resultOp));
			}
			case Order_Greater:
			case Order_GreaterOrEqual: {
				// (a % b) > b -> 0
				// (a % b) >= b -> 0
				return LOG(GetBool(false, resultOp));
			}
		}
	}
	else if(Match<ModInstr>(MatchAny(&a), MatchAny(&b))(opA) && 
            (b == opB)) {
		switch(order) {
			case Order_Equal: {
				// (a % b) == b -> 0
				return LOG(GetBool(false, resultOp));
			}
			case Order_NotEqual: {
				// (a % b) != b -> 1
				return LOG(GetBool(true, resultOp));
			}
			case Order_Less:
			case Order_LessOrEqual: {
				// The result can be 'true' only if 'b' is positive, 
				// because the sign of the result depends only on 'a'.
				// (a % -5) < -5 -> false
				return LOG(CreateCompare(b, GetZeroInt(b), Order_GreaterOrEqual,
									     resultOp->GetType(), false));
			}
			case Order_Greater:
			case Order_GreaterOrEqual: {
				// This is the reverse of the above, meaning that the result
				// can be 'true' only if 'b' is negative.
				return LOG(CreateCompare(b, GetZeroInt(b), Order_Less,
									     resultOp->GetType(), false));
			}
		}
	}

	// Test for other instructions. For all cases, the two instructions must be
	// the same and must share at least one of the operands.
	if((opA->DefiningInstrIs<ArithmeticInstr>() &&
		opB->DefiningInstrIs<ArithmeticInstr>()) == false) {
			return nullptr;
	}

	if(opA->DefiningInstruction()->GetOpcode() != 
       opB->DefiningInstruction()->GetOpcode()) {
		return nullptr;
	}

	// Test comparisons involving other arithmetic operations than 'add' and 'sub'.
	if(auto mulInstrA = opA->DefiningInstrAs<MulInstr>()) {
		auto mulInstrB = opB->DefiningInstrAs<MulInstr>();

		return HandleIntegerCmpMul(mulInstrA, mulInstrB, resultOp, 
                                   order ,isSigned, block);
	}
	
	// Test comparisons that involve logical operations.
	auto logicalInstrA = opA->DefiningInstrAs<LogicalInstr>();
	auto logicalInstrB = opB->DefiningInstrAs<LogicalInstr>();

	if(logicalInstrA && logicalInstrB) {
		return HandleIntegerCmpLogical(logicalInstrA, logicalInstrB, resultOp,
									   order, isSigned, block);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpMul(MulInstr* mulInstrA, MulInstr* mulInstrB, 
									   Operand* resultOp, OrderType order, 
                                       bool isSigned, Block* block) {
	if(((order == Order_Equal) || (order == Order_NotEqual)) == false) {
		return nullptr;
	}

	// Simplify the comparison of two operands multiplied by the same constant.
	// (a * C) == (b * C) -> (a & M) == (b & M)
	// If 'C' is not 0 or 1 we can replace the multiplication with a mask.
	// Consider the following example:
	// (a * 16) == (b * 16). This "throws away" 4 bits from the left part,
    // and introduces 4 zero bits on the right part, effect that can be
	// reproduced by applying the mask '000011...11'.
	if(auto intConst = AsIntConstant(mulInstrA->RightOp(), block)) {
		auto intKind = intConst->GetType()->GetSubtype();

		if((IA::AreEqual(intConst->Value(), 0, intKind) == false) &&
		   (IA::AreEqual(intConst->Value(), 1, intKind) == false)) {
			// We can apply the transformation.
			int bits = intConst->GetType()->SizeInBits();
			int maskBits = bits - IA::RightmostZeroBits(intConst);
			__int64 mask = IA::ValueFromBitCount(maskBits);
			auto maskOp = irGen_.GetIntConst(intConst->GetType(), mask);

			auto andOp1 = GetTemporary(mulInstrA->ResultOp());
			irGen_.GetAnd(mulInstrA->LeftOp(), maskOp, andOp1);

			auto andOp2 = GetTemporary(mulInstrA->ResultOp());
			irGen_.GetAnd(mulInstrB->LeftOp(), maskOp, andOp2);
			return LOG(CreateCompare(andOp1, andOp2, order, 
                                     resultOp->GetType(), isSigned == false));
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpBool(Operand* opA, Operand* opB, Operand* resultOp, 
										OrderType order, bool isSigned, Block* block) {
	// First test for cases when a constant is involved.
	switch(order) {
		case Order_Equal: {
			// a == 1 -> a
			if(IsOneInt(opB, block)) {
				return opA;
			}

            // a == 0 -> a ^ 1
            if(IsZeroInt(opB, block)) {
                auto xorOp = GetTemporary(resultOp);
		        irGen_.GetXor(opA, GetOneInt(opB), xorOp);
                return LOG(xorOp);
            }
			break;
		}
		case Order_NotEqual: {
			// a != 0 -> a
			if(IsZeroInt(opB, block)) {
				return LOG(opA);
			}

            // a != 1 -> a ^ 1
            if(IsOneInt(opB, block)) {
                auto xorOp = GetTemporary(resultOp);
		        irGen_.GetXor(opA, GetOneInt(opB), xorOp);
                return LOG(xorOp);
            }
			break;
		}
		case Order_Less: {
			// a < 0 -> a, but only for signed numbers.
			if(IsZeroInt(opB, block) && isSigned) {
				return LOG(opA);
			}
			break;
		}
		case Order_LessOrEqual: {
			// a <= -1 -> a, but only for signed numbers.
			if(IsMinusOneInt(opB, block) && isSigned) {
				return LOG(opA);
			}
			break;
		}
		case Order_Greater: {
			// a > 0 -> a, but only for unsigned numbers.
			if(IsZeroInt(opB, block) && (isSigned == false)) {
				return LOG(opA);
			}
			break;
		}
		case Order_GreaterOrEqual: {
			// a >= 1 -> a, but only for unsigned numbers.
			if(IsOneInt(opB, block) && (isSigned == false)) {
				return LOG(opA);
			}
			break;
		}
	}

	// Comparisons involving 'bool' can always be converted to logical operations.
	// This creates new opportunities when more such conditions are merged.
	switch(order) {
	case Order_Equal: {
		// a == b -> ~(a ^ b)
		auto xorOp = GetTemporary(resultOp);
		irGen_.GetXor(opA, opB, xorOp);
		return LOG(GetNegated(xorOp));
	}
	case Order_NotEqual: {
		// a == b -> a ^ b
		auto xorOp = GetTemporary(resultOp);
		irGen_.GetXor(opA, opB, xorOp);
		return LOG(xorOp);
	}
	case Order_Less: {
		if(isSigned) {
			// a < b -> a & ~b (meaning that a = 1, b = 0)
			auto andOp = GetTemporary(resultOp);
			irGen_.GetAnd(opA, GetNegated(opB), andOp);
			return LOG(andOp);
		}
		else {
			// a < b -> ~a & b (meaning that a = 0, b = 1)
			auto andOp = GetTemporary(resultOp);
			irGen_.GetAnd(GetNegated(opA), opB, andOp);
			return LOG(andOp);
		}
	}
	case Order_LessOrEqual: {
		if(isSigned) {
			// a <= b -> a | ~b (meaning that a = 1/b = 0, 
            // a = 1/b = 0 or a = 1/b = 1)
			auto orOp = GetTemporary(resultOp);
			irGen_.GetOr(opA, GetNegated(opB), orOp);
			return LOG(orOp);
		}
		else {
			// a <= b -> ~a | b (meaning that a = 0/b = 0,
            // a = 0, b = 1 or a = 1/b = 1)
			auto orOp = GetTemporary(resultOp);
			irGen_.GetOr(opA, GetNegated(opB), orOp);
			return LOG(orOp);
		}
	}
	case Order_Greater: {
		if(isSigned) {
			// a > b -> ~a & b (meaning that a = 0, b = 1)
			auto andOp = GetTemporary(resultOp);
			irGen_.GetAnd(GetNegated(opA), opB, andOp);
			return LOG(andOp);
		}
		else {
			// a > b -> a & ~b (meaning that a = 1, b = 0)
			auto andOp = GetTemporary(resultOp);
			irGen_.GetAnd(opA, GetNegated(opB), andOp);
			return LOG(andOp);
		}
	}
	case Order_GreaterOrEqual: {
		if(isSigned) {
			// a >= b -> ~a | b (meaning that a = 0/b = 0 or
            // a = 0/b = 1 or a = 1/b = 1)
			auto orOp = GetTemporary(resultOp);
			irGen_.GetOr(GetNegated(opA), opB, orOp);
			return LOG(orOp);
		}
		else {
			// a >= b -> a | ~b (meaning that a = 0/b = 0 or 
            // a = 1/b = 0 or a = 1/b = 1)
			auto orOp = GetTemporary(resultOp);
			irGen_.GetOr(opA, GetNegated(opB), orOp);
			return LOG(orOp);
		}
	}
	}

	return nullptr;									
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpEquality(Instruction* instr, IntConstant* intConst, 
											Operand* resultOp, OrderType order, 
											bool isSigned, Block* block) {
	DebugValidator::IsTrue((order == Order_Equal) ||
                           (order == Order_NotEqual));
	Operand* a;
	Operand* b;

	// Most tests are small, so we treat them here.
	switch(instr->GetOpcode()) {
		case Instr_Add: {
			auto addInstr = instr->As<AddInstr>();
			
			// (a + C1) == C2 -> a == C2 - C1
			if(auto addConst = AsIntConstant(addInstr->RightOp(), block)) {
				// We can fold the constants.
				auto constantOp = folder_.FoldBinary(Instr_Sub, intConst, 
                                                     addConst, block);
				return LOG(CreateCompare(addInstr->LeftOp(), constantOp, order,
									     resultOp->GetType(), isSigned == false));
			}

            // Try to simplify even if the 'add' doesn't involve a constant.
			if(intConst->IsZero()) {
				// (a + -b) == 0 -> a == b
				if(Match<SubInstr>(MatchInt(0, block), 
                                   MatchAny(&b))(addInstr->RightOp())) {
					return LOG(CreateCompare(addInstr->LeftOp(), b, order,
										     resultOp->GetType(), isSigned == false));
				}
				// (-a + b) == 0 -> b == a
				else if(Match<SubInstr>(MatchInt(0, block), 
                                        MatchAny(&a))(addInstr->LeftOp())) {
					return LOG(CreateCompare(a, addInstr->LeftOp(), order,
										     resultOp->GetType(), isSigned == false));
				}
			}
			break;
		}
        case Instr_Sub: {
            auto subInstr = instr->As<SubInstr>();

            // (a - C1) == C2 -> a == C2 + C1
            if(auto subConst = AsIntConstant(subInstr->RightOp(), block)) {
				// We can fold the constants.
				auto constantOp = folder_.FoldBinary(Instr_Add, intConst, 
                                                     subConst, block);
				return LOG(CreateCompare(subInstr->LeftOp(), constantOp, order,
									     resultOp->GetType(), isSigned == false));
			}
            break;
        }
		case Instr_And: {
			// Many cases to test, do it in a separate method.
			return HandleIntegerCmpEqualityAnd(instr->As<AndInstr>(), intConst,
											   resultOp, order, isSigned, block);
		}
		case Instr_Or: {
			auto orInstr = instr->As<OrInstr>();

			if(auto orConst = AsIntConstant(orInstr->RightOp(), block)) {
				// a | C1 == C2 -> 0, if 'C2' doesn't have all the bits of 'C1'.
				// All bits from 'C1' are in 'C2' if after we perform an
				// 'and' between the two we obtain 'C1'.
				auto intKind = intConst->GetType()->GetSubtype();
				__int64 andResult = IA::And(orConst, intConst);

				if(IA::AreEqual(andResult, orConst->Value(), intKind) == false) {
					if(order == Order_Equal) {
						return LOG(GetBool(false, resultOp));
					}
					else return LOG(GetBool(true, resultOp));
				}
			}
			break;			
		}
		case Instr_Xor: {
			// Many cases to test, do it in a separate method.
			return HandleIntegerCmpEqualityXor(instr->As<XorInstr>(), intConst,
											   resultOp, order, isSigned, block);
		}
        case Instr_Mul: {
            return HandleIntegerCmpEqualityMul(instr->As<MulInstr>(), intConst,
											   resultOp, order, isSigned, block);
        }
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpEqualityMul(MulInstr* mulInstr, IntConstant* intConst, 
											   Operand* resultOp, OrderType order, 
											   bool isSigned, Block* block) {
    // mul a, C1 == C2 -> false, if 'C1' is even and 'C2' is odd.
    // mul a, C1 != C2 -> true, if 'C1' is even and 'C2' is odd.
    // Example: for any value of 'a', 'a * 2 == 3' is always false.
    if(auto C = AsIntConstant(mulInstr->RightOp(), mulInstr->ParentBlock())) {
        if(((C->Value() & 1) == 0) && (intConst->Value() & 1 != 0)) {
            if(order == Order_Equal) {
                return LOG(GetBool(false, resultOp));
            }
            else if(order == Order_NotEqual) {
                return LOG(GetBool(true, resultOp));
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* Peephole::HandleIntegerCmpXorEquality(Operand* opA, Operand* opB, 
											   Operand* resultOp, OrderType order,
											   bool isSigned, Block* block) {
	DebugValidator::IsTrue((order == Order_Equal) || 
                           (order == Order_NotEqual));
	Operand* a;
	Operand* b;
	Operand* c;
	Operand* d;
	IntConstant* C1;
	IntConstant* C2;

	// (a ^ b) == a -> b == 0
	// (a ^ b) == b -> a == 0
	if(Match<XorInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
	   ((a == opB) || (b == opB))) {
		auto zeroConst = GetZeroInt(opA);

		if(a == opB) {
			return LOG(CreateCompare(b, zeroConst, order, 
                                     resultOp->GetType(), false));
		}
		else return LOG(CreateCompare(a, zeroConst, order, 
                                      resultOp->GetType(), false));
	}

	// a == (a ^ b) -> b == 0
	// b == (a ^ b) -> a == 0
	if(Match<XorInstr>(MatchAny(&a), MatchAny(&b))(opB) &&
	   ((a == opA) || (b == opA))) {
		auto zeroConst = GetZeroInt(opA);

		if(a == opA) {
			return LOG(CreateCompare(b, zeroConst, order, 
                                     resultOp->GetType(), false));
		}
		else return LOG(CreateCompare(a, zeroConst, order, 
                                      resultOp->GetType(), false));
	}

	// (a & b) == (c & d) -> ((a ^ c) & b) == 0, if 'b == d'.
	if(Match<AndInstr>(MatchAny(&a), MatchAny(&b))(opA) && 
	   Match<AndInstr>(MatchAny(&c), MatchAny(&d))(opB)) {
		// Because there are 4 possibilities, and we must generate
        // a bit of code, we use 3 operands to represent the relation
		// (x ^ y) & z == 0
		Operand* x;
		Operand* y;
		Operand* z;
		bool matched = false;

		if(b == d) {
			x = a, y = c, z = b;
			matched = true;
		}
		else if(b == c) {
			x = a, y = d, z = b;
			matched = true;
		}
		else if(a == c) {
			x = b, y = d, z = a;
			matched = true;
		}
		else if(a == d) {
			x = b, y = c, z = a;
			matched = true;
		}

		if(matched) {
			// We can do the simplification.
			auto xorOp = GetTemporary(a);
			irGen_.GetXor(x, y, xorOp);

			auto andOp = GetTemporary(a);
			irGen_.GetAnd(xorOp, z, andOp);

			return LOG(CreateCompare(andOp, GetZeroInt(a), order,
								     resultOp->GetType(), isSigned == false));
		}
	}

	// (a ^ C1) == (b ^ C2) -> a == (b ^ (C1 ^ C2))
	if(Match<XorInstr>(MatchAny(&a), MatchIC(&C1, block))(opA) &&
	   Match<XorInstr>(MatchAny(&b), MatchIC(&C2, block))(opB)) {
		__int64 result = IA::Xor(C1, C2);
		auto constantOp = irGen_.GetIntConst(C1->GetType(), result);
		
        auto xorOp = GetTemporary(a);
		irGen_.GetXor(b, constantOp, xorOp);
		
        return LOG(CreateCompare(a, xorOp, order, 
                                 resultOp->GetType(), isSigned == false));
	}

	// a ^ b == a ^ d -> b == d
	if(Match<XorInstr>(MatchAny(&a), MatchAny(&c))(opA) &&
	   Match<XorInstr>(MatchAny(&b), MatchAny(&d))(opB)) {
        auto resultType = resultOp->GetType();

		if(a == c) {
			return LOG(CreateCompare(b, d, order, 
                                     resultType, isSigned == false));
		}
		else if(a == d) {
			return LOG(CreateCompare(b, c, order, 
                                     resultType, isSigned == false));
		}
		else if(b == c) {
			return LOG(CreateCompare(a, d, order, 
                                     resultType, isSigned == false));
		}
		else if(b == d) {
			return LOG(CreateCompare(a, c, order, 
                                     resultType, isSigned == false));
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpSelf(Operand* opA, Operand* opB, Operand* resultOp, 
										OrderType order, bool isSigned) {
	// Test for things like 'a + C > a' and 'a > a + C'
	Operand* a;
	IntConstant* C;

	if((Match<AddInstr>(MatchAny(&a), MatchIC(&C))(opA) && (a == opB)) == false) {
		if((Match<AddInstr>(MatchAny(&a), MatchIC(&C))(opB) && (a == opA)) == false) {
			// None of the two variants matched.
			return nullptr;
		}
		else {
			// The second variant matched, invert the comparison order
			// (this simplifies all the tests below).
			if((order != Order_Equal) && (order != Order_NotEqual)) {
				order = CmpInstrBase::InvertedOrder(order);
			}
		}
	}

	// If the constant is 0 don't do any simplification here,
	// because it will be done in another place at a later time.
	if(C->IsZero()) {
        return nullptr;
    }
	
	// If we're testing for 'equal' or 'not equal' we know that left and right
	// can't never be equal, so we return the result directly.
	if(order == Order_Equal) {
		// a + C == a -> false
		return LOG(GetBool(false, resultOp));
	}
	else if(order == Order_NotEqual) {
		// a + C != a -> true
		return LOG(GetBool(true, resultOp));
	}

	// First test for unsigned numbers, there are fewer cases.
	// Note that we treat the 'less or equal'/'greater or equal' cases together
	// with 'less'/'greater' because the 'equal' part can't actually happen,
	// because this relation would appear only if 'C' is 0, but we treat this case above.
	auto intKind = C->GetType()->GetSubtype();

	if(isSigned == false) {
		__int64 maxConst = IA::GetMax(C->GetType(), false /* isSigned */);
		auto result = IA::Sub(maxConst, C->Value(), intKind);
		auto constantOp = irGen_.GetIntConst(C->GetType(), result);

		// Testing with other orders means that we need to test for the overflow condition.
		// For example, 'a + 5 < a' means that 'a + 5' must overflow for the comparison
		// to be 'true', meaning that 'a' must be '> 255 - 5' (for 'int8' integers).
		if((order == Order_Less) || 
           (order == Order_LessOrEqual)) {
			return LOG(CreateCompare(a, constantOp, Order_Greater,
                                     resultOp->GetType(), true));
		}
		else {
			// 'greater' and 'greater or equal'.
			// For example, 'a + 2 > a' as long as 'a <= 255 - 2'.
			return LOG(CreateCompare(a, constantOp, Order_LessOrEqual, 
                                     resultOp->GetType(), true));
		}
	}

	// 'a + 5 < a' only if 'a + 5' overflows, meaning that 'a > 127 - 5'.
	// 'a - 5 < a' only if 'a - 5' overflows, meaning that 'a < -128 + 5',
	// which is equivalent to 'a > 127 - 5 + 1'.
	// We can use the same order to test for both cases.
	__int64 maxConst = IA::GetMax(C->GetType(), true /* isSigned */);
	auto result = IA::Sub(maxConst, C->Value(), intKind);
	auto constantOp = irGen_.GetIntConst(C->GetType(), result);

	if((order == Order_Less) || 
       (order == Order_LessOrEqual)) {
		return LOG(CreateCompare(a, constantOp, Order_Greater,
                                 resultOp->GetType(), true));
	}
	else {
		// 'greater' and 'greater or equal'.
		// 'a + 5 > a' as long as 'a + 5' doesn't overflow, meaning that
		// 'a + 5 <= 127 -> a <= 127 - 5'.
		// 'a - 5 > a' as long as 'a - 5' doesn't overflow, meaning that
		// 'a - 5 >= -128 -> a >= -128 + 5', which is equivalent to 'a <= 127 - 5 + 1'.
		// We can use the same order to test for both cases.
		return LOG(CreateCompare(a, constantOp, Order_LessOrEqual, 
                                 resultOp->GetType(), true));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpDiv(ArithmeticInstr* divInstr, 
                                       IntConstant* intConst, 
									   Operand* resultOp, OrderType order, 
                                       bool isSigned, Block* block) {
	// In many cases an operand divided by a constant and compared by another
	// constant can be replaced by a range test. Consider the following examples:
	// (a / 4)  == 0 -> (a >= 0) && (a < 4)
	// (a / 8)  == 3 -> (a >= 24) && (a < 32)
	// (a / -2) == 0 -> (a >= -1) && (a < 2)
	// We need to determine a minimum (LOW) and maximum (HIGH) value for 'a' 
	// and test if the value is in the range [LOW, HIGH). For the generic case
	// '(a / C1) == C2', 'LOW = C2 * C1' and 'HIGH = C2 * (C1 + 1)'
	// Note that we also need to take care of overflow.
	IntConstant* C1 = AsIntConstant(divInstr->RightOp(), block);
	IntConstant* C2 = intConst; // The right operand of ==
	if((C1 == nullptr) || C1->IsZero()) {
        return nullptr;
    }

	// Try to determine the range, based on the type of the divide instruction
	// and on the sign of both constants.
	__int64 low;
	__int64 high;
	bool lowOverflow = false;
	bool highOverflow = false;
	bool lowOverflowLess;  // Means the low value is lower that the range of 'a'.
	bool highOverflowLess; // Means the high value is lower that the range of 'a'.

	__int64 product = ComputeIntegerCmpDivBounds(divInstr, C1, C2, order,
												 low, high, 
                                                 lowOverflow, highOverflow,
												 lowOverflowLess, highOverflowLess);

	// Based on the comparison order create the range tests, 
	// and in some cases return the result directly.
	switch(order) {
		case Order_Equal: {
			// If both bounds overflow then the result will be always 'false'.
			if(lowOverflow && highOverflow) {
				return LOG(GetBool(false, resultOp));
			}
			else if(lowOverflow) {
				// If LOW overflows we don't need to test for the fist condition.
				auto highConst = irGen_.GetIntConst(intConst->GetType(), high);

				return LOG(CreateCompare(divInstr->LeftOp(), highConst, 
                                         Order_Less, resultOp->GetType(), 
                                         isSigned == false));
			}
			else if(highOverflow) {
				// If HIGH overflows we don't need to test for the second condition.
				auto lowConst = irGen_.GetIntConst(intConst->GetType(), low);

				return LOG(CreateCompare(divInstr->LeftOp(), lowConst, 
                                         Order_GreaterOrEqual, resultOp->GetType(),
                                         isSigned == false));
			}
			else {
				// Both conditions need to be tested.
				auto highConst = irGen_.GetIntConst(intConst->GetType(), high);
				auto lowConst = irGen_.GetIntConst(intConst->GetType(), low);

				return LOG(CreateRangeCompare(divInstr->LeftOp(), lowConst, highConst,
										      resultOp->GetType(), isSigned == false));
			}
		}
		case Order_NotEqual: {
			// If both bounds overflow then the result will be always 'true'.
			if(lowOverflow && highOverflow) {
				return LOG(GetBool(true, resultOp));
			}
			else if(lowOverflow) {
				// If LOW overflows we don't need to test for the fist condition.
				auto highConst = irGen_.GetIntConst(intConst->GetType(), high);

				return LOG(CreateCompare(divInstr->LeftOp(), highConst, 
                                         Order_GreaterOrEqual, resultOp->GetType(),
                                         isSigned == false));
			}
			else if(highOverflow) {
				// If HIGH overflows we don't need to test for the second condition.
				auto lowConst = irGen_.GetIntConst(intConst->GetType(), low);

				return LOG(CreateCompare(divInstr->LeftOp(), lowConst, 
                                         Order_Less, resultOp->GetType(), 
                                         isSigned == false));
			}
			else {
				// Both conditions need to be tested and combined using an OR.
				auto highConst = irGen_.GetIntConst(intConst->GetType(), high);
				auto lowConst = irGen_.GetIntConst(intConst->GetType(), low);
				
				// For 'true' we have '(a >= LOW) & (a < HIGH)', and applying De Morgan's
				// law to negate the result we obtain '(a < LOW) | (a >= HIGH)'.
				auto leftResult = CreateCompare(divInstr->LeftOp(), lowConst, 
                                                Order_Less, resultOp->GetType(),
                                                isSigned == false);

				auto rightResult = CreateCompare(divInstr->LeftOp(), highConst, 
												 Order_GreaterOrEqual, 
												 resultOp->GetType(), 
                                                 isSigned == false);

				auto orOp = GetTemporary(resultOp);
				irGen_.GetOr(leftResult, rightResult, orOp);
				return LOG(orOp);
			}
			break;
		}
		case Order_Less:
		case Order_LessOrEqual: {
			if(lowOverflow) {
				if(lowOverflowLess) {
					// There can't be a divided number to be smaller than the right part.
					return LOG(GetBool(false, resultOp));
				}
				else {
					// All numbers lead to a result that is smaller than the right part.
					return LOG(GetBool(true, resultOp));
				}
			}
			else {
				// For 'less or equal' range between LOW and HIGH too is included too.
				auto lowConst = order == Order_LessOrEqual ?
								irGen_.GetIntConst(intConst->GetType(), high) :
								irGen_.GetIntConst(intConst->GetType(), low);

				return LOG(CreateCompare(divInstr->LeftOp(), lowConst, 
                                         Order_Less, resultOp->GetType(),
                                         isSigned == false));
			}
			break;
		}
		case Order_Greater:
		case Order_GreaterOrEqual: {
			if(highOverflow) {
				if(highOverflowLess) {
					// All numbers lead to a result that is larger than the right part.
					return LOG(GetBool(true, resultOp));
				}
				else {
					// There can't be a divided number to be larger than the right part.
					return LOG(GetBool(false, resultOp));
				}
			}
			else {
				// For 'greater or equal' the range between LOW and HIGH is included too.
				auto lowConst = order == Order_GreaterOrEqual ?
								irGen_.GetIntConst(intConst->GetType(), low) :
								irGen_.GetIntConst(intConst->GetType(), high);

				return LOG(CreateCompare(divInstr->LeftOp(), lowConst, 
                                         Order_GreaterOrEqual, resultOp->GetType(),
                                         isSigned == false));
			}
			break;
		}
		default: DebugValidator::Unreachable();
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
__int64 Peephole::ComputeIntegerCmpDivBounds(ArithmeticInstr* divInstr, 
                                             IntConstant* C1, IntConstant* C2, 
                                             OrderType& order,
										     __int64& low, __int64& high, 
											 bool& lowOverflow, bool& highOverflow, 
											 bool& lowOverflowLess,
											 bool& highOverflowLess) {
	// We detect overflow by verifying that the product divided by one of the constants
	// yields the other constant. If it overflows, then we know that both the LOW
	// and HIGH part overflow. If not, overflow can still happen, but only
	// for LOW or for HIGH. For example, 'a / 14 == 9' (for signed int8) has 'LOW = 126',
	// which doesn't overflow, but 'HIGH = 126 + 13 = -117' does overflow.
	// In this case it's enough to test that 'a >= LOW'.
	auto intKind = C1->GetType()->GetSubtype();
	__int64 product = IA::Mul(C2, C1);

	if(IA::MulOverflows(C2, C1)) {
		lowOverflow = true;
		highOverflow = true; // If 'LOW' overflows, 'HIGH' overflows too.
		lowOverflowLess = false;
		highOverflowLess = false;
	}

	// Try to determine the range, based on the type of the divide instruction
	// and on the sign of both constants.
	if(divInstr->IsUdiv()) {
		// 'udiv' is much easier to deal with.
		low = product;

		// Check for overflow for the high part.
		if(highOverflow == false) {
			// HIGH = LOW + C1 - 1
			highOverflow = IA::AddOverflows(low, C1->Value(), intKind);
			high = IA::Add(low, C1->Value(), intKind);
		}

		return LOG(product);
	}

	if(IA::IsPositive(C1)) {
		if(IA::IsPositive(C2)) { // a / + == +
			if(C2->IsZero()) {   // a / + == 0
				// When we multiply with 0 there can't be any overflow.
				// a / 4 == 0 -> (a >= -3) && (a < 4)
				low = IA::Sub(0, C1->Value(), intKind);
				low = IA::Add(low, 1, intKind);
				high = C1->Value();
			}
			else {
				// a / 4 == 2 -> (a >= 8) && (a < 12)
				low = product;

				if(highOverflow == false) {
					// HIGH could still overflow.
					highOverflow = IA::AddOverflows(product, C1->Value(), intKind);
					high = IA::Add(product, C1->Value(), intKind);
				}
			}
		}
		else { // a / + == -
			// a / 4 == -2 -> (a >= -11) && (a < -7)
			if(lowOverflow == false) {
				// LOW could overflow.
				lowOverflow = IA::SubOverflows(product, C1->Value(), intKind);
				lowOverflowLess = true;
			}

			low = IA::Sub(product, C1->Value(), intKind);
			low = IA::Add(low, 1, intKind); // LOW = product - C1 + 1

			if(highOverflow) highOverflowLess = true;
			high = IA::Add(product, 1, intKind);
		}
	}
	else {
		if(IA::IsPositive(C2)) { // a / - == +
			if(C2->IsZero()) {   // a / - == 0
				// When we multiply with 0 there can't be any overflow.
				// a / -4 == 0 -> (a >= -3) && (a < 4)
				low = C1->Value();
				low = IA::Add(low, 1, intKind);
				high = IA::Sub(0, C1->Value(), intKind);
			}
			else {
				// a / -4 == 2 -> (a >= -11) && (a < -7)
				if(lowOverflow == false) {
					// LOW could overflow.
					lowOverflow = IA::SubOverflows(product, C1->Value(), intKind);
					lowOverflowLess = true;
				}

				low = IA::Sub(product, C1->Value(), intKind);
				low = IA::Add(low, 1, intKind); // LOW = product + C1 + 1

				if(highOverflow) highOverflowLess = true;
				high = IA::Add(product, 1, intKind);
			}
		}
		else { // a / - == -
			// a / -4 == -2 -> (a >= 8) && (a < 12)
			low = product;

			if(highOverflow == false) {
				// HIGH could still overflow.
				// Because 'C1' is negative we use 'sub' to compute the high bound.
				highOverflow = IA::SubOverflows(product, C1->Value(), intKind);
				high = IA::Sub(product, C1->Value(), intKind);
			}
		}
	}

	// If we compare with an order that is not 'equal' or 'not equal'
	// then we need to invert it to get the right result.
	// This is a consequence of the negative divisor.
	switch(order) {
		case Order_Less:           { order = Order_Greater;        break; }
		case Order_LessOrEqual:    { order = Order_GreaterOrEqual; break; }
		case Order_Greater:        { order = Order_Less;           break; }
		case Order_GreaterOrEqual: { order = Order_LessOrEqual;    break; }
	}

	return LOG(product);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIntegerCmpNegated(Operand* opA, Operand* opB, 
                                           Operand* resultOp, OrderType order, 
                                           bool isSigned, Block* block) {
    // A comparison that compares two negated operands
    // can be replaced by one that compares the operands directly.
    // -a < -b -> a > b
    Operand* a;
    Operand* b;

    if(Match<SubInstr>(MatchInt(0, block), MatchAny(&a))(opA) &&
       Match<SubInstr>(MatchInt(0, block), MatchAny(&b))(opB)) {
        // We use the inverted order.
        // -a <= -b -> a >= b
        // -a > -b -> a < b
        // -a == -b -> a == b
        OrderType inverted = CmpInstrBase::InvertedOrder(order, false);
        return LOG(CreateCompare(a, b, inverted, resultOp->GetType(),
                                 isSigned == false));
    }

    return nullptr;
}

} // namespace Optimization