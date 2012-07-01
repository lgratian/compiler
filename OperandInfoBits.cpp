// OperandInfo.cpp
// Copyright (c) Lup Gratian
//
// Implements the OperandInfo class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "OperandInfo.hpp"

namespace Analysis {

void OperandInfo::EstimateZeroBits(Operand* op, Mask& bits, int depth) {
	DebugValidator::IsNotNull(op);
    DebugValidator::IsFalse(op->IsFloating());
    
    if(op->IsUndefinedConstant()) {
        bits = 0;
        return;
    }
    
    // Try to take the mask from the cache first.
    if(TryTakeZeroBitsFromCache(op, bits)) {
        return;
    }

    // Compute the bits that are definitely zero.
	bits = 0;
	EstimateZeroBitsImpl(op, bits, (depth != -1) ? depth : MAXIMUM_BITS_DEPTH);
    
    // Cache the result into a Known Bits Tag.
    if(auto temp = op->As<Temporary>()) {
        auto bitsTag = temp->GetTag<KnownBitsTag>();

        if(bitsTag == nullptr) {
            bitsTag = KnownBitsTag::GetKnownBits();
            temp->AddTag(bitsTag);
        }

        bitsTag->SetZeroBits(bits);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::TryTakeZeroBitsFromCache(Operand* op, Mask& bits) {
    if(auto temp = op->As<Temporary>()) {
        if(auto bitsTag = temp->GetTag<KnownBitsTag>()) {
            if (bitsTag->HasZeroBitInfo()) {
                bits = bitsTag->ZeroBits();
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsImpl(Operand* op, Mask& bits, int depth) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsLargerOrEqual(depth, 0);

	// Don't recourse too many times, in most cases it won't improve the results.
	if(depth == 0) return;

    auto intType = op->GetType()->As<IntegerType>();
    bool isPositive = false;
	bits = 0; // We start knowing nothing.

	// First test for constants and references.
    IntConstant* intConst = op->As<IntConstant>();

    if(intConst == nullptr) {
        isPositive = IsOperandPositive(op, intConst);
    }

	if(intConst) {
		// We know all the bits that are not set for constants.
        __int64 mask = IA::GetMinusOneMask(intType);
        bits = ~intConst->Value() & mask;
        return;
	}
	else if(op->IsNullConstant() && target_) {
		// All bits are zero.
		auto ptrIntType = IntegerType::GetHavingSize(target_->GetPointerSize());
		bits = IA::GetMinusOneMask(ptrIntType);
        return;
	}
	else if(auto variableRef = op->As<VariableReference>()) {
		// For global variables we can consider the alignment.
        if(target_) {
            int alignment = GetVariableAlignment(variableRef, target_);
		    bits = IA::ValueFromBitCount(alignment);
        }
        return;
	}
    else if(isPositive) {
        bits = IA::GetSignBitMask(intType);
    }

	// Now check for instructions for which we can determine some of the zero bits.
	auto definingInstr = op->DefiningInstruction();
	
    if(definingInstr == nullptr) {
        return;
    }

	// What we know about the left and right operands of binary instructions.
	Mask bitsLeft = 0;
	Mask bitsRight = 0;

	switch(definingInstr->GetOpcode()) {
		case Instr_And: {
			// A bit is zero if it's zero in the left operand 
			// or if it's zero in the right operand.
			EstimateZeroBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
			EstimateZeroBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);
			bits = bitsLeft | bitsRight;
			break;
		}
		case Instr_Or: {
			// A bit is zero if it's zero in both left and right operands.
			EstimateZeroBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
			EstimateZeroBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);
			bits = bitsLeft & bitsRight;
			break;
		}
		case Instr_Xor: {
			// A bit is zero if it has the same value in both operands.
			EstimateZeroBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
			EstimateZeroBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);

            Mask oneBitsLeft;
            Mask oneBitsRight;
            EstimateOneBits(definingInstr->GetSourceOp(0), oneBitsLeft, depth - 1);
			EstimateOneBits(definingInstr->GetSourceOp(1), oneBitsRight, depth - 1);

			bits = (bitsLeft & bitsRight) |
                   (oneBitsLeft & oneBitsRight);
			break;
		}
		case Instr_Shl:  { EstimateZeroBitsShl(definingInstr, bits, depth);  break; }
		case Instr_Ushr: { EstimateZeroBitsUshr(definingInstr, bits, depth); break; }
		case Instr_Shr:  { EstimateZeroBitsShr(definingInstr, bits, depth);  break; }
		case Instr_Add:
        case Instr_Sub:  { EstimateZeroBitsAddSub(definingInstr, bits, depth); break; }
		case Instr_Mul:  { EstimateZeroBitsAddSub(definingInstr, bits, depth); break; }
		case Instr_Mod:  { EstimateZeroBitsMod(definingInstr, bits, depth);    break; }
		case Instr_Umod: { EstimateZeroBitsUmod(definingInstr, bits, depth);   break; }
		case Instr_Udiv: { EstimateZeroBitsUdiv(definingInstr, bits, depth);   break; }
		case Instr_Trunc:
		case Instr_Zext: { EstimateZeroBitsZextTrunc(definingInstr, bits, depth); break; }
		case Instr_Sext: { EstimateZeroBitsSext(definingInstr, bits, depth);      break; }
		case Instr_Ptoi: { EstimateZeroBitsPtoi(definingInstr, bits, depth);      break; }
		case Instr_Itop: { EstimateZeroBitsItop(definingInstr, bits, depth);      break; }
        case Instr_Phi:  { EstimateZeroBitsPhi(definingInstr, bits, depth);       break; }
        case Instr_Call: { EstimateZeroBitsCall(definingInstr, bits, depth);      break; }
        case Instr_Question: { EstimateZeroBitsQuest(definingInstr, bits, depth); break; }
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::IsOperandPositive(Operand* op, IntConstant*& intConst) {
    if(op->IsInteger() == false) {
        return false;
    }
    
    // Try to use value-range information.
    if(auto rangeTag = GetRangeTag(op)) {
        intConst = rangeTag->GetAsConstant(op);

        if(intConst == nullptr) {
            // We don't have a constant, but try at least
            // to determine if the number is positive.
            if(IsPositive(op, nullptr, 0)) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsShl(Instruction* definingInstr, 
                                      Mask& bits, int depth) {
    Mask bitsLeft = 0;

    // The first C bits are definitely zero.
    if(auto intConst = definingInstr->GetSourceOp(1)->As<IntConstant>()) {
        EstimateZeroBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
        bitsLeft <<= intConst->Value();
        bits = bitsLeft | IA::ValueFromBitCount(intConst->Value());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsShr(Instruction* definingInstr,
                                      Mask& bits, int depth) {
    Mask bitsLeft = 0;

    // The last C bits are definitely zero if we now that the sign bit is zero.
    if(auto intConst = definingInstr->GetSourceOp(1)->As<IntConstant>()) {
        EstimateZeroBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
        auto intKind = intConst->GetType()->GetSubtype();

        if(IA::GetSignBit(bitsLeft, intKind) == 1) {
            bitsLeft >>= intConst->Value();
            Mask zeroBits = IA::ValueFromBitCount(intConst->Value());
            int shift = intConst->GetType()->SizeInBits() - intConst->Value();
            bits = bitsLeft | (zeroBits << shift);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsUshr(Instruction* definingInstr, 
                                       Mask& bits, int depth) {
    Mask bitsLeft = 0;

    // The last C bits are definitely zero.
    if(auto intConst = definingInstr->GetSourceOp(1)->As<IntConstant>()) {
        EstimateZeroBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
        bitsLeft >>= intConst->Value();
        Mask zeroBits = IA::ValueFromBitCount(intConst->Value());
        int shift = intConst->GetType()->SizeInBits() - intConst->Value();
        bits = bitsLeft | (zeroBits << shift);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsAddSub(Instruction* definingInstr, 
                                         Mask& bits, int depth) {
    Mask bitsLeft = 0;
    Mask bitsRight = 0;

    // If there are zero bits on the right side of both operands,
    // those bits will remain zero after addition/subtraction.
    auto arithInstr = definingInstr->As<ArithmeticInstr>();
    EstimateZeroBits(arithInstr->LeftOp(), bitsLeft, depth - 1);
    EstimateZeroBits(arithInstr->RightOp(), bitsRight, depth - 1);
    auto intType = arithInstr->LeftOp()->GetType()->As<IntegerType>();
    auto intKind = intType->GetSubtype();

    int zeroLeft = IA::RightmostOneBits(bitsLeft, intKind);
    int zeroRight = IA::RightmostOneBits(bitsRight, intKind);
    
    if(zeroLeft || zeroRight) {
        if(arithInstr->IsAdd()) {
            if(zeroLeft > zeroRight) {
                bits = bitsRight & IA::ValueFromBitCount(zeroLeft);
            }
            else bits = bitsLeft & IA::ValueFromBitCount(zeroRight);
        }
    }

    // If the instruction is marked as "no signed overflow",
    // and on both sides the sign bit is zero, then the sign bit
    // can't become one after addition (negative number).
    if(arithInstr->IsAdd() && arithInstr->HasUndefinedOverflow()) {
        if(IA::GetSignBit(bitsLeft, intKind) && 
           IA::GetSignBit(bitsRight, intKind)) {
            bits |= IA::GetSignBitMask(intKind);
        }
    }
    else if(arithInstr->IsSub() && arithInstr->HasUndefinedOverflow()) {
        // If we subtract from a constant, the top bits are zero
        // if the subtracted value is positive and smaller than the constant
        // (it means that the result is not negative).
        if(auto intConst = arithInstr->LeftOp()->As<IntConstant>()) {
            if(intConst->IsPositive()) {
                int leftmostOne = IA::LeftmostSetBit(intConst);
                int bits = intType->SizeInBits();
                Mask mask = IA::ValueFromBitCount(bits - leftmostOne) << leftmostOne;

                if((bitsRight & mask) == mask) {
                    bits |= mask;
                }
            }
        }
        else if(arithInstr->RightOp()->IsOneInt()) {
            // If we subtract 1, and the rightmost bit is definitely set,
            // then this bit will be 0 after subtraction.
            unsigned __int64 oneBits;
            EstimateOneBits(arithInstr->LeftOp(), oneBits);

            if(oneBits & 1) {
                bits = bitsLeft | 1;
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsMul(Instruction* definingInstr, 
                                      Mask& bits, int depth) {
    Mask bitsLeft = 0;
    Mask bitsRight = 0;

    // Bits that are zero in both right parts will be zero in the result.
    EstimateZeroBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
    EstimateZeroBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);
    auto intType = definingInstr->GetSourceOp(0)->GetType()->As<IntegerType>();
    auto intKind = intType->GetSubtype();

    // Estimate how many zero bits are on the right side.
    int rightZeroA = IA::RightmostOneBits(bitsLeft, intKind);
    int rightZeroB = IA::RightmostOneBits(bitsLeft, intKind);
    int zeroBitsRight = std::min(rightZeroA + rightZeroB, 
                                 intType->SizeInBits());

    // Estimate how many zero bits are on the left side.
    int leftZeroA = IA::LeftmostOneBits(bitsLeft, intKind);
    int leftZeroB = IA::LeftmostOneBits(bitsLeft, intKind);
    int zeroBitsLeft = std::min(rightZeroA + rightZeroB, 
        intType->SizeInBits());

    bits = IA::ValueFromBitCount(zeroBitsRight) |
           (IA::ValueFromBitCount(zeroBitsLeft) << 
           (intType->SizeInBits() - zeroBitsLeft));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsUdiv(Instruction* definingInstr, 
                                       Mask& bits, int depth) {
    Mask bitsLeft = 0;
    Mask bitsRight = 0;

    // Presume that we divide by the nearest power of two;
    // this gives a good estimate of the rightmost zero bits.
    EstimateZeroBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
    EstimateZeroBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);
    auto intType = definingInstr->GetSourceOp(0)->GetType()->As<IntegerType>();
    auto intKind = intType->GetSubtype();

    int leftZero = IA::RightmostOneBits(bitsRight, intKind);
    int shift = intType->SizeInBits() - leftZero;
    Mask zeroBits = IA::ValueFromBitCount(leftZero);
    bits = bitsLeft | (zeroBits << shift);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsMod(Instruction* definingInstr, 
                                      Mask& bits, int depth) {
    Mask bitsLeft = 0;
    Mask bitsRight = 0;

    EstimateZeroBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
    EstimateZeroBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);
    auto intType = definingInstr->GetSourceOp(0)->GetType()->As<IntegerType>();
    auto intKind = intType->GetSubtype();
    bool done = false;

    // Test for a power of two constant as the right operand.
    if(auto C = definingInstr->GetSourceOp(0)->As<IntConstant>()) {
        if(IA::IsPowerOfTwo(C)) {
            int oneBitPos = IA::Log2(C);
            Mask lowMask = IA::ValueFromBitCount(oneBitPos);

            // The low bits are not modified at all (the 'umod' acts
            // like an 'and' with a mask only with ones).
            bits = bitsLeft & lowMask;

            // The left bits are zero if the first operand is not negative,
            // or if all the right bits are zero (means that the result
            // is either positive, or zero).
            if(IA::GetSignBit(bitsLeft, intKind) || 
               ((bitsLeft & lowMask) == lowMask)) {
                int oneBits = intType->SizeInBits() - oneBitPos;
                bits = IA::ValueFromBitCount(oneBits) << oneBitPos;
            }
        }
    }

    // The sign bit is zero if the left operand is positive.
    if(IA::GetSignBit(bitsLeft, intKind)) {
        bits |= IA::GetSignBitMask(intKind);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsUmod(Instruction* definingInstr, 
                                       Mask& bits, int depth) {
    Mask bitsLeft = 0;
    Mask bitsRight = 0;

    EstimateZeroBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
    EstimateZeroBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);
    auto intType = definingInstr->GetSourceOp(0)->GetType()->As<IntegerType>();
    auto intKind = intType->GetSubtype();
    bool done = false;

    // If the right operand is a power of two constant we know
    // that the left bits are zero.
    if(auto C = definingInstr->GetSourceOp(0)->As<IntConstant>()) {
        if(IA::IsPowerOfTwo(C)) {
            int oneBitPos = IA::Log2(C);

            // The low bits are not modified at all.
            bits = bitsLeft & IA::ValueFromBitCount(oneBitPos);

            int maskBits = intType->SizeInBits() - oneBitPos;
            Mask mask = IA::ValueFromBitCount(maskBits);
            bits |= mask << oneBitPos;
            done = true;
        }
    }

    if(done == false) {
        // The number of zero bits in the left part is at least 
        // the maximum number of zero bits in both operands.
        int leftBitsA = IA::LeftmostOneBits(bitsLeft, intKind);
        int leftBitsB = IA::LeftmostOneBits(bitsRight, intKind);
        int leftBits = std::max(leftBitsA, leftBitsB);
        bits = IA::ValueFromBitCount(leftBits) << (intType->SizeInBits());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsZextTrunc(Instruction* definingInstr, 
                                            Mask& bits, int depth) {
    Mask bitsLeft = 0;

    // The left bits are definitely zero.
    auto convInstr = definingInstr->As<ConversionInstr>();
    EstimateZeroBits(convInstr->TargetOp(), bitsLeft, depth - 1);
    auto toType = convInstr->CastType()->As<IntegerType>();
    auto fromType = convInstr->TargetOp()->GetType()->As<IntegerType>();

    // We need to invert the types for 'trunc'.
    if(convInstr->IsTrunc()) {
        std::swap(toType, fromType);
    }

    int toBits = toType->SizeInBits();
    int fromBits = fromType->SizeInBits();
    Mask zeroBits = IA::ValueFromBitCount(toBits - fromBits);
    bits = bitsLeft | (zeroBits << fromBits);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsSext(Instruction* definingInstr, 
                                       Mask& bits, int depth) {
    Mask bitsLeft = 0;

    // The left bits are zero only if the sign bit is not set.
    auto convInstr = definingInstr->As<ConversionInstr>();
    EstimateZeroBits(convInstr->TargetOp(), bitsLeft, depth - 1);
    auto toType = convInstr->CastType()->As<IntegerType>();
    auto fromType = convInstr->TargetOp()->GetType()->As<IntegerType>();

    if(IA::GetSignBit(bitsLeft, toType->GetSubtype()) == 1) {
        int fromBits = fromType->SizeInBits();
        Mask zeroBits = IA::ValueFromBitCount(toType->SizeInBits() -
                                              fromType->SizeInBits());
        bits = bitsLeft | (zeroBits << fromBits);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsPtoi(Instruction* definingInstr, 
                                       Mask& bits, int depth) {
    if(target_ == nullptr) return;
    Mask bitsLeft = 0;

    // The left bits are zero if the integer has fewer bits
    // than the pointer on the target.
    auto convInstr = definingInstr->As<ConversionInstr>();
    EstimateZeroBits(convInstr->TargetOp(), bitsLeft, depth - 1);
    auto toType = convInstr->CastType()->As<IntegerType>();
    int toSize = toType->SizeInBits();
    int ptrSize = target_->GetPointerSizeInBits();

    if(ptrSize > toSize) {
        int zeroBits = IA::ValueFromBitCount(ptrSize - toSize);
        bits = bitsLeft | (zeroBits << toSize);
    }
    else {
        Mask zeroBits = IA::ValueFromBitCount(toSize - ptrSize);
        bits = bitsLeft | (zeroBits << ptrSize);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsItop(Instruction* definingInstr, 
                                       Mask& bits, int depth) {
    if(target_ == nullptr) return;
    Mask bitsLeft = 0;

    // If the pointer size is larger than the integer (the usual case),
    // then the left bits are definitely zero.
    auto convInstr = definingInstr->As<ConversionInstr>();
    auto fromType = convInstr->TargetOp()->GetType()->As<IntegerType>();
    int fromSize = fromType->SizeInBits();
    int ptrSize = target_->GetPointerSizeInBits();

    if(ptrSize > fromSize) {
        EstimateZeroBits(convInstr->TargetOp(), bitsLeft, depth - 1);
        Mask zeroBits = IA::ValueFromBitCount(ptrSize - fromSize);
        bits = zeroBits << fromSize;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsPhi(Instruction* definingInstr, 
                                      Mask& bits, int depth) {
    // Take the intersection of the zero bits for all incoming operands.
    auto phiInstr = definingInstr->As<PhiInstr>();
    bits = -1;

    for(int i = 0; i < phiInstr->OperandCount(); i++) {
        // Skip over over operands that reference this 'phi'.
        auto op = phiInstr->GetOperand(i);
        if(op == phiInstr->ResultOp()) continue;

        Mask zeroBits;
        EstimateZeroBits(phiInstr->GetOperand(i), zeroBits, depth - 1);
        bits &= zeroBits;

        // If 'bits' is now zero we give up, 
        // because it cannot change anymore.
        if(bits == 0) break;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsCall(Instruction* definingInstr, 
                                       Mask& bits, int depth) {
    auto callInstr = definingInstr->As<CallInstr>();
    auto mathIntrinsic = callInstr->GetIntrinsicAs<MathIntrinsic>();
    if(mathIntrinsic == nullptr) return;
    
    // Handle the 'abs' intrinsic.
    if(mathIntrinsic->IsAbs()) {
        // The bits that are zero in the argument
        // will remain zero after the 'abs'.
        EstimateZeroBits(callInstr->GetArgument(0), bits, depth - 1);
        return;
    }

    // Handle the 'min' and 'max' intrinsic.
    if((mathIntrinsic->IsMin() || 
        mathIntrinsic->IsMax()) == false) return;

    Mask argBitsA;
    Mask argBitsB;
    EstimateZeroBits(callInstr->GetArgument(0), argBitsA, depth - 1);
    EstimateZeroBits(callInstr->GetArgument(1), argBitsB, depth - 1);

    // We know the zero bits in the left part, but only if the sign bit
    // is not set in any of the operands.
    auto intType = callInstr->GetArgument(0)->GetType()->As<IntegerType>();
    auto intKind = intType->GetSubtype();

    if(IA::GetSignBit(argBitsA, intKind) || 
       IA::GetSignBit(argBitsB, intKind)) {
        return;
    }

    int zeroBitsA = IA::LeftmostZeroBits(argBitsA, intKind);
    int zeroBitsB = IA::LeftmostZeroBits(argBitsB, intKind);
    int maskBits = mathIntrinsic->IsMin() ? zeroBitsA : zeroBitsB;
    bits = IA::ValueFromBitCount(maskBits) << 
           (intType->SizeInBits() - maskBits);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateZeroBitsQuest(Instruction* definingInstr, 
                                        Mask& bits, int depth) {
    // Take the intersection of the zero bits of the operands.
    auto questInstr = definingInstr->As<QuestionInstr>();
    bits = -1;
    
    Mask zeroBits;
    EstimateZeroBits(questInstr->TrueOp(), zeroBits, depth - 1);
    bits &= zeroBits;

    EstimateZeroBits(questInstr->FalseOp(), zeroBits, depth - 1);
    bits &= zeroBits;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateOneBits(Operand* op, Mask& bits, int depth) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsFalse(op->IsFloating());

    if(op->IsUndefinedConstant()) {
        bits = 0;
        return;
    }

    // Try to take the mask from the cache first.
    if(TryTakeOneBitsFromCache(op, bits)) {
        return;
    }

    // Compute the bits that are definitely one.
    bits = 0;
    EstimateOneBitsImpl(op, bits, (depth != -1) ? depth : MAXIMUM_BITS_DEPTH);
    
    // Cache the result into a Known Bits Tag.
    if(auto temp = op->As<Temporary>()) {
        auto bitsTag = temp->GetTag<KnownBitsTag>();

        if(bitsTag == nullptr) {
            bitsTag = KnownBitsTag::GetKnownBits();
            temp->AddTag(bitsTag);
        }

        bitsTag->SetOneBits(bits);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::TryTakeOneBitsFromCache(Operand* op, Mask& bits) {
    if(auto temp = op->As<Temporary>()) {
        if(auto bitsTag = temp->GetTag<KnownBitsTag>()) {
            if (bitsTag->HasOneBitInfo()) {    
                bits = bitsTag->OneBits();
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateOneBitsImpl(Operand* op, unsigned __int64& bits, int depth) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsLargerOrEqual(depth, 0);

    // Don't recourse too many times, in most cases it won't improve the results.
    if(depth == 0) return;

    auto intType = op->GetType()->As<IntegerType>();
    bool isNegative = false;
    bits = 0; // We start knowing nothing.

    // First test for constants and references.
    IntConstant* intConst = op->As<IntConstant>();

    if(intConst == nullptr) {
        isNegative = IsOperandNegative(op, intConst);
    }

	if(intConst) {
        // We know all the bits that are set for constants.
        __int64 mask = IA::GetMinusOneMask(intConst->GetType());
        bits = intConst->Value() & mask;
        return;
    }
    else if(isNegative) {
        bits = IA::GetSignBitMask(intType);
    }

    // Now check for instructions for which we can determine some of the zero bits.
    auto definingInstr = op->DefiningInstruction();
    
    if(definingInstr == nullptr) {
        return;
    }

    // What we know about the left and right operands of binary instructions.
    Mask bitsLeft = 0;
    Mask bitsRight = 0;

    switch(definingInstr->GetOpcode()) {
        case Instr_And: {
            // A bit is one if it's one in both operands.
            EstimateOneBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
            EstimateOneBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);
            bits = bitsLeft & bitsRight;
            break;
        }
        case Instr_Or: {
            // A bit is one if it's one in at least one of the operands.
            EstimateOneBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
            EstimateOneBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);
            bits = bitsLeft | bitsRight;
            break;
        }
        case Instr_Xor: {
            // A bit is one if it has different values.
            EstimateOneBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
            EstimateOneBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);
            
            Mask zeroBitsLeft;
            Mask zeroBitsRight;
            EstimateZeroBits(definingInstr->GetSourceOp(0), zeroBitsLeft, depth - 1);
			EstimateZeroBits(definingInstr->GetSourceOp(1), zeroBitsRight, depth - 1);

			bits = (bitsLeft & bitsRight) |
                   (zeroBitsLeft & zeroBitsRight);
            break;
        }
        case Instr_Shl: {
            // The first C bits are definitely zero.
            if(auto intConst = definingInstr->GetSourceOp(1)->As<IntConstant>()) {
                EstimateOneBits(definingInstr->GetSourceOp(0), bits, depth - 1);
                bits <<= intConst->Value();
            }
            break;
        }
        case Instr_Ushr: {
            // The last C bits are definitely zero.
            if(auto intConst = definingInstr->GetSourceOp(1)->As<IntConstant>()) {
                EstimateOneBits(definingInstr->GetSourceOp(0), bits, depth - 1);
                bits >>= intConst->Value();
            }
            break;
        }
        case Instr_Shr:  { EstimateOneBitsShr(definingInstr, bits, depth);    break; }
        case Instr_Add:  { EstimateOneBitsAddSub(definingInstr, bits, depth); break; }
        case Instr_Mod:  { EstimateOneBitsMod(definingInstr, bits, depth);    break; }
        case Instr_Umod: { EstimateOneBitsUmod(definingInstr, bits, depth);   break; }
        case Instr_Trunc:
        case Instr_Zext: { EstimateOneBitsZextTrunc(definingInstr, bits, depth); break; }
        case Instr_Sext: { EstimateOneBitsSext(definingInstr, bits, depth);      break; }
        case Instr_Phi:  { EstimateOneBitsPhi(definingInstr, bits, depth);       break; }
        case Instr_Question: { EstimateOneBitsQuest(definingInstr, bits, depth); break; }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::IsOperandNegative(Operand* op, IntConstant*& intConst) {
    if(op->IsInteger() == false) {
        return false;
    }

    // Try to use value-range information.
    if(auto rangeTag = GetRangeTag(op)) {
        intConst = rangeTag->GetAsConstant(op);

        if(intConst == nullptr) {
            // We don't have a constant, but try at least
            // to determine if the number is positive.
            if(IsNegative(op, nullptr, 0)) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateOneBitsShr(Instruction* definingInstr, 
                                     Mask& bits, int depth) {
    // The last C bits are definitely one if we know that the sign bit is one.
    if(auto intConst = definingInstr->GetSourceOp(1)->As<IntConstant>()) {
        EstimateOneBitsImpl(definingInstr->GetSourceOp(0), bits, depth - 1);
        auto intKind = intConst->GetType()->GetSubtype();

        if(IA::GetSignBit(bits, intKind) == 1) {
            bits >>= intConst->Value();
            Mask oneBits = IA::ValueFromBitCount(intConst->Value());
            int shift = intConst->GetType()->SizeInBits() - intConst->Value();
            bits = bits | (oneBits << shift);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateOneBitsAddSub(Instruction* definingInstr, 
                                        Mask& bits, int depth) {
    Mask oneBitsLeft = 0;
    Mask oneBitsRight = 0;
    Mask zeroBitsLeft = 0;
    Mask zeroBitsRight = 0;

    // If there are zero bits on the right side of any of the operands,
    // the one bits in the other operand are preserved after addition/subtraction.
    auto arithInstr = definingInstr->As<ArithmeticInstr>();
    EstimateOneBitsImpl(arithInstr->LeftOp(), oneBitsLeft, depth - 1);
    EstimateOneBitsImpl(arithInstr->RightOp(), oneBitsRight, depth - 1);
    EstimateZeroBitsImpl(arithInstr->LeftOp(), zeroBitsLeft, depth - 1);
    EstimateZeroBitsImpl(arithInstr->RightOp(), zeroBitsRight, depth - 1);

    auto intType = arithInstr->LeftOp()->GetType()->As<IntegerType>();
    auto intKind = intType->GetSubtype();
    int zeroLeft = IA::RightmostZeroBits(zeroBitsLeft, intKind);
    int zeroRight = IA::RightmostZeroBits(zeroBitsRight, intKind);

    if(zeroLeft || zeroRight) {
        if(arithInstr->IsAdd()) {
            if(zeroLeft > zeroRight) {
                bits = oneBitsRight & IA::ValueFromBitCount(zeroLeft);
            }
            else bits = oneBitsLeft & IA::ValueFromBitCount(zeroRight);
        }
    }

    // If the instruction is marked as "no signed overflow",
    // and on both sides the sign bit is one, then the sign bit
    // can't become zero after addition (positive number).
    if(arithInstr->IsAdd() && arithInstr->HasUndefinedOverflow()) {
        if(IA::GetSignBit(oneBitsLeft, intKind) && 
           IA::GetSignBit(oneBitsRight, intKind)) {
            bits |= IA::GetSignBitMask(intKind);
        }
    }
    else if(arithInstr->IsSub() && arithInstr->HasUndefinedOverflow()) {
        // If we subtract a positive number from a negative one,
        // the sign bit can't change from one to zero if there is no overflow.
        if(IA::GetSignBit(oneBitsLeft, intKind) && 
           IA::GetSignBit(zeroBitsRight, intKind)) {
            bits |= IA::GetSignBitMask(intKind);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateOneBitsMod(Instruction* definingInstr, 
                                     Mask& bits, int depth) {
    Mask bitsLeft = 0;
    Mask bitsRight = 0;

    EstimateOneBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
    EstimateOneBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);
    auto intType = definingInstr->GetSourceOp(0)->GetType()->As<IntegerType>();
    auto intKind = intType->GetSubtype();
    bool done = false;

    // Test for a power of two constant as the right operand.
    if(auto C = definingInstr->GetSourceOp(0)->As<IntConstant>()) {
        if(IA::IsPowerOfTwo(C)) {
            int oneBitPos = IA::Log2(C);
            Mask lowMask = IA::ValueFromBitCount(oneBitPos);

            // The low bits are not modified at all (the 'umod' acts
            // like an 'and' with a mask only with ones).
            bits = bitsLeft & lowMask;

            // The the left operand is negative and the result 
            // will not be zero (all bits in the right part are not zero),
            // then the left bits are one.
            if(IA::GetSignBit(bitsLeft, intKind) || 
               ((bitsLeft & lowMask) != 0)) {
                int oneBits = intType->SizeInBits() - oneBitPos;
                bits = IA::ValueFromBitCount(oneBits) << oneBitPos;
            }
        }
    }

    // The sign bit is one if the left operand is negative.
    if(IA::GetSignBit(bitsLeft, intKind)) {
        bits |= IA::GetSignBitMask(intKind);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateOneBitsUmod(Instruction* definingInstr, 
                                      Mask& bits, int depth) {
    Mask bitsLeft = 0;
    Mask bitsRight = 0;

    EstimateOneBits(definingInstr->GetSourceOp(0), bitsLeft, depth - 1);
    EstimateOneBits(definingInstr->GetSourceOp(1), bitsRight, depth - 1);
    auto intType = definingInstr->GetSourceOp(0)->GetType()->As<IntegerType>();
    auto intKind = intType->GetSubtype();

    // If the right operand is a power of two constant we know
    // that the left bits are zero.
    if(auto C = definingInstr->GetSourceOp(0)->As<IntConstant>()) {
        if(IA::IsPowerOfTwo(C)) {
            // The low bits are not modified at all.
            int oneBitPos = IA::Log2(C);
            bits = bitsLeft & IA::ValueFromBitCount(oneBitPos);
            int maskBits = intType->SizeInBits() - oneBitPos;
            Mask mask = IA::ValueFromBitCount(maskBits);
            bits |= mask << oneBitPos;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateOneBitsZextTrunc(Instruction* definingInstr, 
                                           Mask& bits, int depth) {
    Mask bitsLeft = 0;

    // The right bits are preserved.
    auto convInstr = definingInstr->As<ConversionInstr>();
    EstimateOneBits(convInstr->TargetOp(), bitsLeft, depth - 1);
    auto toType = convInstr->CastType()->As<IntegerType>();
    auto fromType = convInstr->TargetOp()->GetType()->As<IntegerType>();

    // We need to invert the types for 'trunc'.
    if(convInstr->IsTrunc()) {
        std::swap(toType, fromType);
    }

    int toBits = toType->SizeInBits();
    int fromBits = fromType->SizeInBits();
    bits = bitsLeft & IA::ValueFromBitCount(toBits - fromBits);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateOneBitsSext(Instruction* definingInstr, 
                                      Mask& bits, int depth) {
    Mask bitsLeft = 0;

    // The right bits are preserved, and the left bits are one
    // if the sign bit is set.
    auto convInstr = definingInstr->As<ConversionInstr>();
    EstimateOneBits(convInstr->TargetOp(), bitsLeft, depth - 1);
    auto toType = convInstr->CastType()->As<IntegerType>();
    auto fromType = convInstr->TargetOp()->GetType()->As<IntegerType>();

    if(IA::GetSignBit(bitsLeft, toType->GetSubtype()) == 1) {
        int fromBits = fromType->SizeInBits();
        Mask oneBits = IA::ValueFromBitCount(toType->SizeInBits() -
                                             fromType->SizeInBits());
        bits = bitsLeft | (oneBits << fromBits);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateOneBitsPhi(Instruction* definingInstr, 
                                     Mask& bits, int depth) {
    // Take the intersection of the one bits for all incoming operands.
    auto phiInstr = definingInstr->As<PhiInstr>();
    bits = -1;

    for(int i = 0; i < phiInstr->OperandCount(); i++) {
        // Skip over over operands that reference this 'phi'.
        auto op = phiInstr->GetOperand(i);
        if(op == phiInstr->ResultOp()) continue;

        Mask zeroBits;
        EstimateOneBits(phiInstr->GetOperand(i), zeroBits, depth - 1);
        bits &= zeroBits;

        // If 'bits' is now zero we give up, because it cannot change anymore.
        if(bits == 0) break;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::EstimateOneBitsQuest(Instruction* definingInstr, 
                                       Mask& bits, int depth) {
    // Take the intersection of the zero bits of the operands.
    auto questInstr = definingInstr->As<QuestionInstr>();
    bits = -1;
    
    Mask oneBits;
    EstimateOneBits(questInstr->TrueOp(), oneBits, depth - 1);
    bits &= oneBits;

    EstimateOneBits(questInstr->FalseOp(), oneBits, depth - 1);
    bits &= oneBits;
}

} // namespace Analysis