// ComparisonPeephole.cpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 
// 'trunc', 'zext', 'sext', 'itof', 'uitof', 'ftoi', 'ftoui',
// 'ptop', 'ptoi' and 'ptop' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"
#include "../IR/IRPrinter.hpp"
#include <fstream>

namespace Optimization {

Operand* Peephole::HandleTrunc(TruncInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Trunc, op, castType, 
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	if(auto result = HandleCastCast(Instr_Trunc, op, castType)) {
		return result;
	}

	// Try to evaluate the target expression using the truncated type directly.
	// This works for many arithmetic and logical operations.
	// trunc(a + b, int8) -> trunc(a, int8) + trunc(a, int8), for example.
	if(CanBeTruncated(op, castType)) {
		// This will get rid of at least one instruction.
#if 1
        Log::Error("Eliminated trunc in " + *instr->ParentBlock()->Name());
#endif
		return LOG(ChangeExpressionType(op, castType, Instr_Trunc));
	}

	// trunc((zext a, intX) >> C, intY) -> 0, if 'C' has at least the number
	// of bytes the type of 'a' has. This means that only 0 bits will be shifted in.
	// This also works for arithmetic shift, because the zero extension to a larger
	// integer type makes the upper part 0.
	Operand* a;
	IntConstant* C;

	if(Match<UshrInstr>(MatchConversion<ZextInstr>(MatchAny(&a)), MatchIC(&C))(op) ||
	   Match<ShrInstr>(MatchConversion<ZextInstr>(MatchAny(&a)), MatchIC(&C))(op)) {
		auto intType = a->GetType()->As<IntegerType>();
		int bits = intType->SizeInBits();

		if(C->Value() >= bits) {
			// We can replace all of this with 0.
			return LOG(GetZeroInt(castType));
		}
	}

    // Try to simplify using value range and known-bits information.
    return HandleTruncOperandInfo(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleTruncOperandInfo(TruncInstr* instr) {
    // If we know that value range of the operand that is truncated
    // we may decide that the 'trunc' is useless. For example, in
    // 'trunc a, int8' for 'a' in range [0, 10] the 'trunc' is useless.
    OperandInfo opInfo(irGen_.GetUnit(), GetTarget());
    Range range;
    
    if(opInfo.GetRange(instr->TargetOp(), instr->ParentBlock(), range)) {
        // The range should not be symbolic.
        if(range.HasBaseOperand() == false) {
            __int64 typeMin = IA::GetMin(instr->CastType()->As<IntegerType>(), true);
            __int64 typeMax = IA::GetMax(instr->CastType()->As<IntegerType>(), true);

            if((range.Low.Constant > typeMin) &&
               (range.High.Constant < typeMax)) {
                // We can't remove the instruction because a type
                // mismatch would result, but we can mark is as useless
                // and hope that the Code Generator doesn't generate it.
                instr->SetIsUseless(true);
                return LOG(nullptr);
            }
        }
    }

    // If we know that the bits that are set to zero by the 'trunc'
    // already are zero then the 'trunc' is useless.
    int fromBits = instr->TargetOp()->GetType()->As<IntegerType>()->SizeInBits();
    int toBits = instr->CastType()->As<IntegerType>()->SizeInBits();
          
    if(opInfo.AreBitsZero(instr->TargetOp(), toBits, fromBits, 
                          instr->ParentBlock())) {
        instr->SetIsUseless(true);
        return LOG(nullptr);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::CanBeTruncated(Operand* op, const Type* truncType, int depth) {
    if(depth == GetMaximumConversionDepth()) {
        // Most probably it's a waste of time
        // to check more instructions.
        return false;
    }

	// This allows operations performed only on extended operands and constants.
	// Constants can always be truncated.
	if(op->IsConstant()) {
		return true;
	}

	// Any other operand can't be truncated if it's not an instruction.
	auto instr = op->DefiningInstruction();
	if(instr == nullptr) {
        return false;
    }

	// Because 'ChangeExpressionType' modifies the existing instructions
	// we can't continue if the instruction has more than one use, because
	// it would be needed to clone the instruction, which isn't beneficial.
	if(instr->HasDestinationOp() && 
	   (instr->GetDestinationOp()->HasSingleUser() == false)) {
        // An exception is when we have an extension directly to 'truncType',
        // because in this case no new instructions are generated.
        if(((instr->IsSext() || instr->IsZext()) &&
            (instr->As<ConversionInstr>()->CastType() == truncType)) == false) {
	        return false;
        }
	}

	// Try to take a decision based on the type of the instruction.
	switch(instr->GetOpcode()) {
		case Instr_Add:
		case Instr_Sub:
		case Instr_Mul:
		case Instr_And:
		case Instr_Or:
		case Instr_Xor: {
			// All these instructions can have their type changed, but only if
			// their operands can also have their type changed.
			return CanBeTruncated(instr->GetSourceOp(0), truncType, depth + 1) &&
				   CanBeTruncated(instr->GetSourceOp(1), truncType, depth + 1);
		}
		case Instr_Udiv:
		case Instr_Umod: {
			// We can evaluate them truncated only if all bits that would
            // have been removed by 'trunc' are definitely zero.
            int toBits = truncType->As<IntegerType>()->SizeInBits();
            int fromBits = op->GetType()->As<IntegerType>()->SizeInBits();

            if(toBits < fromBits) {
                if(opInfo_.AreBitsZero(op, toBits, fromBits)) {
                    return CanBeTruncated(instr->GetSourceOp(0), truncType, depth + 1) &&
				           CanBeTruncated(instr->GetSourceOp(1), truncType, depth + 1);
                }
            }
            return false;
		}
		case Instr_Shl: {
			// We can evaluate the logical left shift truncated only if
			// the shift amount is a constant.
			if(instr->GetSourceOp(1)->IsConstant()) {
				return CanBeTruncated(instr->GetSourceOp(0), truncType, depth + 1);
			}
			else return false;
		}
		case Instr_Ushr: {
			// We can evaluate the right shift truncated only if we know
            // that all bits that would have been removed by 'trunc' are zero.
            // Note that we take into consideration the shift amount.
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                int firstBit = truncType->As<IntegerType>()->SizeInBits();
                int lastBit = op->GetType()->As<IntegerType>()->SizeInBits() -
                              intConst->Value();
                if(lastBit >= firstBit) {
                    // The shift amount is large enough to guarantee
                    // that the bits are zero.
                    return false;
                }
                else if(opInfo_.AreBitsZero(op, firstBit, lastBit) == false) {
                    return false;
                }
                
                // Now test the operand that is not a constant.
                return CanBeTruncated(instr->GetSourceOp(0), truncType, depth + 1);
            }
			return false;
		}
		case Instr_Trunc:
		case Instr_Sext:
		case Instr_Zext: {
			// We can always truncate an extended operand.
			return true;
		}
        case Instr_Question: {
            return CanBeTruncated(instr->GetSourceOp(1), truncType, depth + 1) &&
                   CanBeTruncated(instr->GetSourceOp(2), truncType, depth + 1);
        }
        case Instr_Phi: {
            // Test each incoming operand.
            auto phiInstr = instr->As<PhiInstr>();

            for(int i = 0; i < phiInstr->OperandCount(); i++) {
                if(CanBeTruncated(phiInstr->GetOperand(i), 
                                  truncType, depth + 1) == false) {
                    return false;
                }
            }

            return true;
        }
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::ChangeExpressionType(Operand* op, const Type* newType, 
                                        Opcode opcode) {
	bool isSigned = opcode == Instr_Sext;
	auto intType = newType->As<IntegerType>();
	DebugValidator::IsNotNull(intType);

	if(auto intConst = op->As<IntConstant>()) { 
		// Convert the constant to the new type, taking into consideration
		// if the number is signed or not.
		__int64 result = isSigned ? IA::Sext(intConst, intType) :
									IA::Zext(intConst, intType);
		return irGen_.GetIntConst(newType, result);
	}
    else if(op->IsNullConstant() ||
            op->IsUndefinedConstant()) {
        return op;
    }
    else if(op->IsVariableReference() ||
            op->IsFunctionReference() ||
            op->IsParameter()) {
        // Nothing needs to be changed in this case.
        return op;
    }

	// The operand should be an instruction otherwise; we change it's type
	// directly to the new one.
	DebugValidator::IsTrue(op->HasDefiningInstruction());
	auto instr = op->DefiningInstruction();

	switch(instr->GetOpcode()) {
		case Instr_Trunc:
		case Instr_Zext:
		case Instr_Sext: {
			// For conversion instructions we may need to create a new instruction,
			// but only if the type from which we convert is not the new type.
			auto convInstr = op->DefiningInstrAs<ConversionInstr>();
			auto fromType = convInstr->TargetOp()->GetType()->As<IntegerType>();

			if(fromType == newType) {
				return convInstr->TargetOp();
			}
			else {
				// (fromType) intX -> intY (newType)
				// Y > X -> sext, for signed
				// Y > X -> zext, for signed
				// Y < X -> trunc
				// We need to change only the type to which we cast.
				convInstr->SetCastType(newType);
				convInstr->ResultOp()->SetType(newType);
				return convInstr->ResultOp();
			}
            break;
		}
        case Instr_Load: {
            return op;
        }
        case Instr_Phi: {
            auto phiInstr = instr->As<PhiInstr>();

            for(int i = 0; i < phiInstr->OperandCount(); i++) {
                auto incomingOp = phiInstr->GetOperand(i);
                auto newIncomingOp = ChangeExpressionType(incomingOp, newType, opcode);

                if(newIncomingOp != incomingOp) {
                    phiInstr->ReplaceSourceOp(i, newIncomingOp);
                }
            }

            if(instr->HasDestinationOp()) {
				instr->GetDestinationOp()->SetType(newType);
				return instr->GetDestinationOp();
			}
            break;
        }
        case Instr_Question: {
            auto questInstr = instr->As<QuestionInstr>();
            
            auto newTrueOp = ChangeExpressionType(questInstr->TrueOp(), 
                                                  newType, opcode);
            if(newTrueOp !=  questInstr->TrueOp()) {
                questInstr->SetTrueOp(newTrueOp);
            }

            auto newFalseOp = ChangeExpressionType(questInstr->FalseOp(), 
                                                   newType, opcode);
            if(newFalseOp != questInstr->FalseOp()) {
                questInstr->SetFalseOp(newFalseOp);
            }

            if(instr->HasDestinationOp()) {
				instr->GetDestinationOp()->SetType(newType);
				return instr->GetDestinationOp();
			}
            break;
        }
		default: {
			// For all other instructions we change the type of the operands.
			// If the operand changes (can happen only if it's a conversion)
			// we need to update the instruction.
			for(int i = 0; i < instr->SourceOpCount(); i++) {
				auto prevOp = instr->GetSourceOp(i);
				auto newOp = ChangeExpressionType(prevOp, newType, opcode);

				if(newOp != prevOp) {
					instr->ReplaceSourceOp(i, newOp);
				}
			}

			if(instr->HasDestinationOp()) {
				instr->GetDestinationOp()->SetType(newType);
				return instr->GetDestinationOp();
			}
		}
	}

    DebugValidator::Unreachable();
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleZext(ZextInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<IntegerType>();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Zext, op, castType,
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	if(auto result = HandleCastCast(Instr_Zext, op, castType)) {
		return result;
	}

	// Check for the extension of a comparison result.
	// This happens in code like 'unsigned var = (a > 3);'.
	if(auto ucmpInstr = op->DefiningInstrAs<UcmpInstr>()) {
		if(auto result = HandleZextOnCmp(ucmpInstr, castType)) {
			return result;
		}
	}

	// A 'trunc' followed by a 'zext' can in many cases be replaced
	// by an 'and' with a mask that eliminates the right bits.
	if(auto result = HandleZextOnTrunc(instr)) {
		return result;
	}

	// Try to evaluate the target expression using the truncated type directly.
	// This works for many arithmetic and logical operations.
	int maskedBits = 0;

	if(CanBeZeroExtended(op, castType, maskedBits)) {
		// zext (trunc (a, int16) >> 4), int32 -> we can eliminated both conversions
		// if we 'and' the operand 'a' with the mask 0xFFF (0...0000111111111111).
		auto sourceType = op->GetType()->As<IntegerType>();
		int sourceBits = sourceType->SizeInBits();

		// All 'maskedBits' from the left part are set to 0.
		__int64 mask = IA::ValueFromBitCount(sourceBits - maskedBits);
		auto maskOp = irGen_.GetIntConst(castType, mask);
		auto resultOp = ChangeExpressionType(op, castType, Instr_Zext);

#if 1
        Log::Error("Eliminated zext in " + *instr->ParentBlock()->Name());
#endif
		// Apply the mask to the modified expression.
		auto andOp = irGen_.GetTemporary(castType);
		irGen_.GetAnd(resultOp, maskOp, andOp);
		return LOG(andOp);
	}

	// zext (trunc (a & C), intX), intY -> a & (zext C, intY)
	// When we extend the constant it will have zeroes in the upper part,
	// eliminating the need of the 'trunc' instruction.
	Operand* a;
	IntConstant* C;

	if(MatchConversion<TruncInstr>(Match<AndInstr>(MatchAny(&a), MatchIC(&C)))(op)) {
		// Note that we can do the simplification only if the type of 'a'
		// and the type to which we cast is the same.
		if(a->GetType() == castType) {
			auto constantOp = irGen_.GetIntConst(castType, IA::Zext(C, castType));
			auto andOp = irGen_.GetTemporary(castType);
			irGen_.GetAnd(a, constantOp, andOp);
			return LOG(andOp);
		}
	}

	// zext ((trunc (a & C) ^ C, intX), intY -> (a & (zext C, intY) ^ (zext C, intY)
	// The same reasons for the simplification as above.
	if(Match<XorInstr>(MatchConversion<TruncInstr>(
	   Match<AndInstr>(MatchAny(&a), MatchIC(&C))), MatchOp(C))(op)) {
		// The types must match.
		if(a->GetType() == castType) {
			auto constantOp = irGen_.GetIntConst(castType, IA::Zext(C, castType));
			auto andOp = irGen_.GetTemporary(castType);
			irGen_.GetAnd(a, constantOp, andOp);

			auto xorOp = irGen_.GetTemporary(castType);
			irGen_.GetXor(andOp, constantOp, xorOp);
			return LOG(xorOp);
		}
	}

    // Try to simplify using value range and known-bits information.
    return HandleZextOperandInfo(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleZextOperandInfo(ZextInstr* instr) {
    // If we know that the bits that are set to zero by the 'trunc'
    // already are zero then the 'trunc' is useless.
    OperandInfo opInfo(irGen_.GetUnit(), GetTarget());
    int fromBits = instr->TargetOp()->GetType()->As<IntegerType>()->SizeInBits();
    int toBits = instr->CastType()->As<IntegerType>()->SizeInBits();

    if(opInfo.AreBitsZero(instr->TargetOp(), fromBits, toBits,
                          instr->ParentBlock())) {
        instr->SetIsUseless(true);
        return LOG(nullptr);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleZextOnCmp(CmpInstrBase* instr, const IntegerType* castType) {
	bool isSigned = instr->IsCmp();
	
	// The following type of code appears for things like
	// 'unsigned long var = a < 0;'.
	if(auto intConst = instr->RightOp()->As<IntConstant>()) {
		// zext (a < 0), int32 -> a >> 31, for signed comparison only.
		// '< 0' tests the sign bit, so it can be replaced by a logical right shift
		// that brings the sign bit to the rightmost position.
		if(((instr->Order() == Order_Less) && intConst->IsZero()) ||
			// zext (a > -1), int32 -> (a >> 31) ^ 1, for signed comparison.
			// As above, it only tests the sign bit.
		   ((instr->Order() == Order_Greater) && intConst->IsMinusOne()) ||
		   (instr->LeftOp()->GetType() == castType) && isSigned) {
			// We handle both cases together, because they have much code in common.
			auto leftType = instr->LeftOp()->GetType();
			int bits = castType->SizeInBits();
			auto constantOp = irGen_.GetIntConst(leftType, bits - 1);

			auto ushrOp = irGen_.GetTemporary(castType);
			irGen_.GetUshr(instr->LeftOp(), constantOp, ushrOp);
			
			if(intConst->IsZero()) {
				// We're done for this case (zext (a < 0), int32).
				return LOG(ushrOp);
			}

			// zext (a > -1), int32
			auto xorOp = irGen_.GetTemporary(castType);
			irGen_.GetXor(ushrOp, GetOneInt(castType), xorOp);
			return LOG(xorOp);
		}

        // All the following tests use Operand Info.
        // zext(a == 0) -> a ^ 1, if only the first bit is set
        // zext(a != 1) -> a ^ 1, if only the first bit is set
        if((instr->IsEqual() && intConst->IsZero()) ||
           (instr->IsNotEqual() && intConst->IsOne()) &&
           opInfo_.IsBitOne(instr->LeftOp(), 0)) {
            // Make sure that all other bits are zero.
            unsigned __int64 zeroBits;
            opInfo_.EstimateZeroBits(instr->LeftOp(), zeroBits);

            if(IA::IsPowerOfTwo(~zeroBits)) {
                auto xorOp = GetTemporary(instr->LeftOp());
                irGen_.GetXor(instr->LeftOp(), GetOneInt(instr->LeftOp()), xorOp);

                // If the type of 'a' is not the one after extension
                // insert a zero-extension.
                return LOG(CreateIntCastIfRequired(xorOp, castType, 
                                                   true /* isSigned */));
            }
        }

        // zext(a == 1) -> a, if only the first bit is set
        // zext(a != 0) -> a, if only the first bit is set
        if((instr->IsEqual() && intConst->IsZero()) ||
           (instr->IsNotEqual() && intConst->IsOne()) &&
           opInfo_.IsBitOne(instr->LeftOp(), 0)) {
            // If the type of 'a' is not the one after extension
            // insert a zero-extension.
            return LOG(CreateIntCastIfRequired(instr->LeftOp(), castType, 
                                               true /* isSigned */));
        }
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleZextOnTrunc(ZextInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<IntegerType>();
	Operand* a;
	const Type* truncType;

	// A 'trunc' followed by a 'zext' can in many cases be replaced
	// by an 'and' with a mask that eliminates the bits. For example,
	// zext (trunc a, int16), int32 -> a & 0xFFFF
	if(MatchConversion<TruncInstr>(MatchAny(&a), nullptr, &truncType)(op)) {
		// There are three situations, depending on the size relation between
		// the source type (type of 'a') and the destination type ('castType').
		auto sourceType = a->GetType()->As<IntegerType>();
		auto intTruncType = truncType->As<IntegerType>();
		
		// sourceType == castType -> a & MASK
		if(sourceType->SameRank(castType)) {
			int bits = sourceType->SizeInBits() - intTruncType->SizeInBits();
			__int64 mask = IA::ValueFromBitCount(bits);
			auto maskOp = irGen_.GetIntConst(castType, mask);
			
			auto andOp = irGen_.GetTemporary(castType);
			irGen_.GetAnd(a, maskOp, andOp);
			return LOG(andOp);
		}

		// sourceType < castType -> zext (a & MASK), castType
		if(sourceType->RankBelow(castType)) {
			int bits = sourceType->SizeInBits() - intTruncType->SizeInBits();
			__int64 mask = IA::ValueFromBitCount(bits);
			auto maskOp = irGen_.GetIntConst(sourceType, mask);

			auto andOp = irGen_.GetTemporary(sourceType);
			irGen_.GetAnd(a, maskOp, andOp);

			auto zextOp = irGen_.GetTemporary(castType);
			irGen_.GetZext(andOp, castType, zextOp);
			return LOG(zextOp);
		}

		// sourceType > castType -> (trunc a, castType) & MASK
		if(sourceType->RankAbove(castType)) {
			auto truncOp = irGen_.GetTemporary(castType);
			irGen_.GetTrunc(a, castType, truncOp);

			int bits = castType->SizeInBits() - intTruncType->SizeInBits();
			__int64 mask = IA::ValueFromBitCount(bits);
			auto maskOp = irGen_.GetIntConst(castType, mask);

			auto andOp = irGen_.GetTemporary(sourceType);
			irGen_.GetAnd(truncOp, maskOp, andOp);
			return LOG(andOp);
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::CanBeZeroExtended(Operand* op, const Type* zextType, 
                                 int& maskedBits, int depth) {
    if(depth == GetMaximumConversionDepth()) {
        // Most probably it's a waste of time
        // to check more instructions.
        return false;
    }

	// This allows operations performed only on extended operands and constants.
	// Constants can always be zero extended.
	if(op->IsConstant()) {
		maskedBits = 0;
		return true;
	}

	// Any other operand can't be zero extended if it's not an instruction.
	auto instr = op->DefiningInstruction();
	if(instr == nullptr) {
        return false;
    }

	// Because 'ChangeExpressionType' modifies the existing instructions
	// we can't continue if the instruction has more than one use, because
	// it would be needed to clone the instruction, which isn't beneficial.
	if(instr->HasDestinationOp() && 
	   (instr->GetDestinationOp()->HasSingleUser() == false)) {
	   return false;
	}

	// What 'maskedBits' is good for:
	// zext (trunc (a, int16) >> 4), int32 -> we can eliminated both conversions
	// if we 'and' the operand 'a' with the mask 0xFFF (0...0000111111111111),
	// which has 16 zeroes in the upper part (the truncation), and 4 more
	// which are the result of the logical right shift.
	// So 'maskedBits' records how many more bits should be set to zero in the mask.

	// Try to take a decision based on the type of the instruction.
	switch(instr->GetOpcode()) {
		case Instr_Add:
		case Instr_Sub:
		case Instr_Mul:
		case Instr_And:
		case Instr_Or:
		case Instr_Xor:
		case Instr_Shl: {
            return CanBeZeroExtendedInstruction(instr, zextType, 
                                                maskedBits, depth);
		}
		case Instr_Ushr: {
			return CanBeZeroExtendedInstructionUshr(instr, zextType, 
                                                    maskedBits, depth);
		}
		case Instr_Trunc:
		case Instr_Sext:
		case Instr_Zext: {
			// We can always zero extend in these cases.
			return true;
		}
        case Instr_Phi: {
            // Give up in this case, we risk to enter a cycle.
            return false;
        }
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::CanBeZeroExtendedInstruction(Instruction* instr, const Type* zextType, 
    int& maskedBits, int depth) {
    int masked1;
    int masked2;

    if((CanBeZeroExtended(instr->GetSourceOp(0), 
        zextType, masked1, depth + 1) == false) ||
        (CanBeZeroExtended(instr->GetSourceOp(1), 
        zextType, masked2, depth + 1) == false)) {
            return false;
    }

    // If no bits need to be masked we're done.
    if((masked1 == 0) && (masked2 == 0)) {
        maskedBits = 0;
        return true;
    }
    else if(masked2 == 0) {
        // If the right operand has no masked bits then for some logical
        // instructions, we can perform the simplification. The condition is
        // that the right operand should be a constant that has no bits
        // in the range that will be cleared.
        if(instr->IsAnd() || instr->IsOr() || instr->IsXor()) {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                // Create a mask having the uppermost 'maskedBits' set to 1.
                auto intType = instr->GetDestinationOp()->GetType()->As<IntegerType>();
                int bits = intType->SizeInBits();

                __int64 mask = IA::ValueFromBitCount(bits - maskedBits);
                mask = IA::Not(mask, intType->GetSubtype());

                // The mask applied to the constant should be zero.
                maskedBits = masked1;
                return IA::And(intConst->Value(), mask, 
                               intType->GetSubtype()) == 0;
            }
        }
    }

    // For any other case we can't conclude anything, so we give up.
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::CanBeZeroExtendedInstructionUshr(Instruction* instr, const Type* zextType, 
                                                int& maskedBits, int depth) {
    int masked;

    if(CanBeZeroExtended(instr->GetSourceOp(0), zextType, 
                         masked, depth + 1) == false) {
            return false;
    }

    // If we shift by a constant we know how many bits need to be masked.
    if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
        maskedBits = masked + intConst->Value();

        // Don't let 'maskedBits' be larger than the number of bits in the type.
        auto intType = instr->GetDestinationOp()->GetType()->As<IntegerType>();
        int bits = intType->SizeInBits();

        if(maskedBits > bits) {
            maskedBits = bits;
        }

        return true;
    }

    // For any other case we can't conclude anything, so we give up.
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleSext(SextInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<IntegerType>();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Sext, op, castType, 
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	if(auto result = HandleCastCast(Instr_Sext, op, castType)) {
		return result;
	}

	// Try to evaluate the target expression using the truncated type directly.
	// This works for many arithmetic and logical operations.
	if(CanBeSignExtended(op, castType)) {
		// sext (trunc (a, int16) >> 4), int32 -> we can eliminated both conversions
		// if we shift left, then right to perform a sign extension.
		// This also exposes some more simplification opportunities.
		auto sourceType = op->GetType()->As<IntegerType>();
		int sourceBits = sourceType->SizeInBits();
		int bits = castType->SizeInBits() - sourceBits;
		auto constantOp = irGen_.GetIntConst(castType, bits);

#if 1
        Log::Error("Eliminated sext in " + *instr->ParentBlock()->Name());
#endif
		auto resultOp = ChangeExpressionType(op, castType, Instr_Sext);
		return LOG(CreateShlShrPair(resultOp, constantOp, castType));
	}

	// Convert a sign-extended 'trunc' to a left shift and an arithmetic right shift,
	// because it may expose further simplification opportunities.
	// The arithmetic right shift is needed in order to extend the sign.
	// sext (trunc a, intX), intY -> (a << C1) >> C2
	Operand* a;
	const Type* truncType;

	if(MatchConversion<TruncInstr>(MatchAny(&a), nullptr, &truncType)(op)) {
		// This can be done only if the type of 'a' is the type of the sign extension.
		if(a->GetType() == castType) {
			int bits = castType->SizeInBits() - 
					   truncType->As<IntegerType>()->SizeInBits();
			auto constantOp = irGen_.GetIntConst(castType, bits);
			return LOG(CreateShlShrPair(a, constantOp, castType));
		}
	}

	// Test for the elimination of some bits, performed at a smaller precision.
	// Consider something like the following case:
	// t1 = trunc a, int8      t2 = shl t1, 5     t3 = shr t2, 5     t3 = sext t3, int32
	// This can be replaced by:
	// t1 = shl a, 31     t2 = shr t1, 31
	// Why 31: 32 - 8 + 5 = 31 (in order to do the sign extension correctly we must
	// shift by 24 bits more - the size difference between the start and the end type).
	IntConstant* C1;
	IntConstant* C2;

	if(Match<ShrInstr>(Match<ShlInstr>(MatchConversion<TruncInstr>(MatchAny(&a)),
	   MatchIC(&C1)), MatchIC(&C2))(op)) {
		// The constants and the start and end type must be the same.
		if((C1 == C2) && (a->GetType() == castType)) {
			int startBits = C1->GetType()->SizeInBits();
			int endBits = castType->SizeInBits();
			int bits = endBits - startBits + C1->Value();

			auto constantOp = irGen_.GetIntConst(castType, bits);
			return LOG(CreateShlShrPair(a, constantOp, castType));
		}
	}

    // sext(a < 0)  -> a >> 31
    // sext(a > -1) -> ~(a >> 31)
    if(auto cmpInstr = op->DefiningInstrAs<CmpInstr>()) {
        if((cmpInstr->IsLess() && cmpInstr->RightOp()->IsZeroInt()) ||
           (cmpInstr->IsGreater() && cmpInstr->RightOp()->IsMinusOneInt())) {
            // a >> 31 (signed right shift)
            Operand* resultOp;
            auto intType = cmpInstr->LeftOp()->GetType()->As<IntegerType>();
            auto shiftAmount = irGen_.GetIntConst(intType, 
                                                  intType->SizeInBits() - 1);

            auto shiftOp = irGen_.GetTemporary(intType);
            irGen_.GetShr(cmpInstr->LeftOp(), shiftAmount, shiftOp);

            if(cmpInstr->IsGreater()) {
                auto xorOp = irGen_.GetTemporary(intType);
                irGen_.GetXor(shiftOp, GetMinusOneInt(intType), xorOp);
                resultOp = xorOp;
            }
            else resultOp = shiftOp;

            // It's possible that the types aren't the same.
            return LOG(CreateIntCastIfRequired(resultOp, castType, 
                                               true /* isSigned */));
        }
    }

    // Try to simplify using value range and known-bits information.
    return HandleSextOperandInfo(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleSextOperandInfo(SextInstr* instr) {
    // If we know that the bits that are set to zero by the 'trunc'
    // already are zero then the 'trunc' is useless.
    OperandInfo opInfo(irGen_.GetUnit(), GetTarget());
    int fromBits = instr->TargetOp()->GetType()->As<IntegerType>()->SizeInBits();
    int toBits = instr->CastType()->As<IntegerType>()->SizeInBits();

    if(opInfo.IsBitZero(instr->TargetOp(), fromBits - 1, instr->ParentBlock())) {
        if(opInfo.AreBitsZero(instr->TargetOp(), fromBits, toBits,
            instr->ParentBlock())) {
            instr->SetIsUseless(true);
            return LOG(nullptr);
        }
    }
    else if(opInfo.IsBitOne(instr->TargetOp(), fromBits - 1, instr->ParentBlock())) {
        if(opInfo.AreBitsZero(instr->TargetOp(), fromBits, toBits,
            instr->ParentBlock())) {
            instr->SetIsUseless(true);
            return LOG(nullptr);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateShlShrPair(Operand* op, IntConstant* amountOp,
								    const Type* type) {
	// Create a left shift, followed by an arithmetic right shift.
	auto shlOp = irGen_.GetTemporary(type);
	irGen_.GetShl(op, amountOp, shlOp);

	auto shrOp = irGen_.GetTemporary(type);
	irGen_.GetShr(shlOp, amountOp, shrOp);
	return shrOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::CanBeSignExtended(Operand* op, const Type* sextType, int depth) {
    if(depth == GetMaximumConversionDepth()) {
        // Most probably it's a waste of time
        // to check more instructions.
        return false;
    }

	// This allows operations performed only on extended operands and constants.
	// Constants can always be zero extended.
	if(op->IsConstant()) {
		return true;
	}

	// Any other operand can't be zero extended if it's not an instruction.
	auto instr = op->DefiningInstruction();
	if(instr == nullptr) {
        return false;
    }

	// Because 'ChangeExpressionType' modifies the existing instructions
	// we can't continue if the instruction has more than one use, because
	// it would be needed to clone the instruction, which isn't beneficial.
	if(instr->HasDestinationOp() && 
	   (instr->GetDestinationOp()->HasSingleUser() == false)) {
	   return false;
	}

	// Try to take a decision based on the type of the instruction.
	switch(instr->GetOpcode()) {
		case Instr_Add:
		case Instr_Sub:
		case Instr_Mul:
		case Instr_And:
		case Instr_Or:
		case Instr_Xor: {
			return CanBeSignExtended(instr->GetSourceOp(0), sextType, depth + 1) &&
				   CanBeSignExtended(instr->GetSourceOp(1), sextType, depth + 1);
		}
		case Instr_Trunc:
		case Instr_Sext:
		case Instr_Zext: {
			// We can always sign extend in these cases.
			return true;
		}
        case Instr_Phi: {
            // Give up in this case, we risk to enter a cycle.
            return false;
        }
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFtrunc(FtruncInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<FloatingType>();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Ftrunc, op, castType,
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	if(auto result = HandleCastCast(Instr_Ftrunc, op, castType)) {
		return result;
	}

	// A truncated arithmetic operation performed with extended operands
	// can be performed without extending the operands anymore.
	// trunc ((fext a, double) + (fext b, double)), float -> a + b
	// This eliminates three instructions.
	if(op->HasDefiningInstruction() && op->DefiningInstruction()->IsFloatArithmetic()) {
		Operand* leftOp = nullptr;
		Operand* rightOp = nullptr;
		auto arithInstr = op->DefiningInstrAs<ArithmeticInstr>();

		if(auto floatConstLeft = arithInstr->LeftOp()->As<FloatConstant>()) {
			// Get a new constant having the extended type.
			leftOp = irGen_.GetFloatingConst(castType, floatConstLeft->Value());
		}
		else if(auto fextLeft = arithInstr->LeftOp()->DefiningInstrAs<FextInstr>()) {
			// Skip over the 'fext'.
			if(fextLeft->TargetOp()->GetType() == castType) {
				leftOp = fextLeft->TargetOp();
			}
		}

		if(auto floatConstRight = arithInstr->RightOp()->As<FloatConstant>()) {
			// Get a new constant having the extended type.
			rightOp = irGen_.GetFloatingConst(castType, floatConstRight->Value());
		}
		else if(auto fextRight = arithInstr->RightOp()->DefiningInstrAs<FextInstr>()) {
			// Skip over the 'fext'.
			if(fextRight->TargetOp()->GetType() == castType) {
				rightOp = fextRight->TargetOp();
			}
		}

		if(leftOp && rightOp) {
			// We can do the simplification.
			auto resultOp = irGen_.GetTemporary(castType);
			auto instr = ArithmeticInstr::GetArithmetic(arithInstr->GetOpcode(),
										                leftOp, rightOp, 
                                                        resultOp, nullptr,
                                                        irGen_.GetInsertionPoint());
            irGen_.SetInsertionPoint(instr);
			return LOG(resultOp);
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFext(FextInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<FloatingType>();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Fext, op, castType,
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	if(auto result = HandleCastCast(Instr_Fext, op, castType)) {
		return result;
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFtoi(FtoiInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<IntegerType>();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Ftoi, op, castType,
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	if(auto result = HandleCastCast(Instr_Ftoi, op, castType)) {
		return result;
	}

	// Test for a 'ftoi' applied on a 'itof' or 'uitof'.
	if(auto result = HandleFtoiItofPair(instr)) {
		return result;
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleItof(ItofInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<FloatingType>();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Itof, op, castType,
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	if(auto result = HandleCastCast(Instr_Itof, op, castType)) {
		return result;
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleUitof(UitofInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<FloatingType>();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Uitof, op, castType,
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	if(auto result = HandleCastCast(Instr_Uitof, op, castType)) {
		return result;
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFtoui(FtouiInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<IntegerType>();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Ftoui, op, castType,
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	if(auto result = HandleCastCast(Instr_Ftoui, op, castType)) {
		return result;
	}

	// Test for a 'ftoui' applied on a 'itof' or 'uitof'.
	if(auto result = HandleFtoiItofPair(instr)) {
		return result;
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleItop(ItopInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<PointerType>();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Itop, op, castType,
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	// This takes care about things like 'itop (ptoi)'.
	if(auto result = HandleCastCast(Instr_Itop, op, castType)) {
		return result;
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandlePtoi(PtoiInstr* instr) {
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<IntegerType>();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Ptoi, op, castType,
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	// This takes care about things like 'ptoi (itop)'.
	if(auto result = HandleCastCast(Instr_Ptoi, op, castType)) {
		return result;
	}

	// ptoi (addr p, 0), intX -> ptoi p, intX
	// The pointer is not changed at all. This also works for the first element
	// of an array, and for the first member of a record, if the field offset is 0.
	// The zero offset condition is because we can have an union-like record.
	Operand* base;
	IntConstant* C;

	if(Match<AddressInstr>(MatchAny(&base), MatchInt(0))(op) ||
	   Match<IndexInstr>(MatchAny(&base), MatchInt(0))(op)) {
		// Use the base operand directly.
		auto ptoiOp = irGen_.GetTemporary(castType);
		irGen_.GetPtoi(base, castType, ptoiOp);
		return LOG(ptoiOp);
	}
	else if(Match<ElementInstr>(MatchAny(&base), MatchInt(0))(op)) {
		// The base operand has type 'pointer to record'.
		auto baseType = base->GetType()->As<PointerType>();
		auto recordType = baseType->PointeeType()->As<RecordType>();

		if(recordType->Fields()[0].FieldOffset == 0) {
			// Use the base operand directly.
			auto ptoiOp = irGen_.GetTemporary(castType);
			irGen_.GetPtoi(base, castType, ptoiOp);
			return LOG(ptoiOp);
		}
	}

	// ptoi (addr (ptop p, X*), C) -> ptoi(p) + (sizeof(*X) * C)
	// We can get rid of both the pointer conversion and the pointer adjustment.
	// It is safe to do this transformation because pointer arithmetic doesn't overflow.
	// This also works with 'index' and 'elem' instructions.
	const Type* ptopType;

	if(Match<AddressInstr>(MatchConversion<PtopInstr>(
	   MatchAny(&base), nullptr, &ptopType), MatchIC(&C))(op) || 
	   Match<IndexInstr>(MatchConversion<PtopInstr>(
	   MatchAny(&base), nullptr, &ptopType), MatchIC(&C))(op) ||
	   Match<ElementInstr>(MatchConversion<PtopInstr>(
	   MatchAny(&base), nullptr, &ptopType), MatchIC(&C))(op)) {
        // We have a match.
		auto baseType = base->GetType()->As<PointerType>();
		bool isElement = op->DefiningInstrIs<ElementInstr>();
		__int64 offset;

		if(op->DefiningInstrIs<ElementInstr>()) {
			// The base is a record, use the offset of the the field.
			auto recordType = baseType->PointeeType()->As<RecordType>();
			DebugValidator::IsSmaller(C->Value(), recordType->FieldCount());
			offset = recordType->Fields()[C->Value()].FieldOffset;
		}
		else if(op->DefiningInstrIs<IndexInstr>()) {
			// The base is an array; the offset is the size 
			// of the element type multiplied by the constant.
			auto arrayType = baseType->PointeeType()->As<ArrayType>();
			offset = Analysis::TypeInfo::GetSize(arrayType->ElementType(), GetTarget());
			offset *= C->Value();
		}
		else {
			// The base is a pointer; the offset is the size 
			// of the pointee multiplied by the constant.
			offset = Analysis::TypeInfo::GetSize(baseType->PointeeType(), GetTarget());
			offset *= C->Value();
		}

		// Now get rid of the 'ptop' instruction and make
		// the address adjustment explicit.
		auto ptoiOp = irGen_.GetTemporary(castType);
		irGen_.GetPtoi(base, castType, ptoiOp);

		// Add the offset to the address of the base.
		auto addOp = irGen_.GetTemporary(castType);
		auto sizeOp = irGen_.GetIntConst(castType, offset);
		irGen_.GetAdd(ptoiOp, sizeOp, addOp);
		return LOG(addOp);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFtoiItofPair(ConversionInstr* instr) {
	// ftoi (itof a, double), intX -> a, if the float type to which we convert
	// has enough precision to hold the entire range of 'a'.
	// The same is valid for 'ftoui' too.
	auto op = instr->TargetOp();
	auto castType = instr->CastType()->As<FloatingType>();
	Operand* a;
	const Type* itofType;

	if(MatchConversion<ItofInstr>(MatchAny(&a), nullptr, &itofType)(op) ||
	   MatchConversion<UitofInstr>(MatchAny(&a), nullptr, &itofType)(op)) {
		auto intType = a->GetType()->As<IntegerType>();
		auto floatType = itofType->As<FloatingType>();

		if(intType->SizeInBits() <= FA::FractionSizeInBits(floatType)) {
			// We can do the simplification.
			return LOG(a);
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandlePtop(PtopInstr* instr) {
	auto op = instr->TargetOp();

	// Try to fold it to a constant.
	if(auto result = folder_.FoldConversion(Instr_Ptop, op, instr->CastType(),
                                            instr->ParentBlock())) {
		return LOG(result);
	}

	// Check for the cast-cast case, it's pretty common.
	if(auto result = HandleCastCast(Instr_Ptop, op, instr->CastType())) {
		return result;
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleCastCast(Opcode opcode, Operand* op, 
                                  const Type* castType) {
	// Test for a cast of a cast. In some cases one of them can be eliminated.
	// If the target is not another cast we have nothing to do.
	ConversionInstr* innerCast = op->DefiningInstrAs<ConversionInstr>();
	if(innerCast == nullptr) {
        return nullptr;
    }

	switch(opcode) {
		case Instr_Trunc: {
			return HandleCastCastTrunc(innerCast, castType);
		}
		case Instr_Sext: {
			return HandleCastCastSext(innerCast, castType);
		}
		case Instr_Zext: {
			return HandleCastCastZext(innerCast, castType);
		}
		case Instr_Itop: {
			return HandleCastCastItop(innerCast, castType);
		}
		case Instr_Ptoi: {
			return HandleCastCastPtoi(innerCast, castType);
		}
		case Instr_Ptop: {
			if(innerCast->Is<PtopInstr>()) {
				// 'ptop (ptop a, X*), Y*' can always be converted to
				// 'ptop a, Y*'.
                auto temp = irGen_.GetTemporary(castType);
                irGen_.GetPtop(innerCast->TargetOp(), castType, temp);
				return LOG(temp);
			}
			break;
		}
		case Instr_Ftrunc: {
			if(innerCast->Is<FtruncInstr>()) {
				// 'ftrunc (ftrunc a, float), float' can always be converted to
				// 'ftrunc a, float'.
				return LOG(innerCast->ResultOp());
			}
			break;
		}
		case Instr_Fext: {
			if(innerCast->Is<FextInstr>()) {
				// 'fext (fext a, X), Y' can be converted to 'fext a, Y'.
				auto temp = irGen_.GetTemporary(castType);
				irGen_.GetFext(innerCast->TargetOp(), castType, temp);
				return LOG(temp);
			}
			break;
		}
		case Instr_Ftoi:
		case Instr_Ftoui: {
			if(innerCast->Is<FextInstr>()) {
				// 'ftoi (fext a, double), intX' can be converted to
				// 'ftoi a, intX', because the extension has no effect on precision.
				auto temp = irGen_.GetTemporary(castType);

				if(opcode == Instr_Ftoi) {
					irGen_.GetFtoi(innerCast->TargetOp(), castType, temp);
				}
				else irGen_.GetFtoui(innerCast->TargetOp(), castType, temp);

				return LOG(temp);
			}
			break;
		}
	}

	// No other cast combinations can be folded.
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleCastCastTrunc(ConversionInstr* innerCast, const Type* castType) {
    auto innerType = innerCast->CastType()->As<IntegerType>();
    auto outerType = castType->As<IntegerType>();

    if(innerCast->Is<TruncInstr>()) {
        // 'trunc (trunc a, intX), intY; can be transformed into
        // 'trunc a, intY', if 'Y < X', or
        // 'trunc a, intX', if 'X == Y'
        auto innerType = innerCast->CastType()->As<IntegerType>();
        auto outerType = castType->As<IntegerType>();

        if(outerType->RankBelow(innerType)) {
            auto temp = irGen_.GetTemporary(outerType);
            irGen_.GetTrunc(innerCast->TargetOp(), outerType, temp);
            return LOG(temp);
        }
        else if(outerType->SameRank(innerType)) {
            return LOG(innerCast->ResultOp());
        }
    }
    else if(innerCast->Is<SextInstr>() || innerCast->Is<ZextInstr>()) {
        // 'trunc (sext a, intX), intY' can be transformed to
        // 'sext a, intX' if 'X == Y' or
        // 'sext a, intY' if 'X < Y' or
        // 'trunc a, intY' if 'Y < X'.
        auto innerType = innerCast->CastType()->As<IntegerType>();
        auto outerType = castType->As<IntegerType>();

        if(innerType->SameRank(outerType)) {
            return LOG(innerCast->ResultOp());
        }
        else if(innerType->RankBelow(outerType)) {
            auto temp = irGen_.GetTemporary(outerType);
            if(innerCast->Is<SextInstr>()) {
                irGen_.GetSext(innerCast->TargetOp(), outerType, temp);
            }
            else irGen_.GetZext(innerCast->TargetOp(), outerType, temp);
            
            return LOG(temp);
        }
        else {
            auto temp = irGen_.GetTemporary(outerType);
            irGen_.GetTrunc(innerCast->TargetOp(), outerType, temp);
            return LOG(temp);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleCastCastSext(ConversionInstr* innerCast, const Type* castType) {
    auto innerType = innerCast->CastType()->As<IntegerType>();
    auto outerType = castType->As<IntegerType>();

    if(innerCast->Is<SextInstr>()) {
        // 'sext (sext a, intX), intY' can be transformed to
        // 'sext a, intX' if 'X == Y' or
        // 'sext a, intY' if 'X < Y'
        if(innerType->SameRank(outerType)) {
            return LOG(innerCast->ResultOp());
        }
        else if(innerType->RankBelow(outerType)) {
            auto temp = irGen_.GetTemporary(outerType);
            irGen_.GetSext(innerCast->TargetOp(), outerType, temp);
            return LOG(temp);
        }
    }
    else if(innerCast->Is<ZextInstr>()) {
        // 'sext (zext a, intX), intY' can be transformed to
        // 'zext a, intX' if 'X == Y' or
        // 'zext a, intY', if 'X < Y'
        auto temp = irGen_.GetTemporary(outerType);
        irGen_.GetZext(innerCast->TargetOp(), outerType, temp);
        return LOG(temp);
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleCastCastZext(ConversionInstr* innerCast, const Type* castType) {
    auto innerType = innerCast->CastType()->As<IntegerType>();
    auto outerType = castType->As<IntegerType>();

    if(innerCast->Is<ZextInstr>()) {
        // 'zext (zext a, intX), intY' can be transformed to
        // 'zext a, intX', if 'X == Y' or
        // 'zext a, intY' if 'X < Y'.
        if(innerType->SameRank(outerType)) {
            return LOG(innerCast->ResultOp());
        }
        else if(innerType->RankBelow(outerType)) {
            auto temp = irGen_.GetTemporary(outerType);
            irGen_.GetZext(innerCast->TargetOp(), outerType, temp);
            return LOG(temp);
        }
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleCastCastItop(ConversionInstr* innerCast, const Type* castType) {
    if(innerCast->Is<PtoiInstr>()) {
        // 'itop (ptoi a, intX), Y*' can be both eliminated if
        // 'intX' has the size (in bytes) of the pointer type.
        // This guarantees that no information is lost.
        auto innerType = innerCast->CastType()->As<IntegerType>();
        int ptrSize = GetTarget()->GetPointerSize();
        int intSize = innerType->Size();

        if(ptrSize == intSize) {
            return LOG(innerCast->TargetOp());
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleCastCastPtoi(ConversionInstr* innerCast, const Type* castType) {
    if(innerCast->Is<ItopInstr>()) {
        // 'ptoi (itop a, X*), intY' can be be both eliminated 
        // if 'intX' has the size (in bytes) of the pointer type.
        auto outerType = castType->As<IntegerType>();
        int ptrSize = GetTarget()->GetPointerSize();
        int intSize = outerType->Size();

        if(ptrSize == intSize) {
            return LOG(innerCast->TargetOp());
        }
    }
    else if(innerCast->Is<PtopInstr>()) {
        // 'ptoi (ptop a, X*), intY' can be transformed to
        // 'ptoi a, intY' all the time.
        auto temp = irGen_.GetTemporary(castType);
        irGen_.GetPtoi(innerCast->TargetOp(), castType, temp);
        return LOG(temp);
    }

    return nullptr;
}

} // namespace Optimization