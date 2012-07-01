// OperandInfo.cpp
// Copyright (c) Lup Gratian
//
// Implements the OperandInfo class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "OperandInfo.hpp"
#include "ConstantFolder.hpp"

namespace Analysis {

int OperandInfo::GetVariableSize(const Operand* op, const TargetInfo* target) {
	// Return the size of the referenced variable, if it's the case.
	if(auto variableRef = op->As<VariableReference>()) {
        const Variable* variable = variableRef->IsLocalVariableRef() ? 
                                   variableRef->GetVariable() : variableRef->GetGlobal();
        return Analysis::TypeInfo::GetSize(variable->GetType(), target);
	}
	
	return 0;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int OperandInfo::GetVariableAlignment(const Operand* op, const TargetInfo* target) {
	// Check for a reference to a variable. If the variable has an
	// associated alignment, use it. Else use the alignment of the type.
	if(auto variableRef = op->As<VariableReference>()) {
        const Variable* variable = variableRef->IsLocalVariableRef() ? 
                                   variableRef->GetVariable() : variableRef->GetGlobal();
		if(variable->HasAlignment()) {
            return variable->Alignment();
        }
		
        return Analysis::TypeInfo::GetAlignment(variable->GetType(), target);
	}
	
	return 0;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int OperandInfo::GetStringLength(const Operand* op) {
	DebugValidator::IsNotNull(op);

	// Make sure we have a reference to a global variable.
	auto variableRef = op->As<VariableReference>();
	if((variableRef == nullptr) ||
	   (variableRef->IsGlobalVariableRef() == false)) {
        return -1;
    }

	// Make sure the variable is a constant with a final initializer.
	auto globalVar = variableRef->GetGlobal();

	if((globalVar->IsConstant() == false)     ||
       (globalVar->HasInitializer() == false) ||
	    globalVar->IsTentative()              ||
        globalVar->HasInitializerList()) {
       return -1;
    }

	// A 'StringConstant' object should be used as an initializer.
    auto stringConst = globalVar->GetInitializer()->Value()->As<StringConstant>();
    if(stringConst == nullptr) {
        return -1;
    }

    auto arrayType = stringConst->GetType()->As<ArrayType>();
    DebugValidator::IsNotNull(arrayType);

    // Scan until we find the 0 terminator or we reach the end of the array.
    for(int i = 0; i < arrayType->Size(); i++) {
        if(stringConst->Value()[i] == 0) {
            // Found the 0 terminator.
            return i + 1;
        }
    }

    // No null terminator was found, use the size of the array.
    return arrayType->Size();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
__int64 OperandInfo::GetMaximumValue(Operand* op, bool isSigned, 
                                     Block* testBlock) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsTrue(op->IsInteger());

    // If we have a constant we're done.
    if(auto intConst = op->As<IntConstant>()) {
        return intConst->Value();
    }
    else if(op->IsUndefinedConstant()) {
        return 0;
    }

    // Special case for 'min'/'max'/'abs'.
    if(auto callInstr = op->DefiningInstrAs<CallInstr>()) {
        if(auto intrinsic = callInstr->GetIntrinsicAs<MathIntrinsic>()) {
            if(intrinsic->IsAbs()) {
                __int64 max = GetMaximumValue(callInstr->GetArgument(0), 
                                              isSigned, testBlock);
                // If the number is signed and the sign bit is set
                // we know that after 'abs' it can't be set anymore.
                if(isSigned) {
                    auto intType = op->GetType()->As<IntegerType>();

                    if(IA::GetSignBit(max, intType->GetSubtype())) {
                        max &= ~IA::GetSignBitMask(intType->GetSubtype());
                    }
                }

                return max;
            }
            else if(intrinsic->IsMax()) {
                __int64 max1 = GetMaximumValue(callInstr->GetArgument(0), 
                                               isSigned, testBlock);
                __int64 max2 = GetMaximumValue(callInstr->GetArgument(1), 
                                               isSigned, testBlock);
                if(isSigned) {
                    return std::max(max1, max2);
                }
                else return std::max((unsigned __int64)max1, (unsigned __int64)max2);
            }
            else if(intrinsic->IsMin()) {
                __int64 max1 = GetMaximumValue(callInstr->GetArgument(0), 
                                           isSigned, testBlock);
                __int64 max2 = GetMaximumValue(callInstr->GetArgument(1), 
                                               isSigned, testBlock);
                if(isSigned) {
                    return std::min(max1, max2);
                }
                else return std::min((unsigned __int64)max1, (unsigned __int64)max2);
            }
        }
    }

    // Try to use value-range information first.
    // 'a = (-INF, 4]' -> max = 4
    if(auto rangeTag = GetRangeTag(op, testBlock)) {
        Range range;

        if(rangeTag->GetRange(op, range)) {
            // If we have a range with a high bound that is not symbolic 
            // and not plus-infinity then we know the maximum value.
            if(range.IsRange &&
               (range.High.HasBaseOperand() == false) &&
               (range.High.IsPlusInfinity(op, isSigned) == false)) {
                return range.High.Constant;
            }
        }
    }

    // The maximum value can be approximated using the bits
    // that are known to be definitely one and zero. 
    // The unknown bits are considered to be set to one, with the exception
    // of the sign bit for signed numbers (else we would get a negative number).
    if(auto intConst = op->As<IntConstant>()) {
        return intConst->Value();
    }
    
    unsigned __int64 oneBits;
    unsigned __int64 zeroBits;
    EstimateOneBits(op, oneBits);
    EstimateZeroBits(op, zeroBits);
    auto intType = op->GetType()->As<IntegerType>();

    __int64 result = (oneBits | ~zeroBits) & IA::GetMinusOneMask(intType);

    if(isSigned) {
        auto mask = IA::GetSignBitMask(intType);
        
        if((oneBits & mask) == 0) {
            // We can reset the sign bit.
            result &= ~IA::GetSignBitMask(intType);
        }
    }
    return IA::LimitToType(result, intType, isSigned);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
__int64 OperandInfo::GetMinimumValue(Operand* op, bool isSigned, Block* testBlock) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsTrue(op->IsInteger());

    // If we have a constant we're done.
    if(auto intConst = op->As<IntConstant>()) {
        return intConst->Value();
    }
    else if(op->IsUndefinedConstant()) {
        return 0;
    }

    // Special case for 'min'/'max'/'abs'.
    if(auto callInstr = op->DefiningInstrAs<CallInstr>()) {
        if(auto intrinsic = callInstr->GetIntrinsicAs<MathIntrinsic>()) {
            if(intrinsic->IsAbs()) {
                __int64 max = GetMinimumValue(callInstr->GetArgument(0), 
                                              isSigned, testBlock);
                // If the number is signed and the sign bit is set
                // we know that after 'abs' it can't be set anymore.
                if(isSigned) {
                    auto intType = op->GetType()->As<IntegerType>();

                    if(IA::GetSignBit(max, intType->GetSubtype())) {
                        max &= ~IA::GetSignBitMask(intType->GetSubtype());
                    }
                }

                return max;
            }
            else if(intrinsic->IsMax()) {
                __int64 max1 = GetMinimumValue(callInstr->GetArgument(0), 
                                               isSigned, testBlock);
                __int64 max2 = GetMinimumValue(callInstr->GetArgument(1), 
                                               isSigned, testBlock);
                if(isSigned) {
                    return std::max(max1, max2);
                }
                else return std::max((unsigned __int64)max1, (unsigned __int64)max2);
            }
            else if(intrinsic->IsMin()) {
                __int64 max1 = GetMinimumValue(callInstr->GetArgument(0), 
                                           isSigned, testBlock);
                __int64 max2 = GetMinimumValue(callInstr->GetArgument(1), 
                                               isSigned, testBlock);
                if(isSigned) {
                    return std::min(max1, max2);
                }
                else return std::min((unsigned __int64)max1, (unsigned __int64)max2);
            }
        }
    }

    // Try to use value-range information first.
    // 'a = [2, +INF)' -> min = 2
    if(auto rangeTag = GetRangeTag(op, testBlock)) {
        Range range;

        if(rangeTag->GetRange(op, range)) {
            // If we have a range with a low bound that is not symbolic 
            // and not minus-infinity then we know the minimum value.
            if(range.IsRange &&
               (range.Low.HasBaseOperand() == false) &&
               (range.Low.IsMinusInfinity(op, isSigned) == false)) {
                return range.Low.Constant;
            }
        }
    }

    // The minimum value can be approximated using the bits
    // that are known to be definitely one and zero. 
    // The unknown bits are considered to be set to one if we have
    // a sign number, zero for unsigned.
    if(auto intConst = op->As<IntConstant>()) {
        return intConst->Value();
    }
    
    unsigned __int64 oneBits;
    unsigned __int64 zeroBits;
    EstimateOneBits(op, oneBits);
    EstimateZeroBits(op, zeroBits);
    auto intType = op->GetType()->As<IntegerType>();
    __int64 result = oneBits;

    if(isSigned) {
        // Any bit that is not zero is presumed to be zero,
        // except from the highest bit, which controls the sign.
        __int64 minusMask = IA::GetSignBitMask(intType);

        if((zeroBits & minusMask) == 0) {
            // Make it negative.
            result = IA::SignExtend(minusMask, intType->GetSubtype());
        }

        result |= oneBits;
    }

    // If we have a parameter that is unsigned we know that
    // the minimum value is 0 (might be better than what we have).
    if(auto parameter = op->As<Parameter>()) {
        if(parameter->GetVariable()->IsUnsigned()) {
            result = result < 0 ? 0 : result;
        }
    }

    return IA::LimitToType(result, intType, isSigned);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::IsPositive(Operand* op, Block* testBlock, int level) {
    DebugValidator::IsNotNull(op);
    
    // If the operand is floating we fail fast, the other methods
    // support only integer operands.
    if(op->IsFloating()) {
        if(auto floatConst = op->As<FloatConstant>()) {
            return floatConst->Value() >= 0;
        }
        else return false;
    }
    else if(op->IsUndefinedConstant()) {
        // We assume that 'undef' is a suitable value.
        return true;
    }
    else if(auto parameter = op->As<Parameter>()) {
        if(parameter->GetVariable()->IsUnsigned()) {
            return true;
        }
    }

    // If we have a constant we're done.
    if(auto intConst = op->As<IntConstant>()) {
        return intConst->Value() >= 0;
    }

    // Try to use value-range information first.
    // 'a = [2, +INF)' -> true
    if(auto rangeTag = GetRangeTag(op, testBlock)) {
        if(rangeTag->IsPositive(op) == Result_Yes) {
            return true;
        }
    }

    // Don't recourse too much, we only waste time.
    if(level == 0) {
        return false;
    }

    // If the sign bit is known to be 0 then the number is positive.
    unsigned __int64 zeroBits;
    EstimateZeroBits(op, zeroBits);
    __int64 mask = IA::GetSignBitMask(op->GetType()->As<IntegerType>());
    
    if(zeroBits & mask) {
        return true;
    }
    else if(auto phiInstr = op->DefiningInstrAs<PhiInstr>()) {
        if(phiInstr->OperandCount() > 8) {
            // Give up if the 'phi' has many incoming operands, 
            // because the chances of knowing that all of them
            // are not zero are extremely small.
            return false;
        }

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            if(IsPositive(phiInstr->GetSourceOp(i), 
                          testBlock, level - 1) == false) {
                // No reason to continue.
                return false;
            }
        }

        return true;
    }
    else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
        // The result is positive if both operands are positive.
        return IsPositive(questInstr->TrueOp(), testBlock, level - 1) &&
               IsPositive(questInstr->FalseOp(), testBlock, level - 1);
    }
    else if(auto orInstr = op->DefiningInstrAs<OrInstr>()) {
        return IsPositive(orInstr->LeftOp(), testBlock, level - 1) &&
               IsPositive(orInstr->RightOp(), testBlock, level - 1);
    }
    else if(auto callInstr = op->DefiningInstrAs<CallInstr>()) {
        // If we have a call to 'min' or 'max' we know
        // that the result is positive if both variants are positive.
        // 'abs' always returns a positive number.
        if(auto intrinsic = callInstr->GetIntrinsicAs<MathIntrinsic>()) {
            if(intrinsic->IsMax() || intrinsic->IsMin()) {
                return IsPositive(callInstr->GetArgument(0), testBlock, level - 1) &&
                       IsPositive(callInstr->GetArgument(1), testBlock, level - 1);
            }
            else if(intrinsic->IsAbs()) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::IsNegative(Operand* op, Block* testBlock, int level) {
    DebugValidator::IsNotNull(op);
    
    // If the operand is floating we fail fast, the other methods
    // support only integer operands.
    if(op->IsFloating()) {
        if(auto floatConst = op->As<FloatConstant>()) {
            return floatConst->Value() < 0;
        }
        else return false;
    }
    else if(op->IsUndefinedConstant()) {
        // We assume that 'undef' is a suitable value.
        return true;
    }

    // If we have a constant we're done.
    if(auto intConst = op->As<IntConstant>()) {
        return intConst->Value() < 0;
    }

    // Try to use value-range information first.
    // 'a = (-INF, -3]' -> true
    if(auto rangeTag = GetRangeTag(op, testBlock)) {
        if(rangeTag->IsNegative(op) == Result_Yes) {
            return true;
        }
    }

    // Don't recourse too much, we only waste time.
    if(level == 0) {
        return false;
    }

    // If the sign bit is known to be 1 then the number is negative.
    unsigned __int64 oneBits;
    EstimateOneBits(op, oneBits);
    __int64 mask = IA::GetSignBitMask(op->GetType()->As<IntegerType>());
    
    if(oneBits & mask) {
        return true;
    }
    else if(auto phiInstr = op->DefiningInstrAs<PhiInstr>()) {
        if(phiInstr->OperandCount() > 8) {
            // Give up if the 'phi' has many incoming operands, 
            // because the chances of knowing that all of them
            // are not zero are extremely small.
            return false;
        }

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            if(IsNegative(phiInstr->GetSourceOp(i),
                          testBlock, level - 1) == false) {
                // No reason to continue.
                return false;
            }
        }

        return true;
    }
    else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
        // The result is negative if both operands are negative.
        return IsNegative(questInstr->TrueOp(), testBlock, level - 1) &&
               IsNegative(questInstr->FalseOp(), testBlock, level - 1);
    }
    else if(auto orInstr = op->DefiningInstrAs<OrInstr>()) {
        return IsNegative(orInstr->LeftOp(), testBlock, level - 1) &&
               IsNegative(orInstr->RightOp(), testBlock, level - 1);
    }
    else if(auto callInstr = op->DefiningInstrAs<CallInstr>()) {
        // If we have a call to 'min' or 'max' we know
        // that the result is negative if both variants are negative.
        if(auto intrinsic = callInstr->GetIntrinsicAs<MathIntrinsic>()) {
            if(intrinsic->IsMax() || intrinsic->IsMin()) {
                return IsNegative(callInstr->GetArgument(0), testBlock, level - 1) &&
                       IsNegative(callInstr->GetArgument(1), testBlock, level - 1);
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::IsZero(Operand* op, Block* testBlock, int level) {
    DebugValidator::IsNotNull(op);

    // If the operand is floating we fail fast, the other methods
    // support only integer operands.
    if(op->IsFloating()) {
        return false;
    }
    else if(op->IsUndefinedConstant()) {
        // We assume that 'undef' is a suitable value.
        return true;
    }

    // If we have a constant we're done.
    if(auto intConst = op->As<IntConstant>()) {
        return intConst->Value() == 0;
    }
    else if(op->IsNullConstant()) {
        return true;
    }

    // Try to use value-range information first.
    // 'a = [0, 0]' -> true
    if(auto rangeTag = GetRangeTag(op, testBlock)) {
        if(rangeTag->IsZero(op) == Result_Yes) {
            return true;
        }
    }

    // Don't recourse too much, we only waste time.
    if(level == 0) return false;

    // Check if all bits are known to be zero.
    unsigned __int64 zeroBits;
    IRIntegerKind intKind;
    EstimateZeroBits(op, zeroBits);

    if(auto intType = op->GetType()->As<IntegerType>()) {
        intKind = intType->GetSubtype();
    }
    else if(target_) {
        intKind = target_->GetPointerType();
    }
    else return false;
    
    // The operand is zero if all bits are known to be zero.
    __int64 mask = IA::GetMinusOneMask(intKind);

    if((zeroBits & mask) == mask) {
        return true;
    }
    else if(auto phiInstr = op->DefiningInstrAs<PhiInstr>()) {
        if(phiInstr->OperandCount() > MAXIMUM_PHI_INCOMING) {
            // Give up if the 'phi' has many incoming operands, 
            // because the chances of knowing that all of them
            // are not zero are extremely small.
            return false;
        }

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            if(IsZero(phiInstr->GetSourceOp(i), 
                      testBlock, level - 1) == false) {
                // No reason to continue.
                return false;
            }
        }

        return true;
    }
    else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
        // The result is zero if both operands are zero.
        return IsZero(questInstr->TrueOp(), testBlock, level - 1) &&
               IsZero(questInstr->FalseOp(), testBlock, level - 1);
    }
    else if(auto orInstr = op->DefiningInstrAs<OrInstr>()) {
        return IsZero(orInstr->LeftOp(), testBlock, level - 1) &&
               IsZero(orInstr->RightOp(), testBlock, level - 1);
    }
    if(auto callInstr = op->DefiningInstrAs<CallInstr>()) {
        // If we have a call to 'abs' we know that the result
        // is zero if the argument is zero.
        if(auto intrinsic = callInstr->GetIntrinsicAs<MathIntrinsic>()) {
            if(intrinsic->IsAbs()) {
                return IsZero(callInstr->GetArgument(0), testBlock, level - 1);
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::IsNotZero(Operand* op, Block* testBlock, int level) {
    DebugValidator::IsNotNull(op);

    // If the operand is floating we fail fast, the other methods
    // support only integer operands.
    if(op->IsFloating()) {
        return false;
    }
    else if(op->IsUndefinedConstant()) {
        // We assume that 'undef' is a suitable value.
        return true;
    }

    // If we have a constant we're done.
    if(auto intConst = op->As<IntConstant>()) {
        return intConst->Value() != 0;
    }
    else if(op->IsVariableReference() ||
            op->IsFunctionReference()) {
        return true;
    }
    else if(op->IsNullConstant()) {
        return false;
    }

    // Try to use value-range information first.
    // 'a = (-INF -1] U [1, +INF)' -> true
    if(auto rangeTag = GetRangeTag(op, testBlock)) {
        if(rangeTag->IsNotZero(op) == Result_Yes) {
            return true;
        }
    }

    // Don't recourse too much, we only waste time.
    if(level == 0) {
        return false;
    }

    // The operand is not zero if at least one bit is known to be one.
    unsigned __int64 oneBits;
    EstimateOneBits(op, oneBits);
    
    if(oneBits) {
        // At least one bit is always set.
        return true;
    }
    else if(auto phiInstr = op->DefiningInstrAs<PhiInstr>()) {
        if(phiInstr->OperandCount() > MAXIMUM_PHI_INCOMING) {
            // Give up if the 'phi' has many incoming operands, 
            // because the chance of knowing that all of them
            // are not zero are extremely small.
            return false;
        }

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            if(IsNotZero(phiInstr->GetSourceOp(i), 
                         testBlock, level - 1) == false) {
                // No reason to continue.
                return false;
            }
        }

        return true;
    }
    else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
        // The result is not zero if both operands are not zero.
        return IsNotZero(questInstr->TrueOp(), testBlock, level - 1) &&
               IsNotZero(questInstr->FalseOp(), testBlock, level - 1);
    }
    else if(auto addInstr = op->DefiningInstrAs<AddInstr>()) {
        // If we have an 'add' instruction and the added operands
        // are positive, the result can't be zero unless
        // both operands are definitely zero.
        if(IsPositive(addInstr->LeftOp(), testBlock, level - 1) && 
           IsPositive(addInstr->RightOp(), testBlock, level - 1)) {
            if(IsNotZero(addInstr->LeftOp(), testBlock, level - 1) ||
               IsNotZero(addInstr->RightOp(), testBlock, level - 1)) {
               return true;
            }
        }

        // If one of the operands is a constant that is a power of two,
        // and the other one is definitely positive, the result is not zero.
        if(auto intConst = addInstr->RightOp()->As<IntConstant>()) {
            if(IA::IsPowerOfTwo(intConst) &&
               IsPositive(addInstr->LeftOp(), testBlock, level - 1)) {
               return true;
            }
        }
        
        if(auto intConst = addInstr->LeftOp()->As<IntConstant>()) {
            if(IA::IsPowerOfTwo(intConst) &&
               IsPositive(addInstr->RightOp(), testBlock, level - 1)) {
               return true;
            }
        }
    }
    else if(auto orInstr = op->DefiningInstrAs<OrInstr>()) {
        return IsNotZero(orInstr->LeftOp(), testBlock, level - 1) &&
               IsNotZero(orInstr->RightOp(), testBlock, level - 1);
    }
    else if(auto callInstr = op->DefiningInstrAs<CallInstr>()) {
        // If we have a call to 'min' or 'max' we know
        // that the result is not zero if both variants are not zero.
        // The same applies for 'abs'.
        if(auto intrinsic = callInstr->GetIntrinsicAs<MathIntrinsic>()) {
            if(intrinsic->IsMax() || intrinsic->IsMin()) {
                return IsNotZero(callInstr->GetArgument(0), testBlock, level - 1) &&
                       IsNotZero(callInstr->GetArgument(1), testBlock, level - 1);
            }
            else if(intrinsic->IsAbs()) {
                return IsNotZero(callInstr->GetArgument(0), testBlock, level - 1);
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::CanAddOverflow(Operand* opA, Operand* opB, 
                                 bool hasUndefinedOverflow) {
    DebugValidator::IsNotNull(opA);
    DebugValidator::IsNotNull(opB);
    DebugValidator::IsTrue(opA->IsInteger());
    DebugValidator::IsTrue(opB->IsInteger());

    // If overflow for signed numbers is undefined then we're allowed
    // to presume that it can never happen.
    if(hasUndefinedOverflow) {
        return false;
    }

    // By looking at the sign bits (actually, the number of times the sign bit
    // is definitely found in the left part of the operand) we know
    // if it's possible or not to have overflow. If the sign bit is found
    // more than once in both operands then we're guaranteed that the sign bit
    // won't be affected (any change stops before the sign bit).
    // 00101110 +
    // 00100001 =
    // 01001111 -> sign bit not changed (last bit changed = 6)
    for(int i = 0; i < 2; i++) {
        auto op = i == 0 ? opA : opB;
        int bits = 1;

        // For some instructions we know exactly how they affect the sign bit.
        if(auto shrInstr = op->DefiningInstrAs<ShrInstr>()) {
            if(auto intConst = shrInstr->RightOp()->As<IntConstant>()) {
                bits += intConst->Value();
            }
        }
        else if(auto sextInstr = op->DefiningInstrAs<SextInstr>()) {
            auto fromType = sextInstr->TargetOp()->GetType()->As<IntegerType>();
            auto toType = sextInstr->CastType()->As<IntegerType>();
            bits += toType->SizeInBits() - fromType->SizeInBits();
        }

        // Stop if we know enough.
        if(bits > 1) continue;

        // Try to use an estimate of the bits that are definitely set.
        auto intType = op->GetType()->As<IntegerType>();
        __int64 mask = IA::GetSignBitMask(intType);
        bool handled = false;

        // Test the case of a zero sign bit.
        unsigned __int64 zeroBits;
        EstimateZeroBits(op, zeroBits);

        if(zeroBits & mask) {
            // The number of ones in the left part indicates
            // how many times the sign bit is replicated.
            for(int j = intType->SizeInBits() - 2; j >= 0; j--) {
                if(zeroBits & (1ULL << j)) bits++;
                else break;
            }

            handled = true;
        }

        // Test the case of an one sign bit.
        if(handled == false) {
            unsigned __int64 oneBits;
            EstimateOneBits(op, oneBits);

            if(oneBits & mask) {
                // The number of ones in the left part indicates
                // how many times the sign bit is replicated.
                for(int j = intType->SizeInBits() - 2; j >= 0; j--) {
                    if(oneBits & (1ULL << j)) bits++;
                    else break;
                }
            }
        }

        if(bits == 1) return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::IsBitZero(Operand* op, int index, Block* testBlock) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsTrue(op->IsInteger());
    DebugValidator::IsLargerOrEqual(index, 0);

    // Try to use value-range information first.
    // If the index is greater than the highest set bit
    // of the maximum value then we know it is zero.
    // Note that this works only if the value is definitely positive.
    if(auto rangeTag = GetRangeTag(op, testBlock)) {
        Range range;

        if(rangeTag->GetRange(op, range)) {
            // We should have a range that is not symbolic
            // and with a high bound that is not plus-infinity.
            if((range.HasBaseOperand() == false) &&
               (range.Low.Constant >= 0) &&
               (range.High.Constant >= 0) &&
               (range.High.IsPlusInfinity(op) == false)) {
                int highestBit = IA::Log2(range.High.Constant);

                // Test if the bit is definitely zero.
                if(index > highestBit) {
                    return true;
                }
            }
        }
    }

    // Use an estimate of one/zero bits.
    unsigned __int64 zeroBits;
    EstimateZeroBits(op, zeroBits);
    return zeroBits & (1ULL << index);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::IsBitOne(Operand* op, int index, Block* testBlock) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsTrue(op->IsInteger());
    DebugValidator::IsLargerOrEqual(index, 0);

    // Try to use value-range information first.
    // If the index is greater than the highest set bit
    // of the minimum value then we know it is one.
    // Note that this works only if the value is definitely negative.
    if(auto rangeTag = GetRangeTag(op, testBlock)) {
        Range range;

        if(rangeTag->GetRange(op, range)) {
            // We should have a range that is not symbolic
            // and with a high bound that is not minus-infinity.
            if((range.HasBaseOperand() == false) &&
               (range.Low.Constant < 0) &&
               (range.High.Constant < 0) &&
               (range.Low.IsMinusInfinity(op) == false)) {
                int highestBit = IA::Log2(~range.Low.Constant);

                // Test if the bit is definitely one.
                if(index > highestBit) {
                    return true;
                }
            }
        }
    }

    // Use an estimate of one/zero bits.
    unsigned __int64 oneBits;
    EstimateOneBits(op, oneBits);
    return oneBits & (1ULL << index);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::AreBitsZero(Operand* op, int firstIndex, int lastIndex, 
                              Block* testBlock) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsTrue(op->IsInteger());
    DebugValidator::IsLargerOrEqual(firstIndex, 0);
    DebugValidator::IsLargerOrEqual(lastIndex, firstIndex);

    auto intType = op->GetType()->As<IntegerType>();
    __int64 mask = ((1ULL << (lastIndex - firstIndex)) - 1) << firstIndex;
    
    unsigned __int64 zeroBits;
    EstimateZeroBits(op, zeroBits);
    return (zeroBits & mask) == mask;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::AreBitsOne(Operand* op, int firstIndex, int lastIndex, 
                             Block* testBlock) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsTrue(op->IsInteger());
    DebugValidator::IsLargerOrEqual(firstIndex, 0);
    DebugValidator::IsLargerOrEqual(lastIndex, firstIndex);

    auto intType = op->GetType()->As<IntegerType>();
    __int64 mask = ((1ULL << (lastIndex - firstIndex)) - 1) << firstIndex;

    unsigned __int64 oneBits;
    EstimateOneBits(op, oneBits);
    return (oneBits & mask) == mask;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IntConstant* OperandInfo::GetIntConstant(Operand* op, Block* testBlock) {
    DebugValidator::IsNotNull(op);

    if((op->IsInteger() == false) || op->IsUndefinedConstant()) {
        // It definitely can't be an integer constant.
        return nullptr;
    }

    // If this is an integer constant we are done.
    if(auto intConst = op->As<IntConstant>()) {
        return intConst;
    }

    // Use value-range information if possible.
    // For example, 'a = [4, 4]' -> 4.
    if(auto rangeTag = GetRangeTag(op, testBlock)) {
        if(auto intConst = rangeTag->GetAsConstant(op)) {
            return intConst;
        }
    }

    // Check if the operand is definitely zero
    // using estimated one/zero bit information.
    if(unit_ && IsZero(op)) {
        return unit_->Constants().GetInt(op->GetType(), 0);
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int OperandInfo::GetProbableIntConstant(Operand* op, ValueList& valueList,
                                        Block* testBlock) {
    DebugValidator::IsNotNull(op);

    if((op->IsInteger() == false) || op->IsUndefinedConstant()) {
        // It definitely can't be an integer constant.
        return 0;
    }

    // First check if the operand is statically known to be constant.
    if(auto intConst = GetIntConstant(op, testBlock)) {
        valueList.Add(ValueProbability(intConst, Probability_Always));
        return 1;
    }

    // Estimate the most probable values the operand could have.
    // If available edge and value profile information is used.
    IRGenerator irGen(unit_);
    ConstantFolder folder(&irGen, target_);
    SparseBitVector visitedBlocks;
    return ComputeProbableValues(op, valueList, testBlock,
                                 visitedBlocks, folder, 0);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int OperandInfo::ComputeProbableValues(Operand* op, ValueList& valueList,
                                       Block* testBlock, SparseBitVector& visited, 
                                       ConstantFolder& folder, int level) {
    // Don't process to many instructions, it's unlikely that
    // we can determine any probable values.
    if(level == MAXIMUM_PROBABLE_LEVEL) {
        return 0;
    }

    // Test for the most simple case first - statically-known constant.
    if(auto intConst = GetIntConstant(op, testBlock)) {
        valueList.Add(ValueProbability(intConst, Probability_Always));
        return 1;
    }

    // We try to estimate the probable values of the instructions
    // by applying the operator on the most probable values
    // of the source operands.
    int count = 0;

    if(auto arithInstr = op->DefiningInstrAs<ArithmeticInstr>()) {
        count = ComputeProbableValuesArithLogical(arithInstr, valueList, 
                                                  testBlock, visited, folder, level);
    }
    else if(auto logicalInstr = op->DefiningInstrAs<LogicalInstr>()) {
        count = ComputeProbableValuesArithLogical(logicalInstr, valueList, 
                                                  testBlock, visited, folder, level);
    }
    else if(auto cmpInstr = op->DefiningInstrAs<CmpInstrBase>()) {
        count = ComputeProbableValuesCompare(cmpInstr, valueList, 
                                             testBlock, visited, folder, level);
    }
    else if(auto phiInstr = op->DefiningInstrAs<PhiInstr>()) {
        count = ComputeProbableValuesPhi(phiInstr, valueList, testBlock, 
                                         visited, folder, level);
    }
    else if(auto loadInstr = op->DefiningInstrAs<LoadInstr>()) {
        count = ComputeProbableValuesLoad(loadInstr, valueList, testBlock,
                                          visited, folder, level);
    }
    else if(auto convInstr = op->DefiningInstrAs<ConversionInstr>()) {
        count = ComputeProbableValuesConversion(convInstr, valueList, 
                                                testBlock, visited, folder, level);
    }
    else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
        count = ComputeProbableValuesQuestion(questInstr, valueList, 
                                              testBlock, visited, folder, level);
    }
    else if(auto parameter = op->As<Parameter>()) {
        //! TODO: HANDLE PARAMETERS
    }
    
    // If no probable value could be computed try to use value range information.
    if(count > 0) return count;
    else return ComputeProbableValuesRange(op, valueList, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int OperandInfo::ComputeProbableValuesRange(Operand* op, ValueList& valueList, 
                                            Block* testBlock) {
    // The probable values might be computed using value range information.
    // For example, if we know that the operand in in the range [2, 5]
    // we add 4 values to the list (2,3,4,5), each with a 25% probability.
    Range range;
    if(GetRange(op, testBlock, range) == false) {
        return 0;
    }

    // The range should not be symbolic and the limits
    // should not be the minimum/maximum value.
    if((range.HasBaseOperand() == false) && 
        (range.Low.IsMinusInfinity(op, false) == false) &&
        (range.High.IsPlusInfinity(op, false) == false)) {
        __int64 limit = MAXIMUM_PROBABLE_VALUES * 2;
        __int64 inRange = (range.High.Constant - range.Low.Constant + 1);
            
        if(inRange <= limit) {
            __int64 startValue = range.Low.Constant;
            float percent = 1.0 / (float)inRange;

            // Add all the values in the range, each with a probability
            // equal to 1.0 / value_count;
            for(int i = 0; i < inRange; i++) {
                auto constant = unit_->Constants().GetInt(op->GetType(),
                                                          startValue + i);
                ValueProbability result(constant, Probability_Percent, percent);
                valueList.Add(result);
            }

            return valueList.Count();
        }
    }

    return 0;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void OperandInfo::ComputeProbability(ValueProbability& a, ValueProbability& b, 
                                     ValueProbability& result) {
    // If both values always hold, then the result also always holds.
    // Otherwise we compute the resulting probability as follows:
    // P(result) = P(a) * P(b).
    if((a.Type == Probability_Always) &&
       (b.Type == Probability_Always)) {
        result.Type = Probability_Always;
    }
    else {
        float probabilityA = a.Type == Probability_Always ?
                             1.0 : a.Probability;
        float probabilityB = b.Type == Probability_Always ?
                             1.0 : b.Probability;
        result.Type = Probability_Percent;
        result.Probability = probabilityA * probabilityB;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int OperandInfo::ComputeProbableValuesArithLogical(Instruction* instr, ValueList& valueList,
                                                   Block* testBlock, SparseBitVector& visited, 
                                                   ConstantFolder& folder, int level) {
    // In order to compute the probable values of the instructions we need
    // first to obtain the probable values for the source operands.
    // If any of the operands has no such value we give up.
    ValueList leftList;
    int leftValues = ComputeProbableValues(instr->GetSourceOp(0), leftList, 
                                           testBlock, visited, folder, level + 1);
    if(leftValues == 0) {
        return 0;
    }

    ValueList rightList;
    int rightValues= ComputeProbableValues(instr->GetSourceOp(1), rightList, 
                                           testBlock, visited, folder, level + 1);
    if(rightValues == 0) {
        return 0;
    }

    // Because we do a cartesian product of the source operand values,
    // their number is limited before applying the operator to the combinations.
    leftValues = std::min(leftValues, MAXIMUM_PROBABLE_VALUES);
    rightValues = std::min(rightValues, MAXIMUM_PROBABLE_VALUES);

    // The probabilities of the results are computed as follows:
    // P(result) = P(operand1) * P(operand2)
    for(int i = 0; i < leftValues; i++) {
        for(int j = 0; j < rightValues; j++) {
            DebugValidator::IsTrue(leftList[i].Value->IsConstant());
            DebugValidator::IsTrue(rightList[j].Value->IsConstant());

            // Constant-fold using the required operator.
            auto value = folder.FoldBinary(instr->GetOpcode(), leftList[i].Value,
                                           rightList[j].Value, testBlock);
            ValueProbability result(value->As<Constant>());
            ComputeProbability(leftList[i], rightList[j], result);
            valueList.Add(result);
        }
    }

    valueList.Sort();
    return valueList.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int OperandInfo::ComputeProbableValuesCompare(CmpInstrBase* instr, ValueList& valueList,
                                              Block* testBlock, SparseBitVector& visited, 
                                              ConstantFolder& folder, int level) {
    // In order to compute the probable values we need first to obtain 
    // the probable values for the source operands. 
    // If any of the operands has no such value we give up.
    ValueList leftList;
    int leftValues = ComputeProbableValues(instr->LeftOp(), leftList, 
                                           testBlock, visited, folder, level + 1);
    if(leftValues == 0) {
        return 0;
    }

    ValueList rightList;
    int rightValues= ComputeProbableValues(instr->RightOp(), rightList, 
                                           testBlock, visited, folder, level + 1);
    if(rightValues == 0) {
        return 0;
    }

    // Because we do a cartesian product of the source operand values,
    // their number is limited before applying the operator to the combinations.
    leftValues = std::min(leftValues, MAXIMUM_PROBABLE_VALUES);
    rightValues = std::min(rightValues, MAXIMUM_PROBABLE_VALUES);

    // The probabilities of the results are computed as follows:
    // P(result) = P(operand1) * P(operand2)
    for(int i = 0; i < leftValues; i++) {
        for(int j = 0; j < rightValues; j++) {
            DebugValidator::IsTrue(leftList[i].Value->IsConstant());
            DebugValidator::IsTrue(rightList[j].Value->IsConstant());

            // Constant-fold the comparison.
            auto value = folder.FoldCompare(instr->GetOpcode(), leftList[i].Value,
                                            rightList[j].Value, instr->Order(), testBlock);
            ValueProbability result(value->As<Constant>());
            ComputeProbability(leftList[i], rightList[j], result);
            valueList.Add(result);
        }
    }

    valueList.Sort();
    return valueList.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int OperandInfo::ComputeProbableValuesQuestion(QuestionInstr* instr, ValueList& valueList,
                                               Block* testBlock, SparseBitVector& visited, 
                                               ConstantFolder& folder, int level) {
    // First we need to obtain the probable values of the condition operand.
    // If no such values are found we give up; otherwise we continue, but
    // consider only the most probable value as the condition (to avoid creating
    // too many values with low probability). Then we obtain the probable values
    // of the true/false operands and add to the list the ones associated
    // with the probable value of the condition operand.
    ValueList conditionList;
    int conditionValues = ComputeProbableValues(instr->ConditionOp(), conditionList, 
                                                testBlock, visited, folder, level + 1);
    if(conditionValues == 0) {
        return 0;
    }

    ValueList trueList;
    ValueList falseList;
    int trueValues = ComputeProbableValues(instr->TrueOp(), trueList, 
                                           testBlock, visited, folder, level + 1);
    int falseValues= ComputeProbableValues(instr->FalseOp(), falseList, 
                                           testBlock, visited, folder, level + 1);
    if((trueValues == 0) && (falseValues == 0)) {
        return 0;
    }

    // The probability of the results are computed as follows:
    // P(result) = P(condition) * P(true), if condition != 0
    //             P(condition) * P(false), if condition == 0
    DebugValidator::IsTrue(conditionList[0].Value->IsConstant());

    if(conditionList[0].Value->IsZeroInt()) {
        // The false operand is most likely to be selected.
        int falseValues = std::min(falseValues, MAXIMUM_PROBABLE_VALUES);

        for(int i = 0; i < falseValues; i++) {
            DebugValidator::IsTrue(falseList[i].Value->IsConstant());
            ValueProbability result(falseList[i].Value);
            ComputeProbability(conditionList[0], falseList[i], result);
            valueList.Add(result);
        }
    }
    else {
        // The true operand is most likely to be selected.
        int trueValues = std::min(trueValues, MAXIMUM_PROBABLE_VALUES);

        for(int i = 0; i < trueValues; i++) {
            DebugValidator::IsTrue(trueList[i].Value->IsConstant());
            ValueProbability result(trueList[i].Value);
            ComputeProbability(conditionList[0], trueList[i], result);
            valueList.Add(result);
        }
    }

    valueList.Sort();
    return valueList.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int OperandInfo::ComputeProbableValuesConversion(ConversionInstr* instr, ValueList& valueList,
                                                 Block* testBlock, SparseBitVector& visited, 
                                                 ConstantFolder& folder, int level) {
    // Obtain the probable values of the conversion target, then apply
    // the conversion to each one.
    ValueList targetList;
    int targets = ComputeProbableValues(instr->TargetOp(), targetList, 
                                        testBlock, visited, folder, level + 1);
    if(targets == 0) {
        return 0;
    }

    for(int i = 0; i < targets; i++) {
        ComputeProbableValues(instr->TargetOp(), targetList, 
                              testBlock, visited, folder, level + 1);
        DebugValidator::IsTrue(targetList[i].Value->IsConstant());

        auto value = folder.FoldConversion(instr->GetOpcode(), targetList[i].Value,
                                           instr->CastType(), testBlock);
        // Conversions involving pointers may fail.
        if(value) {
            valueList.Add(ValueProbability(value->As<Constant>(), 
                                           targetList[i].Type,
                                           targetList[i].Probability));
        }
    }

    valueList.Sort();
    return valueList.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int OperandInfo::ComputeProbableValuesPhi(PhiInstr* instr, ValueList& valueList,
                                          Block* testBlock, SparseBitVector& visited,
                                          ConstantFolder& folder, int level) {
    // If the 'phi' has been already processed it means we're inside
    // a loop and we should stop, else we would end in an infinite cycle.
    int parentId = instr->ParentBlock()->Id();

    if(visited.IsSet(parentId)) {
        return 0;
    }
    else visited.SetBit(parentId);

    // If the 'phi' has many incoming operands and no information
    // is available about the edge execution counts we give up
    // because we would end with many values having the same (small) probability.
    if((instr->OperandCount() > 4) && (GetProfileInfo() == nullptr)) {
        return 0;
    }

    // Each incoming value is considered and its probability is
    // the product of the probability of the value being a certain constant
    // and the probability of the formed edge to be taken:
    // P(incoming) = P(value) * P(edge_taken)
    for(int i = 0; i < instr->OperandCount(); i++) {
        ValueList incomingValues;
        ComputeProbableValues(instr->GetOperand(i), incomingValues, 
                              testBlock, visited, folder, level + 1);

        // Compute the probability for each of the probable
        // constants of the incoming operand. At the end only
        // the most probable values will be selected.
        for(int j = 0; j < incomingValues.Count(); j++) {
            float edgeProbability = EdgeProbability(instr->ParentBlock(),
                                                    instr->GetOperandBlock(i)->Target());
            float probability = incomingValues[j].GetProbability() * edgeProbability;
            valueList.Add(ValueProbability(incomingValues[j].Value, 
                                           Probability_Percent, probability));
        }
    }

    // Make sure we don't have too many values in the list.
    valueList.Sort();

    if(valueList.Count() > MAXIMUM_PROBABLE_VALUES) {
        valueList.RemoveRange(MAXIMUM_PROBABLE_VALUES, 
                              valueList.Count() - MAXIMUM_PROBABLE_VALUES);
    }

    return valueList.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
float OperandInfo::EdgeProbability(Block* fromBlock, Block* toBlock) {
    // If we have profile information we take the normalized edge
    // execution count, else we presume that each edge is taken
    // with the same probability.
    if(auto profileInfo = GetProfileInfo()) {
        return profileInfo->GetNormalizedExecutionFrequency(fromBlock, toBlock);
    }
    else return 1.0 / (float)toBlock->PredecessorCount();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int OperandInfo::ComputeProbableValuesLoad(LoadInstr* instr, ValueList& valueList,
                                           Block* testBlock, SparseBitVector& visited,
                                           ConstantFolder& folder, int level) {
    ValueList sourceList;
    int sources =  ComputeProbableValues(instr->SourceOp(), valueList, 
                                         testBlock, visited, folder, level + 1);
    
    StaticList<Operand*, 4> loadedValues;
    sources = std::min(sources, MAXIMUM_PROBABLE_VALUES);

    if(sources > 0) {
        for(int i = 0; i < sources; i++) {
            if(auto loadedValue = folder.FoldLoad(sourceList[i].Value)) {
                valueList.Add(ValueProbability(loadedValue->As<Constant>(), 
                                               sourceList[i].Type, 
                                               sourceList[i].Probability));
            }
        }
    }
    else {
        if(auto loadedValue = folder.FoldLoad(instr->SourceOp())) {
            valueList.Add(ValueProbability(loadedValue->As<Constant>(),
                                           Probability_Always));
        }
    }

    //! TODO: Use value profile if available
    
    valueList.Sort();
    return valueList.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::GetRange(Operand* op, Block* testBlock, Range& range) {
    DebugValidator::IsNotNull(op);

    // If the operand is floating there definitely
    // isn't any range for it.
    if(op->IsFloating()) {
        return false;
    }
    
    if(auto rangeTag = GetRangeTag(op, testBlock)) {
        return rangeTag->GetRange(op, range);
    }
    else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult OperandInfo::AreEqual(Operand* a, Operand* b, Block* testBlock) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    DebugValidator::IsTrue(a->IsInteger() || a->IsPointer());

    // Test for trivial equality.
    if(a == b) {
        return Result_Yes;
    }

    // Check if both operands are constants.
    if(auto intConstA = a->As<IntConstant>()) {
        if(auto intConstB = b->As<IntConstant>()) {
            return IA::AreEqual(intConstA, intConstB) ?
                   Result_Yes : Result_No;
        }
    }

    // Try to use value-range information.
    if(auto rangeTag = GetRangeTag(a, testBlock)) {
        auto result = rangeTag->AreEqual(a, b);
        if(result != Result_Maybe) {
            return result;
        }
    }

    // If we compare pointers we can do better:
    // 1. if the pointers are definitely null, they are equal
    // 2. if one is definitely null and the other one definitely not null,
    //    they can't be equal
    if(a->IsPointer()) {
        bool aIsNull = IsPointerNull(a, testBlock);
        bool bIsNull = IsPointerNull(b, testBlock);

        if(aIsNull && bIsNull) {
            return Result_Yes;
        }
        else if((aIsNull && IsPointerNotNull(b, testBlock)) ||
                (bIsNull && IsPointerNotNull(a, testBlock))) {
            return Result_No;
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult OperandInfo::AreNotEqual(Operand* a, Operand* b, Block* testBlock) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    DebugValidator::IsTrue(a->IsInteger() || a->IsPointer());

    // Check if both operands are constants.
    if(auto intConstA = a->As<IntConstant>()) {
        if(auto intConstB = b->As<IntConstant>()) {
            return IA::AreNotEqual(intConstA, intConstB) ?
                   Result_Yes : Result_No;
        }
    }

    // Try to use value-range information.
    if(auto rangeTag = GetRangeTag(a, testBlock)) {
        auto result = rangeTag->AreNotEqual(a, b);
        if(result != Result_Maybe) {
            return result;
        }
    }

    // If we compare pointers we can do better:
    // 1. if the pointers are definitely null, the result is negative
    // 2. if one is definitely null and the other one definitely not null,
    //    the result is positive
    if(a->IsPointer()) {
        bool aIsNull = IsPointerNull(a, testBlock);
        bool bIsNull = IsPointerNull(b, testBlock);

        if(aIsNull && bIsNull) {
            return Result_No;
        }
        else if((aIsNull && IsPointerNotNull(b, testBlock)) ||
            (bIsNull && IsPointerNotNull(a, testBlock))) {
            return Result_Yes;
        }
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult OperandInfo::IsSmaller(Operand* a, Operand* b, bool isSigned, 
                                   Block* testBlock) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    DebugValidator::IsTrue(a->IsInteger());

    // Check if both operands are constants.
    if(auto intConstA = a->As<IntConstant>()) {
        if(auto intConstB = b->As<IntConstant>()) {
            if(isSigned) {
                return IA::IsSmaller(intConstA, intConstB) ?
                                     Result_Yes : Result_No;
            }
            else return IA::IsSmallerUnsigned(intConstA, intConstB) ?
                                              Result_Yes : Result_No;
        }
    }

    // Try to use value-range information.
    if(auto rangeTag = GetRangeTag(a, testBlock)) {
        return rangeTag->IsSmaller(a, b, isSigned);
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult OperandInfo::IsSmallerOrEqual(Operand* a, Operand* b, bool isSigned,
                                          Block* testBlock) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    DebugValidator::IsTrue(a->IsInteger());

    // Check if both operands are constants.
    if(auto intConstA = a->As<IntConstant>()) {
        if(auto intConstB = b->As<IntConstant>()) {
            if(isSigned) {
                return IA::IsSmallerOrEqual(intConstA, intConstB) ?
                                            Result_Yes : Result_No;
            }
            else return IA::IsSmallerOrEqualUnsigned(intConstA, intConstB) ?
                                                     Result_Yes : Result_No;
        }
    }

    // Try to use value-range information.
    if(auto rangeTag = GetRangeTag(a, testBlock)) {
        return rangeTag->IsSmallerOrEqual(a, b, isSigned);
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult OperandInfo::IsGreater(Operand* a, Operand* b, bool isSigned,
                                   Block* testBlock) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    DebugValidator::IsTrue(a->IsInteger());

    // Check if both operands are constants.
    if(auto intConstA = a->As<IntConstant>()) {
        if(auto intConstB = b->As<IntConstant>()) {
            if(isSigned) {
                return IA::IsLarger(intConstA, intConstB) ?
                                    Result_Yes : Result_No;
            }
            else return IA::IsLargerUnsigned(intConstA, intConstB) ?
                                             Result_Yes : Result_No;
        }
    }

    // Try to use value-range information.
    if(auto rangeTag = GetRangeTag(a, testBlock)) {
        return rangeTag->IsGreater(a, b, isSigned);
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeResult OperandInfo::IsGreaterOrEqual(Operand* a, Operand* b, bool isSigned,
                                          Block* testBlock) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    DebugValidator::IsTrue(a->IsInteger());

    // Check if both operands are constants.
    if(auto intConstA = a->As<IntConstant>()) {
        if(auto intConstB = b->As<IntConstant>()) {
            if(isSigned) {
                return IA::IsLargerOrEqual(intConstA, intConstB) ?
                                           Result_Yes : Result_No;
            }
            else return IA::IsLargerOrEqualUnsigned(intConstA, intConstB) ?
                                                    Result_Yes : Result_No;
        }
    }

    // Try to use value-range information.
    if(auto rangeTag = GetRangeTag(a, testBlock)) {
        return rangeTag->IsGreaterOrEqual(a, b, isSigned);
    }

    return Result_Maybe;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::IsPointerNull(Operand* op, Block* testBlock) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsTrue(op->GetType()->IsPointer());

    if(op->IsNullConstant()) {
        return true;
    }
    else return IsZero(op, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool OperandInfo::IsPointerNotNull(Operand* op, Block* testBlock, bool handlePhi) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsTrue(op->GetType()->IsPointer());

    // Obtain the operand that acts as a base (this strips off
    // any 'addr', 'index', 'elem' and 'ptop' instructions).
    // If the base is not null then the computed pointer can't be too.
    auto baseOp = GetBaseOperand(op);

    // References to variables and functions are never null.
    if(baseOp->IsVariableReference() ||
       baseOp->IsFunctionReference()) {
       return true;
    }

    // For 'phi' and 'quest' instructions we know that the result
    // operand is not null if all operands are not null.
    if(auto phiInstr = op->DefiningInstrAs<PhiInstr>()) {
        // 'phi' are sometimes not handled to prevent cycles.
        if(handlePhi && (phiInstr->OperandCount() <= 16)) {
            bool notNull = true;

            for(int i = 0; i < phiInstr->OperandCount(); i++) {
                auto op = phiInstr->GetOperand(i);
                if(op->IsUndefinedConstant()) {
                    continue;
                }

                if(IsPointerNotNull(op, testBlock, false) == false) {
                    // May be null, give up.
                    notNull = false;
                    break;
                }
            }

            if(notNull) return true;
        }
    }
    else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
        if(IsPointerNotNull(questInstr->TrueOp(), testBlock, false) &&
           IsPointerNotNull(questInstr->FalseOp(), testBlock, false)) {
            return true;
        }
    }
#if 1 //! TODO: ONLY FOR C-FAMILY LANGUAGES
    else if(auto callInstr = op->DefiningInstrAs<CallInstr>()) {
        // If this is a call to 'malloc' or 'alloca' that is checked
        // (meaning that the program exists if no memory could be allocated)
        // we know that the pointer can't be null.
        if(auto cFamilyTag = callInstr->GetTag<CFamilyTag>()) {
            if(cFamilyTag->IsCheckedAllocLike()) {
                return true;
            }
        }
    }
#endif

    // Try to use range and bit information.
    return IsNotZero(baseOp, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* OperandInfo::GetBaseOperand(Operand* op) {
    DebugValidator::IsNotNull(op);

    if(op->DefiningInstrIs<IndexInstr>()   ||
       op->DefiningInstrIs<AddressInstr>() ||
       op->DefiningInstrIs<ElementInstr>() ||
       op->DefiningInstrIs<PtopInstr>()) {
        auto baseOp = op->DefiningInstruction()->GetSourceOp(0);
        return GetBaseOperand(baseOp);
    }
    
    return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeTag* OperandInfo::GetRangeTagForEntry(Parameter* parameter) {
    DebugValidator::IsNotNull(parameter);

    auto symbolTable = parameter->GetVariable()->ParentTable();
    auto function = static_cast<Function*>(symbolTable->Parent());
    return function->FirstBlock()->GetTag<RangeTag>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RangeTag* OperandInfo::GetRangeTag(Operand* op, Block* testBlock) {
    if(testBlock) {
        return testBlock->GetTag<RangeTag>();
    }
    else if(auto parameter = op->As<Parameter>()) {
        // If this is a parameter we may know it's range in the 
        // entry block already (may have been set by an interprocedural pass).
        return GetRangeTagForEntry(parameter);
    }
    
    return nullptr;
}

} // namespace Analysis