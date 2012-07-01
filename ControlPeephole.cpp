// ControlPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 
// 'if', 'switch' and 'call' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

Operand* Peephole::HandleCall(CallInstr* instr) {
    // First try to constant-fold the 'call'; it may be possible if it's
    // a math function from the standard library, for example.
    if(auto result = folder_.FoldCall(instr)) {
        return result;
    }

    // A call to 'nullptr' is undefined.
    if(instr->TargetOp()->IsNullConstant()) {
        return LOG(GetCallUndefined(instr));
    }

    // A call with the wrong calling convention is undefined.
    if(instr->HasOverridenCallConvention()) {
        auto functionRef = instr->TargetOp()->As<FunctionReference>();

        if(functionRef->Target()->CallConvention() != instr->CallConvention()) {
            // Remove the call.
            return LOG(GetCallUndefined(instr));
        }
    }

    // Try to simplify a call that uses a converted target.
    // This improves alias analysis in many cases.
	if(instr->HasDestinationOp() == false) {
        return nullptr;
    }

    if(auto ptopInstr = instr->TargetOp()->DefiningInstrAs<PtopInstr>()) {
        return HandleCallPtop(instr, ptopInstr);
    }

    // Try to simplify calls to 'varargs' functions.
    if(auto functionRef = instr->TargetOp()->As<FunctionReference>()) {
        if(functionRef->Target()->IsVarargs()) {
            return HandleCallVarargs(instr, functionRef->Target()->GetType());
        }
    }

    // Try to simplify intrinsics.
    if(auto intrinsic = instr->GetIntrinsic()) {
        return HandleIntrinsic(instr, intrinsic);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleCallPtop(CallInstr* instr, PtopInstr* ptopInstr) {
    // Consider the following case:
    // funct f (int32, int64, int32*)
    // t1 = ptop f, (int32, int32*, int8*)
    // call t1, a, b, c
    //
    // This can be transformed so that it uses 'f' directly, by changing the arguments:
    // t1 = ptoi b, int64
    // t2 = ptop c, int32*
    // call f, a, b, c
    // We do this only if we don't lose information when converting the arguments.
    
    // The type of the target is 'pointer to function'.
    auto targetType = ptopInstr->TargetOp()->GetType()->As<PointerType>();
    auto functionType = targetType->PointeeType()->As<FunctionType>();

    // Check if it's possible to do the transformation.
    if(VerifyCallPtop(instr, ptopInstr, functionType) == false) {
        return nullptr;
    }

    // Now do the transformation; we may need to create new casts.
    for(int i = 0; i < instr->ArgumentCount(); i++) {
        auto argument = instr->GetArgument(i);
        auto argumentType = argument->GetType();
        auto parameterType = functionType->Parameters()[i];

        if(argumentType == parameterType) {
            continue; // Nothings need to be done.
        }

        // Determine the type of the needed cast.
        if(argumentType->IsPointer()) {
            if(parameterType->IsPointer()) { // ptop
                auto ptopOp = irGen_.GetTemporary(parameterType);
                irGen_.GetPtop(argument, parameterType, ptopOp);
                instr->ReplaceArgument(i, ptopOp);
            }
            else if(parameterType->IsInteger()) { // ptoi
                auto ptoiOp = irGen_.GetTemporary(parameterType);
                irGen_.GetPtoi(argument, parameterType, ptoiOp);
                instr->ReplaceArgument(i, ptoiOp);
            }
            else DebugValidator::Unreachable();
        }
        else if(argumentType->IsInteger()) {
            if(parameterType->IsPointer()) { // itop
                auto itopOp = irGen_.GetTemporary(parameterType);
                irGen_.GetItop(argument, parameterType, itopOp);
                instr->ReplaceArgument(i, itopOp);
            }
            else DebugValidator::Unreachable();
        }
        else DebugValidator::Unreachable();
    }

    // Make the call use the original function.
    instr->SetTargetOp(ptopInstr->TargetOp());

    // Check if we need to patch the return value.
    // If necessary we insert a 'ptop' cast (the only case that can appear).
    if(instr->ResultOp() && 
       (instr->ResultOp()->GetType() != functionType->ReturnType())) {
        DebugValidator::IsTrue(instr->ResultOp()->IsPointer());

        auto originalType = instr->ResultOp()->GetType();
        auto ptopOp = GetTemporary(originalType);
        auto newReturnOp = GetTemporary(functionType->ReturnType());

        irGen_.GetPtop(newReturnOp, originalType, ptopOp);
        instr->ResultOp()->ReplaceWith(ptopOp);
        instr->SetResultOp(newReturnOp);
    }

    return LOG(instr->ResultOp());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::VerifyCallPtop(CallInstr* instr, PtopInstr* ptopInstr,
                              const FunctionType* functionType) {
    // This is not done for 'varargs' functions.
    if(functionType->IsVarargs()) {
        return nullptr;
    }

    // If the number of arguments doesn't match we give up.
    if(functionType->ParameterCount() != instr->ArgumentCount()) {
        return false;
    }

    // If the return types are not compatible give up.
    // We consider compatible types to be the ones that don't lose information:
    // void - because there is actually no information
    // pointers - a pointer can be cast to another pointer type
    auto returnTypeA = functionType->ReturnType();
    auto returnTypeB = instr->CalledFunctionType()->ReturnType();

    if(((returnTypeA->IsVoid() && returnTypeB->IsVoid()) == false) &&
       ((returnTypeA->IsPointer() && returnTypeB->IsPointer()) == false)) {
        return false;
    }
       
    for(int i = 0; i < instr->ArgumentCount(); i++) {
        auto argument = instr->GetArgument(i);

        // Test the type of the argument and the expected one by the original function.
        auto argumentType = argument->GetType();
        auto parameterType = functionType->Parameters()[i];

        if(argumentType != parameterType) {
            // The types don't match, but there are a few exceptions when we can
            // convert the argument without losing any information.
            if(argumentType->IsPointer()) {
                if(parameterType->IsPointer()) {
                    // OK, ptop
                }
                else if(auto intType = parameterType->As<IntegerType>()) {
                    // OK as long as the integer is large enough to store the pointer.
                    if(intType->Size() <= GetTarget()->GetPointerSize()) {
                        return false;
                    }
                }
                else return false;
            }
            else if(auto argumentIntType = argumentType->As<IntegerType>()) {
                if(parameterType->IsPointer()) {
                    // OK as long as the pointer is large enough to store the integer.
                    // Should be true for most targets.
                    if(argumentIntType->Size() > GetTarget()->GetPointerSize()) {
                        return false;
                    }
                }
                else return false;
            }
            else {
                // Any other type combination is invalid.
                return false;
            }
        }
    }

    return true;
 }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleCallVarargs(CallInstr* instr, const FunctionType* functionType) {
    // Converted pointers can be passed through the 'varargs' area
    // without any conversion, because all pointers are represented the same.
    for(int i = functionType->ParameterCount() - 1; i < instr->ArgumentCount(); i++) {
        if(auto ptopInstr = instr->GetArgument(i)->DefiningInstrAs<PtopInstr>()) {
            // call f, (ptop p, X*) -> call f, p
            instr->ReplaceArgument(i, ptopInstr->TargetOp());
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UndefinedConstant* Peephole::GetCallUndefined(CallInstr* instr) {
    if(instr->HasDestinationOp()) {
        return GetUndefined(instr->ResultOp());
    }
    else {
        // Create a bogus result, it's just to announce that the instruction is dead.
        return GetUndefined(IntegerType::GetInt32());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::HandleIf(IfInstr* instr) {
    // If we have an 'if' that depends on a comparison with 0 simplify:
    // if (cmp neq a, 0), T, F -> if a, T, F
    // if (cmp eq a, 0), T, F -> if a, F, T
    // The same applies to 'logical not' too.
    Operand* a = nullptr;
    OrderType order = Order_Less;
    bool boolCmpWithOne = false;

    if(instr->ConditionOp()->HasDefiningInstruction() &&
       instr->ConditionOp()->DefiningInstruction()->IsComparison()) {
        auto cmpInstr = static_cast<CmpInstrBase*>(instr->ConditionOp()
												   ->DefiningInstruction());
        if(cmpInstr->RightOp()->IsZeroInt()) {
            a = cmpInstr->LeftOp();
            order = cmpInstr->Order();
        }
        else if(cmpInstr->RightOp()->IsOneInt() && cmpInstr->LeftOp()->IsBoolean()) {
            // If we compare a boolean with 1 we can do some simplifications.
            // if (cmp eq a, 1), T, F -> if a, T, F, if 'a' is boolean
            // if (cmp neq a, 1), T, F -> if a, F, T, if 'a' is boolean
            a = cmpInstr->LeftOp();
            order = cmpInstr->Order();
            boolCmpWithOne = true;
        }
    }
    else if(IsLogicalNot(instr->ConditionOp(), &a)) {
        // if (~a), T, F -> if a, F, T
        order = Order_Equal;
    }

    if((a && (order == Order_NotEqual)) ||
       (a && boolCmpWithOne && (order == Order_Equal))) {
        // Skip over the comparison, it's unnecessary.
        instr->SetConditionOp(a);
        return LOG(true);
    }
    else if((a && (order == Order_Equal)) || 
            (a && boolCmpWithOne && (order == Order_NotEqual))) {
        // Invert the branches for the 'not equal' case.
        instr->SetConditionOp(a);
		InvertIfTargets(instr);
        return LOG(true);
    }

    // Change a comparison with order 'not equal' to one with order 'equal',
    // because it may expose more simplification opportunities.
    // if(a != 5) E1 else E2 -> if(a == 5) E2 else E1
    if(instr->ConditionOp()->HasDefiningInstruction() &&
       instr->ConditionOp()->DefiningInstruction()->IsComparison()) {
        auto cmpInstr = static_cast<CmpInstrBase*>(instr->ConditionOp()
												   ->DefiningInstruction());
        
        // Note that we can do this change only if the 'if' is the single user.
        if(cmpInstr->IsNotEqual() && cmpInstr->ResultOp()->HasSingleUser()) {
            cmpInstr->SetOrder(Order_Equal);
            InvertIfTargets(instr);
            return LOG(true);
        }
    }

    // There is no reason to extend a value if it's used in an 'if'.
    // if(zext a, int32), T, F -> if a, T, F
    // if(sext a, int32), T, F -> if a, T, F
    if(auto zextInstr = instr->ConditionOp()->DefiningInstrAs<ZextInstr>()) {
        instr->SetConditionOp(zextInstr->TargetOp());
        return LOG(true);
    }
    else if(auto sextInstr = instr->ConditionOp()->DefiningInstrAs<SextInstr>()) {
        instr->SetConditionOp(zextInstr->TargetOp());
        return LOG(true);
    }
    else if(auto questInstr = instr->ConditionOp()->DefiningInstrAs<QuestionInstr>()) {
        // If one of the operands is zero and the other one definitely not zero,
        // then we can use the operand that decides which 'quest' operand is chosen.
        // t = quest a, 1, 0              t = quest a, 0, 1
        // if t, T, F  ->  if a, T, F     if t, T, F  ->  if t, F, T
        if(IsNotZeroInt(questInstr->TrueOp(), instr->ParentBlock()) &&
           IsZeroInt(questInstr->FalseOp(), instr->ParentBlock())) {
            instr->SetConditionOp(questInstr->ConditionOp());
            return LOG(true);
        }
        else if(IsZeroInt(questInstr->TrueOp(), instr->ParentBlock()) &&
                IsNotZeroInt(questInstr->FalseOp(), instr->ParentBlock())) {
            instr->SetConditionOp(questInstr->ConditionOp());
            InvertIfTargets(instr);
            return LOG(true);
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Peephole::InvertIfTargets(IfInstr* instr) {
    auto trueBlock = instr->TrueTargetOp()->Target();
    auto falseBlock = instr->FalseTargetOp()->Target();
	instr->SetTrueTargetOp(nullptr);
	instr->SetFalseTargetOp(nullptr);
    instr->SetTrueTargetOp(irGen_.GetBlockRef(falseBlock));
    instr->SetFalseTargetOp(irGen_.GetBlockRef(trueBlock));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::HandleSwitch(SwitchInstr* instr) {
    // If we have a 'switch' around an arithmetic operation we can get rid of the
    // operation and modify all the 'case' values instead. For example,
    // switch (a + 2) {  ->  switch(a) {
    //     case 1:               case -1:
    //     case 3:               case 1:
    if(auto definingInstr = instr->ConditionOp()->DefiningInstruction()) {
        if(auto arithInstr = definingInstr->As<ArithmeticInstr>()) {
            return HandleSwitchOnAddSub(instr, arithInstr);
        }
        else if(auto shlInstr = definingInstr->As<ShlInstr>()) {
            return HandleSwitchOnShl(instr, shlInstr);
        }
        else if(definingInstr->IsSext() || definingInstr->IsZext()) {
            return HandleSwitchOnExtension(instr);
        }
    }

    // A 'switch' around a modulo operation can have the number of 'case' values reduced,
    // because we know the minimum and maximum possible values of the result.
    // switch (a % 4)  ->  switch(a % 4)
    //     case 1:             case 1:
    //     case 5:             // removed, can't be reached
    if(HandleSwitchOnMod(instr)) {
        return true;
    }

    // Try to remove 'case's using information about the bits of the operand.
    if(HandleSwitchOperandInfo(instr)) {
        return true;
    }

    // Delete any 'case' that targets the default block.
    auto& caseList = instr->CaseList();
    bool changed = false;

    for(int i = 0; i < caseList.Count(); i++) {
        if(caseList[i].Target == instr->DefaultTargetOp()) {
            instr->RemoveCase(i);
            changed = true;
            i--;
        }
    }

    return changed;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::HandleSwitchOnAddSub(SwitchInstr* switchInstr, 
                                    ArithmeticInstr* arithInstr) {
    // switch (a + 2) {  ->  switch(a) {
    //     case 1:               case -1:
    //     case 3:               case 1:
    // The right operand must be a constant.
    // Only 'add' and 'sub' are supported.
    if((arithInstr->IsAdd() || arithInstr->IsSub()) == false) {
        return false;
    }
    
    auto constantOp = arithInstr->RightOp()->As<IntConstant>();

    if(constantOp == nullptr) {
        // For 'add' we're allowed to have the constant on the left.
        if(arithInstr->IsAdd()) {
            constantOp = arithInstr->LeftOp()->As<IntConstant>();
        }

        if(constantOp == nullptr) {
            return false;
        }
    }

    // Compute the amount by which the 'case' values need to be changed.
    auto intKind = constantOp->GetType()->GetSubtype();
    __int64 delta;

    if(arithInstr->IsAdd()) {
        delta = IA::Sub(0, constantOp->Value(), intKind);
    }
    else delta = IA::LimitToType(constantOp->Value(), intKind);

    // Now change all 'case' values.
    for(int i = 0; i < switchInstr->CaseCount(); i++) {
        __int64 prevValue = switchInstr->GetCase(i).Value;
        __int64 newValue = IA::Add(prevValue, delta, intKind);
        switchInstr->GetCase(i).Value = newValue;
    }

    switchInstr->SetConditionOp(arithInstr->LeftOp() == constantOp ?
                                arithInstr->RightOp() : arithInstr->LeftOp());
    return LOG(true);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::HandleSwitchOnShl(SwitchInstr* switchInstr, 
                                 ShlInstr* shlInstr) {
    // A 'shl' instruction as the condition allows us to remove some 'case's
    // sometimes (if we shift by a constant). For example,
    // switch (a << 4)
    //     case 1:  // too small
    //     case 19: // not a multiple of 16
    //     case 32: // valid
    auto shlConst = shlInstr->RightOp()->As<IntConstant>();

    if(shlConst == nullptr) {
        return false;
    }

    // Eliminate any case that don't mach the criteria.
    __int64 pow2Value = IA::ValueFromBitCount(shlConst->Value()) + 1;
    bool changed = false;

     for(int i = 0; i < switchInstr->CaseCount(); i++) {
        __int64 value = switchInstr->GetCase(i).Value;

        if(((value > 0) && (value < pow2Value)) ||
           ((value % pow2Value) != 0)) {
            switchInstr->RemoveCase(i);
            changed = true;
            i--;
        }
    }

     return changed;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::HandleSwitchOnMod(SwitchInstr* switchInstr) {
    // A 'switch' around a modulo operation can have the number of 'case' values 
    // reduced, because we know the minimum and maximum possible values.
    // switch (a % 4)  ->  switch(a % 4)
    //     case 1:             case 1:
    //     case 5:             // removed, can't be reached
    bool valid = false;
    bool changed = false;
    auto conditionOp = switchInstr->ConditionOp();
    __int64 minValue;
    __int64 maxValue;

    if(auto modInstr = conditionOp->DefiningInstrAs<ModInstr>()) {
        if(auto constantOp = modInstr->RightOp()->As<IntConstant>()) {
            // If the constant is negative the ranges don't change
            // (the sign of the result depends only on the left operand for modulo).
            __int64 value = constantOp->Value() > 0 ?  constantOp->Value() : 
                                                      -constantOp->Value();
            minValue = -value;
            maxValue = value;
            valid = true;
        }
    }
    else if(auto umodInstr = conditionOp->DefiningInstrAs<UmodInstr>()) {
        if(auto constantOp = modInstr->RightOp()->As<IntConstant>()) {
            minValue = 0;
            maxValue = constantOp->Value();
            valid = true;
        }
    }

    if(valid) {
        // Eliminate all 'case' values that are not in the determined range.
        for(int i = 0; i < switchInstr->CaseCount(); i++) {
            __int64 value = switchInstr->GetCase(i).Value;

            if((value < minValue) || (value >= maxValue)) {
                // Remove the 'case'. It's supposed that a subsequent dead-code
                // elimination is run to remove the targeted block, if needed.
                switchInstr->RemoveCase(i);
                changed = true;
                i--;
            }
        }
    }

    return changed;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::HandleSwitchOnExtension(SwitchInstr* switchInstr) {
    DebugValidator::IsTrue(switchInstr->ConditionOp()->HasDefiningInstruction());
    DebugValidator::IsTrue(switchInstr->ConditionOp()->DefiningInstrIs<ZextInstr>() ||
                           switchInstr->ConditionOp()->DefiningInstrIs<SextInstr>());
    // Sometimes we have a 'switch' around an integer extension, but the extension
    // isn't necessary because all case values are between the limits
    // of the original type. For example,
    // t1 = sext a, int32  // a has type 'int8'
    // switch t1 {1,4,6,8,2}  ->  switch a {1,4,6,8,2}
    auto extInstr = switchInstr->ConditionOp()->DefiningInstrAs<ConversionInstr>();
    auto originalOp = extInstr->TargetOp();
    auto originalType = originalOp->GetType()->As<IntegerType>();

    __int64 maxValue = IA::GetMax(originalType, extInstr->IsSext());
    __int64 minValue = IA::GetMin(originalType, extInstr->IsSext());

    // Check if all case values are in the range.
    for(int i = 0; i < switchInstr->CaseCount(); i++) {
        __int64 value = switchInstr->GetCase(i).Value;

        if((value > maxValue) || (value < minValue)) {
            // The conversion is really needed.
            return false;
        }
    }

    // If we reach this point the conversion is not needed.
    switchInstr->SetConditionOp(originalOp);
    return LOG(true);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::HandleSwitchOperandInfo(SwitchInstr* switchInstr) {
    // switch (a | 1) {
    //     0 : B1  // unreachable, because condition is odd
    //     1 : B2 
    //     3 : B3  // unreachable too
    auto conditionOp = switchInstr->ConditionOp();
    auto intType = conditionOp->GetType()->As<IntegerType>();
    unsigned __int64 oneBits;
    unsigned __int64 zeroBits;

    opInfo_.EstimateOneBits(conditionOp, oneBits);
    opInfo_.EstimateZeroBits(conditionOp, zeroBits);

    // Estimate the minimum and maximum value of the operand.
    __int64 minValue = opInfo_.GetMinimumValue(conditionOp);
    __int64 maxValue = opInfo_.GetMaximumValue(conditionOp, false);

    bool isOdd = oneBits & 1;
    bool isEven = zeroBits & 1;
    bool isNegative = oneBits & IA::GetSignBitMask(intType);

    DebugValidator::IsFalse(isOdd && isEven);
    bool changed = false;

    for(int i = 0; i < switchInstr->CaseCount(); i++) {
        __int64 value = switchInstr->GetCase(i).Value;
        bool remove = false;

        if((isOdd && ((value % 2) == 0))  ||
           (isEven && ((value % 2) == 1)) ||
           (isNegative && (value >= 0))) {
            remove = true;
        }
        else remove = (value < minValue) || (value > maxValue);

        if(remove) {
            // The 'case' can safely be removed.
            switchInstr->RemoveCase(i);
            i--;
            changed = true;
        }
    }

    return changed;
}

} // namespace Optimization