// IntrinsicPeephole.cpp
// Copyright (c) Lup Gratian
//
// Implements peephole optimizations for intrinsics.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

Operand* Peephole::HandleIntrinsic(CallInstr* instr, Intrinsic* intrinsic) {
    if(auto memIntr = intrinsic->As<MemoryIntrinsic>()) {
        if(memIntr->IsCopyMemoryIntr()) {
            return HandleCopyMemoryIntr(instr);
        }
        else if(memIntr->IsSetMemoryIntr()) {
            return HandleSetMemoryIntr(instr);
        }
    }
    else if(auto mathIntrinsic = intrinsic->As<MathIntrinsic>()) {
        switch(mathIntrinsic->GetMathKind()) {
            case MathIntr_Abs8:
            case MathIntr_Abs16:
            case MathIntr_Abs32:
            case MathIntr_Abs64: {
                return HandleAbsIntrinsic(instr);
            }
            case MathIntr_Sqrt:
            case MathIntr_Sqrtf: {
                return HandleSqrtIntrinsic(instr);
            }
            case MathIntr_Pow: {
                return HandlePowIntrinsic(instr);
            }
            case MathIntr_Min8:
            case MathIntr_Min16:
            case MathIntr_Min32:
            case MathIntr_Min64:
            case MathIntr_Max8:
            case MathIntr_Max16:
            case MathIntr_Max32:
            case MathIntr_Max64: {
                return HandleMinMaxIntrinsics(instr);
            }
        }
    }
    else if(auto bitInstr = intrinsic->As<BitwiseIntrinsic>()) {
        switch(bitInstr->GetBitwiseKind()) {
            case BitwiseIntr_Rotate: {
                HandleRotateIntrinsic(instr);
            }
            case BitwiseIntr_ByteSwap: {
                return HandleByteSwapIntrinsic(instr);
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleCopyMemoryIntr(CallInstr* instr) {
    // If we're copying a small amount of data (1-8 bytes),
    // we can replace the 'copyMemory' call with a 'load/store' pair.
    auto sizeConst = AsIntConstant(CopyMemoryIntr::GetLength(instr),
                                   instr->ParentBlock());
    if(sizeConst == nullptr) {
        return nullptr;
    }
    
    __int64 size = sizeConst->Value();

    if(size > 8) {
        return nullptr;
    }

    // We do the optimization only if the size is a multiple of 2,
    // else pointer casts and address computations would be needed,
    // which would be expansive, and it would confuse some analyses.
    if(IA::IsPowerOfTwo(size) == false) {
        return nullptr;
    }

    // The operands of the intrinsic probably have a pointer cast, because arguments
    // having 'int8*' type are expected. Check if these casts are present, and
    // don't take them into consideration anymore.
    auto destOp = WithoutPointerCasts(CopyMemoryIntr::GetDestination(instr));
    auto sourceOp = WithoutPointerCasts(CopyMemoryIntr::GetSource(instr));
    const Type* commonType = nullptr;

    // Determine an integer/floating type that can be used to perform the copy.
    // If we're copying from a floating type we prefer a floating type.
    if(sourceOp->IsFloating()) {
        if(size == FloatingType::GetFloat()->Size()) {
            commonType = FloatingType::GetFloat();
        }
        else if(size == FloatingType::GetDouble()->Size()) {
            commonType = FloatingType::GetDouble();
        }
    }

    if(commonType == nullptr) {
        // For all other cases we use an integer type.
        commonType = IntegerType::GetHavingSize(size);
    }

    // It's possible that casts are needed to bring both operands to the common type.
    auto commonPtrType = irGen_.GetPointer(commonType);
    destOp = InsertPtopIfRequired(destOp, commonPtrType);
    sourceOp = InsertPtopIfRequired(sourceOp, commonPtrType);

    // Now create the 'load' and the 'store'.
    auto loadOp = irGen_.GetTemporary(commonType);
    irGen_.GetLoad(sourceOp, loadOp);
    irGen_.GetStore(destOp, loadOp);
    return LOG(GetUndefined(loadOp)); // A signal that the intrinsic is dead.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleSetMemoryIntr(CallInstr* instr) {
    // If we're setting a small amount of data (1-8 bytes), we can replace 
    // the 'setMemory' call with a 'store'. 
    auto sizeConst = AsIntConstant(SetMemoryIntr::GetLength(instr),
                                   instr->ParentBlock());
    if(sizeConst == nullptr) {
        return nullptr;
    }
    
    // We do this optimization only if the size is a multiple of 2 
    // (otherwise it isn't profitable, as is the case for 'copyMemory').
    __int64 size = sizeConst->Value();

    if((size > 8) || (IA::IsPowerOfTwo(size) == false)) {
        return nullptr;
    }

    // The operand of the intrinsic probably has a pointer cast, because arguments
    // having 'int8*' type are expected. Check if these casts are present, and
    // don't take them into consideration anymore.
    auto destOp = WithoutPointerCasts(SetMemoryIntr::GetDestination(instr));
    auto valueOp = SetMemoryIntr::GetSource(instr);
    auto storeType = IntegerType::GetHavingSize(size);

    // It's possible that casts are needed to bring both operands to the common type.
    auto storePtrType = irGen_.GetPointer(storeType);
    destOp = InsertPtopIfRequired(destOp, storePtrType);

    // It may be needed to extend the value we set, because the intrinsic works
    // only with 'int8' types, and we have an arbitrary power of two size.
    // 'setMemory(p, 0xFF, 2)' becomes 'store p, 0xFFFF', with 'p' having 'int16*' type.
    if(storeType != valueOp->GetType()) {
        valueOp = CreateSetMemoryValue(valueOp, size, storeType);
    }

    // Now create the 'store'.
    irGen_.GetStore(destOp, valueOp);
    return LOG(GetUndefined(valueOp)); // A signal that the intrinsic is dead.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateSetMemoryValue(Operand* valueOp, int size,
                                        const IntegerType* storeType) {
    if(auto intConst = valueOp->As<IntConstant>()) {
        // An integer constant can be extended at compile time.
        int size = intConst->GetType()->Size();
        int endSize = storeType->As<IntegerType>()->Size();
        __int64 value = intConst->Value();

        while(size < endSize) {
            value = (value << 8) | intConst->Value();
            size++;
        }

        return irGen_.GetIntConst(storeType, value);
    }
    else if(size < 8) {
        // We need to generate instructions that extend the value. Example:
        // a <int8> -> <int32>
        // t1 = a << 8       t2 = a << 16       t3 = a << 24
        // t4 = a | t1       t5 = t4 | t2       t6 = t5 | t3
        Operand* t[8];
        int count = 0;
        int size = intConst->GetType()->Size();
        int endSize = storeType->As<IntegerType>()->Size();

        valueOp = irGen_.GetTemporary(storeType);
        irGen_.GetZext(valueOp, storeType, valueOp);

        // Create the temporary value of the form 'a << N'.
        while(size < endSize) {
            auto constantOp = irGen_.GetIntConst(storeType, 8 * size);
            auto resultOp = irGen_.GetTemporary(storeType);
            irGen_.GetShl(valueOp, constantOp, resultOp);

            t[count] = resultOp;
            size++;
            count++;
        }

        // Combine the temporary values using 'or'.
        for(int i = 0; i < size; i++) {
            auto resultOp = irGen_.GetTemporary(storeType);
            irGen_.GetOr(valueOp, t[i], resultOp);
            valueOp = resultOp;
        }

        return valueOp;
    }
    else {
        // We ca also do the following, which results in fewer instructions:
        // t1 = a << 8      t2 = a | t1       t3 = t2 << 16
        // t4 = t2 | t3     t5 = t4 << 32     t6 = t4 | t5
        // We use the second approach only if we extend to 'int64', because the
        // reduction for 'int32' is only 2 instructions, while introducing a
        // RAW dependencies, which may slow things down on modern CPUs.
        // For 8 byte stores we save 6 instructions, which should make it faster.
        valueOp = irGen_.GetTemporary(storeType);
        irGen_.GetZext(valueOp, storeType, valueOp);

        auto shlOp8 = irGen_.GetIntConst(storeType, 8);
        auto t1 = irGen_.GetTemporary(storeType);
        irGen_.GetShl(valueOp, shlOp8, t1);

        auto t2 = irGen_.GetTemporary(storeType);
        irGen_.GetOr(valueOp, t1, t2);

        auto shlOp16 = irGen_.GetIntConst(storeType, 16);
        auto t3 = irGen_.GetTemporary(storeType);
        irGen_.GetShl(t2, shlOp16, t1);

        auto t4 = irGen_.GetTemporary(storeType);
        irGen_.GetOr(t2, t3, t4);

        auto shlOp32 = irGen_.GetIntConst(storeType, 32);
        auto t5 = irGen_.GetTemporary(storeType);
        irGen_.GetShl(t4, shlOp32, t5);

        auto t6 = irGen_.GetTemporary(storeType);
        irGen_.GetOr(t4, t5, t6);
        return valueOp;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::WithoutPointerCasts(Operand* op) {
    while(auto ptopInstr = op->DefiningInstrAs<PtopInstr>()) {
        op = ptopInstr->TargetOp();
    }

    return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAbsIntrinsic(CallInstr* instr) {
    // abs(abs(a)) -> a
    // abs(-a) -> abs(a)
    auto argument = instr->GetArgument(0);
    Operand* absOp;

    if(auto callInstr = argument->DefiningInstrAs<CallInstr>()) {
        if(auto intrinsic = callInstr->GetIntrinsic()) {
            auto mathIntrinsic = intrinsic->As<MathIntrinsic>();
            if((mathIntrinsic == nullptr) ||
               (mathIntrinsic->IsAbs() == false)) {
                return nullptr;
            }

            return LOG(callInstr->ResultOp());
        }
    }
    else if(Match<SubInstr>(MatchInt(0), MatchAny(&absOp))(argument)) {
        return LOG(absOp);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleSqrtIntrinsic(CallInstr* instr) {
    // Handle cases involving at least one 'sqrt'/'sqrtf'.
    auto mathIntrA = instr->GetIntrinsicAs<MathIntrinsic>();
    if((mathIntrA == nullptr) || (((mathIntrA->GetMathKind() == MathIntr_Sqrt) ||
       (mathIntrA->GetMathKind() == MathIntr_Sqrtf)) == false)) {
        return nullptr;
    }

    // Check the type of the operand.
    if(auto mulInstr = instr->GetArgument(0)->DefiningInstrAs<FmulInstr>()) {
        // sqrt(a * a) -> a
        if(mulInstr->LeftOp() == mulInstr->RightOp()) {
            return LOG(mulInstr->LeftOp());
        }
    }
    else if(auto callInstr = instr->GetArgument(0)->DefiningInstrAs<CallInstr>()) {
        auto mathIntrB = callInstr->GetIntrinsicAs<MathIntrinsic>();
        if(mathIntrB == nullptr) return nullptr;

        switch(mathIntrB->GetMathKind()) {
            case MathIntr_Pow: {
                // sqrt(pow(a, C)) -> pow(a, C / 2)
                // sqrt(pow(a, 2.0)) -> pow(a, 1.0) -> a
                // sqrt(pow(a, 4.0)) -> pow(a, 2.0)
                auto a = callInstr->GetArgument(0);
                auto C = callInstr->GetArgument(1)->As<FloatConstant>();

                if(C == nullptr) {
                    return nullptr;
                }

                double value = FA::Fdiv(C, 2.0);
                auto constantOp = irGen_.GetFloatingConst(mathIntrB->ReturnType(), value);

                return LOG(CreateTwoArgCall(callInstr->TargetOp(), a, constantOp, 
                                            mathIntrB->ReturnType()));
            }
        }
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandlePowIntrinsic(CallInstr* instr) {
    // We handle a 'pow' with constants until 8; after that the intrinsic/standard
    // library function should not be slower anymore.
    auto a = instr->GetArgument(0);
    auto floatConst = instr->GetArgument(1)->As<FloatConstant>();

    if(floatConst == nullptr) {
        return nullptr;
    }

    // pow (a, 1.0) -> a
    if(FA::AreClose(floatConst, 1.0)) {
        return LOG(a);
    }

    // pow (a, 2.0) -> a * a
    if(FA::AreClose(floatConst, 2.0)) {
        return LOG(CreatePow2(a));
    }

    // pow (a, 3.0) -> pow(a, 2.0) * a
    if(FA::AreClose(floatConst, 3.0)) {
        return LOG(CreateFmul(CreatePow2(a), a));
    }

    // pow(a, 4.0) -> pow(a, 2.0) * pow(a, 2.0)
    if(FA::AreClose(floatConst, 4.0)) {
        return LOG(CreatePow4(a));
    }

    // pow(a, 5.0) -> pow(a, 4.0) * a
    if(FA::AreClose(floatConst, 5.0)) {
        return LOG(CreateFmul(CreatePow4(a), a));
    }

    // pow(a, 6.0) -> pow(a, 4.0) * pow(a, 2.0)
    if(FA::AreClose(floatConst, 6.0)) {
        return LOG(CreateFmul(CreatePow4(a), CreatePow2(a)));
    }

    // pow(a, 7.0) -> pow(a, 4.0) * pow(a, 2.0) * a
    if(FA::AreClose(floatConst, 7.0)) {
        return LOG(CreateFmul(CreateFmul(CreatePow4(a), CreatePow2(a)), a));
    }

    // pow(a, 8.0) -> pow(a, 4.0) * pow(a, 4.0)
    if(FA::AreClose(floatConst, 8.0)) {
        auto pow4 = CreatePow4(a);
        return LOG(CreateFmul(pow4, pow4));
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreatePow2(Operand* op) {
    // op * op
    auto mulOp = GetTemporary(op);
    irGen_.GetFmul(op, op, mulOp);
    return mulOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreatePow4(Operand* op) {
    // op * op * op * op is equivalent to
    // t1 = op * op        t2 = t1 * t1
    auto pow2Op = CreatePow2(op);
    auto mulOp = GetTemporary(op);
    irGen_.GetFmul(pow2Op, pow2Op, mulOp);
    return mulOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateFmul(Operand* opA, Operand* opB) {
    DebugValidator::IsTrue(opA->GetType() == opB->GetType());
    auto mulOp = GetTemporary(opA);
    irGen_.GetFmul(opA, opB, mulOp);
    return mulOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFmulIntrinsics(Operand* opA, Operand* opB, FloatMode mode) {
    if(auto callInstrA = opA->As<CallInstr>()) {
        auto mathIntrA = callInstrA->GetIntrinsicAs<MathIntrinsic>();

        if(auto callInstrB = opB->As<CallInstr>()) {
            auto mathIntrB = callInstrB->GetIntrinsicAs<MathIntrinsic>();

            if(mathIntrB == nullptr) return nullptr; {
                return nullptr;
            }

            // intrinsic * intrinsic
            return HandleFmulIntrIntr(callInstrA, callInstrB, 
                                      mathIntrA, mathIntrB, mode);
        }
        else {
            // intrinsic * operand
            switch(mathIntrA->GetMathKind()) {
                case MathIntr_Pow: {
                    return HandleFmulPowOperandIntr(callInstrA, opB, mode);
                }
            }
        }
    }
    else if(auto callInstr = opB->As<CallInstr>()) {
        // operand * intrinsic
        auto mathIntrinsic = callInstr->GetIntrinsicAs<MathIntrinsic>();

        if(mathIntrinsic == nullptr) {
            return nullptr;
        }

        switch(mathIntrinsic->GetMathKind()) {
            case MathIntr_Pow: {
                return HandleFmulPowOperandIntr(callInstr, opA, mode);
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFmulIntrIntr(CallInstr* instrA, CallInstr* instrB, 
                                      MathIntrinsic* mathIntrA, 
                                      MathIntrinsic* mathIntrB, FloatMode mode) {
    switch(mathIntrA->GetMathKind()) {
        case MathIntr_Pow: {
            if(mathIntrB->GetMathKind() == MathIntr_Pow) {
                // pow * pow
                return HandleFmulPowPowIntr(instrA, instrB, mode);
            }
            break;
        }
        case MathIntr_Sqrt:
        case MathIntr_Sqrtf: {
            if(mathIntrB->GetMathKind() == MathIntr_Sqrt) {
                // sqrt(a) * sqrt(a) -> a
                // sqrt(a) * sqrt(b) -> sqrt(a * b)
                if(instrA->GetArgument(0) == instrB->GetArgument(0)) {
                    return LOG(instrA->GetArgument(0));
                }
                else {
                    auto mulOp = irGen_.GetTemporary(mathIntrA->ReturnType());
                    irGen_.GetFmul(instrA->GetArgument(0), 
                                   instrB->GetArgument(1), mulOp);
                    return LOG(CreateOneArgCall(instrA->TargetOp(), mulOp, 
                                                mathIntrA->ReturnType()));
                }
            }
            break;
        }
        case MathIntr_Exp: {
            if(mathIntrB->GetMathKind() == MathIntr_Exp) {
                // exp(a) * exp(b) -> exp(a + b)
                auto addOp = GetTemporary(instrA->GetArgument(0));                
                irGen_.GetFadd(instrA->GetArgument(0), 
                               instrB->GetArgument(1), addOp);
                return LOG(CreateOneArgCall(instrA->TargetOp(), addOp,
                                            addOp->GetType()));
            }
            break;
        }
        case MathIntr_Tan: {
            if(mathIntrB->GetMathKind() == MathIntr_Cos) {
                // tan(a) * cos(a) -> sin(a)
                if(instrA->GetArgument(0) == instrB->GetArgument(0)) {
                    auto sinIntr = SinIntr::GetSin(irGen_.GetUnit());
                    return LOG(CreateOneArgCall(irGen_.GetFunctionRef(sinIntr),
                                                instrA->GetArgument(0), 
                                                instrA->GetArgument(0)->GetType()));
                }
            }
            break;
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFmulPowOperandIntr(CallInstr* instr, Operand* op, 
											FloatMode mode) {
    FloatConstant* C;
    Operand* x;

    // pow(x, C) * x -> pow(x, C + 1.0)
    // x * pow(x, C) -> pow(x, C + 1.0)
    C = instr->GetArgument(1)->As<FloatConstant>();

    if(C == nullptr) {
        return nullptr;
    }

    x = instr->GetArgument(0);

    if(x != op) {
        return nullptr;
    }

    auto constantOp = irGen_.GetFloatingConst(C->GetType(), FA::Fadd(C, 1.0));
    return LOG(CreateTwoArgCall(instr->TargetOp(), x, constantOp, x->GetType()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFmulPowPowIntr(CallInstr* instrA, CallInstr* instrB,
										FloatMode mode) {
    Operand* a = instrA->GetArgument(0);
    Operand* b = instrA->GetArgument(1);
    Operand* c = instrB->GetArgument(0);
    Operand* d = instrB->GetArgument(1);

    //     a  b        c  d
    // pow(x, y) * pow(z, y) -> pow(x * z, y)
    if(b == d) {
        auto xz = CreateFmul(a, c);
        return LOG(CreateTwoArgCall(instrA->TargetOp(), xz, b, a->GetType()));
    }
    
    //     a  b        c  d
    // pow(x, y) * pow(x, z) -> pow(x, y + z)
    if(a == c) {
        auto addOp = GetTemporary(a);
        irGen_.GetFadd(b, d, addOp);
        return LOG(CreateTwoArgCall(instrA->TargetOp(), a, addOp, a->GetType()));
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFdivIntrinsics(Operand* opA, Operand* opB, FloatMode mode) {
    if(auto callInstrA = opA->As<CallInstr>()) {
        auto mathIntrA = callInstrA->GetIntrinsicAs<MathIntrinsic>();

        if(mathIntrA == nullptr) {
            return nullptr;
        }

        if(auto callInstrB = opB->As<CallInstr>()) {
            auto mathIntrB = callInstrB->GetIntrinsicAs<MathIntrinsic>();

            if(mathIntrB == nullptr) {
                return nullptr;
            }

            // intrinsic / intrinsic
            return HandleFdivIntrIntr(callInstrA, callInstrB, 
                                      mathIntrA, mathIntrB, mode);
        }
        else {
            // intrinsic / operand
            switch(mathIntrA->GetMathKind()) {
                case MathIntr_Pow: {
                    return HandleFdivPowOperandIntr(callInstrA, opB, mode, false);
                }
            }
        }
    }
    else if(auto callInstr = opB->As<CallInstr>()) {
        // operand / intrinsic
        auto mathIntrinsic = callInstr->GetIntrinsicAs<MathIntrinsic>();

        if(mathIntrinsic == nullptr) {
            return nullptr;
        }

        switch(mathIntrinsic->GetMathKind()) {
            case MathIntr_Pow: {
                return HandleFdivPowOperandIntr(callInstr, opA, mode, true);
            }
            case MathIntr_Exp: {
                // a / exp(b) -> a * exp(-b)
                auto argument = callInstr->GetArgument(0);
                auto minusB = GetTemporary(argument);
		        irGen_.GetSub(GetZeroFloat(argument), argument, minusB);

                auto exp = CreateOneArgCall(callInstr->TargetOp(), 
                                            minusB, opA->GetType());
                auto mulOp = GetTemporary(opA);
                irGen_.GetFmul(opA, exp, mulOp);
                return LOG(mulOp);
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFdivPowOperandIntr(CallInstr* instr, Operand* op, 
											FloatMode mode, bool reversed) {
    if(reversed == false) {
        // pow(a, b) / a -> pow(a, b - 1.0)
        if(instr->GetArgument(0) == op) {
            auto subOp = GetTemporary(op);
            irGen_.GetFsub(instr->GetArgument(1), GetOneFloat(op), subOp);
            return LOG(CreateTwoArgCall(instr->TargetOp(), op, subOp, op->GetType()));
        }
    }
    else {
        // a / pow(b, c) -> a * pow(b, -c)
        // We always do this transformation, because fp division is really expensive.
        auto argument = instr->GetArgument(0);
        auto minusB = GetTemporary(argument);
		irGen_.GetSub(GetZeroFloat(argument), argument, minusB);

        auto pow = CreateTwoArgCall(instr->TargetOp(), instr->GetArgument(0),
                                    minusB, op->GetType());
        auto mulOp = GetTemporary(op);
        irGen_.GetFmul(op, pow, mulOp);
        return LOG(mulOp);
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleFdivIntrIntr(CallInstr* instrA, CallInstr* instrB, 
                                      MathIntrinsic* mathIntrA, MathIntrinsic* mathIntrB,
                                      FloatMode mode) {
    switch(mathIntrA->GetMathKind()) {
        case MathIntr_Cos: {
            if(mathIntrB->GetMathKind() == MathIntr_Sin) {
                // cos(a) / sin(a) -> 1.0 / tan(a)
                if(instrA->GetArgument(0) == instrB->GetArgument(0)) {
                    auto op = instrA->GetArgument(0);
                    auto tanIntr = TanIntr::GetTan(irGen_.GetUnit());
                    auto tan = CreateOneArgCall(irGen_.GetFunctionRef(tanIntr),
                                                op, op->GetType());
                    auto divOp = GetTemporary(op);
                    irGen_.GetFdiv(GetOneFloat(op), tan, divOp);
                    return LOG(divOp);
                }
            }
            else if(mathIntrB->GetMathKind() == MathIntr_Cos) {
                // cos(a) / cos(a) -> 1.0
                if(instrA->GetArgument(0) == instrB->GetArgument(0)) {
                    return LOG(GetOneFloat(instrA->GetArgument(0)));
                }
            }
            break;
        }
        case MathIntr_Sin: {
            if(mathIntrB->GetMathKind() == MathIntr_Tan) {
                // sin(a) / tan(a) -> cos(a)
                if(instrA->GetArgument(0) == instrB->GetArgument(0)) {
                    auto cosIntr = CosIntr::GetCos(irGen_.GetUnit());
                    return LOG(CreateOneArgCall(irGen_.GetFunctionRef(cosIntr),
                                                instrA->GetArgument(0), 
                                                cosIntr->ReturnType()));
                }
            }
            else if(mathIntrB->GetMathKind() == MathIntr_Sin) {
                // sin(a) / sin(a) -> 1.0
                if(instrA->GetArgument(0) == instrB->GetArgument(0)) {
                    return LOG(GetOneFloat(instrA->GetArgument(0)));
                }
            }
            break;
        }
        case MathIntr_Tan: {
            if(mathIntrB->GetMathKind() == MathIntr_Sin) {
                // tan(a) / sin(a) -> 1.0 / cos(a)
                if(instrA->GetArgument(0) == instrB->GetArgument(0)) {
                    auto op = instrA->GetArgument(0);
                    auto cosIntr = CosIntr::GetCos(irGen_.GetUnit());
                    auto cos = CreateOneArgCall(irGen_.GetFunctionRef(cosIntr),
                                                op, cosIntr->ReturnType());
                    auto divOp = GetTemporary(op);
                    irGen_.GetFdiv(GetOneFloat(op), cos, divOp);
                    return LOG(divOp);
                }
            }
            else if(mathIntrB->GetMathKind() == MathIntr_Tan) {
                // tan(a) / tan(a) -> 1.0
                if(instrA->GetArgument(0) == instrB->GetArgument(0)) {
                    return LOG(GetOneFloat(instrA->GetArgument(0)));
                }
            }
            break;
        }
    }

    return nullptr;
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleMinMaxIntrinsics(CallInstr* instr) {
    auto intrinsic = instr->GetIntrinsic();
    DebugValidator::IsNotNull(intrinsic);

    auto mathIntrinsic = intrinsic->As<MathIntrinsic>();
    DebugValidator::IsNotNull(mathIntrinsic);
    DebugValidator::IsTrue(mathIntrinsic->IsMin() || mathIntrinsic->IsMax());

    auto opA = instr->GetArgument(0);
    auto opB = instr->GetArgument(1);

    // min(a, a) -> a
    // max(a, a) -> a
    if(opA == opB) {
        return opA;
    }

    // Check if we have a max/min with 'quest' instructions as operands.
    // Move the 'quest' from right to left (fewer cases to test for).
    if(opB->DefiningInstrIs<QuestionInstr>() &&
       (opA->DefiningInstrIs<QuestionInstr>() == false)) {
        instr->ReplaceArgument(0, opB);
        instr->ReplaceArgument(1, opA);
        std::swap(opA, opB);
    }
    
    if(auto questInstrA = opA->DefiningInstrAs<QuestionInstr>()) {
        if(auto questInstrB = opB->DefiningInstrAs<QuestionInstr>()) {
            // min/max(quest, quest)
            return HandleMinMaxOnQuestion(questInstrA, questInstrB, 
                                          mathIntrinsic->IsMax());
        }
        else if(auto intConst = opB->As<IntConstant>()) {
            // min/max(quest, C)
            return HandleMinMaxOnQuestion(questInstrA, intConst, 
                                          mathIntrinsic->IsMax());
        }
        else return nullptr;
    }

    // Try to simplify a min/max applied on another min/max intrinsic.
    // min(max(a, b), b) -> b
    if(opA->HasDefiningInstruction() && 
       opB->HasDefiningInstruction()) {
        return nullptr;
    }

    // Move the function call on the left (fewer cases to test for).
    if(opB->DefiningInstrIs<CallInstr>()) {
        instr->ReplaceArgument(0, opB);
        instr->ReplaceArgument(1, opA);
        std::swap(opA, opB);
    }

    // Check if the left argument is a min/max intrinsic.
    if(auto callInstr = opA->DefiningInstrAs<CallInstr>()) {
        auto intr2 = callInstr->GetIntrinsic();

        if(intr2 == nullptr) {
            return nullptr;
        }

        auto mathIntr2 = intr2->As<MathIntrinsic>();

        if((mathIntr2 == nullptr) || (mathIntrinsic->IsMin() || 
            mathIntrinsic->IsMax()) == false) {
            return nullptr;
        }

        auto opC = callInstr->GetArgument(0);
        auto opD = callInstr->GetArgument(1);

        if(mathIntrinsic->IsMax()) {
            if(mathIntr2->IsMax()) {
                //         C  D   B                        
                // max(max(a, b), a) -> max(a, b)
                // max(max(a, b), b) -> max(a, b)
                if((opC == opB) || (opD == opB)) {
                    return LOG(callInstr->ResultOp());
                }
            }
            else {
                // max(min(a, b), a) -> a
                // max(min(a, b), b) -> b
                if((opC == opB) || (opD == opB)) {
                    return LOG(opB);
                }
            }
        }
        else {
            if(mathIntr2->IsMax()) {
                // min(max(a, b), a) -> a
                // min(max(a, b), b) -> b
                if((opC == opB) || (opD == opB)) {
                    return opB;
                }
            }
            else {
                // min(min(a, b), a) -> min(a, b)
                // min(min(a, b), b) -> min(a, b)
                if((opC == opB) || (opD == opB)) {
                    return callInstr->ResultOp();
                }
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* Peephole::HandleMinMaxOnQuestion(QuestionInstr* instr, 
                                          IntConstant* intConst, bool isMax) {
    // min (quest c, 3, 5), 1 -> 1
    // max (quest c, 3, 5), 8 -> 8
    auto trueConst = AsIntConstant(instr->TrueOp(), instr->ParentBlock());

    if(trueConst == nullptr) {
        return nullptr;
    }

    auto falseConst = AsIntConstant(instr->FalseOp(), instr->ParentBlock());

    if(falseConst == nullptr) {
        return nullptr;
    }

    if(isMax) {
        if(intConst->Value() >= std::max(trueConst->Value(), 
                                         falseConst->Value())) {
            return LOG(intConst);
        }
    }
    else if(intConst->Value() <= std::min(trueConst->Value(), 
                                          falseConst->Value())) {
        return LOG(intConst);
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* Peephole::HandleMinMaxOnQuestion(QuestionInstr* instrA, 
                                          QuestionInstr* instrB, bool isMax) {
    // min (quest c, 3, 5), (quest c, 2, 6) -> quest c, 2, 5
    // max (quest c, 3, 5), (quest c, 2, 6) -> quest c, 3, 6
    // Note that the same condition operand must be used
    // by both 'quest' instructions.
    if(instrA->ConditionOp() != instrB->ConditionOp()) {
        return nullptr;
    }

    // We do the simplification only if the 'quest' result operands are used
    // only by the min/max intrinsics (prevents creating new instructions).
    if((instrA->ResultOp()->HasSingleUser() == false) ||
       (instrB->ResultOp()->HasSingleUser() == false)) {
        return nullptr;
    }

    // Check if only constants are used.
    auto block  = instrA->ParentBlock();

    auto trueConstA = AsIntConstant(instrA->TrueOp(), block);
    if(trueConstA == nullptr) {
        return nullptr;
    }

    auto falseConstA = AsIntConstant(instrA->FalseOp(), block);

    if(falseConstA == nullptr) {
        return nullptr;
    }

    auto trueConstB = AsIntConstant(instrB->TrueOp(), block);

    if(trueConstB == nullptr) {
        return nullptr;
    }

    auto falseConstB = AsIntConstant(instrB->FalseOp(), block);

    if(falseConstB == nullptr) {
        return nullptr;
    }

    // min((quest c, a, b), (quest c, d, e)) -> quest c, min(a, d), min(b, e)
    // max((quest c, a, b), (quest c, d, e)) -> quest c, max(a, d), max(b, e)
    __int64 newTrueConst;
    __int64 newFalseConst;

    if(isMax) {
        newTrueConst = std::max(trueConstA->Value(), 
                                trueConstB->Value());
        newFalseConst = std::max(falseConstA->Value(), 
                                 falseConstB->Value());
    }
    else {
        newTrueConst = std::min(trueConstA->Value(), 
                                trueConstB->Value());
        newFalseConst = std::min(falseConstA->Value(), 
                                 falseConstB->Value());
    }

    // Create the new 'quest'.
    auto temp = GetTemporary(trueConstA);
    irGen_.GetQuestion(instrA->ConditionOp(), 
                       irGen_.GetIntConst(trueConstA->GetType(), 
                                          newTrueConst),
                       irGen_.GetIntConst(falseConstA->GetType(), 
                                          newFalseConst), temp);
    return LOG(temp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* Peephole::HandleCmpIntrinsic(CmpInstrBase* instr) {
    Operand* opB = instr->RightOp();
    auto callInstrA = instr->LeftOp()->DefiningInstrAs<CallInstr>();
    auto callInstrB = instr->RightOp()->DefiningInstrAs<CallInstr>();

    // Nothing to do if neither is a potential intrinsic call.
    if((callInstrA || callInstrB) == false) {
        return nullptr;
    }

    // Move the intrinsic to the left side (fewer cases to test for).
    if(callInstrB && (callInstrA == nullptr)) {
        callInstrA = callInstrB;
        instr->InvertOrder(true /* invertOperand*/, 
                           false /* invertEquality */);
        opB = instr->RightOp();
    }

    OrderType order = instr->Order();
    bool isUnsigned = instr->IsUdiv();
    auto resultType = instr->ResultOp()->GetType();

    // Make sure we are working with intrinsics.
    Intrinsic* intrA = callInstrA->GetIntrinsic();

    if(intrA == nullptr) {
        return nullptr;
    }

    Intrinsic* intrB = callInstrB ? callInstrB->GetIntrinsic() : nullptr;

    if(callInstrB && (intrB == nullptr)) {
        return nullptr;
    }

    MathIntrinsic* mathIntrA;
    MathIntrinsic* mathIntrB;

    if(intrA && intrB) {
        mathIntrA = intrA->As<MathIntrinsic>();

        if(mathIntrA == nullptr) {
            return nullptr;
        }
    }
        
    mathIntrB = intrB->As<MathIntrinsic>();

    if(mathIntrB == nullptr) {
        return nullptr;
    }

    // If we have 'min' on the right side move it to the left,
    // so we have fewer cases to test for.
    if(mathIntrB->IsMin() && (mathIntrA->IsMin() == false)) {
        std::swap(mathIntrA, mathIntrB);
        std::swap(callInstrA, callInstrB);
        order = CmpInstrBase::InvertedOrder(order, false /* invertEquality */);
    }

    switch(mathIntrA->GetMathKind()) {
        case MathIntr_Min8:
        case MathIntr_Min16:
        case MathIntr_Min32:
        case MathIntr_Min64: {
            return HandleMinMaxIntrCmp(callInstrA, callInstrB, 
                                        mathIntrA, mathIntrB, 
                                        order, resultType, isUnsigned);
        }
    }

    // intrinsic ORDER a
    auto mathIntrinsic = intrA->As<MathIntrinsic>();

    if(mathIntrinsic == nullptr) {
        return nullptr;
    }

    switch(mathIntrinsic->GetMathKind()) {
        case MathIntr_Min8:
        case MathIntr_Min16:
        case MathIntr_Min32:
        case MathIntr_Min64: {
            // min ORDER C can be simplified in many cases.
            if(auto intConst = AsIntConstant(opB, instr->ParentBlock())) {
                return HandleMinIntrConstCmp(callInstrA, intConst, order, 
                                             resultType, isUnsigned);
            }

            return HandleMinIntrOpCmp(callInstrA, opB, order, 
                                      resultType, isUnsigned);
        }
        case MathIntr_Max8:
        case MathIntr_Max16:
        case MathIntr_Max32:
        case MathIntr_Max64: {
            // max ORDER C can be simplified in many cases.
            if(auto intConst = AsIntConstant(opB, instr->ParentBlock())) {
                return HandleMaxIntrConstCmp(callInstrA, intConst, order, 
                                             resultType, isUnsigned);
            }
            
            return HandleMaxIntrOpCmp(callInstrA, opB, order, 
                                      resultType, isUnsigned);
        }
        case MathIntr_Abs8:
        case MathIntr_Abs16:
        case MathIntr_Abs32:
        case MathIntr_Abs64: 
        case MathIntr_Fabs: {
            return HandleAbsIntrOpCmp(callInstrA, opB, order, 
                                      resultType, isUnsigned);
            break;
        }
        case MathIntr_Sqrt:
        case MathIntr_Sqrtf: {
            auto a = callInstrA->GetArgument(0);
            auto C = callInstrA->GetArgument(1)->As<FloatConstant>();

            if(C == nullptr) {
                return nullptr;
            }
            
            return HandleSqrtIntrConstCmp(a, C, order, resultType);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleMinMaxIntrCmp(CallInstr* instrA, CallInstr* instrB, 
                                       MathIntrinsic* mathIntrA, 
                                       MathIntrinsic* mathIntrB,
                                       OrderType order, const Type* resultType,
                                       bool isUnsigned) {
    if(mathIntrB->IsMin() && SameMinMaxOperands(instrA, instrB)) {
        // min(a, b) </!=/>   min(a, b) -> false
        // min(a, b) <=/==/>= min(a, b) -> true
        if((order == Order_Less) || (order == Order_NotEqual) ||
            (order == Order_Greater)) {
            return LOG(GetBool(false, resultType));
        }
        else return LOG(GetBool(true, resultType));
    }
    else if(mathIntrB->IsMax() && SameMinMaxOperands(instrA, instrB)) {
        // min(a, b) <= max(a, b) -> true
        if(order == Order_LessOrEqual) {
            return LOG(GetBool(true, resultType));
        }

        // min(a, b) < max(a, b) -> a != b
        if(order == Order_Less) {
            if(instrA->ResultOp()->HasSingleUser() &&
               instrB->ResultOp()->HasSingleUser()) {
                return LOG(CreateCompare(instrA->GetArgument(0),
                                         instrB->GetArgument(1),
                                         Order_NotEqual, resultType, isUnsigned));
            }
        }

        // min(a, b) >  max(a, b) -> false
        if(order == Order_Greater) {
            return LOG(GetBool(false, resultType));
        }

        // min(a, b) == max(a, b) -> a == b
        if(order == Order_Equal) {
            return LOG(CreateCompare(instrA->GetArgument(0),
                                     instrB->GetArgument(1),
                                     Order_Equal, resultType, isUnsigned));
        }

        // min(a, b) != max(a, b) -> a != b
        if(order == Order_NotEqual) {
            return LOG(CreateCompare(instrA->GetArgument(0),
                                     instrB->GetArgument(1),
                                     Order_NotEqual, resultType, isUnsigned));
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleMinIntrConstCmp(CallInstr* instrA, IntConstant* intConst,
                                         OrderType order, const Type* resultType, 
                                         bool isUnsigned) {
    // min(a, 0) ==  0 -> a >= 0
    // min(a, 0) ==  4 -> false
    // min(a, 0) == -1 -> a == -1
    // min(a, 0) >   0 -> false
    // min(a, 0) >   4 -> false
    // min(a, 0) >  -1 -> a > -1
    auto minOpA = instrA->GetArgument(0);
    auto minOpB = instrA->GetArgument(1);

    // Swap so that we have fewer cases to test.
    if(IsZeroInt(minOpA, instrA->ParentBlock())) {
        std::swap(minOpA, minOpB);
    }

    // The second 'min' argument must be 0.
    if(IsZeroInt(minOpB, instrA->ParentBlock()) == false) {
        return nullptr;
    }

    if(IA::IsPositive(intConst)) {
        if((order == Order_Equal) && intConst->IsZero()) {
            // min(a, 0) == 0 -> a >= 0
            return LOG(CreateCompare(minOpA, intConst, Order_GreaterOrEqual, 
                                     resultType, isUnsigned));
        }
        else if((order == Order_Equal) || (order == Order_Greater)) {
            // min(a, 0) == 4 -> false
            // min(a, 0) >  0 -> false
            // min(a, 0) >  4 -> false
            return LOG(GetBool(false, minOpA));
        }
    }
    else if(order == Order_Equal) {
        // min(a, 0) == -1 -> a == -1
        return LOG(CreateCompare(minOpA, intConst, Order_Equal,
                                 minOpA->GetType(), isUnsigned));
    }
    else if(order == Order_Greater) {
        // min(a, 0) >  -1 -> a > -1
        return LOG(CreateCompare(minOpA, intConst, Order_Greater,
                                 minOpA->GetType(), isUnsigned));
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleMinIntrOpCmp(CallInstr* instrA, Operand* opB,
                                      OrderType order, const Type* resultType,
                                      bool isUnsigned) {
    auto block = instrA->ParentBlock();

    // min(a, b) > a -> false
    if(order == Order_Greater) {
        if(AreEqual(instrA->GetArgument(0), opB, block) ||
           AreEqual(instrA->GetArgument(1), opB, block)) {
            return LOG(GetBool(false, resultType));
        }
    }

    // min(a, b) == a -> a <= b
    // min(a, b) == b -> b <= a
    if(order == Order_Equal) {
        if(AreEqual(instrA->GetArgument(0), opB, block)) {
            return LOG(CreateCompare(opB, instrA->GetArgument(1),
                                     Order_LessOrEqual, resultType, isUnsigned));
        }
        else if(AreEqual(instrA->GetArgument(1), opB, block)) {
            return LOG(CreateCompare(opB, instrA->GetArgument(0),
                                     Order_LessOrEqual, resultType, isUnsigned));
        }
    }

    // min(a, b) != a -> a > b
    // min(a, b) != b -> b > a
    if(order == Order_NotEqual) {
        if(AreEqual(instrA->GetArgument(0), opB, block)) {
            return LOG(CreateCompare(opB, instrA->GetArgument(1),
                                     Order_Greater, resultType, isUnsigned));
        }
        else if(AreEqual(instrA->GetArgument(1), opB, block)) {
            return LOG(CreateCompare(opB, instrA->GetArgument(0),
                                     Order_Greater, resultType, isUnsigned));
        }
    }

    // min(a, b) </<= a -> b </<= a
    // min(a, b) </<= b -> a </<= b
    if((order == Order_Less) || (order == Order_LessOrEqual)) {
        if(AreEqual(instrA->GetArgument(0), opB, block)) {
            return LOG(CreateCompare(instrA->GetArgument(1), opB,
                                     order, resultType, isUnsigned));
        }
        else if(AreEqual(instrA->GetArgument(1), opB, block)) {
            return LOG(CreateCompare(instrA->GetArgument(0), opB,
                                     order, resultType, isUnsigned));
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleMaxIntrConstCmp(CallInstr* instrA, IntConstant* intConst,
                                         OrderType order, const Type* resultType, 
                                         bool isUnsigned) {
    // max(a, 0) ==  0 -> a <= 0
    // max(a, 0) ==  5 -> a == 5
    // max(a, 0) == -1 -> false
    // max(a, 0) >   0 -> a > 0
    // max(a, 0) >   5 -> a > 5
    // max(a, 0) >  -1 -> true
    auto maxOpA = instrA->GetArgument(0);
    auto maxOpB = instrA->GetArgument(1);

    // Swap so that we have fewer cases to test.
    if(IsZeroInt(maxOpA, instrA->ParentBlock())) {
        std::swap(maxOpA, maxOpB);
    }

    // The second 'min' argument must be 0.
    if(IsZeroInt(maxOpB, instrA->ParentBlock()) == false) {
        return nullptr;
    }

    if(IA::IsPositive(intConst)) {
        if(order == Order_Equal) {
            if(intConst->IsZero()) {
                // max(a, 0) == 0 -> a <= 0
                return LOG(CreateCompare(maxOpA, intConst, Order_LessOrEqual, 
                                         resultType, isUnsigned));
            }
                        
            // max(a, 0) == 5 -> a == 5
            return LOG(CreateCompare(maxOpA, intConst, Order_Equal, 
                                     resultType, isUnsigned));
        }
        else if(order == Order_Greater) {
            // max(a, 0) > 0 -> a > 0
            // max(a, 0) > 5 -> a > 5
            return LOG(CreateCompare(maxOpA, intConst, Order_Greater, 
                                     resultType, isUnsigned));
        }
    }
    else if(order == Order_Equal) {
        // max(a, 0) == -1 -> false
        return LOG(GetBool(false, maxOpA));
    }
    else if(order == Order_Greater) {
        // max(a, 0) > -1 -> true
        return LOG(GetBool(true, maxOpA));
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleMaxIntrOpCmp(CallInstr* instrA, Operand* opB,
                                      OrderType order, const Type* resultType, 
                                      bool isUnsigned) {
    auto block = instrA->ParentBlock();

    // max(a, b) < a -> false
    if(order == Order_Greater) {
        if(AreEqual(instrA->GetArgument(0), opB, block) ||
           AreEqual(instrA->GetArgument(1), opB, block)) {
            return LOG(GetBool(false, resultType));
        }
    }

    // max(a, b) == a -> a >= b
    // max(a, b) == b -> b >= a
    if(order == Order_Equal) {
        if(AreEqual(instrA->GetArgument(0), opB, block)) {
            return LOG(CreateCompare(opB, instrA->GetArgument(1),
                                     Order_Greater, resultType, isUnsigned));
        }
        else if(AreEqual(instrA->GetArgument(1), opB, block)) {
            return LOG(CreateCompare(opB, instrA->GetArgument(0),
                                     Order_Greater, resultType, isUnsigned));
        }
    }

    // max(a, b) != a -> a < b
    // max(a, b) != b -> b < a
    if(order == Order_NotEqual) {
        if(AreEqual(instrA->GetArgument(0), opB, block)) {
            return LOG(CreateCompare(opB, instrA->GetArgument(1),
                                     Order_Less, resultType, isUnsigned));
        }
        else if(AreEqual(instrA->GetArgument(1), opB, block)) {
            return LOG(CreateCompare(opB, instrA->GetArgument(0),
                                     Order_Less, resultType, isUnsigned));
        }
    }

    // max(a, b) >/>= a -> b >/>= a
    // max(a, b) >/>= b -> a >/>= b
    if((order == Order_Greater) || (order == Order_GreaterOrEqual)) {
        if(AreEqual(instrA->GetArgument(0), opB, block)) {
            return LOG(CreateCompare(instrA->GetArgument(1), opB,
                                     order, resultType, isUnsigned));
        }
        else if(AreEqual(instrA->GetArgument(1), opB, block)) {
            return LOG(CreateCompare(instrA->GetArgument(0), opB,
                                     order, resultType, isUnsigned));
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAbsIntrOpCmp(CallInstr* instrA, Operand* opB,
                                      OrderType order, const Type* resultType, 
                                      bool isUnsigned) {
    auto absOp = instrA->GetArgument(0);

    // abs(a) < 0 -> false
    if(IsZeroInt(opB, instrA->ParentBlock()) &&
       (order == Order_Less)) {
        return LOG(GetBool(false, resultType));
    }

    // abs(a) >= 0 -> true
    if(IsZeroInt(opB, instrA->ParentBlock()) &&
       (order == Order_GreaterOrEqual)) {
        return LOG(GetBool(true, resultType));
    }

    // abs(a) > a -> a < 0
    // abs(a) != a -> a < 0
    if((absOp == opB) && ((order == Order_Greater) || (order == Order_NotEqual))) {
        return LOG(CreateCompare(absOp, GetZeroInt(absOp), Order_Less,
                                 resultType, isUnsigned));
    }

    // abs(a) == a -> a >= 0
    if((absOp == opB) && (order == Order_Equal)) {
        return LOG(CreateCompare(absOp, GetZeroInt(absOp), 
                                 Order_GreaterOrEqual, resultType, isUnsigned));
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleSqrtIntrConstCmp(Operand* a, FloatConstant* C, OrderType order, 
                                          const Type* resultType) {
    // sqrt(a) < C -> false, if 'C < 0'
    if((order == Order_Less) && FA::IsNegative(C)) {
        return LOG(GetBool(false, resultType));
    }

    // sqrt(a) > C -> true, if 'C < 0'
    if((order == Order_Greater) && FA::IsNegative(C)) {
        return LOG(GetBool(true, resultType));
    }

    // sqrt(a) > C -> false, if 'C == +INF'
    if((order == Order_Greater) && FA::IsPositiveInfinity(C)) {
        return LOG(GetBool(false, resultType));
    }

    // sqrt(a) > C -> a > (C * C)
    if(order == Order_Greater) {
        double value = FA::Fmul(C, 2.0);
        auto constantOp = irGen_.GetFloatingConst(C->GetType(), value);
        return LOG(CreateCompare(a, constantOp, Order_Greater, resultType, false));
    }

    // sqrt(a) < C -> true, if 'C == +INF'
    if((order == Order_Less) && FA::IsPositiveInfinity(C)) {
        return LOG(GetBool(true, resultType));
    }

    // sqrt(a) < C -> a < (C * C)
    if(order == Order_Less) {
        double value = FA::Fmul(C, 2.0);
        auto constantOp = irGen_.GetFloatingConst(C->GetType(), value);
        return LOG(CreateCompare(a, constantOp, Order_Less, resultType, false));
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::SameMinMaxOperands(CallInstr* instrA, CallInstr* instrB) {
    // Test if the two 'min'/'max' calls have exactly
    // the same operands, in any order. Only two cases 
    // need to be tested, because the other two are symmetric.
    auto block = instrA->ParentBlock();

    // a, b    a, b
    if(AreEqual(instrA->GetArgument(0), 
                instrB->GetArgument(0), block) 
        &&
       AreEqual(instrA->GetArgument(1), 
                instrB->GetArgument(1), block)) {
        return true;
    }

    // a, b    b, a
    if(AreEqual(instrA->GetArgument(0), 
                instrB->GetArgument(1), block) 
        &&
       AreEqual(instrA->GetArgument(1), 
                instrB->GetArgument(0), block)) {
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleByteSwapIntrinsic(CallInstr* instr) {
    //! byteSwap(byteSwap(a)) -> a
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleRotateIntrinsic(CallInstr* instr) {
    //! rotateLeft a, 32 -> a, if 'a' has type 'int32'
    //! rotateRight a, 32 -> a, if 'a' has type 'int32'

    //! rotateLeft (rotateLeft a, 16), 16 -> a, if 'a' has type 'int32'
    //! rotateLeft (rotateRight a, 16), 16 -> a, if 'a' has type 'int32'
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* Peephole::CreateOneArgCall(Operand* targetOp, Operand* argument, 
                                    const Type* resultType) {
    Operand* resultOp = resultType ? 
                        irGen_.GetTemporary(resultType) : nullptr;

    auto callInstr = irGen_.GetCall(targetOp, resultOp, 1);
    callInstr->AddArgument(argument);
    return resultOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateTwoArgCall(Operand* targetOp, Operand* arg1, 
                                    Operand* arg2, const Type* resultType) {
    Operand* resultOp = resultType ? 
                        irGen_.GetTemporary(resultType) : nullptr;

    auto callInstr = irGen_.GetCall(targetOp, resultOp, 2);
    callInstr->AddArgument(arg1);
    callInstr->AddArgument(arg2);
    return resultOp;
}

} // namespace Optimization