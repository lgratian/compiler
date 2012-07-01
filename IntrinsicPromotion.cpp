// IntrinsicPromotion.hpp
// Copyright (c) Lup Gratian
//
// Implements the 'IntrinsicPromotion' pass.

//! ROTATE
// used by gcc
/* (A << C1) + (A >>u C2) if A is unsigned and C1+C2 is the size of A
	 is a rotate of A by C1 bits.  */
      /* (A << B) + (A >>u (Z - B)) if A is unsigned and Z is the size of A
	 is a rotate of A by B bits.  */



//! BSWAP
/*  (some example code)
    static inline unsigned short bswap_16(unsigned short x) {
      return (x>>8) | (x<<8);
    }

    static inline unsigned int bswap_32(unsigned int x) {
      return (bswap_16(x&0xffff)<<16) | (bswap_16(x>>16));
    }

    static inline unsigned long long bswap_64(unsigned long long x) {
      return (((unsigned long long)bswap_32(x&0xffffffffull))<<32) | (bswap_32(x>>32));
    }

    OR

    return (((nLongNumber&0x000000FF)<<24)+((nLongNumber&0x0000FF00)<<8)+
   ((nLongNumber&0x00FF0000)>>8)+((nLongNumber&0xFF000000)>>24));

   OR

    *p_dest = (*p_src >> 24) | ((*p_src & 0x00ff0000) >> 8) | 
              ((*p_src & 0x0000ff00) << 8) | (*p_src << 24)
*/

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/*

*/
#include "IntrinsicPromotion.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

void IntrinsicPromotion::Execute(Function* function) {
    for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
        Execute(block);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IntrinsicPromotion::Execute(Block* block) {
    Unit* unit = block->ParentFunction()->ParentUnit();
    IRGenerator irGen(unit);

    // Scan all instructions in the block.
    for(auto instr = block->FirstInstruction(); instr;
        instr = instr->NextInstruction()) {
        if(auto questInstr = instr->As<QuestionInstr>()) {
            // Try to detect things like abs/min/max.
            PromoteQuestion(questInstr);
            continue;
        }

        auto callInstr = instr->As<CallInstr>();
		if(callInstr == nullptr) continue;
            
		// If the instruction is already an intrinsic ignore it.
        if(callInstr->IsIntrinsic()) {
            continue;
        }

        // Try to determine if this is a call to a function
        // from the standard library. If it is we try to convert it
        // to an intrinsic, which exposes some optimization opportunities.
        StdlibType stdlibType = StdlibRecognizer::Recognize(callInstr);
            
        if(stdlibType == Stdlib_None) {
            // This is not a function from the standard library, skip.
            continue;
        }

        // To change from a call to a function to a call to an intrinsic
        // we only need to change the call target.
        switch(stdlibType) {
            case Stdlib_memcpy: {
				if(callInstr->HasDestinationOp() &&
					callInstr->GetDestinationOp()->HasUsers()) {
					// 'memcpy' returns the destination, while the intrinsic
					// returns nothing; in this case we can't promote.
					continue;
				}
				
                PromoteMemcpy(callInstr, unit, &irGen);
                break;
            }
            case Stdlib_memmove: {
				if(callInstr->HasDestinationOp() &&
					callInstr->GetDestinationOp()->HasUsers()) {
					// 'memset' returns the destination, while the intrinsic
					// returns nothing; in this case we can't promote.
					continue;
				}

                PromoteMemmove(callInstr, unit, &irGen);
                break;
            }
            case Stdlib_memset: {
				if(callInstr->HasDestinationOp() &&
				   callInstr->GetDestinationOp()->HasUsers()) {
					// 'memset' returns the destination, while the intrinsic
					// returns nothing; in this case we can't promote.
					continue;
				}

                auto intrinsic = SetMemoryIntr::GetSetMemory(unit);
                callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));

				// Same conversion as for 'memcpy' is needed.
				auto destOp = irGen.GetTemporary(irGen.GetInt8Pointer());
				auto instr = irGen.GetPtop(callInstr->GetArgument(0), 
                                           irGen.GetInt8Pointer(), destOp);
                block->InsertInstructionBefore(instr, callInstr);
				callInstr->ReplaceArgument(0, destOp);
                break;
            }
            case Stdlib_strcpy: {
                TryToPromoteStrcpy(callInstr, unit, &irGen);
                break;
            }
            case Stdlib_strncpy: {
                TryToPromoteStrcpy(callInstr, unit, &irGen, true);
                break;
            }
            case Stdlib_abs: {
                auto intrinsic = Abs32Intr::GetAbs32(unit);
                callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                break;
            }
            case Stdlib_fabs: {
                // We can promote math functions only if we're allowed
                // (the user opted for "fast" floating point math, for example).
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = FabsIntr::GetFabs(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
            case Stdlib_labs: {
                auto intrinsic = Abs64Intr::GetAbs64(unit);
                callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                break;
            }
            case Stdlib_atan: {
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = AtanIntr::GetAtan(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
            case Stdlib_atan2: {
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = Atan2Intr::GetAtan2(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
            case Stdlib_cos: {
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = CosIntr::GetCos(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
            case Stdlib_sin: {
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = SinIntr::GetSin(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
            case Stdlib_tan: {
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = TanIntr::GetTan(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
            case Stdlib_exp: {
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = ExpIntr::GetExp(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
            case Stdlib_sqrt: {
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = SqrtIntr::GetSqrt(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
            case Stdlib_sqrtf: {
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = SqrtfIntr::GetSqrtf(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
            case Stdlib_pow: {
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = PowIntr::GetPow(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
            case Stdlib_log: {
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = LogIntr::GetLog(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
            case Stdlib_log10: {
                if(callInstr->IsPromotableMathFunction()) {
                    auto intrinsic = Log10Intr::GetLog10(unit);
                    callInstr->SetTargetOp(irGen.GetFunctionRef(intrinsic));
                }
                break;
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IntrinsicPromotion::PromoteQuestion(QuestionInstr* instr) {
    auto block = instr->ParentBlock();

    if(auto result = DetectAbs(instr)) {
        block->InsertInstructionAfter(result->DefiningInstruction(), instr);
        instr->ResultOp()->ReplaceWith(result);
        IntrinsicDetected(instr, "abs");
        return;
    }

    if(auto result = DetectMin(instr)) {
        block->InsertInstructionAfter(result->DefiningInstruction(), instr);
        instr->ResultOp()->ReplaceWith(result);
        IntrinsicDetected(instr, "min");
        return;
    }

    if(auto result = DetectMax(instr)) {
        block->InsertInstructionAfter(result->DefiningInstruction(), instr);
        instr->ResultOp()->ReplaceWith(result);
        IntrinsicDetected(instr, "max");
        return;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IntrinsicPromotion::PromoteMemcpy(CallInstr* callInstr, Unit* unit,
                                       IRGenerator* irGen) {
    auto block = callInstr->ParentBlock();
    auto intrinsic = CopyMemoryIntr::GetCopyMemory(unit);
    callInstr->SetTargetOp(irGen->GetFunctionRef(intrinsic));

	// The standard 'memcpy' has parameters of type 'void*', which in the IR
	// are represented as 'int64*', while the 'CopyMemory' intrinsic 
	// uses 'int8*' parameters. Redundant casts will be simplified later.
	auto destOp = irGen->GetTemporary(irGen->GetInt8Pointer());
	auto ptop1 = irGen->GetPtop(callInstr->GetArgument(0), 
                                irGen->GetInt8Pointer(), destOp);
    block->InsertInstructionBefore(ptop1, callInstr);
	callInstr->ReplaceArgument(0, destOp);

	auto sourceOp = irGen->GetTemporary(irGen->GetInt8Pointer());
	auto ptop2 = irGen->GetPtop(callInstr->GetArgument(1),
                                irGen->GetInt8Pointer(), sourceOp);
    block->InsertInstructionBefore(ptop2, callInstr);
	callInstr->ReplaceArgument(1, sourceOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IntrinsicPromotion::PromoteMemmove(CallInstr* callInstr, Unit* unit,
                                        IRGenerator* irGen) {
    // If we're copying from two different variables we know their
    // storage can't overlap, so we promote it to 'CopyMemory'.
    auto variableRefA = callInstr->GetArgument(0)->As<VariableReference>();
    auto variableRefB = callInstr->GetArgument(1)->As<VariableReference>();

    if((variableRefA && variableRefB) == false) {
        return;
    }

    if(variableRefA->GetVariable() != variableRefB->GetVariable()) {
        return;
    }

    // Now use the intrinsic.
    auto intrinsic = CopyMemoryIntr::GetCopyMemory(unit);
    callInstr->SetTargetOp(irGen->GetFunctionRef(intrinsic));

    // The standard 'memove' has parameters of type 'void*', which in the IR
	// are represented as 'int64*', while the 'MoveMemory' intrinsic 
	// uses 'int8*' parameters. Redundant casts will be simplified later.
    auto block = callInstr->ParentBlock();
	auto destOp = irGen->GetTemporary(irGen->GetInt8Pointer());
	auto ptop1 = irGen->GetPtop(callInstr->GetArgument(0), 
                                irGen->GetInt8Pointer(), destOp);
    block->InsertInstructionBefore(ptop1, callInstr);
	callInstr->ReplaceArgument(0, destOp);

	auto sourceOp = irGen->GetTemporary(irGen->GetInt8Pointer());
	auto ptop2 = irGen->GetPtop(callInstr->GetArgument(1),
                                irGen->GetInt8Pointer(), sourceOp);
    block->InsertInstructionBefore(ptop2, callInstr);
	callInstr->ReplaceArgument(1, sourceOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IntrinsicPromotion::TryToPromoteStrcpy(CallInstr* callInstr, Unit* unit,
                                            IRGenerator* irGen, bool isStrncpy) {
	if(callInstr->HasDestinationOp() &&
	   callInstr->GetDestinationOp()->HasUsers()) {
		// 'strcpy' returns the destination, while the intrinsic
		// returns nothing; in this case we can't promote.
		return;
	}

    // If we're copying from a constant global variable we can
    // promote it to 'CopyMemory'.
    auto variableRef = callInstr->GetArgument(1)->As<VariableReference>();

    if((variableRef == nullptr) ||
       (variableRef->IsGlobalVariableRef() == false)) {
        return;
    }

    auto globalVar = variableRef->GetGlobal();

    if((globalVar->HasInitializer() == false) ||
       (globalVar->IsConstant() == false)) {
        return;
    }

    // The type of the variable is 'array of int8', and it's
    // terminated with 0, so the array size is exactly
    // the amount of memory we need to copy.
    auto arrayType = globalVar->GetType()->As<ArrayType>();

    if(arrayType->ElementType()->IsInt8() == false) {
        return;
    }

    // For now we presume we need to copy the entire array (not true for 'strncpy').
    __int64 copySize = arrayType->Size();

    // If this is 'strncpy' we need to copy only the first 'n' characters.
    // Note that we can do this only if 'n' is smaller than the length
    // of the string, because if it's not the 0 character must be used 
    // until we have 'n' characters, something that 'CopyMemory' is not able to do.
    if(isStrncpy) {
        auto constantOp = callInstr->GetArgument(2)->As<IntConstant>();
        
        if(constantOp == nullptr) {
            return;
        }

        // Give up if 'n' is larger than the length of the string.
        OperandInfo opInfo;
        int strlen = opInfo.GetStringLength(variableRef);

        if(constantOp->Value() > strlen) {
            return;
        }

        // We need to copy only 'n' characters.
        copySize = constantOp->Value();
    }

    // Now use the intrinsic.
    auto intrinsic = CopyMemoryIntr::GetCopyMemory(unit);
    callInstr->SetTargetOp(irGen->GetFunctionRef(intrinsic));

    // Add an argument that specified the amount of memory to copy.
    auto sizeOp = irGen->GetInt64Const(copySize);

    if(isStrncpy) callInstr->ReplaceArgument(2, sizeOp);
    else callInstr->AddArgument(sizeOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* IntrinsicPromotion::DetectAbs(QuestionInstr* instr) {
    // There are three forms that we detect here:
    // a < 0 ? -a : a          a >= 0 ? a : -a         a >= 0 ? a : -a (after GVN)
    //                                                 
    // t1 = cmp lt a, 0        t1 = cmp gte a, 0       t1 = cmp lt -1, a
    // t2 = sub 0, a           t2 = sub 0, a           t2 = sub 0, a
    // t3 = quest t1, t2, a    t3 = quest t1, a, t2    t3 = quest t1, a, t2
    auto condInstr = instr->ConditionOp()->DefiningInstruction();

    if((condInstr == nullptr) ||
       ((condInstr->IsCmp() || condInstr->IsUcmp()) == false)) {
        // A compare instruction should be used.
        return nullptr;
    }

    // Check if we have one of the above cases.
    auto cmpInstr = condInstr->As<CmpInstrBase>();
    Operand* a;
    Operand* b;

    if(cmpInstr->IsLess() && 
       cmpInstr->RightOp()->IsZeroInt()) {
        a = cmpInstr->LeftOp();

        if(Match<SubInstr>(MatchIC(0), MatchOp(a))(instr->TrueOp()) &&
           (instr->FalseOp() == a)) {
           return CreateAbs(a, cmpInstr->IsCmp(), 
                            instr->ParentFunction()->ParentUnit());
        }
    }
    else if(cmpInstr->IsGreaterOrEqual() && 
            cmpInstr->RightOp()->IsZeroInt()) {
        a = cmpInstr->LeftOp();

        if(Match<SubInstr>(MatchIC(0), MatchOp(a))(instr->FalseOp()) &&
           (instr->TrueOp() == a)) {
            return CreateAbs(a, cmpInstr->IsCmp(), 
                             instr->ParentFunction()->ParentUnit());
        }
    }
    else if(cmpInstr->IsLess() && 
            cmpInstr->LeftOp()->IsMinusOneInt()) {
        a = cmpInstr->RightOp();

        if(Match<SubInstr>(MatchIC(0), MatchOp(a))(instr->FalseOp()) &&
           (instr->TrueOp() == a)) {
            return CreateAbs(a, cmpInstr->IsCmp(), 
                             instr->ParentFunction()->ParentUnit());
        }
    }

    // Another form of abs found sometimes is the following:
    // a - b < 0 ? b - a : a - b
    if(cmpInstr->IsLess() && cmpInstr->RightOp()->IsZeroInt()) {
        auto cmpSub = cmpInstr->LeftOp()->DefiningInstrAs<SubInstr>();
        auto trueSub = instr->TrueOp()->DefiningInstrAs<SubInstr>();
        auto falseSub = instr->FalseOp()->DefiningInstrAs<SubInstr>();

        if(cmpSub && trueSub && falseSub) {
            a = cmpSub->LeftOp();
            b = cmpSub->RightOp();

            if((trueSub->LeftOp() == b) &&
               (trueSub->RightOp() == a) &&
               (falseSub->LeftOp() == a) &&
               (falseSub->RightOp() == b)) {
                return CreateAbs(cmpInstr->LeftOp(), cmpInstr->IsCmp(), 
                                 instr->ParentFunction()->ParentUnit());
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* IntrinsicPromotion::CreateAbs(Operand* op, bool isSigned, Unit* unit) {
    auto intType = op->GetType()->As<IntegerType>();
    DebugValidator::IsNotNull(intType);
    Intrinsic* absIntr;

    switch(intType->GetSubtype()) {
        case IR_Int8: {
            absIntr = Abs8Intr::GetAbs8(unit);
            break;
        }
        case IR_Int16: {
            absIntr = Abs16Intr::GetAbs16(unit);
            break;
        }
        case IR_Int32: {
            absIntr = Abs32Intr::GetAbs32(unit);
            break;
        }
        case IR_Int64: {
            absIntr = Abs64Intr::GetAbs64(unit);
            break;
        }
        default: DebugValidator::Unreachable();
    }

    auto signOp = isSigned ? unit->Constants().GetInt32(1) :
                             unit->Constants().GetInt32(0);
    auto resultOp = Temporary::GetTemporary(intType);
    auto absCall = CreateIntrinsicCall(absIntr, resultOp);

    absCall->AddArgument(op);
    absCall->AddArgument(signOp);
    return resultOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* IntrinsicPromotion::DetectMin(QuestionInstr* instr) {
    // This are the only forms that we are interested in,
    // because the >/>= variants are transformed into these ones.
    // a </<= b ? a : b
    //
    // t1 = cmp lt a, b
    // t2 = quest t1, a, b
    auto condInstr = instr->ConditionOp()->DefiningInstruction();

    if((condInstr == nullptr) ||
       ((condInstr->IsCmp() || condInstr->IsUcmp()) == false)) {
        // A compare instruction should be used.
        return nullptr;
    }

    // Check if we have one of the above cases.
    auto cmpInstr = condInstr->As<CmpInstrBase>();

    if(cmpInstr->IsLess() || cmpInstr->IsLessOrEqual()) {
        auto a = cmpInstr->LeftOp();
        auto b = cmpInstr->RightOp();

        if((instr->TrueOp() == a) &&
           (instr->FalseOp() == b)) {
            return CreateMin(a, b, cmpInstr->IsCmp(), 
                             instr->ParentFunction()->ParentUnit());
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* IntrinsicPromotion::CreateMin(Operand* opA, Operand* opB, 
                                       bool isSigned, Unit* unit) {
    auto intType = opA->GetType()->As<IntegerType>();
    DebugValidator::IsNotNull(intType);
    Intrinsic* minIntr;

    switch(intType->GetSubtype()) {
        case IR_Int8: {
            minIntr = Min8Intr::GetMin8(unit);
            break;
        }
        case IR_Int16: {
            minIntr = Min16Intr::GetMin16(unit);
            break;
        }
        case IR_Int32: {
            minIntr = Min32Intr::GetMin32(unit);
            break;
        }
        case IR_Int64: {
            minIntr = Min64Intr::GetMin64(unit);
            break;
        }
        default: DebugValidator::Unreachable();
    }

    auto signOp = isSigned ? unit->Constants().GetInt32(1) :
                             unit->Constants().GetInt32(0);
    auto resultOp = Temporary::GetTemporary(intType);
    auto minCall = CreateIntrinsicCall(minIntr, resultOp);

    minCall->AddArgument(opA);
    minCall->AddArgument(opB);
    minCall->AddArgument(signOp);
    return resultOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* IntrinsicPromotion::DetectMax(QuestionInstr* instr) {
    // This are the only forms that we are interested in,
    // because the >/>= variants are transformed into these ones.
    // a </<= b ? a : b
    //
    // t1 = cmp lt a, b
    // t2 = quest t1, b, a
    auto condInstr = instr->ConditionOp()->DefiningInstruction();

    if((condInstr == nullptr) ||
       ((condInstr->IsCmp() || condInstr->IsUcmp()) == false)) {
        // A compare instruction should be used.
        return nullptr;
    }

    // Check if we have one of the above cases.
    auto cmpInstr = condInstr->As<CmpInstrBase>();

    if(cmpInstr->IsLess() || cmpInstr->IsLessOrEqual()) {
        auto a = cmpInstr->LeftOp();
        auto b = cmpInstr->RightOp();

        if((instr->TrueOp() == b) &&
           (instr->FalseOp() == a)) {
            return CreateMax(a, b, cmpInstr->IsCmp(), 
                             instr->ParentFunction()->ParentUnit());
        }
    }

    // There is another form of 'max' that we can detect.
    // (a > 4) ? a : 5  ->  (a >= 5) ? a : 5  ->  max(a, 5)
    if(cmpInstr->IsGreaterOrEqual() && 
       cmpInstr->RightOp()->IsIntConstant() &&
       (cmpInstr->RightOp() == instr->FalseOp()) && 
       (cmpInstr->LeftOp() == instr->TrueOp())) {
        return CreateMax(instr->TrueOp(), instr->FalseOp(), cmpInstr->IsCmp(), 
                         instr->ParentFunction()->ParentUnit());
    }

    if(cmpInstr->IsGreater()) {
        auto rightConst = cmpInstr->RightOp()->As<IntConstant>();
        auto falseConst = instr->FalseOp()->As<IntConstant>();

        if(rightConst && falseConst &&
           (falseConst->Value() == (rightConst->Value() + 1)) &&
           (cmpInstr->LeftOp() == instr->TrueOp())) {
            return CreateMax(instr->TrueOp(), instr->FalseOp(), cmpInstr->IsCmp(), 
                             instr->ParentFunction()->ParentUnit());
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* IntrinsicPromotion::CreateMax(Operand* opA, Operand* opB, 
                                       bool isSigned, Unit* unit) {
    auto intType = opA->GetType()->As<IntegerType>();
    DebugValidator::IsNotNull(intType);
    Intrinsic* maxIntr;

    switch(intType->GetSubtype()) {
        case IR_Int8: {
            maxIntr = Max8Intr::GetMax8(unit);
            break;
        }
        case IR_Int16: {
            maxIntr = Max16Intr::GetMax16(unit);
            break;
        }
        case IR_Int32: {
            maxIntr = Max32Intr::GetMax32(unit);
            break;
        }
        case IR_Int64: {
            maxIntr = Max64Intr::GetMax64(unit);
            break;
        }
        default: DebugValidator::Unreachable();
    }

    auto signOp = isSigned ? unit->Constants().GetInt32(1) :
                             unit->Constants().GetInt32(0);
    auto resultOp = Temporary::GetTemporary(intType);
    auto maxCall = CreateIntrinsicCall(maxIntr, resultOp);

    maxCall->AddArgument(opA);
    maxCall->AddArgument(opB);
    maxCall->AddArgument(signOp);
    return resultOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
CallInstr* IntrinsicPromotion::CreateIntrinsicCall(Intrinsic* intrinsic, Operand* resultOp) {
    auto unit = intrinsic->ParentUnit();
    auto intrType = unit->Types().GetPointer(intrinsic->GetType());
    auto intrRef = unit->References().GetFunctionRef(intrinsic, intrType);
    return CallInstr::GetCall(intrRef, resultOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void IntrinsicPromotion::IntrinsicDetected(Instruction* instr, const string& name) {
#if 1
	auto block = instr->ParentBlock();
	auto function = instr->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string text = IRPrinter(instr).ToString();
	Log::Warning("Intrinsic promotion in " + functionName + ":" + blockName +
				 ": " +  text + " => " + name);
#endif
}

} // namespace Optimization