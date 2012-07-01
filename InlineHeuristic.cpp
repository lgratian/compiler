// InlineHeuristic.cpp
// Copyright (c) Lup Gratian
//
// Implements the InlineHeuristic class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "InlineHeuristic.hpp"

namespace Analysis {

bool InlineHeuristic::IsLightweightCall(CallInstr* instr) {
    DebugValidator::IsNotNull(instr);

    // We consider calls to some intrinsic to be "lightweight",
    // because they are usually lowered to a few target instructions.
    auto intrinsic = instr->GetIntrinsic();
    if(intrinsic == nullptr) return false;

    if(intrinsic->IsBitwiseIntrinsic() || 
       intrinsic->IsStackIntrinsic() ||
       intrinsic->IsMathIntrinsic()) {
        return true;
    }
    else if(auto memoryIntrinsic = intrinsic->As<MemoryIntrinsic>()) {
        // If the number of bytes to copy is not known 
        // we need to emit a call, which we consider expensive.
        auto lengthIntConst = instr->GetArgument(2)->As<IntConstant>();
        if(lengthIntConst == nullptr) return false;

        // For 'copyMemory' more instructions need to be generated
        // than for 'setMemory'. 
        if(memoryIntrinsic->IsCopyMemoryIntr()) {
            return lengthIntConst->Value() < 32;
        }
        else return lengthIntConst->Value() < 64;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int InlineHeuristic::EstimateCallWeight(CallInstr* instr) {
    DebugValidator::IsNotNull(instr);

    // For a 'call' we need to count the weight of the call itself,
    // but also the code required to set up the parameters.
    // We assume that each argument takes a single instruction,
    // like the arithmetic ones (true on most targets).
    if(auto intrinsic = instr->GetIntrinsic()) {
        if(intrinsic->IsBitwiseIntrinsic()) {
            return BITWISE_INTRINSIC_WEIGHT;
        }
        else if(auto mathIntrinsic = intrinsic->As<MathIntrinsic>()) {
            if(mathIntrinsic->IsMin() ||
               mathIntrinsic->IsMax() ||
               mathIntrinsic->IsAbs()) {
                return MIN_MAX_ABS_INTRINSIC_WEIGHT;
            }
            else return MATH_INTRINSIC_WEIGHT;
        }
        else if(auto memoryIntrinsic = intrinsic->As<MemoryIntrinsic>()) {
            // A 'copyMemory'/'setMemory' with a constant length
            // is highly likely to be lowered to a a series of instructions,
            // instead of doing a regular call.
            if(auto lengthIntConst = instr->GetArgument(2)->As<IntConstant>()) {
                if(memoryIntrinsic->IsCopyMemoryIntr() &&
                   (lengthIntConst->Value() < 32)) {
                    return std::max(COPY_MEMORY_INTRINSIC_WEIGHT_CONST, 
                                    (int)(lengthIntConst->Value() / 8) * 
                                          COPY_MEMORY_INTRINSIC_WEIGHT_CONST);
                }
                else if(lengthIntConst->Value() < 64) {
                    return std::max(SET_MEMORY_INTRINSIC_WEIGHT_CONST, 
                                    (int)(lengthIntConst->Value() / 8) * 
                                          SET_MEMORY_INTRINSIC_WEIGHT_CONST);
                }
            }
        }
    }

    // Compute the weight of the call.
    return CALL_WEIGHT + (instr->ArgumentCount() * CALL_ARGUMENT_WEIGHT);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int InlineHeuristic::EstimateInstructionWeight(Instruction* instr) {
    DebugValidator::IsNotNull(instr);

    // We estimate the size of the code based on the type
    // of the instructions. We also take into consideration constant
    // operands, because these instructions can usually be lowered
    // to fewer target instructions.
    if(instr->IsArithmetic()) {
        // It's highly probable that an instruction that already
        // has a constant operand will be folded away after inlining.
        if(instr->GetSourceOp(0)->IsConstant() ||
           instr->GetSourceOp(1)->IsConstant()) {
            return ARITHMETIC_WEIGHT;
        }
        else return ARITHMETIC_WEIGHT_CONST;
    }
    else if(instr->IsLogical()) {
        if(instr->GetSourceOp(0)->IsConstant() ||
           instr->GetSourceOp(1)->IsConstant()) {
            return LOGICAL_WEIGHT;
        }
        else return LOGICAL_WEIGHT_CONST;
    }
    else if(instr->IsConversion()) {
        // Integer extensions usually don't need any instructions.
        if(instr->IsZext() || instr->IsSext() || instr->IsTrunc()) {
            return CONVERSION_WEIGHT_EXT_TRUNC;
        }
        else if(instr->IsPtoi() || instr->IsItop() || instr->IsPtop()) {
            return CONVERSION_WEIGHT_POINTER;
        }
        else return CONVERSION_WEIGHT;
    }
    else if(instr->IsComparison()) {
        IntConstant* intConst = instr->GetSourceOp(0)->As<IntConstant>();
        if(intConst == nullptr) intConst = instr->GetSourceOp(1)->As<IntConstant>();

        if(intConst) {
            // Comparisons with zero usually don't need any
            // instructions on most targets.
            if(intConst->IsZero()) {
                return COMPARE_WEIGHT_ZERO;
            }
            else COMPARE_WEIGHT_CONST;
        }
        else if(instr->GetSourceOp(0)->IsNullConstant() ||
                instr->GetSourceOp(1)->IsIntConstant()) {
            return COMPARE_WEIGHT_ZERO;
        }
        else COMPARE_WEIGHT;
    }
    else if(instr->IsAddress() || instr->IsIndex()) {
        // If we know the index we get rid of a multiplication.
        if(instr->GetSourceOp(1)->IsIntConstant()) {
            return ADDRESS_INDEX_WEIGHT_CONST;
        }
        else return ADDRESS_INDEX_WEIGHT;
    }
    else if(instr->IsElement()) {
        return ELEMENT_WEIGHT;
    }
    else if(auto questInstr = instr->As<QuestionInstr>()) {
        // 'quest' with 0/1 as both operands can usually
        // be converted to an integer extension.
        if((questInstr->TrueOp()->IsZeroInt() ||
            questInstr->TrueOp()->IsOneInt()  ||
            questInstr->TrueOp()->IsMinusOneInt()) &&
           (questInstr->FalseOp()->IsZeroInt() ||
            questInstr->FalseOp()->IsOneInt()  ||
            questInstr->FalseOp()->IsMinusOneInt())) {
            return QUESTION_WEIGHT_ZERO_ONE;
        }
        else if(questInstr->TrueOp()->IsConstant() &&
                questInstr->FalseOp()->IsConstant()) {
            return QUESTION_WEIGHT_CONST;
        }
        else return QUESTION_WEIGHT;
    }
    else if(auto phiInstr = instr->As<PhiInstr>()) {
        return phiInstr->OperandCount() * PHI_INCOMING_WEIGHT;
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        return EstimateCallWeight(callInstr);
    }
    else if(instr->IsGoto()) {
        return GOTO_WEIGHT;
    }
    else if(instr->IsIf()) {
        return IF_WEIGHT;
    }
    else if(auto switchInstr = instr->As<SwitchInstr>()) {
        return SWITCH_WEIGHT + (switchInstr->CaseCount() * SWITCH_CASE_WEIGHT);
    }

    return AVERAGE_WEIGHT;
}

} // namespace Analysis