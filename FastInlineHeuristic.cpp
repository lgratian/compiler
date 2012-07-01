// InlineHeuristic.cpp
// Copyright (c) Lup Gratian
//
// Implements the FastInlineHeuristic class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "FastInlineHeuristic.hpp"

namespace Analysis {

int FastInlineHeuristic::GetInliningCost(Function* function) {
    // First we check if we already computed the cost.
    // If not we compute it now and add it to the cache.
    DebugValidator::IsNotNull(function);
    InlineInfo info;
    AnalyzeFunction(function, info);
    return info.Cost;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int FastInlineHeuristic::GetInliningBenefit(Function* function, 
                                            ArgumentList* specializationArguments) {
    DebugValidator::IsNotNull(function);
    InlineInfo info;
    AnalyzeFunction(function, info);

    // There is a benefit when we inline any type of call,
    // because we eliminate the call and at least one return instruction.
    // There is also benefit for each parameter that will be eliminated.
    int benefit = Adjust(CALL_BENEFIT);

    if(specializationArguments) {
        for(int i = 0; i < specializationArguments->Count(); i++) {
            auto argument = (*specializationArguments)[i];
        
            if(argument->IsConstant()) {
                benefit += GetConstantParameterBenefit(i, argument, info, function);
            }
            else benefit += GetParameterBenefit(i, function);
        }
    }
    else {
        for(int i = 0; i < function->ParameterCount(); i++) {
            benefit += GetParameterBenefit(i, function);
        }
    }

    // A tail call is a very good candidate for inlining,
    // so we boost the benefit.
    if((info.CallCount - info.LightweightCallCount) == 0) {
        benefit *= TAIL_CALL_BENEFIT_FACTOR;
    }

    return benefit;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int FastInlineHeuristic::GetAverageInstructionsPerBlock(Function* function) {
    DebugValidator::IsNotNull(function);
    InlineInfo info;
    AnalyzeFunction(function, info);
    return info.AverageInstrsPerBlock;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int FastInlineHeuristic::GetCallCount(Function* function, bool ignoreLightweight) {
    DebugValidator::IsNotNull(function);

    InlineInfo info;
    AnalyzeFunction(function, info);
    
    if(ignoreLightweight) {
        return info.CallCount - info.LightweightCallCount;
    }
    else return info.CallCount;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FastInlineHeuristic::Invalidate(Function* function) {
    DebugValidator::IsNotNull(function);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FastInlineHeuristic::AnalyzeFunction(Function* function, InlineInfo& info) {
    // First we check if we already computed the cost.
    // If not we compute it now and add it to the cache.
    if(functionInlineInfo_.TryGetValue(function->Id(), &info)) {
        return;
    }

    // Here we compute the cost of inlining the specified function.
    // The cost is equal to the sum of the estimated size
    // of each instruction. At the same time we count how many
    // calls we have, and which parameters are used by the instructions.
    info.Cost = 0;
    info.CallCount = 0;
    info.LightweightCallCount = 0;
    info.AverageInstrsPerBlock = 0;
    
    for(int i = 0; i < function->ParameterCount(); i++) {
        info.ParameterUsers.Add(0);
        info.ParameterBranchingUsers.Add(0);
    }

    // Build a map from parameter to its index
    // in order to do a fast lookup.
    Dictionary<Parameter*, int> paramToIndex;

    for(int i = 0; i < function->ParameterCount(); i++) {
        paramToIndex.Add(function->GetParameter(i), i);
    }

    // We use an incremental algorithm to compute
    // the average number of instructions/block.
    float average = 0;
    int blockCount = 0;

    for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
        for(auto instr = block->FirstInstruction(); instr; 
            instr = instr->NextInstruction()) {
            int weight = EstimateInstructionWeight(instr);
            info.Cost += weight;

            // Verify if we have a 'call'.
            if(auto callInstr = instr->As<CallInstr>()) {
                info.CallCount++;
                
                if(IsLightweightCall(callInstr)) {
                    info.LightweightCallCount++;
                }
            }

            // Check if one of the parameters is used by the instruction.
            if(instr->IsIf() || instr->IsSwitch()) {
                TParameterList usedParameters;
                GetConditionParameters(instr->GetSourceOp(0), usedParameters);
                
                // We estimate that only one of the successors
                // will "survive" after inlining.
                for(int i = 0; i < usedParameters.Count(); i++) {
                    int paramIndex = paramToIndex[usedParameters[i]];
                    info.ParameterBranchingUsers[paramIndex] += 
                        block->SuccessorCount() - 1;
                }
            }
            else {
                // Check which parameters are used by the instruction.
                for(int i = 0; i < instr->SourceOpCount(); i++) {
                    if(auto parameter = instr->GetSourceOp(i)->As<Parameter>()) {
                        int paramIndex = paramToIndex[parameter];
                        info.ParameterUsers[paramIndex] += weight;
                    }
                }
            }
        }

        // Update the average instruction count/block.
        average = UpdateAverageInstrsPerBlock(block->InstructionCount(), 
                                              average, blockCount++);
    }

    info.AverageInstrsPerBlock = std::max(1, (int)average);
    functionInlineInfo_.Add(function->Id(), info);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int FastInlineHeuristic::GetParameterBenefit(int paramIndex, Function* function) {
    // We estimate that after inlining for each parameter
    // we reduce the instruction count by 2 (a push/pop pair).
    // If the parameter is used to pass a record "by value" we give it
    // a larger benefit, because inlining favors other transformations
    // (scalar replacement of aggregates, for example).
    // We do the same for pointers that behave like arrays.
    auto paramVariable = function->GetParameterVariable(paramIndex);

    if(auto pointerType = paramVariable->GetType()->As<PointerType>()) {
        if(pointerType->PointeeType()->IsRecord()) {
            if(paramVariable->IsNoWrite() &&
               paramVariable->IsNoEscape()) {
                return Adjust(VALUE_RECORD_PARAMETER_BENEFIT);
            }
            else return Adjust(RECORD_PARAMETER_BENEFIT);
        }
        else if(pointerType->PointeeType()->IsInteger() &&
                (pointerType->PointeeType()->IsInt8() == false)) {
            return Adjust(ARRAY_PARAMETER_BENEFIT);
        }
    }

    return Adjust(PARAMETER_BENEFIT);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int FastInlineHeuristic::GetConstantParameterBenefit(int paramIndex, Operand* constantOp,
                                                     InlineInfo& info, Function* function) {
    // A parameter that is replaced by a constant can lead to
    // the constant folding of most instructions that use it.
    // If it is used by branching instructions it can decide
    // the destination, rendering the other destinations dead.
    int benefit = GetParameterBenefit(paramIndex, function);

    benefit += info.ParameterUsers[paramIndex] * 
               GetSpecialBenefitFactor(constantOp) / 2; // 50% chance of folding.

    benefit += info.ParameterBranchingUsers[paramIndex] *
               info.AverageInstrsPerBlock *
               AverageInstructionWeight();
    return benefit;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int FastInlineHeuristic::GetSpecialBenefitFactor(Operand* op) {
    // Parameters that are replaced by 0, 1 and -1 usually lead
    // to the simplification of many other instructions.
    if(op->IsZeroInt() || op->IsOneInt()  || 
       op->IsMinusOneInt() || op->IsNullConstant()) {
        return SPECIAL_PARAMETER_BENEFIT_FACTOR;
    }
    else if(auto intConst = op->As<IntConstant>()) {
        // Instructions using powers of two constants
        // usually can be strength reduced (multiply -> shift, for example).
        if(IntArithmetic::IsPowerOfTwo(intConst)) {
            return POW2_PARAMETER_BENEFIT_FACTOR;
        }
    }
    
    return 1;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
float FastInlineHeuristic::UpdateAverageInstrsPerBlock(float value, float previousValue,
                                                       int count) {
    // We use a formula that updates the average incrementally.
    return previousValue + ((value - previousValue) / (count + 1));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FastInlineHeuristic::GetConditionParameters(Operand* op, TParameterList& list,
                                                 int level) {
    if(auto parameter = op->As<Parameter>()) {
        list.Add(parameter);
    }
    else if(level == 2) {
        return;
    }
    else if(auto definingInstr = op->DefiningInstruction()) {
        for(int i = 0; i < definingInstr->SourceOpCount(); i++) {
            GetConditionParameters(definingInstr->GetSourceOp(i), list, level + 1);
        }
    }
}

} // namespace Analysis