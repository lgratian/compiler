// InlineHeuristic.hpp
// Copyright (c) Lup Gratian
//
// Base class for the heuristics that are used to drive the inliner.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_INLINE_HEURISTIC_HPP
#define PC_ANALYSIS_INLINE_HEURISTIC_HPP

#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Base/Log.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class InlineHeuristic {
protected:
    typedef CallInstr::ArgumentList ArgumentList;

    // Constants used to compute the weight of a function.
    // They estimate the size of the instruction, and we take
    // into consideration the fact that constant operands
    // sometimes make the generated instruction smaller.
    static const int AVERAGE_WEIGHT = 2;
    static const int ARITHMETIC_WEIGHT = 2;
    static const int ARITHMETIC_WEIGHT_CONST = 1;
    static const int CONVERSION_WEIGHT = 2;
    static const int CONVERSION_WEIGHT_EXT_TRUNC = 1;
    static const int CONVERSION_WEIGHT_POINTER = 0;
    static const int LOGICAL_WEIGHT = 2;
    static const int LOGICAL_WEIGHT_CONST = 1;
    static const int COMPARE_WEIGHT = 2;
    static const int COMPARE_WEIGHT_CONST = 1;
    static const int COMPARE_WEIGHT_ZERO = 0;
    static const int ADDRESS_INDEX_WEIGHT = 4;
    static const int ADDRESS_INDEX_WEIGHT_CONST = 2;
    static const int ELEMENT_WEIGHT = 2;
    static const int QUESTION_WEIGHT = 8;
    static const int QUESTION_WEIGHT_CONST = 4;
    static const int QUESTION_WEIGHT_ZERO_ONE = 2;
    static const int PHI_INCOMING_WEIGHT = 2;
    static const int CALL_WEIGHT = 3;
    static const int CALL_ARGUMENT_WEIGHT = 2;
    static const int BITWISE_INTRINSIC_WEIGHT = 2;
    static const int MIN_MAX_ABS_INTRINSIC_WEIGHT = 3;
    static const int COPY_MEMORY_INTRINSIC_WEIGHT_CONST = 4;
    static const int SET_MEMORY_INTRINSIC_WEIGHT_CONST = 2;
    static const int MATH_INTRINSIC_WEIGHT = 7;
    static const int GOTO_WEIGHT = 2;
    static const int IF_WEIGHT = 4;
    static const int SWITCH_WEIGHT = 4;
    static const int SWITCH_CASE_WEIGHT = 3;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Should return 'true' if the specified 'call' targets
    // an intrinsic that is lowered to few instructions.
    virtual bool IsLightweightCall(CallInstr* instr);

    // Should return an estimate of the size of the code needed
    // to set up the parameters and call the specified function.
    virtual int EstimateCallWeight(CallInstr* instr);

    // Should return an estimate of the size of the instruction.
    virtual int EstimateInstructionWeight(Instruction* instr);

    virtual int AverageInstructionWeight() {
        return AVERAGE_WEIGHT;
    }

public:
    // Should return the cost for inlining the specified function,
    // cost usually measured in terms of code size increase.
    virtual int GetInliningCost(Function* function) = 0;

    // Should return the benefit for inlining the function using
    // the specified arguments to replace the parameters.
    // The benefit is usually measured in terms of code size reduction
    // and optimization opportunities that are enabled by the inlining.
    virtual int GetInliningBenefit(Function* function, 
                                   ArgumentList* specializationArguments) = 0;

    // Should returns an estimate of the number of instructions/block
    // as an average for the entire function.
    virtual int GetAverageInstructionsPerBlock(Function* function) = 0;

    // Should return the number of calls that are made by the function.
    // If 'ignoreLightweight' is set calls to intrinsics, for example,
    // should not be counted.
    virtual int GetCallCount(Function* function, bool ignoreLightweight = false) = 0;

    // Called when a function was modified after inlining information
    // was requested for it. Can be used to invalidate a possible
    // caching mechanism.
    virtual void Invalidate(Function* function) = 0;
};

} // namespace Analysis
#endif