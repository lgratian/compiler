// FastInlineHeuristic.hpp
// Copyright (c) Lup Gratian
//
// Implements a fast/minimal inlining heuristic that should be used
// to inline small functions in the early stages of optimization.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_FAST_INLINE_HEURISTIC_HPP
#define PC_ANALYSIS_FAST_INLINE_HEURISTIC_HPP

#include "InlineHeuristic.hpp"
#include "IntArithmetic.hpp"
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

class FastInlineHeuristic : public InlineHeuristic {
private:
    struct InlineInfo {
        int Cost;
        int AverageInstrsPerBlock;
        int CallCount;
        int LightweightCallCount;
        StaticList<int, 2> ParameterUsers;
        StaticList<int, 2> ParameterBranchingUsers;
    };

    // Constants used to compute the benefit obtained
    // by inlining a function. These values are represented as
    // multiples of the average instruction weight.
    static const int CALL_BENEFIT = 4;
    static const int TAIL_CALL_BENEFIT_FACTOR = 3;
    static const int PARAMETER_BENEFIT = 1;
    static const int ARRAY_PARAMETER_BENEFIT = 2;
    static const int RECORD_PARAMETER_BENEFIT = 4;
    static const int VALUE_RECORD_PARAMETER_BENEFIT = 6;
    static const int SPECIAL_PARAMETER_BENEFIT_FACTOR = 3; // 0, 1, -1
    static const int POW2_PARAMETER_BENEFIT_FACTOR = 2;

    typedef StaticList<Parameter*, 2> TParameterList;
    Dictionary<int, InlineInfo> functionInlineInfo_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Computes the cost of inlining the function and store
    // the result into the information object. At the same time
    // it computes the average instructions/block and the benefits
    // that can be obtained if a parameter is replaced by a constant.
    void AnalyzeFunction(Function* function, InlineInfo& info);

    // Returns an estimate of the benefit of eliminating
    // the parameter. Record/array parameters have a higher benefit
    // because more simplifications can be made after inlining.
    int GetParameterBenefit(int paramIndex, Function* function);

    // Returns an estimate of the benefit of eliminating
    // the parameter and replacing it by a constant.
    // The benefit is higher than for a standard parameter because
    // usually constant folding and algebraic simplifications 
    // can be applied after inlining.
    int GetConstantParameterBenefit(int paramIndex, Operand* constantOp,
                                    InlineInfo& info, Function* function);

    // Computes the average instructions/block
    // using an incremental algorithm.
    float UpdateAverageInstrsPerBlock(float value, float previousValue,
                                      int count);

    // A parameter replaced by 0, 1, or -1 usually leads
    // to many simplification opportunities after inlining,
    // so we boost the benefit.
    int GetSpecialBenefitFactor(Operand* op);

    // Adds to the list the parameters that are used
    // by a branching instructions condition. It looks through
    // one level of instructions.
    void GetConditionParameters(Operand* op, TParameterList& list, int level = 0);

    int Adjust(int value) {
        return value * AverageInstructionWeight();
    }

public:
    // Methods that implement the InlineHeuristic interface.
    virtual int GetInliningCost(Function* function);

    virtual int GetInliningBenefit(Function* function,
                                   ArgumentList* specializationArguments);

    virtual int GetAverageInstructionsPerBlock(Function* function);

    virtual int GetCallCount(Function* function, bool ignoreLightweight = false);

    virtual void Invalidate(Function* function);
};

} // namespace Analysis {
#endif