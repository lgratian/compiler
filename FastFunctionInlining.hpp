// FastFunctionInlining.hpp
// Copyright (c) Lup Gratian
//
// Implements a simple and fast inlining algorithm intended 
// to be run in the early phases of the compilation.
// It inlines starting with the leaves in the call graph,
// but doesn't use one (it works only at the Unit level).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_FAST_FUNCTION_INLINING_HPP
#define PC_OPTIMIZATION_FAST_FUNCTION_INLINING_HPP

#include "Inliner.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Constants.hpp"
#include "../Analysis/FastInlineHeuristic.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Analysis/CFamilyTag.hpp"
using namespace IR;
using namespace Analysis;

namespace Optimization {

class FastFunctionInlining {
private:
    // The maximum cost for which we still inline the callee.
    static const int MAXIMUM_COST = 30;
    static const int LOOP_BOOST_FACTOR = 5;
    static const int MAXIMUM_INLINING_DEPTH = 7;

    FastInlineHeuristic heuristic_;
    SparseBitVector processedFuncts_;
    int inlinedFunctions_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns the function that is the target of the 'call'
    // if it's known statically and it's valid to inline it.
	Function* GetCalledFunction(CallInstr* instr);

    // Inlines the called function if it's profitable.
    // Calls found inside a loop are more more likely to be inlined.
    void InlineCallee(CallInstr* callInstr, Function* caller, 
                      SparseBitVector& loopBlocks, int depth = 0);

    // Tries to inline the functions called by 'funct'.
    // The function is marked as processed to prevent cycles
    // in the call graph and to process it only once.
    void InlineCallees(Function* function, int depth = 0);

    // Returns 'true' if it's profitable to inline the callee
    // into the caller. We inline if the difference between the cost
    // and the benefit is below a threshold. The threshold is larger for
    // calls inside a loop because loop optimizations might be more effective.
    bool ShouldInline(Function* caller, Function* callee, 
                      CallInstr* callInstr,
                      SparseBitVector& loopBlocks);

    // Marks the blocks that might be part of a loop. 
    // It uses only information provided by the frontend.
    void EstimateLoopBlocks(Function* function, SparseBitVector& loopBlocks);

    void CallInlined(CallInstr* instr);

public:
    void Execute(Unit* unit);
};

} // namespace Optimization
#endif