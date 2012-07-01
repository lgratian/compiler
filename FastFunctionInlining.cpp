// FastFunctionInlining.cpp
// Copyright (c) Lup Gratian
//
// Implements the FastFunctionInlining class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "FastFunctionInlining.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

void FastFunctionInlining::Execute(Unit* unit) {
    DebugValidator::IsNotNull(unit);
    inlinedFunctions_ = 0;

    for(auto function = unit->Functions().First(); function; function = function->Next) {
#if 1
        Log::Error("@@@ INLINING IN @@@ " + *function->Value->Name());
#endif
        InlineCallees(function->Value);
    }

#if 1
    Log::Error(string::Format(L"INLINED   %d", inlinedFunctions_));
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* FastFunctionInlining::GetCalledFunction(CallInstr* instr) {
    DebugValidator::IsNotNull(instr);

    // To be inlined, the called function should be available
    // in this unit (i.e have a definition), and not contain
    // flags that indicate that we shouldn't touch inline it
    // (the presence of 'longjmp' in C, for example).
    auto calledFunct = instr->GetCalledFunction();
    
    if(calledFunct == nullptr) {
        return nullptr;
    }

    if(calledFunct->IsDefinition() == false) {
        return nullptr;
    }

    // Don't mess with varargs functions.
    if(calledFunct->IsVarargs()) {
        return nullptr;
    }

    // We don't inline recursive functions here, because
    // Tail Recursion Elimination usually does a better job.
    if(calledFunct == instr->ParentFunction()) {
        return nullptr;
    }

    // Check if it calls 'longjmp'. If it does we don't inline it.
    if(auto familyTag = calledFunct->GetTag<CFamilyTag>()) {
        if(familyTag->HasLongjmp()) {
            return nullptr;
        }
    }

    return calledFunct;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FastFunctionInlining::InlineCallees(Function* function, int depth) {
    // Don't inline at a too large depth, it's unlikely to be profitable.
    if(depth > MAXIMUM_INLINING_DEPTH) {
        return;
    }

    // If the function has been already processed we return.
    // This also prevents cycles in the call graph.
    if(processedFuncts_.IsSet(function->Id())) {
        return;
    }
    else processedFuncts_.SetBit(function->Id());

    // If we don't heave the body of the function (i.e this is
    // only a declaration), then we have nothing to do.
    if(function->IsDefinition() == false) {
        return;
    }

    // Before we inline the called functions we try
    // to inline the functions that the called ones call.
    // This is done recursively, ultimately leading to the inlining
    // of the leaves in the call graph first. This allows us
    // to compute a single time the inlining cost per function.
    StaticList<CallInstr*, 4> calls;
    SparseBitVector loopBlocks;

    function->ForEachInstruction([&calls](Instruction* instr) -> bool {
        if(auto callInstr = instr->As<CallInstr>()) {
            calls.Add(callInstr);
        }
        return true;
    });

    // Estimate which blocks are inside loops, because we allow
    // a higher inlining cost for calls found inside these blocks.
    if(calls.Count() > 0) {
        EstimateLoopBlocks(function, loopBlocks);
    }

    for(int i = 0; i < calls.Count(); i++) {
        // Check if we call a known function (it's possible
        // that the target is a pointer, in which case we don't
        // know here what it may call).
        InlineCallee(calls[i], function, loopBlocks, depth);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FastFunctionInlining::InlineCallee(CallInstr* callInstr, Function* caller,
                                        SparseBitVector& loopBlocks, int depth) {
    // Check if we call a known function (it's possible
    // that the target is a pointer, in which case we don't
    // know here what it may call).
    auto calledFunction = GetCalledFunction(callInstr);
    if(calledFunction == nullptr) return;

    // First try to inline the functions called by the target,
    // then we compute the cost/benefit and inline it.
    InlineCallees(calledFunction, depth + 1);
        
    if(ShouldInline(caller, calledFunction, callInstr, loopBlocks)) {
        // We let the Inliner do all the work.
        CallInlined(callInstr);
        Inliner inliner;
        inliner.InlineCall(callInstr);
        inlinedFunctions_++;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FastFunctionInlining::ShouldInline(Function* caller, Function* callee, 
                                        CallInstr* callInstr,
                                        SparseBitVector& loopBlocks) {
    // Look at inlining flags first, we should respect
    // the whishes of the user.
    if(callee->Inline() == Inline_Always) {
        return true;
    }
    else if(callee->Inline() == Inline_Never) {
        return false;
    }

    // We inline a function if the benefit is likely to outweigh
    // the cost (measured in code size growth). We allow the caller
    // to grow by a small amount. This amount is larger when the call
    // is found in a loop, because it's more likely that optimizations
    // will benefit from the inlining.
    int cost = heuristic_.GetInliningCost(callee);
    int benefit = heuristic_.GetInliningBenefit(callee, callInstr->Arguments());
    int maximumCost = MAXIMUM_COST;

    if(loopBlocks.IsSet(callInstr->ParentBlock()->Id())) {
        maximumCost *= LOOP_BOOST_FACTOR;
    }

#if 1
    Log::Warning(string::Format(L"Cost = %d  Benefit = %d", cost, benefit));
#endif
    return (cost - benefit) <= maximumCost;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FastFunctionInlining::EstimateLoopBlocks(Function* function, 
                                              SparseBitVector& loopBlocks) {
    // We estimate which are the blocks found in a loop
    // by using information provided by the frontend.
    // We look for 'goto' instructions marked with a "loop" flag,
    // and mark as part of the loop every block between the parent
    // of the 'goto' and its target block.
    auto block = function->LastBlock();

    while(block) {
        auto gotoInstr = block->BranchInstruction()->As<GotoInstr>();
        if((gotoInstr == nullptr) || 
           (gotoInstr->IsFromLoop() == false)) {
            block = block->PreviousBlock();
            continue;
        }
        
        // Mark the blocks that probably are in the loop.
        auto endBlock = gotoInstr->TargetOp()->Target();

        while(block && (block != endBlock)) {
            loopBlocks.SetBit(block->Id());
            block = block->PreviousBlock();
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FastFunctionInlining::CallInlined(CallInstr* instr) {
#if 1
	auto block = instr->ParentBlock();
	auto function = instr->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string text = IRPrinter(instr).ToString();
	Log::Warning("Call inlined in " + functionName + ":" + blockName +
				 ": " + text);
#endif
}

} // namespace Optimization