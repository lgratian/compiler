// IndirectCallPromotion.hpp
// Copyright (c) Lup Gratian
//
// Replaces calls through a pointer with a call/a series of calls
// to known functions, as determined by the call graph information.
// Doing this replacement allows us to inline more functions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_INDIRECT_CALL_PROMOTION_HPP
#define PC_OPTIMIZATION_INDIRECT_CALL_PROMOTION_HPP

#include "BlockUtils.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Analysis/CallGraph.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class IndirectCallPromotion : public Pass {
private:
    typedef List<CallEdge> CallEdgeList;
    typedef List<CallInstr*> CallList;
    typedef List<Block*> BlockList;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // The maximum number of call targets that are promoted.
    // For each one a test is created.
    static const int MAX_PROMOTED_TARGETS = 4;

    CallGraph* callGraph_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Returns the single node called by the specified call site,
    // or 'nullptr' if there are more targeted nodes.
    CallNode* GetSingleTarget(CallSite* callSite);

    // Adds to the 'edges' list all target nodes that are not
    // the External or Unknown nodes.
    void CollectTargets(CallSite* callSite, CallEdgeList& edges);

    // Promotes a call from a call through pointer to a single
    // or more calls to known functions.
    void PromoteCall(CallInstr* instr, CallSite* callSite, int knownTargets);

    // Promotes a call from a call through pointer to a series
    // of calls to known functions, each having a test associated.
    void PromoteCallWithMultipleTargets(CallInstr* instr, CallSite* callSite,
                                        CallEdgeList& targetEdges, int promotedTargets, 
                                        bool hasUnpromotedTargets);

    // Generates 'if' instructions that connect the test blocks
    // with the blocks containing the actual calls.
    void ConnectGeneratedBlocks(BlockList& testBlocks, BlockList& callBlocks);

    // Creates a 'phi' instruction that merges the returned values
    // from the newly created calls and replaces the old value.
    void MergeReturnedValues(BlockList& callBlocks, Block* continuationBlock,
                             CallInstr* instr);

    // Updates the call graph after new calls were generated.
    void UpdateCallGraph(CallList& createdCalls, CallInstr* originalInstr);

    // Helpers for creating various instructions and blocks.
    UcmpInstr* CreateTargetComparison(Operand* targetOp, 
                                      FunctionReference* functionRef);

    Block* CreateTestBlock(SymbolTable& symbols, Block* previousBlock);

    Block* CreateCallBlock(SymbolTable& symbols, Block* previousBlock);

    CallInstr* CreateTargetCall(Operand* targetOp, CallInstr* clonedInstr);

    GotoInstr* CreateGoto(Block* targetBlock);

public:
    void Execute(CallGraph* callGraph);
};

} // namespace Optimization
#endif