// IndirectCallPromotion.hpp
// Copyright (c) Lup Gratian
//
// Implements the IndirectCallPromotion pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IndirectCallPromotion.hpp"

namespace Optimization {

void IndirectCallPromotion::Execute(CallGraph* callGraph) {
    // Look at each call site and consider the ones
    // that call through a pointer. If we know any of the targets
    // we create specialized versions of the calls. For example,
    // if we known that 'p' can point to function 'a', 'b' or 'c'
    // we replace 'p(2)' with the following sequence:
    // if(p == a) a(2);
    // else if(p == b) b(2);
    // else c(2);
    callGraph_ = callGraph;
    auto& nodes = callGraph->GetCallNodes();

    for(int i = 0; i < nodes.Count(); i++) {
        auto node = nodes[i];

        // Ignore node groups.
        if(node->IsNodeGroup()) {
            continue;
        }

        // Process each call site.
        auto callNode = static_cast<CallNode*>(node);
        StaticList<CallSite*, 2> candidates;

        for(int j = 0; j < callNode->CallSiteCount(); j++) {
            // The call should be through a pointer.
            auto callSite = callNode->GetCallSite(j);
            auto callInstr = callSite->GetCallInstruction();

            if((callInstr == nullptr) ||
                callInstr->TargetOp()->IsFunctionReference()) {
                continue;
            }
            else candidates.Add(callSite);
        }

        for(int j = 0; j < candidates.Count(); j++) {
            auto callSite = candidates[j];

            // At least one target should be known
            // (we don't consider the Unknown and External nodes).
            int ignored = (callSite->CallsUnknownFunctions()  ? 1 : 0) +
                          (callSite->CallsExternalFunctions() ? 1 : 0);
            int knownTargets = callSite->CalledFunctionsCount() - ignored;

            if(knownTargets > 0) {
                PromoteCall(callSite->GetCallInstruction(),
                            callSite, knownTargets);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IndirectCallPromotion::PromoteCall(CallInstr* instr, CallSite* callSite, 
                                        int knownTargets) {
    // If the call has a single target we just replace the target
    // operand of the 'call' instruction. Otherwise tests need to be inserted.
    if(knownTargets == 1 && (callSite->CallsUnknownFunctions() == false)) {
        // Note that it might fail if a node group is called.
        if(auto singleNode = GetSingleTarget(callSite)) {
            instr->SetTargetOp(singleNode->GetFunctionReference());
        }

        // The call graph doesn't need to be updated in this case.
        return;
    }

    // Collect the targets as call edges. If no targets were found
    // it means that all are node groups, which we ignore.
    CallEdgeList targetEdges;
    CollectTargets(callSite, targetEdges);

    if(targetEdges.Count() == 0) {
        return;
    }

    // Sort the edges based on the weight (call frequency)
    // and consider only the first ones.
    targetEdges.Sort();
    int promotedTargets = std::min(targetEdges.Count(), MAX_PROMOTED_TARGETS);
    bool hasUnpromotedTargets = callSite->CallsUnknownFunctions() ||
                                (targetEdges.Count() > promotedTargets);

    // Select the promoted target edges and put them
    // in decreasing weight order so that the most likely target
    // is tested first.
    CallEdgeList selectedTargetEdges;

    for(int i = targetEdges.Count() - 1; i >= 0; i--) {
        if(selectedTargetEdges.Count() == promotedTargets) {
            break;
        }

        selectedTargetEdges.Add(targetEdges[i]);
    }

    PromoteCallWithMultipleTargets(instr, callSite, selectedTargetEdges,
                                   promotedTargets, hasUnpromotedTargets);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IndirectCallPromotion::PromoteCallWithMultipleTargets(CallInstr* instr,
                                                           CallSite* callSite,
                                                           CallEdgeList& targetEdges, 
                                                           int promotedTargets, 
                                                           bool hasUnpromotedTargets) {
    // Split the block that contains the 'call' before it.
    // If the called functions return values this block will contain
    // a 'phi' that combines them.
    auto originalBlock = instr->ParentBlock();
    auto& symbols = originalBlock->ParentFunction()->Symbols();
    auto continuationName = BlockUtils::CreateUniqueName("#indir_prom_cont", symbols);
    auto continuationBlock = instr->ParentBlock()->SplitAt(instr, continuationName);

    // Any incoming 'phi' operand from the original block
    // is now incoming from 'continuationBlock'.
    for(int i = 0; i < continuationBlock->SuccessorCount(); i++) {
        BlockUtils::ReplacePhiOperandsBlock(continuationBlock->SuccessorAt(i),
                                            originalBlock, continuationBlock);
    }

    // For each potential target we create a test block
    // and a block that contains the actual call. For the first target
    // the test block is not created (the block where the original call
    // was found is used) and for the last one too, but only if we have
    // no unpromoted targets. If we have we create an "else" block 
    // where we move the original call (otherwise the call is deleted).
    BlockList testBlocks;
    BlockList callBlocks;
    CallList createdCalls;
    Block* previousBlock; // Used to insert the blocks in order.

    // Remove the 'goto' to 'continuationBlock'.
    originalBlock->RemoveInstruction(originalBlock->LastInstruction());

    for(int i = 0; i < targetEdges.Count(); i++) {
        DebugValidator::IsFalse(targetEdges[i].Callee()->IsNodeGroup());
        auto targetCallNode = static_cast<CallNode*>(targetEdges[i].Callee());
        auto targetFunctionRef = targetCallNode->GetFunctionReference();

        // Create the test block.
        Block* testBlock = nullptr;
        auto targetComparison = CreateTargetComparison(instr->TargetOp(),
                                                       targetFunctionRef);

        if(i == 0) {
            // The first comparison is inserted in the original block.
            testBlock = originalBlock;
        }
        else if((i < (targetEdges.Count() - 1)) || hasUnpromotedTargets) {
            // Create a new block for the other comparisons.
            testBlock = CreateTestBlock(symbols, previousBlock);
        }

        if(testBlock) {
            testBlock->InsertInstruction(targetComparison);
            testBlocks.Add(testBlock);
            previousBlock = testBlock;
        }

        // Create the call block.
        Block* callBlock = CreateCallBlock(symbols, previousBlock);
        auto callInstr = CreateTargetCall(targetFunctionRef, instr);
        createdCalls.Add(callInstr);

        callBlock->InsertInstruction(callInstr);
        callBlock->InsertInstruction(CreateGoto(continuationBlock));
        callBlocks.Add(callBlock);
        previousBlock = callBlock;
    }

    // If there are calls to unpromoted targets (or to Unknown)
    // create a block where the original 'call' is cloned.
    // This block is executed if none of the known targets match.
    if(hasUnpromotedTargets) {
        auto unknownBlock = CreateCallBlock(symbols, previousBlock);
        callBlocks.Add(unknownBlock);

        auto callInstr = CreateTargetCall(instr->TargetOp(), instr);
        createdCalls.Add(callInstr);

        unknownBlock->InsertInstruction(callInstr);
        unknownBlock->InsertInstruction(CreateGoto(continuationBlock));
    }

    // Connect the generated blocks so that the targets
    // are tested in a linear order, most frequently called first.
    ConnectGeneratedBlocks(testBlocks, callBlocks);

    // Create a 'phi' in the continuation block that merges
    // the values returned by each of the created calls.
    MergeReturnedValues(callBlocks, continuationBlock, instr);

    // Create a new call site for each new call
    // and delete the one associated with the original one.
    UpdateCallGraph(createdCalls, instr);
    
    // The original 'call' is no longer needed.
    instr->RemoveFromBlock(true /* free */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IndirectCallPromotion::ConnectGeneratedBlocks(BlockList& testBlocks, 
                                                   BlockList& callBlocks) {
    // Connect the blocks based on the following pattern:
    // TEST_BLOCK0:
    //     t0 = ucmp targetOp, FUNCT_REF0
    //     if t0, CALL_BLOCK0, TEST_BLOCK1
    // CALL_BLOCK0:
    //     call FUNCT_REF0
    //     goto CONTINUATION_BLOCK
    // TEST_BLOCK1:
    //     t1 = ucmp targetOp, FUNCT_REF1
    //     if t1, CALL_BLOCK1, TEST_BLOCK2
    // ...
    // CALL_BLOCK_N:   // only if unpromoted targets
    //     call targetOp
    //     goto CONTINUATION_BLOCK
    // CONTINUATION_BLOCK:
    auto unit = callBlocks[0]->ParentFunction()->ParentUnit();
    auto& references = unit->References();

    for(int i = 0; i < testBlocks.Count(); i++) {
        auto testBlock = testBlocks[i];
        auto ucmpResultOp = testBlocks[i]->LastInstruction()->GetDestinationOp();
        auto trueBlockRef = references.GetBlockRef(callBlocks[i]);
        Block* falseBlock;
        
        if((i + 1) < testBlocks.Count()) {
            // There is a next target test.
            falseBlock = testBlocks[i + 1];
        }
        else {
            // Unpromoted targets exist, always do the call.
            falseBlock = callBlocks[i + 1];
        }

        auto falseBlockRef = references.GetBlockRef(falseBlock);
        auto ifInstr = IfInstr::GetIf(ucmpResultOp, trueBlockRef, falseBlockRef);
        testBlock->InsertInstruction(ifInstr);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IndirectCallPromotion::MergeReturnedValues(BlockList& callBlocks, 
                                                Block* continuationBlock,
                                                CallInstr* instr) {
    // If the call returns 'void' or the result is not used
    // there are no values to merge.
    if(instr->IsVoid() || (instr->HasDestinationOp() == false)) {
        return;
    }

    auto unit = callBlocks[0]->ParentFunction()->ParentUnit();
    auto& references = unit->References();

    // Create the 'phi' that merges the returned values.
    auto phiResultOp = Temporary::GetTemporary(instr->ResultOp()->GetType());
    auto phiInstr = PhiInstr::GetPhi(phiResultOp, callBlocks.Count());
    continuationBlock->InsertInstructionFirst(phiInstr);

    // Now add the incoming operands.
    for(int i = 0; i < callBlocks.Count(); i++) {
        auto beforeGoto = callBlocks[i]->LastInstruction()->PreviousInstruction();
        auto callInstr = beforeGoto->As<CallInstr>();
        
        DebugValidator::IsNotNull(callInstr);
        DebugValidator::IsNotNull(callInstr->ResultOp());

        auto blockRef = references.GetBlockRef(callBlocks[i]);
        phiInstr->AddOperand(callInstr->ResultOp(), blockRef);
    }

    // The original returned value is replaced by the 'phi' result.
    instr->ResultOp()->ReplaceWith(phiResultOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IndirectCallPromotion::UpdateCallGraph(CallList& createdCalls, 
                                            CallInstr* originalInstr) {
    // Remove the call site associated with the original call
    // and create call sites for each new call.
    // If we still have a call through a pointer the associated
    // call sites gets all targets not covered by the other calls.
    auto originalCallSite = callGraph_->GetCallSite(originalInstr);

    // Remove the covered calls.
    for(int i = 0; i < createdCalls.Count(); i++) {
        auto calledFunctionRef = createdCalls[i]->TargetOp()->As<FunctionReference>();

        if(calledFunctionRef) {
            originalCallSite->RemoveCalledFunction(calledFunctionRef);
        }
    }

    auto callNode = static_cast<CallNode*>(callGraph_->GetNode(originalInstr));
    
    for(int i = 0; i < createdCalls.Count(); i++) {
        auto callSite = callGraph_->GetCallSite(createdCalls[i]);
        auto calledFunctionRef = createdCalls[i]->TargetOp()->As<FunctionReference>();

        if(calledFunctionRef) {
            // A single (known) function is called.
            auto calledNode = callGraph_->GetNode(calledFunctionRef);
            callSite->AddCalledNode(calledNode);
        }
        else {
            // This calls site gets all targets not covered by the other calls.
            originalCallSite->ForEachCalledNode([callSite](CallNodeBase* node) -> bool {
                callSite->AddCalledNode(node);
                return true;
            });
        }
    }

    // Remove the original call site.
    callNode->RemoveCallSite(originalInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CallNode* IndirectCallPromotion::GetSingleTarget(CallSite* callSite) {
    CallNode* singleNode = nullptr;

    // The External node and node groups are ignored.
    callSite->ForEachCalledNode([&singleNode](CallNodeBase* node) -> bool {
        if((node->IsExternalNode() || node->IsNodeGroup()) == false) {
            singleNode = static_cast<CallNode*>(node);
            return false; // Stop search.
        }

        return true;
    });

    return singleNode;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IndirectCallPromotion::CollectTargets(CallSite* callSite, CallEdgeList& edges) {
    // The External/Unknown nodes and node groups are ignored.
    callSite->ForEachCalledEdge([&edges](CallEdge edge) -> bool {
        if((edge.Callee()->IsUnknownNode()  ||
            edge.Callee()->IsExternalNode() ||
            edge.Callee()->IsNodeGroup()) == false) {
            edges.Add(edge);
        }
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UcmpInstr* IndirectCallPromotion::CreateTargetComparison(Operand* targetOp, 
                                                         FunctionReference* functionRef) {
    // Create an equality comparison between 'targetOp' and 'functionRef'.
    auto resultOp = Temporary::GetTemporary(IntegerType::GetInt32());
    resultOp->SetIsBoolean(true);
    return UcmpInstr::GetUcmp(Order_Equal, targetOp, functionRef, resultOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* IndirectCallPromotion::CreateTestBlock(SymbolTable& symbols, 
                                              Block* previousBlock) {
    auto name = BlockUtils::CreateUniqueName("#indir_prom_test", symbols);
    return Block::GetBlock(name, previousBlock->ParentFunction(), previousBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* IndirectCallPromotion::CreateCallBlock(SymbolTable& symbols, 
                                              Block* previousBlock) {
    auto name = BlockUtils::CreateUniqueName("#indir_prom_call", symbols);
    return Block::GetBlock(name, previousBlock->ParentFunction(), previousBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CallInstr* IndirectCallPromotion::CreateTargetCall(Operand* targetOp,  
                                                   CallInstr* clonedInstr) {
    // Clone the original 'call' instruction and change its target.
    auto targetCall = static_cast<CallInstr*>(clonedInstr->Clone());
    targetCall->SetTargetOp(targetOp);

    // If the 'call' returns a value create a new temporary for it.
    if((clonedInstr->IsVoid() == false) && clonedInstr->ResultOp()) {
        auto returnedOp = Temporary::GetTemporary(clonedInstr->ResultOp()->GetType());
        targetCall->SetResultOp(returnedOp);
    }

    return targetCall;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GotoInstr* IndirectCallPromotion::CreateGoto(Block* targetBlock) {
    auto unit = targetBlock->ParentFunction()->ParentUnit();
    auto targetBlockRef = unit->References().GetBlockRef(targetBlock);
    return GotoInstr::GetGoto(targetBlockRef);
}

} // namespace Optimization