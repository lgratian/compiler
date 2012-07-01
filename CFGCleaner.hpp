// CFGCleaner.hpp
// Copyright (c) Lup Gratian
//
// A simple, fast pass that removes useless blocks and branches from the CFG.
// Handles things like empty blocks, unreachable blocks, branch to branch,
// branch to conditional branch, single successor-single predecessor blocks.
// A dead-code elimination pass should be run after this one.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_CFG_CLEANER_HPP
#define PC_OPTIMIZATION_CFG_CLEANER_HPP

#include "BlockUtils.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
#include "../Analysis/CFGWalker.hpp"
#include "../Analysis/IRDominators.hpp"
#include "../Analysis/ConstantFolder.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/Log.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class CFGCleaner : public Pass {
private:
    typedef CFGInfo<Block, Function> IRCFGInfo;
    typedef Dictionary<Block*, bool> ProcessedBlockDict;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Helper used by the DepthFirstWalker.
    // Marks the blocks that are reachable from the function entry.
    struct ReachableMarker {
        CFGCleaner* parent_;

        ReachableMarker(CFGCleaner* parent) : parent_(parent) {}

        bool operator() (const Block* block) {
            parent_->MarkReachable(block);
            return true;
        }
    };

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    Function* funct_;
    IRDominatorTree* domTree_;
	ConstantFolder folder_;
    SparseBitVector reachable_;
    bool domTreeInvalidated_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Tries to merge single successor-single predecessor blocks.
    bool MergeWithPredecessor(Block* block);

    // Tries to eliminate an 'if' that branches to the same block.
    bool EliminateRedundantBranch(Block* block);

    // Tries to eliminate an 'if' that is related to a previous 'if'.
    // It checks for a predecessor that ends with an 'if' that has
    // the same condition operand and a common true/false destination.
    bool EliminateRedundantIf(Block* block);

    // Eliminates the block if it contains only a 'goto'.
    // This is also known as "edge forwarding".
    bool EliminateEmptyBlock(Block* block);

    // Modifies each 'phi' instruction from 'block' so that
    // the operand that is incoming from 'incomingBlock' appears
    // to be incoming from 'otherBlock' too.
    void AddSamePhiOperand(Block* block, Block* incomingBlock, 
                           Block* otherBlock);

    // Tries to eliminate a block that contains only 'phi' instructions.
    // The block must end with a 'goto' to be considered.
    bool EliminatePhiBlock(Block* block);

    // Modifies each 'phi' instruction from 'newTarget' so that
    // the operands that were incoming from 'block' appear to be
    // incoming from each of 'block's predecessors.
    // Used when eliminating blocks which contain only 'phi's.
    void PatchPhiBlockTarget(Block* newTarget, Block* block);

    // Returns 'true' if it's safe to eliminate 'block'.
    // This doesn't happen if 'newTarget' has a 'phi' instruction
    // with an incoming operand from both 'block' and one of its predecessors.
    bool SafeToEliminateBlock(Block* block, Block* newTarget);

    // Returns 'true' if the block contains only 'phi' instructions
    // that are used by other 'phi' instructions found in 'newTarget'.
    bool IsValidPhiBlock(Block* block, Block* newTarget);

    // Removes all the instructions from the unreachable block.
    void ClearBlock(Block* block);

    // Removes all incoming 'phi' values from 'toBlock'
    // that are incoming from 'fromBlock'.
    void RemoveIncomingFromBlock(Block* toBlock, Block* fromBlock);

    // Tries to eliminate a 'goto' that jumps to an 'if'.
    bool HoistBranch(Block* block);

    // Returns 'true' if it is safe to hoist an 'if' to 'replacementBlock'.
    // This doesn't happen if any of 'trueBlock' or 'falseBlock' have
    // 'phi' instructions with incoming operands from 'replacementBlock'.
    bool SafeToHoistBranch(Block* replacementBlock, 
                           Block* trueBlock, Block* falseBlock);

	// Replaces a branching instruction that depends on a constant by a 'goto'.
	bool ConstantFoldBranch(Block* block);

    // Removes all unreachable blocks from the function.
    bool RemoveUnreachableBlocks();

    // Marks the specified block as reachable from the function entry.
    void MarkReachable(const Block* block);

    // Removes the specified instruction, if it's definitely dead.
    // It also removes the operands of the instruction, if they are dead too.
    void EliminateDeadInstructions(Operand* op);

    // Tries to eliminate the 'phi' if it has the same incoming value,
    // or only a single incoming value (can happen after simplifications).
    Operand* SimplifyPhi(PhiInstr* instr);

    // Tries to simplify all the 'phi' instructions in the specified block.
    void SimplifyPhis(Block* block);

	void BlockSimplified(Block* block, int type);

public:
    void Execute(Function* function, IRDominatorTree* domTree = nullptr);
};

} // namespace Optimization
#endif