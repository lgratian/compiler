// BlockUtils.hpp
// Copyright (c) Lup Gratian
//
// Defines various helper methods for working with blocks (block merging,
// block cloning, critical-edge splitting, etc.).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_BLOCK_UTILS_HPP
#define PC_OPTIMIZATION_BLOCK_UTILS_HPP

#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../Analysis/IRDominators.hpp"
#include "../Base/String.hpp"
#include "../Base/DebugValidator.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;

namespace Optimization {

class BlockUtils {
public:
    // Merges block 'b' into block 'a', removing the branching instruction from 'a'.
    static Block* MergeBlocks(Block* a, Block* b, bool removeB = true);

    // Returns 'true' if all predecessors, except 'exeptionBlock',
    // are dominated by the block. This happens in case of loops, for example.
    static bool DominatesItself(Block* block, Block* exceptioBlock,
                                IRDominatorTree* domTree);

    // Updates the 'phi' instructions found in 'phiBlock' so that the incoming values
    // from 'previousBlock' are incoming values from 'newBlock'.
    static bool ReplacePhiOperandsBlock(Block* block, Block* previousBlock,
                                        Block* newBlock);

    // Modifies each 'phi' instruction in 'block' so that an operand
    // is incoming from 'oldBlock' appears to be incoming from 'newBlock' too.
    static void InsertSameIncoming(Block* block, Block* oldBlock, Block* newBlock);

    // Creates a block name of the form 'pattern_0'
    // that is unique in the specified symbol table.
    static string CreateUniqueName(const string& pattern, SymbolTable& symbols);
};

} // namespace Optimization
#endif

//? Critical-edge splitting