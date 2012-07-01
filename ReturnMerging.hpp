// ReturnMerging.hpp
// Copyright (c) Lup Gratian
//
// Creates a single return point for functions that have multiple return points
// and the returned type is not 'void'. This simplifies the inlining process,
// and also can lead to the reduction of the basic block count.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_RETURN_MERGING_HPP
#define PC_OPTIMIZATION_RETURN_MERGING_HPP  

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Base/String.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
using namespace IR;
using namespace Base;

namespace Optimization {

class ReturnMerging {
private:
    typedef StaticList<Operand*, 8> ValueList;
    typedef StaticList<Block*, 8> BlockList;

    // Creates a 'phi' instruction that merges the specified values.
    Operand* MergeValues(ValueList& values, BlockList& blocks, Function* function);

    // Creates a block in which the 'ret' instruction will be placed.
    BlockReference* AppendReturnBlock(Function* function);

    // Replaces each 'ret' instruction with a 'goto' to the new return block.
    void RewriteReturnBlocks(BlockList& blocks, BlockReference* returnBlock);

public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif