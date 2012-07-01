// IRDominators.hpp
// Copyright (c) Lup Gratian
//
// Defines the dominator and post-dominator trees that work on the IR representation.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_IR_DOMINATORS_HPP
#define PC_ANALYSIS_IR_DOMINATORS_HPP

#include "DominatorAlgorithm.hpp"
#include "DominatorsOrder.hpp"
#include "DominatorTree.hpp"
#include "DominanceFrontier.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
using namespace IR;

namespace Analysis {

namespace Detail {
    typedef DominatorsOrder<Block, Function> TDomOrder;
    typedef DominatorAlgorithm<Block, Function, TDomOrder> TDomAlgorithm;

    typedef PostDominatorsOrder<Block, Function> TPostDomOrder;
    typedef DominatorAlgorithm<Block, Function, TPostDomOrder> TPostDomAlgorithm;
}

// Definitions for the dominator trees that work on the IR.
typedef DominatorTree<Block, Function, Detail::TDomAlgorithm> IRDominatorTree;
typedef DominatorTree<Block, Function, Detail::TPostDomAlgorithm> IRPostDominatorTree;

// Definitions for the dominance frontiers that work on the IR.
typedef DominanceFrontier<IRDominatorTree> IRDominanceFrontier;
typedef DominanceFrontier<IRPostDominatorTree> IRPostDominanceFrontier;

} // namespace Analysis
#endif