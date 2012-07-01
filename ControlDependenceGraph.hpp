// ControlDependenceGraph.hpp
// Copyright (c) Lup Gratian
//
// Implements a class that computes the control dependence graph
// of a function. We say that block 'B' is control-dependent on block 'A'
// if block 'A' decides if block 'B' will be executed or not
// (it can be imagined that it acts like a "switch").
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_CONTROL_DEPENDENCE_GRAPH_HPP
#define PC_ANALYSIS_CONTROL_DEPENDENCE_GRAPH_HPP

#include "IRDominators.hpp"
#include "DominatorTreePrinter.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/StringBuilder.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
using namespace Base;
using namespace IR;

namespace Analysis {

class ControlDependenceGraph {
public:
    typedef StaticList<Block*, 2> BlockList;

private:
    Function* funct_;
    IRPostDominatorTree postDomTree_;

    // Stores the blocks that are control-dependent on the key block.
    Dictionary<Block*, BlockList> controlledBlocks_;
    bool hasControlledBlocks_;

    // Maps a block to the blocks that control it.
    Dictionary<Block*, BlockList> controllingBlocks_;
    bool hasControllingBlocks_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Determines the blocks that are control-dependent on 'controllingBlock'.
    void ComputeControlledBlocks(Block* controllingBlock, 
                                 Block* firstControlledBlock) {
        // The blocks that are controlled by 'controllingBlock' (CB) 
        // start with 'firstControlledBlock' (FCB). The next one is
        // the immediate postdominator of FCB (IM-FCB), next is the immediate
        // postdominator of IM-FCB, and so on, until we find the postdominator of CB.
        auto controllingPostdomBlock = postDomTree_.GetImmediateDominator(controllingBlock);
        auto block = firstControlledBlock;

        // Create the list with the blocks controlled by this block.
        // Taking the address of the list is valid here, because the dictionary
        // is not resized.
        BlockList* controlledBlocks = nullptr;

        if(hasControlledBlocks_) {
            if(controlledBlocks_.ContainsKey(controllingBlock) == false) {
                controlledBlocks_.Add(controllingBlock, BlockList());
            }

            controlledBlocks = &controlledBlocks_[controllingBlock];
        }

        while(block && (block != controllingPostdomBlock)) {
            if(controlledBlocks) {
                // The user wants to know which blocks are controlled by another one.
                controlledBlocks->Add(block);
            }

            if(hasControllingBlocks_) {
                // The user wants to know which blocks control another one.
                if(controllingBlocks_.ContainsKey(block) == false) {
                    controllingBlocks_.Add(block, BlockList());
                }

                controllingBlocks_[block].Add(controllingBlock);
            }

            // The next block that could be controlled is the immediate
            // postdominator (for an if-then-else construct, this means
            // that from the "if" block we jump directly to the "continuation" block).
            block = postDomTree_.GetImmediateDominator(block);
        }
    }

    // Computes the control dependence graph for all blocks
    // in the specified function.
    void ComputeGraph(Function* function) {
        // Build the post-dominator tree.
        postDomTree_.Build();

        for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
            // A block with no/a single successor can't control anything.
            if(block->SuccessorCount() < 2) continue;

            for(int i = 0; i < block->SuccessorCount(); i++) {
                auto successorBlock = block->SuccessorAt(i);
                ComputeControlledBlocks(block, successorBlock);
            }
        }
    }

public:
    ControlDependenceGraph(Function* function, bool computeControlledBlocks = true,
                           bool computeControllingBlocks = true) :
            hasControlledBlocks_(computeControlledBlocks),
            hasControllingBlocks_(computeControllingBlocks), 
            funct_(function), postDomTree_(function) {
        DebugValidator::IsNotNull(function);
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    void Build() {
        ComputeGraph(funct_);
    }

    // Returns the post-dominator tree used when computing the graph.
    IRPostDominatorTree& GetPostdominatorTree() {
        return postDomTree_;
    }

    // Returns the list with the blocks that controls the execution of the specified block,
    // or 'nullptr' if such blocks do not exist (the block always executes).
    const BlockList* GetControllingBlocks(Block* block) const {
        DebugValidator::IsNotNull(block);
        DebugValidator::IsTrue(hasControllingBlocks_);

        if(controllingBlocks_.ContainsKey(block)) {
            return &controllingBlocks_[block];
        }

        return nullptr;
    }

    // Returns 'true' if the execution of the specified block depends
    // on the actions taken in another block.
    bool IsControlDependent(Block* block) const {
        DebugValidator::IsNotNull(block);
        return controllingBlocks_.ContainsKey(block);
    }

    // Returns a list with the blocks that are control-dependent
    // on the specified block, or 'nullptr' if such blocks don't exist.
    const BlockList* GetControlledBlocks(Block* block) const {
        DebugValidator::IsNotNull(block);
        DebugValidator::IsTrue(hasControlledBlocks_);

        if(controlledBlocks_.ContainsKey(block)) {
            return &controlledBlocks_[block];
        }
        
        return nullptr;
    }

    // Returns a string representation of the information found in the graph.
    string ToString() const {
        StringBuilder sb;
        
        if(hasControlledBlocks_) {
            sb.AppendLine("\nControlling blocks:");

            funct_->ForEachBlock([&sb, this](Block* block) -> bool {
                if(auto controlled = GetControlledBlocks(block)) {
                    sb.Append(*block->Name()).Append(": ");

                    for(int i = 0; i < controlled->Count(); i++) {
                        sb.Append(*((*controlled)[i])->Name()).Append(", ");
                    }

                    sb.AppendLine();
                }
                return true;
            });
        }

        if(hasControllingBlocks_) {
            sb.AppendLine("\nControlled blocks:");

            funct_->ForEachBlock([&sb, this](Block* block) -> bool {
                if(auto controlled = GetControllingBlocks(block)) {
                    sb.Append(*block->Name()).Append(": ");

                    for(int i = 0; i < controlled->Count(); i++) {
                        sb.Append(*((*controlled)[i])->Name()).Append(", ");
                    }

                    sb.AppendLine();
                }
                return true;
            });
        }

        return sb.ToString();
    }

    void Dump() const {
        ObjectDumper(ToString(), "Control Dependence Graph").Dump();
    }
};

} // namespace Analysis
#endif 