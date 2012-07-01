// DominatorAlgorithm.hpp
// Copyright (c) Lup Gratian
//
// Implements a pretty simple, but fast algorithm that computes the dominators,
// immediate dominators and the dominator tree.
// In practice it should be faster than the Lengauer-Tarjan algorithm.
// Based on the paper "A Simple, Fast Dominance Algorithm" by Cooper, Harvey and Kennedy.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_DOMINATORS_BASE_HPP
#define PC_ANALYSIS_DOMINATORS_BASE_HPP

#include "CFGWalker.hpp"
#include "DominatorTree.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace Analysis {

// 'Order' should be a traits class that provides the following methods:
// template <class TBlock, class TFunction>
// class Order {
// protected:
//		typedef StaticList<TBlock*, N> BlockList;
//      typedef TBlock::PredecessorEnum TPredecessorEnum
//		static const bool IsPostdominator;
//		const TBlock* GetStartBlock(const TFunction*);
//      bool HasFakeExitBlock();
//      const TBlock* GetFakeExitBlock();
//      bool IsReturnBlock(const TBlock*);
//		BlockList* GetPostorderList();
//		static TPredecessorEnum GetPredecessorEnum(const TBlock*);
//      static int PredecessorCount(const TBlock*);
//		void FreeExitBlock(); // if 'IsPostdominator' is set.
// };

template <class TBlock, class TFunction, class TOrder>
class DominatorAlgorithm : protected TOrder {
public:
    typedef TBlock TBlock;
    typedef TOrder TOrder;
    typedef TFunction TFunction;

private:
    typedef typename TOrder::BlockList BlockList;
    typedef typename TOrder::TPredecessorEnum TPredecessorEnum;
    typedef DominatorAlgorithm<TBlock, TFunction, TOrder> TDominatorsBase;
    typedef DominatorTree<TBlock, TFunction, TDominatorsBase> TDominatorTree;
    typedef DominatorNode<TBlock> TDominatorNode;
    typedef BlockPair<TBlock> TBlockPair;
    typedef Dictionary<const TBlock*, int> TBlockIdDict;

    // 128 blocks should be enough for most functions.
    typedef StaticList<int, 128> TImmDomsList;

    TBlockIdDict blockToId_;    // Maps blocks to their postorder number.
    TImmDomsList immDoms;       // Array that holds the immediate dominators.
    BlockList* postorderList_; // The list with the CFG blocks in postorder.
    const TBlock* startBlock_;  // The start (entry) block of the CFG.

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Computes the immediate dominators for the CFG of the specified function.
    void ComputeDominators(const TFunction* function) {
        int blockCount = function->BlockCount();
        startBlock_ = TOrder::GetStartBlock(function);

        // Obtain the list of the blocks in postorder.
        postorderList_ = TOrder::GetPostorderList(startBlock_);
        int postorderCount = postorderList_->Count();

        // Build a dictionary that maps each block to its preorder number.
        for(int i = 0; i < postorderCount; i++) {
            blockToId_.Add((*postorderList_)[i], i);
        }

        // Mark each immediate dominators as "not initialized", except for the start block.
        InitializeImmediateDoms();

        // Iterate until no further changes are made.
        bool changed;

        do {
            // Presume no change is going to be made.
            changed = false;

            // Iterate over the block list. Note that we don't start with the last node
            // because we want to skip over the start (entry) block.
            for(int i = postorderCount - 2; i >= 0; i--) {
                // We need to choose the first predecessor that was processed.
                // Then we intersect its dominator set with the sets of all
                // the other predecessors that have been processed.
                int newIdomId = -1;
                const TBlock* block = (*postorderList_)[i];

                // If we compute the post-dominator tree for a function with
                // multiple return points, we need to check if this block
                // is one of them. If it is a return point it is postdominated
                // by the unique exit block.
                if(TOrder::IsPostdominator && TOrder::HasFakeExitBlock()) {
                    if(TOrder::IsReturnBlock(block)) {
                        const TBlock* exitBlock = TOrder::GetFakeExitBlock();
                        int exitBlockId = blockToId_[exitBlock];
                        immDoms[i] = exitBlockId;
                        continue;
                    }  
                }

                auto predecessorEnum = TOrder::GetPredecessorEnum(block);

                while(predecessorEnum.IsValid()) {
                    const TBlock* predecessorBlock = predecessorEnum.Next();
                    
                    if(blockToId_.ContainsKey(predecessorBlock) == false) {
                        // This happens when the predecessor is unreachable,
                        // but this block is reachable.
                        continue;
                    }

                    int predBlockId = blockToId_[predecessorBlock];

                    if(immDoms[predBlockId] == -1) {
                        // Skip the predecessor if it wasn't processed yet.
                        continue;
                    }
                    else if(newIdomId == -1) {
                        // This is the first predecessor that was processed.
                        newIdomId = predBlockId;
                    }
                    else {
                        // This is a predecessor that was processed. Intersect its
                        // dominator set with the current new immediate dominator.
                        newIdomId = Intersect(predBlockId, newIdomId);
                    }
                }

                // If the new immediate dominator is not the same as the last one
                // save it and mark that a change has been made.
                if(immDoms[i] != newIdomId) {
                    immDoms[i] = newIdomId;
                    changed = true;
                }
            }
        } while(changed);
    }

    // Initializes the array with the immediate dominators.
    void InitializeImmediateDoms() {
        int count = postorderList_->Count();
        for(int i = 0; i < count; i++) {
            immDoms.Add(-1);
        }

        // The start node is dominated only by itself.
        int startBlockId = blockToId_[startBlock_];
        immDoms[startBlockId] = startBlockId;
    }

    // Finds the common dominator for the specified blocks.
    int Intersect(int a, int b) {
        // Walk up the immediate dominator array until the "fingers" point to the
        // same postorder number. Note that a higher postorder number means that
        // we're closer to the entry block of the CFG (exit block if we're
        // talking about a post-dominator tree).
        while(a != b) {
            while(a < b) {
                a = immDoms[a]; // PostNumb(immDoms[a]) > PostNumb(a)
            }

            while(b < a) {
                b = immDoms[b]; // Same as above.
            }
        }

        return a;
    }

    // Returns the tree node associated with the specified block.
    // If the node is not available it is created now.
    TDominatorNode* GetOrCreateNode(const TBlock* block, TDominatorTree* tree) {
        TDominatorNode* node;
        if(tree->BlockToDominatorNode().TryGetValue(block, &node)) {
            return node;
        }

        // Create a new node for the block.
        node = new TDominatorNode(block);
        tree->BlockToDominatorNode().Add(block, node);
        return node;
    }

    // Removes the (possible) fake exit node and populates the "multiple exit" list.
    void HandlePostdominators(TDominatorTree* tree) {
        // The start block may be a fake block that was created by the
        // postdominator order because the CFG has multiple exit blocks.
        if(TOrder::IsPostdominator == false) return;
        
        if(TOrder::HasFakeExitBlock()) {
            // All predecessors must be added to the "multiple roots" list.
            // Note that in this case we want the real predecessor enumerator.
            auto predecessorEnum = startBlock_->GetPredecessorEnum();

            while(predecessorEnum.IsValid()) {
                const TBlock* predecessorBlock = predecessorEnum.Next();
                TDominatorNode* predNode = tree->BlockToDominatorNode()[predecessorBlock];
                tree->MultipleRoots().Add(predNode);
            }

            // All blocks that where post-dominated by the exit block
            // aren't anymore, and they are not post-dominated by any other
            // block (otherwise a fake exit block wouldn't have been necessary).
            auto& children = tree->GetChildrenList(startBlock_);

            for(int i = 0; i < children.Count(); i++) {
                children[i]->SetImmediateDominator(nullptr);
            }

            // Free the exit block.
            tree->GetRootNode()->Children().Clear(); // Don't remove the children.
            tree->SetRoot(nullptr); // The root node is no longer valid.
            TOrder::FreeExitBlock();
        }
    }

    // Builds the dominator tree and the dominator cache.
    void BuildTreeAndCache(const TFunction* function, TDominatorTree* tree) {
        // If there are no blocks we're done.
        if(postorderList_->Count() == 0) return;

        // Create the start node.
        TDominatorNode* root = GetOrCreateNode(startBlock_, tree);
        tree->SetRoot(root);

        // Walk the blocks in reverse postorder and construct the dominator tree.
        int postorderCount = postorderList_->Count();
        int startBlockId = postorderCount - 1;
        bool useCache = tree->ShouldUseCache();

        for(int i = postorderCount - 2; i >= 0; i--) {
            // While constructing the tree populate the dominator cache.
            const TBlock* currentBlock = (*postorderList_)[i];
            TDominatorNode* currentNode = GetOrCreateNode(currentBlock, tree);

            // Add this block as a child to its immediate dominator node.
            int blockId = immDoms[i];
            const TBlock* block = (*postorderList_)[blockId];

            TDominatorNode* immDomNode = tree->BlockToDominatorNode()[block];
            immDomNode->Children().Add(currentNode);
            currentNode->SetImmediateDominator(immDomNode);

            // If profitable, populate the dominator-dominated cache.
            if(useCache) {
                int lastBlockId;

                do {
                    // Add this block to the cache of dominators.
                    TBlockPair pair(block, currentBlock);
                    tree->DominatorCache().Add(pair, currentBlock);

                    // Walk up in the dominator tree.
                    lastBlockId = blockId;
                    blockId = immDoms[blockId];
                    block = (*postorderList_)[blockId];
                } while(blockId != lastBlockId);
            }
        }

        // Set the "cache available" flag;
        tree->SetCacheValid(useCache);
        HandlePostdominators(tree);
    }

public:
    // Builds the dominator tree for the specified function.
    void Build(const TFunction* function, TDominatorTree* tree) {
        DebugValidator::IsNotNull(function);
        ComputeDominators(function);
        BuildTreeAndCache(function, tree);
    }

    void Reset() {
        blockToId_.Clear();
        immDoms.Clear();
    }
};

} // namespace Analysis
#endif