// DominatorsOrder.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_DOMINATORS_HPP
#define PC_ANALYSIS_DOMINATORS_HPP

#include "DominatorAlgorithm.hpp"
#include "SparseBitVector.hpp"
#include "CFGWalker.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace Analysis {

// Defines a block order that computes the dominators of a CFG.
template <class TBlock, class TFunction>
class DominatorsOrder {
private:
	typedef CFGInfo<TBlock, TFunction> TCFGInfo;

	const TBlock* startBlock_; // The function entry block.
	TCFGInfo cfgInfo;          // Helper that computes the postorder of the blocks.

public:
	// A list that stores immediate dominators. 128 blocks should be enough for most CFGs.
	typedef typename TCFGInfo::TOrderList BlockList;
	typedef typename TBlock::PredecessorEnum TPredecessorEnum;
	static const bool IsPostdominator = false;

	const TBlock* GetStartBlock(const TFunction* function) {
		startBlock_ = function->FirstBlock();
		return startBlock_;
	}

	bool HasFakeExitBlock() {
		return false;
	}

    const TBlock* GetFakeExitBlock() {
        return nullptr;
    }

    bool IsReturnBlock(const TBlock* block) {
        return false;
    }

	BlockList* GetPostorderList(const TBlock* startBlock) {
		cfgInfo.GetInfo(startBlock, false /* no edge info */);
		return &cfgInfo.PostorderList();
	}

	static const TPredecessorEnum GetPredecessorEnum(const TBlock* block) {
		return block->GetPredecessorEnum();
	}

	static int PredecessorCount(const TBlock* block) {
		return block->PredecessorCount();
	}

	void FreeExitBlock() {
		// Nothing to do in this case.
	}
};

template <class TBlock, class TFunction>
struct Dominators {
	typedef DominatorsOrder<TBlock, TFunction> TOrder;
	typedef DominatorAlgorithm<TBlock, TFunction, TOrder> TAlgorithm;
};


// Defines a block order that computes the postdominators of a CFG.
template <class TBlock, class TFunction>
class PostDominatorsOrder {
public:
	// A list that stores immediate dominators. 
    // 128 blocks should be enough for most CFGs.
	typedef StaticList<const TBlock*, 128> BlockList;

private:
    // Helper that adds the walked blocks to the postorder list.
    struct ListBuilder {
        PostDominatorsOrder* parent_;

        ListBuilder(PostDominatorsOrder* parent) : parent_(parent) {}

        bool operator() (const TBlock* block) {
            parent_->postorderList_.Add(block);
            return true;
        }
    };

    typedef NoAction<TBlock> TNoAction;
    typedef ReverseDepthFirstWalker<TBlock, TNoAction, ListBuilder> TPosterderWalker;

	BlockList postorderList_;
    SparseBitVector returnBlocks_;
	TBlock* exitBlock_;

public:
	PostDominatorsOrder() : exitBlock_(nullptr) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// The block's successors need to be considered for postdominators.
	typedef typename TBlock::SuccessorEnum TPredecessorEnum;
	static const bool IsPostdominator = true;

	const TBlock* GetStartBlock(const TFunction* function) {
		// It's possible that the function has multiple exit blocks.
		// In this case we need to create a fake block that acts like an
		// "unique" exit block, whose predecessors are the real exit blocks.
        if(exitBlock_) {
            // A fake exit block has been already created.
            return exitBlock_;
        }

		StaticList<const TBlock*, 16> exitBlocks;
		const TBlock* block = function->FirstBlock();

		while(block) {
			if(block->SuccessorCount() == 0) {
				// This is an exit block.
				exitBlocks.Add(block);
                returnBlocks_.SetBit(block->Id());
			}

			block = block->NextBlock();
		}

        // Now return the exit block. If there is no unique block
        // we create a "fake" one that has all the exit blocks 
        // as its predecessors. This block will be deleted after the analysis.
		if(exitBlocks.Count() == 1) {
			// There is a single exit block.
			return exitBlocks[0];
		}
		else if(exitBlocks.Count() > 1) {
			// There are multiple exit blocks, create a fake block.
			exitBlock_ = TBlock::GetBlock("#fakeBlock");

			for(int i = 0; i < exitBlocks.Count(); i++) {
				exitBlock_->AddPredecessor(const_cast<TBlock*>(exitBlocks[i]));	
			}

			return exitBlock_;
		}
		else {
			// There is no exit block. This can happen, for example, if the last
			// block represents an infinite loop. In this case return the last block.
			return function->LastBlock();
		}
	}

	bool HasFakeExitBlock() {
		// Returns 'true' if a fake exit block is used.
		return exitBlock_ != nullptr;
	}

    const TBlock* GetFakeExitBlock() {
        return exitBlock_;
    }

    bool IsReturnBlock(const TBlock* block) {
        return returnBlocks_.IsSet(block->Id());
    }

	BlockList* GetPostorderList(const TBlock* startBlock) {
		// Add the blocks to the list in reverse postorder.
        // Make a list with the blocks walked in postorder, starting
        // with the exit block and following the predecessors.
		TPosterderWalker().Walk(startBlock, NoAction<TBlock>(), 
                                ListBuilder(this));
		return &postorderList_;
	}

	static const TPredecessorEnum GetPredecessorEnum(const TBlock* block) {
		return block->GetSuccessorEnum();
	}

	static int PredecessorCount(const TBlock* block) {
		return block->SuccessorCount();
	}

	void FreeExitBlock() {
		// Delete the fake exit block.
		if(exitBlock_) {
            delete exitBlock_;
        }
	}
};

template <class TBlock, class TFunction>
struct PostDominators {
	typedef PostDominatorsOrder<TBlock, TFunction> TOrder;
	typedef DominatorAlgorithm<TBlock, TFunction, TOrder> TAlgorithm;
};

} // namespace Analysis
#endif