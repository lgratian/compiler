// DominanceFrontier.hpp
// Copyright (c) Lup Gratian
//
// Implements an algorithm that computes the dominance frontier of a block.
// This also computes the post-dominance frontier, if a post-dominator tree is used.
// Based on the paper "A Simple, Fast Dominance Algorithm" by Cooper, Harvey and Kennedy.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_DOMINANCE_FRONTIER_HPP
#define PC_ANALYSIS_DOMINANCE_FRONTIER_HPP

#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace Analysis {

template <class TDominatorTree>
class DominanceFrontier {
private:
	typedef DominanceFrontier<TDominatorTree> TDominanceFrontier;
	typedef typename TDominatorTree::TBlock TBlock;
	typedef typename TDominatorTree::TFunction TFunction;
	typedef typename TDominatorTree::TOrder TOrder;
	typedef StaticList<const TBlock*, 8> TFrontierList; // 8 blocks should be enough.
	typedef Dictionary<const TBlock*, TFrontierList*> TFrontierDict;

	TFrontierDict frontiers_;    // Maps from a block to the list with its dominance frontier.
	const TDominatorTree* tree_; // The associated dominator tree.
	const TFunction* funct_;     // The function for which the dominance frontiers are computed.
	bool built_;                 // 'true' if the dominance frontiers where built.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Adds a block to the dominance frontier of the specified block.
	void AddToFrontier(const TBlock* frontierBlock, const TBlock* block) {
		TFrontierList* list;

		if(frontiers_.TryGetValue(block, &list) == false) {
			// Create the list now.
			list = new TFrontierList();
			frontiers_.Add(block, list);
		}

		// The block should be added a single time (the lists acts as a set).
		// We use a linear search, because the number of blocks should be small.
		if(list->Contains(frontierBlock) == false) {
			return list->Add(frontierBlock);
		}
	}

public:
    DominanceFrontier() {}

	DominanceFrontier(TDominatorTree* tree) : 
			tree_(tree), funct_(tree->GetFunction()), built_(false) {
		DebugValidator::IsNotNull(tree);
	}

	~DominanceFrontier() {
		// Free the memory allocated to the lists.
		frontiers_.ForEachValue([](TFrontierList* list) {
			delete list;
		});
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Constructs the dominance frontiers for all the blocks in the function.
	void Build() {
		// Iterate over all blocks in the function. When we find a block with at least
		// 2 predecessors, walk the dominator trees of each predecessor until we find
		// the immediate dominator of the current block: all the blocks found on the path
		// contain the current block in their dominance frontier list.
		for(const TBlock* block = funct_->FirstBlock(); block; 
			block = block->NextBlock()) {
			if(TOrder::PredecessorCount(block) < 2)  continue;

			const TBlock* immDomBlock = tree_->GetImmediateDominator(block);
			TOrder::TPredecessorEnum predecessorEnum = TOrder::GetPredecessorEnum(block);

			while(predecessorEnum.IsValid()) {
				const TBlock* predecessorBlock = predecessorEnum.Next();

				// Walk the immediate dominators of the predecessor.
				while(predecessorBlock != immDomBlock) {
					AddToFrontier(block, predecessorBlock);
					predecessorBlock = tree_->GetImmediateDominator(predecessorBlock);
				}
			}
		}

		built_ = true;
	}

	// Returns the list with the blocks found on the dominance frontier
	// of the specified block.
	TFrontierList* GetFrontier(const TBlock* block) {
		DebugValidator::IsNotNull(block);
		DebugValidator::IsTrue(built_);
		TFrontierList* list;
		
		if(frontiers_.TryGetValue(block, &list)) {
			return list;
		}
		else return nullptr;
	}

	const TFrontierList* GetFrontier(const TBlock* block) const {
		return const_cast<TDominanceFrontier*>(this)->GetFrontier(block);
	}

	// Returns the number of blocks found on the dominance frontier
	// of the specified block.
	int FrontierBlockCount(const TBlock* block) const {
		DebugValidator::IsNotNull(block);
		DebugValidator::IsTrue(built_);

		if(auto list = GetFrontier(block)) {
			return list->Count();
		}
		else return 0;
	}

	// Returns 'true' if the first block is in the dominance frontier of the second one.
	bool IsOnFrontier(TBlock* frontierBlock, TBlock* block) const {
		if(auto list = GetFrontier(block)) {
			return list->Contains(frontierBlock);
		}
		else return false;
	}

	// Returns a string representation of the dominance frontiers for all blocks.
	string ToString() const {
		StringBuilder sb;

		frontiers_.ForEach([&sb](TFrontierDict::TPair& pair) -> bool {
			TBlock* block = pair.First;
			TFrontierList* list = pair.Value;

			if(block->HasName()) {
				sb.Append(*block->Name());
				sb.Append(": ");

				for(int i = 0; i < list->Count(); i++) {
					TBlock* temp = (*list)[i];
					if(temp->HasName()) {
						sb.Append(*temp->Name());
						sb.Append(",");
					}
				}
			}

			sb.AppendLine();
			return true;
		});

		return sb.ToString();
	}

	void Dump() const {
		ObjectDumper(ToString(), "Dominance Frontiers").Dump();
	}
};

} // namespace Analysis
#endif