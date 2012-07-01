// DominatorTree.hpp
// Copyright (c) Lup Gratian
//
// Implements the dominator tree and methods that can be used to query
// about dominance relationships among the blocks of an CFG.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_DOMINATOR_TREE_HPP
#define PC_ANALYSIS_DOMINATOR_TREE_HPP

#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp" 
#include "../Base/ObjectDumper.hpp"
using namespace Base;

template <class TBlock>
class DominatorNode {
public:
	typedef DominatorNode<TBlock> TDominatorNode;
	typedef StaticList<TDominatorNode*, 6> TChildrenList;

private:
	const TBlock* block_;
	TDominatorNode* immDom_;
	TChildrenList children_;

	// Don't allow using the copy constructor and assignment operator (expensive operations).
	DominatorNode(const TDominatorNode& other);
	TDominatorNode& operator= (const TDominatorNode& other);

public:
	DominatorNode() : block_(nullptr), immDom_(nullptr) {}

	DominatorNode(const TBlock* block, TDominatorNode* immDom = nullptr) : 
			block_(block), immDom_(immDom) {}

	~DominatorNode() {
		// Free the memory used by the children.
		for(int i = 0; i < children_.Count(); i++) {
			delete children_[i];
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Returns the block associated with this node.
	TBlock* Block() {
		return const_cast<TBlock*>(block_);
	}

	const TBlock* Block() const {
		return block_;
	}

	// Returns 'true' if this node has an immediate dominator.
	bool HasImmediateDominator() const {
		return immDom_ != nullptr;
	}

	// Returns the immediate dominator for this node.
	TDominatorNode* ImmediateDominator() {
		return immDom_;
	}

	const TDominatorNode* ImmediateDominator() const {
		return immDom_;
	}

	void SetImmediateDominator(TDominatorNode* value) {
		immDom_ = value;
	}

	// Returns 'true' if this node has children (nodes that are dominated by it).
	bool HasChildren() const {
		return children_.Count() > 0;
	}

	// Returns the node with the nodes dominated by this node.
	TChildrenList& Children() {
		return children_;
	}

	const TChildrenList& Children()  const {
		return children_;
	}
};


// Represents a pair of dominator-dominated blocks.
template <class TBlock>
struct BlockPair {
	const TBlock* Dominator;
	const TBlock* Dominated;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	BlockPair() : Dominator(nullptr), Dominated(nullptr) {}

	BlockPair(const TBlock* a, const TBlock* b) : Dominator(a), Dominated(b) {}
	
	BlockPair(const BlockPair& other) : 
			Dominator(other.Dominator), Dominated(other.Dominated) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	unsigned GetHashCode() const {
		unsigned hash = 0;
		hash = (unsigned)Dominator;
		hash ^= (unsigned)Dominated;

		if(sizeof(TBlock*) > 4) {
			// For 64-bit pointers.
			hash ^= (unsigned)((__int64)Dominator >> 32);
			hash ^= (unsigned)((__int64)Dominated >> 32);
		}

		return hash;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	bool operator== (const BlockPair& other) const {
		return (Dominator == other.Dominator) &&
			   (Dominated == other.Dominated);
	}

	bool operator< (const BlockPair& other) const {
		return false;
	}
};


template <class TBlock, class TFunction, class TDominatorAlgorithm>
class DominatorTree {
public:
	typedef TBlock TBlock;
	typedef TFunction TFunction;
	typedef TDominatorAlgorithm TDominatorAlgorithm;
	typedef typename TDominatorAlgorithm::TOrder TOrder;
	typedef DominatorNode<TBlock> TDominatorNode;
	typedef DominatorTree<TBlock, TFunction, TDominatorAlgorithm> TDominatorTree;

private:
	typedef StaticList<TDominatorNode*, 16> TMultipleRootsList;
	typedef Dictionary<const TBlock*, TDominatorNode*> TDomNodeDict;
	typedef BlockPair<TBlock> TBlockPair;
	typedef Dictionary<TBlockPair, const TBlock*> TDomCacheDict;

    // The maximum number of blocks for which a dominator-dominated cache is still built.
    static const int DOMINATOR_CACHE_LIMIT = 128;

	// The maximum number of queries that should be performed
	// until the invalidated cache is rebuilt.
	static const int INVALID_CACHE_REQUEST_LIMIT = 24;

	const TFunction* funct_;           // The function for which this tree is built.
	TDominatorAlgorithm algorithm_;    // The algorithm used to build the tree.
	TDominatorNode* root_;             // The root (entry block) of the function.
	TMultipleRootsList multipleRoots_; // The list of secondary root nodes (postdominators).
	TDomNodeDict blockToNode_;         // Mapping from a 'TBlock' to its 'DominatorNode'.
	TDomCacheDict domCache_;           // Pairs of cached dominator-dominated nodes.
	mutable bool cacheValid_;          // 'true' if the dominator cache is up to date.
	mutable int invalidCacheRequests_; // The number of requests when the cache was invalid.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Deletes the dominator tree and any available caches.
	void Reset() {
		if(root_) {
			delete root_;
			root_ = nullptr;
		}

		for(int i = 0; i < multipleRoots_.Count(); i++) {
			delete multipleRoots_[i];
		}

		multipleRoots_.Clear();
		blockToNode_.Clear();
		domCache_.Clear();
		cacheValid_ = false;
	}

	// Returns 'true' if block 'a' dominates block 'b'.
	// Note that this is much slower than using the cache.
	bool DominatesUsingTree(const TBlock* a, const TBlock* b) const {
		// Rebuild the dominator tree and the cache if there are many queries
		// while the cache is invalid. This decision is based on the presumption
		// that there will be many more queries in the future.
		if((++invalidCacheRequests_ >= INVALID_CACHE_REQUEST_LIMIT) && ShouldUseCache()) {
			const_cast<TDominatorTree*>(this)->Build();
			invalidCacheRequests_ = 0;
		}

		if(blockToNode_.ContainsKey(b) == false) {
			// This means that 'b' is an isolated node and was not included in the tree.
			return false;
		}

		// Walk the dominator tree of block 'b' until we find block 'a'.
		// If we reach after the root node then 'a' doesn't dominate 'b'.
		TDominatorNode* nodeB = blockToNode_[b];

		while((nodeB->Block() != a) && (nodeB->ImmediateDominator() != nullptr)) {
			nodeB = nodeB->ImmediateDominator();
		}

		return nodeB != nullptr;
	}

	// Returns the block that is the nearest common dominator of the specified blocks.
	TBlock* GetCommonDominatorUsingTree(const TBlock* a, const TBlock* b) const{
		// We need to test which blocks that dominates 'b' also dominate 'a'.
		// The first such node will be the common dominator.
		Dictionary<TBlock*, TBlock*> domsA;
		TDominatorNode* nodeA = blockToNode_[a];
		TDominatorNode* nodeAIdom = nodeA->ImmediateDominator();

		while(nodeAIdom) {
			domsA.Add(nodeAIdom->Block(), nodeAIdom->Block());
			nodeAIdom = nodeAIdom->ImmediateDominator();
		}

		// Walk the dominator tree of block 'b' and see which 
        // is the first dominator that is part of 'domsA'. 
        // If we reach outside the tree there is no common dominator.
		TDominatorNode* nodeB = blockToNode_[b];
		TDominatorNode* nodeBIdom = nodeB->ImmediateDominator();

		while(nodeBIdom) {
			if(domsA.ContainsKey(nodeBIdom->Block())) {
				// Found the common dominator.
				return nodeBIdom->Block();
			}

			nodeBIdom = nodeBIdom->ImmediateDominator();
		}

		return nullptr;
	}

	// Removes the specified node from the child list of its immediate dominator.
	void RemoveChild(TDominatorNode* node) {
		TDominatorNode* immDomNode = node->ImmediateDominator();

		if(immDomNode) {
			immDomNode->Children().Remove(node);

			// Remove the pair from the cache, if it's still valid.
			if(cacheValid_) {
				domCache_.Remove(TBlockPair(immDomNode->Block(), node->Block()));
			}
		}
	}

public:
	DominatorTree(const TFunction* function) : 
			funct_(function), root_(nullptr), invalidCacheRequests_(0) {
		DebugValidator::IsNotNull(function);
	}

	~DominatorTree() {
		Reset();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Constructs the dominator tree using the specified algorithm.
	void Build() {
		Reset();
        algorithm_.Reset();
		algorithm_.Build(funct_, this);
	}

	// Returns the function for which the dominator tree is built.
	const TFunction* GetFunction() const {
		return funct_;
	}

	// Returns the root node of the tree. Not valid for postdominators.
	TDominatorNode* GetRootNode() {
		return root_;
	}

	const TDominatorNode* GetRootNode() const {
		return root_;
	}

	// Methods that should be used only by the dominator tree construction algorithms.
	void SetRoot(TDominatorNode* node) {
        if(node == nullptr) {
            blockToNode_.Remove(root_->Block());
            delete root_;
        }

		root_ = node;
	}

	TDomNodeDict& BlockToDominatorNode() {
		return blockToNode_;
	}
	
	TDomCacheDict& DominatorCache() {
		return domCache_;
	}

	void SetCacheValid(bool value) {
		cacheValid_ = value;
	}

	TMultipleRootsList& MultipleRoots() {
		return multipleRoots_;
	}

    // Returns 'true' if the tree contains information about the specified block.
    // Note that this is 'false' for blocks that are unreachable from the entry block.
    bool HasBlock(const TBlock* block) const {
        return blockToNode_.ContainsKey(block);
    }
    
	// Returns 'true' if block 'a' dominates block 'b'.
	bool Dominates(const TBlock* a, const TBlock* b) const {
		DebugValidator::IsNotNull(a);
		DebugValidator::IsNotNull(b);

		// A block dominates itself.
		if(a == b) return true;

		// If the cache is valid get the result directly.
		if(cacheValid_) {
			TBlockPair pair(a, b);
			return domCache_.ContainsKey(pair);
		}

		// Use a dominator tree walk to answer the query.
		return DominatesUsingTree(a, b);
	}

	// Returns 'true' if block 'a' strictly dominates block 'b'.
	// 'a sdom b' iff 'a dom b' and 'a != b'.
	bool StrictlyDominates(const TBlock* a, const TBlock* b) const {
		DebugValidator::IsNotNull(a);
		DebugValidator::IsNotNull(b);

		// If the block dominates itself we can't have a strict dominator.
		if(a == b) return false;
		else return Dominates(a, b);
	}

	// Returns 'true' if the block is dominated by the start (entry) block.
	bool DominatedByEntry(const TBlock* block) const {
		DebugValidator::IsNotNull(block);

		if(root_) {
			// This is a dominator tree, or a postdominator tree with a single exit block.
			return Dominates(root_->Block(), block);
		}

		// This is a post dominator tree, check each of the roots.
		for(int i = 0; i < multipleRoots_.Count(); i++) {
			if(Dominates(multipleRoots_[i]->Block(), block)) {
				// The block is postdominated by this exit block.
				return true;
			}
		}

		return false;
	}

	// Returns the immediate dominator of the specified block.
	// If such a block doesn't exists 'nullptr' is returned. Note that this can happen
	// only if the block is one of the root blocks.
	const TBlock* GetImmediateDominator(const TBlock* block) const {
		DebugValidator::IsNotNull(block);
		TDominatorNode* node;
		
		if(blockToNode_.TryGetValue(block, &node)) {
			if(node->HasImmediateDominator()) {
				return node->ImmediateDominator()->Block();
			}
		}
		
		// This means that the block is isolated and was not included in the tree,
		// or it's the start (entry) block.
		return nullptr;
	}

	TBlock* GetImmediateDominator(TBlock* block) {
		auto temp = const_cast<const TDominatorTree*>(this)->GetImmediateDominator(block);
		return const_cast<TBlock*>(temp);
	}

	// Returns the block that is the nearest common dominator of the specified blocks.
	const TBlock* GetCommonDominator(const TBlock* a, const TBlock* b) const {
		DebugValidator::IsNotNull(a);
		DebugValidator::IsNotNull(b);

		// Perform some trivial and fast tests first.
		if(a == b) return a;
		else if(root_) {
			// This is valid only for normal dominator trees.
			// If 'a' or 'b' are the root block then the root is the common dominator.
			if((root_->Block() == a) || (root_->Block() == b)) {
				return root_->Block();
			}
		}
		
		if(Dominates(a, b)) {
			// If block 'a' dominates block 'b', there cannot be a lower
			// common dominator in the tree other than block 'a'.
			return a;
		}
		else if(Dominates(b, a)) {
			// Same as above, but for block 'b'.
			return b;
		}

		// We need to perform a slow walk on the tree.
		return GetCommonDominatorUsingTree(a, b);
	}

	TBlock* GetCommonDominator(TBlock* a, TBlock* b) {
		auto block = const_cast<const TDominatorTree*>(this)->GetCommonDominator(a, b);
		return const_cast<TBlock*>(block);
	}

	// Returns the tree node associated with the specified block.
	TDominatorNode* GetBlockNode(const TBlock* block) {
		DebugValidator::IsNotNull(block);
		return blockToNode_[block];
	}

	const TDominatorNode* GetBlockNode(const TBlock* block) const {
		DebugValidator::IsNotNull(block);
		return blockToNode_[block];
	}

	// Returns the number of blocks immediately dominated by the specified one.
	int ImmediatelyDominatedCount(const TBlock* block) const {
		GetBlockNode(block)->Children().Count();
	}

	// Returns 'true' if the specified block dominates other blocks.
	bool IsDominator(const TBlock* block) const {
		return GetBlockNode(block)->HasChildren();
	}

    // Returns the list with the blocks that are dominated by the specified one.
    typename TDominatorNode::TChildrenList& GetChildrenList(const TBlock* block) {
        return GetBlockNode(block)->Children();
    }

    const typename TDominatorNode::TChildrenList& GetChildrenList(const TBlock* block) const {
        return GetBlockNode(block)->Children();
    }

	// Returns the immediately dominated block found at the specified position.
	TBlock* GetImmediatelyDominatedAt(const TBlock* block, int index) {
		return GetBlockNode(block)->Children()[index];
	}

	const TBlock* GetImmediatelyDominatedAt(const TBlock* block, int index) const {
		return GetBlockNode(block)->Children()[index];
	}

	// Removes the specified block from the dominator tree.
	// Note that the block should have no successors.
	void BlockRemoved(const TBlock* block) {
		DebugValidator::IsNotNull(block);

		// If the block has an immediate dominator remove it from its children list.
		TDominatorNode* node = blockToNode_[block];
		RemoveChild(node);

		// Remove the block from the map and free it.
		blockToNode_.Remove(block);
		delete node;
	}

    // Updates the tree when 'blockB' is merged into 'blockB'. It presumes that
    // 'blockB' was the only successor of 'blockA', and it considers that
    // 'blockB' will be removed from the function.
    void BlocksMerged(const TBlock* blockA, const TBlock* blockB) {
        DebugValidator::IsNotNull(blockA);
		DebugValidator::IsNotNull(blockB);
        TDominatorNode* blockBNode = blockToNode_[blockB];

        // Remove 'blockB's children from the cache, if it's still valid.
        if(cacheValid_) {
			auto& children = blockBNode->Children();

            for(int i = 0; i < children.Count(); i++) {
				auto child = children[i]->Block();
                domCache_.Remove(TBlockPair(blockB, child));
            }
        }

        // All the blocks that were dominated by 'blockB' are now dominated by 'blockA'.
		auto blockANode = blockToNode_[blockA];
		auto& children = blockBNode->Children();

        for(int i = 0; i < children.Count(); i++) {
            auto child = children[i];
            child->SetImmediateDominator(blockANode);

            // Update the cache, if it's still valid.
            if(cacheValid_) {
                domCache_.Add(TBlockPair(blockA, child->Block()), blockA);
            }
        }

        // Now remove 'blockB'; this will unlink it from 'blockA'.
        BlockRemoved(blockB);
    }

	// Connects the specified block to a new immediate dominator.
	void ImmediateDominatorChanged(const TBlock* block, const TBlock* newImmDom) {
		DebugValidator::IsNotNull(block);
		DebugValidator::IsNotNull(newImmDom);

		// If the block had an immediate dominator it must be removed from its list.
		TDominatorNode* node = blockToNode_[block];
		RemoveChild(node);

		// Connect the block with its new immediate dominator.
		TDominatorNode* newImmNode = blockToNode_[newImmDom];
		node->SetImmediateDominator(newImmNode);
		newImmNode->Children().Add(node);

		// Add the blocks to the cache, if it's still valid.
		if(cacheValid_) {
			domCache_.Add(TBlockPair(newImmDom, block), newImmDom);
		}
	}

	// Adds a block to the dominator tree, and links it with its immediate dominator.
	void BlockAdded(const TBlock* block, const TBlock* immDom) {
		DebugValidator::IsNotNull(block);
		DebugValidator::IsNotNull(immDom);

		// Create a tree node for the block, and link it with its immediate dominator.
		TDominatorNode* immDomNode = blockToNode_[immDom];
		TDominatorNode* node = new TDominatorNode(block, immDomNode);
		immDomNode->Children().Add(node);

		// Add the blocks to the cache, if it's still valid.
		if(cacheValid_) {
			while(immDomNode) {
				domCache_.Add(TBlockPair(immDom, block), immDom);
				immDomNode = immDomNode->ImmediateDominator();
			}
		}
	}

	// A block was split and 'blockSucc' is the second part, which is the only
	// successor of 'block'. This makes the dominator tree reflect this changes.
	void BlockSplitted(const TBlock* block, const TBlock* blockSucc) {
		// Create a node for the successor block; its's dominated by 'block'.
		TDominatorNode* blockNode = blockToNode_[block];
		TDominatorNode* blockSuccNode = new TDominatorNode(blockSucc, block);

		// All the blocks that were immediately dominated by 'block' 
		// are now dominated by 'blockSucc'.
		int childCount = node->Children();

		for(int i = 0; i < childCount; i++) {
			TDominatorNode* child = node->Children()[i];
			child->SetImmediateDominator(blockSuccNode);
			blockSuccNode->Children().Add(child);

			// Add the pair to the cache, if it's still valid.
			if(cacheValid_) {
				domCache_.Add(TBlockPair(blockSucc, child), blockSucc);
			}
		}

		// 'blockSucc' is now dominated by all the blocks that dominate 'block'.
		TDominatorNode* immDom = blockNode->ImmediateDominator();

		while(immDom) {
			domCache_.Add(TBlockPair(immDom, blockSucc));
			immDom = immDom->ImmediateDominator();
		}

		// The only child of 'block' is now 'blockSucc'.
		node->Children().Clear();
		node->Children().Add(blockSuccNode);
	}

    // Returns 'true' if a dominator-dominated cache should be used.
    bool ShouldUseCache() const {
        return blockToNode_.Count() <= DOMINATOR_CACHE_LIMIT;
    }

	// Returns a string representation of the information found in the tree.
	string ToString() const {
		StringBuilder sb;
		sb.AppendFormat(L"Blocks: %d\n", blockToNode_.Count());
		sb.AppendLine("Block: dominated block list");

		blockToNode_.ForEach([&sb](TDomNodeDict::TPair& pair) -> bool {
			if(pair.Key->HasName()) {
				sb.Append(*pair.Key->Name());
				sb.AppendLine(":");

				// Append the immediate dominator.
				TDominatorNode* node = pair.Value;
				sb.Append("\tIdom: ");
				if(node->HasImmediateDominator()) {
					TDominatorNode* idom = node->ImmediateDominator();
					if(idom->Block()->HasName()) {
						sb.Append(*idom->Block()->Name());
					}
				}
				else sb.Append("nullptr");

				// Append the list of dominated blocks.
				sb.Append("\n\tDominated:");
				for(int i = 0; i < node->Children().Count(); i++) {
					TBlock* child = node->Children()[i]->Block();
					if(child->HasName()) {
						sb.Append(*child->Name());
						sb.Append(", ");
					}
				}

				sb.AppendLine();
				return true;
			}
		});

		return sb.ToString();
	}

	void Dump() const {
		ObjectDumper(ToString(), "Dominator Tree").Dump();
	}
};

#endif