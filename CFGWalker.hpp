// CFGWalker.hpp
// Copyright (c) Lup Gratian
//
// Implements helper classes that operate on a Control Flow Graph.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_FLOW_GRAPH_WALKER_HPP
#define PC_ANALYSIS_FLOW_GRAPH_WALKER_HPP

#include "../Base/StaticStack.hpp"
#include "../Base/StaticQueue.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
using namespace Base;

namespace Analysis {

// Placeholders that can be used when a notification is not desired.
template <class TBlock>
struct NoAction {
    bool operator() (const TBlock* block) {
        return true;
    }
};


// Walks over the blocks in a CFG using a depth-first order.
template<class TBlock, 
         class THandler1 = NoAction<TBlock>, 
         class THandler2 = NoAction<TBlock>, 
         class THandler3 = NoAction<TBlock>, 
         class THandler4 = NoAction<TBlock>>
class DepthFirstWalker {
private:
	Dictionary<const TBlock*, const TBlock*> processed_;

public:
	void Walk(const TBlock* block,
			  THandler1 before = NoAction<TBlock>(), 
			  THandler2 after = NoAction<TBlock>(),
			  THandler3 beforeSucc = NoAction<TBlock>() ,
			  THandler4 afterSucc = NoAction<TBlock>()) {
		// Call the action handlers in each point.
		processed_.Add(block, block);
		if(before(block) == false) return;

		// Process all successors.
		auto successorEnum = block->GetSuccessorEnum();

		while(successorEnum.IsValid()) {
			// Walk the successor if not already processed.
			const TBlock* next = successorEnum.Next();

			if(processed_.ContainsKey(next) == false) {
				if(beforeSucc(next) == false) return;
				Walk(next, before, after, beforeSucc, afterSucc);
       			if(afterSucc(next) == false) return;
			}
		}

		if(after(block) == false) return;
	}
};


// Walks over the blocks in a CFG using a breath-first order.
template<class TBlock, 
         class THandler1 = NoAction<TBlock>, 
         class THandler2 = NoAction<TBlock>, 
         class THandler3 = NoAction<TBlock>, 
         class THandler4 = NoAction<TBlock>>
class BreathFirstWalker {
private:
	Dictionary<const TBlock*, const TBlock*> processed_;

public:
	void Walk(const TBlock* block,
			  THandler1 before = NoAction<TBlock>(), 
			  THandler2 after = NoAction<TBlock>(),
			  THandler3 beforeSucc = NoAction<TBlock>() ,
			  THandler4 afterSucc = NoAction<TBlock>()) {
		// Call the action handlers in each point.
		StaticList<const TBlock*, 32> succs; // The list with the successors to be processed.
		processed_.Add(block, block);
		if(before(block) == false) return;

		// Process all successors.
		auto successorEnum = block->GetSuccessorEnum();

		while(successorEnum.IsValid()) {
			// Walk the successor if not already processed.
			const TBlock* next = successorEnum.Next();

			if(processed_.ContainsKey(next) == false) {
				succs.array_(next);
			}
		}

		for(int i = 0; i < succs.Count(); i++) {
			if(beforeSucc(next) == false) return;
			Walk(succs[i], before, after, beforeSucc, afterSucc);
			if(afterSucc(next) == false) return;
		}

		if(after(block) == false) return;
	}
};


// Walks over the blocks in a CFG using a reverse depth-first order.
// Note that this follows the predecessor edges of the blocks.
template<class TBlock, 
         class THandler1 = NoAction<TBlock>, 
         class THandler2 = NoAction<TBlock>, 
         class THandler3 = NoAction<TBlock>, 
         class THandler4 = NoAction<TBlock>>
class ReverseDepthFirstWalker {
private:
	Dictionary<const TBlock*, const TBlock*> processed_;

public:
	void Walk(const TBlock* block,
			  THandler1 before = NoAction<TBlock>(), 
			  THandler2 after = NoAction<TBlock>(),
			  THandler3 beforeSucc = NoAction<TBlock>(),
			  THandler4 afterSucc = NoAction<TBlock>()) {
		// Call the action handlers in each point.
		processed_.Add(block, block);
		if(before(block) == false) return;

		// Process all predecessors.
		auto predecessorEnum = block->GetPredecessorEnum();

		while(predecessorEnum.IsValid()) {
			// Walk the predecessor if not already processed.
			const TBlock* pred = predecessorEnum.Next();

			if(processed_.ContainsKey(pred) == false) {
				if(beforeSucc(pred) == false) return;
				Walk(pred, before, after, beforeSucc, afterSucc);
				if(afterSucc(pred) == false) return;
			}
		}

		if(after(block) == false) return;
	}
};


// Represents the type of an edge from the CFG.
enum EdgeType {
	Edge_Tree,
	Edge_Forward,
	Edge_Backward,
	Edge_Crossing
};

// Helper that provides information about edge type and preorder/postorder traversals.
template <class TBlock, class TFunction>
class CFGInfo {
public:
	// Most functions have few blocks, so we use a 'StaticList'.
	typedef StaticList<const TBlock*, 128> TOrderList;

private:
	// Represents an edge that connects two blocks in the CFG.
	struct FlowEdge {
		const TBlock* First;
		const TBlock* Second;

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		FlowEdge() {}

		FlowEdge(const TBlock* first, const TBlock* second) : 
                First(first), Second(second) {}
		
		FlowEdge(const FlowEdge& other) : 
                First(other.First), Second(other.Second) {}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		unsigned GetHashCode() const {
			unsigned hash = 0;
			hash = (unsigned)First;
			hash ^= (unsigned)Second;
			
			// For 64 bit pointers also consider the high part.
			if(sizeof(TBlock*) > 4) {
				hash ^= (unsigned)((__int64)First >> 32);
				hash ^= (unsigned)((__int64)Second >> 32);
			}

			return hash;
		}

		bool operator== (const FlowEdge& other) const {
			return (First == other.First) && (Second == other.Second);
		}

		bool operator< (const FlowEdge& other) const {
			return false;
		}
	};
	
	// Stores information about an edge.
	struct EdgeInfo {
		EdgeType Type;

		EdgeInfo() {}
		EdgeInfo(EdgeType type) : Type(type) {}
		EdgeInfo(const EdgeInfo& other) : Type(other.Type) {}
	};

	typedef Dictionary<FlowEdge, EdgeInfo> TEdgeDict;
	typedef Dictionary<const TBlock*, const TBlock*> TProcessedDict;
	typedef Dictionary<const TBlock*, int> TOrderDict;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	TEdgeDict edgeInfo_;        // Maps a CFG edge to additional info.
	TProcessedDict processed_;  // The blocks that were processed.
	TOrderDict preorder_;       // Maps a block to its preorder  number.
	TOrderDict postorder_;      // Maps a block to its postorder number.
	TOrderList preorderList_;   // The list with the blocks in preorder.
	TOrderList postorderList_;  // The list with the blocks in postorder.
	int preIndex_;              // The preorder counter.
	int postIndex_;             // The postorder counter.
	bool edgeInfoNeeded_;       // 'true' if edge type information should be collected.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Resets the previously computed data.
	void Reset() {
		edgeInfo_.Clear();
		processed_.Clear();
		preorder_.Clear();
		postorder_.Clear();
		preIndex_ = 0;
		postIndex_ = 0;
	}

	// Performs a depth-first search starting with the specified node
	// and collects information about pre and postorder and about edge type.
	void GetInfoImpl(const TBlock* block) {
		// Record preorder information.
		preorderList_.Add(block);

		if(edgeInfoNeeded_) {
			preorder_.Add(block, preIndex_);
			preIndex_++;
		}

        // Walk over all the successors.
		auto successorEnum = block->GetSuccessorEnum();

		while(successorEnum.IsValid()) {
			const TBlock* next = successorEnum.Next();
			FlowEdge edge(block, next);
			EdgeInfo info;

			if(processed_.ContainsKey(next) == false) {
				// The block hasn't been processed yet.
				processed_.Add(next, next);
				if(edgeInfoNeeded_) edgeInfo_.Add(edge, Edge_Tree);
				GetInfoImpl(next);
			}
			else if(edgeInfoNeeded_) {
				// Test the type of the edge.
				if(preorder_.ContainsKey(next) && (preorder_[block] < preorder_[next])) {
					// This is a forward edge.
					edgeInfo_.Add(edge, Edge_Forward);
				}
				else if(postorder_.ContainsKey(block) == false) {
					// This is a backward edge.
					edgeInfo_.Add(edge, Edge_Backward);
				}
				else {
					// This is a crossing edge.
					edgeInfo_.Add(edge, Edge_Crossing);
				}
			}
		}

		// Record postorder information.
		postorderList_.Add(block);

		if(edgeInfoNeeded_) {
			postorder_.Add(block, postIndex_);
			postIndex_++;
		}
	}

public:
	CFGInfo() {}

	CFGInfo(const TBlock* startBlock, bool edgeInfoNeeded = true) {
		GetInfo(startBlock, edgeInfoNeeded);
	}

	CFGInfo(const TFunction* function, bool edgeInfoNeeded = true) {
		GetInfo(function, edgeInfoNeeded);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void GetInfo(const TBlock* startBlock, bool edgeInfoNeeded = true) {
		Reset();
		edgeInfoNeeded_ = edgeInfoNeeded;
		GetInfoImpl(startBlock);
	}

	void GetInfo(const TFunction* function, bool edgeInfoNeeded = true) {
		GetInfo(function->FirstBlock(), edgeInfoNeeded);
	}

	// Returns the type of the edge that connects the specified blocks.
	EdgeType GetEdgeType(const TBlock* first, const TBlock* second) const {
		FlowEdge edge(first, second);
		return edgeInfo_[edge].Type;
	}

	// Returns 'true' if the edge that connects the specified blocks is a tree edge.
	bool IsTreeEdge(const TBlock* first, const TBlock* second) const {
		return GetEdgeType(first, second) == Edge_Tree;
	}

	// Returns 'true' if the edge that connects the specified blocks is a forward edge.
	bool IsForwardEdge(const TBlock* first, const TBlock* second) const {
		return GetEdgeType(first, second) == Edge_Forward;
	}

	// Returns 'true' if the edge that connects the specified blocks is a backward edge.
	bool IsBackwardEdge(const TBlock* first, const TBlock* second) const {
		return GetEdgeType(first, second) == Edge_Backward;
	}

	// Returns 'true' if the edge that connects the specified blocks is a crossing edge.
	bool IsCrossingEdge(const TBlock* first, const TBlock* second) const {
		return GetEdgeType(first, second) == Edge_Crossing;
	}

	// Calls the specified predicate for each block, in postorder.
	// bool Predicate(Block* block, int dfsNumber)
	template <class Predicate>
	void PostorderWalk(Predicate action) {
		for(int i = 0; i < postorderList_.Count(); i++) {
			if(action(postorderList_[i], i) == false) {
				return; // The user aborted.
			}
		}
	}

	// Calls the specified predicate for each block, in reverse postorder.
	// bool Predicate(Block* block, int dfsNumber)
	template <class Predicate>
	void ReversePostorderWalk(Predicate action) {
		for(int i = postorderList_.Count() - 1; i >= 0; i--) {
			if(action(postorderList_[i], i) == false) {
				return; // The user aborted.
			}
		}
	}

	// Calls the specified predicate for each block, in preorder.
	// bool Predicate(Block* block, int dfsNumber)
	template <class Predicate>
	void PreorderWalk(Predicate action) {
		for(int i = 0; i < preorderList_.Count(); i++) {
			if(action(preorderList_[i], i) == false) {
				return; // The user aborted.
			}
		}
	}

	// Calls the specified predicate for each block, in reverse preorder.
	// bool Predicate(Block* block, int dfsNumber)
	template <class Predicate>
	void ReversePreorderWalk(Predicate action) {
		for(int i = preorderList_.Count() - 1; i >= 0; i--) {
			if(action(preorderList_[i], i) == false) {
				return; // The user aborted.
			}
		}
	}

	// Returns the list of blocks, sorted by their postorder number.
	TOrderList& PostorderList() {
		return postorderList_;
	}

	// Returns the list of blocks, sorted by their preorder number.
	TOrderList& PreorderList() {
		return preorderList_;
	}
};

} // namespace Analysis
#endif