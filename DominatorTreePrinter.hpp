// DominatorTreePrinter.hpp
// Copyright (c) Lup Gratian
//
// Prints a dominator tree to a Graphviz DOT file.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_DOMINATOR_TREE_PRINTER_HPP
#define PC_ANALYSIS_DOMINATOR_TREE_PRINTER_HPP

#include "DotPrinterBase.hpp"
#include "DominatorTree.hpp"
using namespace Base;

namespace Analysis {

template <class TDominatorTree>
class DominatorTreePrinter : public DotPrinterBase {
private:
	typedef typename TDominatorTree::TDominatorNode TNode;
	typedef typename TDominatorTree::TBlock TBlock;
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	void NewTreeNode(TNode* node) {
		const char* color = COLOR_LIGHT_GREY;
		if(node->HasImmediateDominator() == false) {
			color = COLOR_LIGHT_BLUE;
		}

		string label;
		auto block = node->Block();

		if(block->HasName()) {
			label = *block->Name();
		}
		else label = string::Format(L"Untitled%d", nodes_);

		NewNode(node, label, color);
	}

	void PrintNode(TNode* node) {
		NewTreeNode(node);

		for(int i = 0; i < node->Children().Count(); i++) {
			auto child = node->Children()[i];
			PrintNode(child);

			// Make a link from 'node' to 'child'.
			Link(node, child);
		}
	}

public:
	DominatorTreePrinter(const string& file) : DotPrinterBase(file) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Prints the specified dominator tree to a DOT file.
	void Print(TDominatorTree* domTree) {
		if(domTree->MultipleRoots().Count() > 0) {
			for(int i = 0; i < domTree->MultipleRoots().Count(); i++) {
				PrintNode(domTree->MultipleRoots()[i]);
			}
		}
		else PrintNode(domTree->GetRootNode());
		
		Close();
	}
};

} // namespace Analysis
#endif