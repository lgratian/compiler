// CFGPrinter.hpp
// Copyright (c) Lup Gratian
//
// Prints a CFG to a Graphviz DOT file.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_FLOW_GRAPH_PRINTER_HPP
#define PC_ANALYSIS_FLOW_GRAPH_PRINTER_HPP

#include "DotPrinterBase.hpp"
#include "DominatorsOrder.hpp"
#include "DominatorTree.hpp"
#include "DominanceFrontier.hpp"
#include "SparseBitVector.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../Base/SharedPointer.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class CFGPrinter : public DotPrinterBase {
private:
	typedef Dominators<Block, Function>::TAlgorithm TAlgorithm;
	typedef DominatorTree<Block, Function, TAlgorithm> TDominatorTree;
	typedef DominanceFrontier<TDominatorTree> TDominanceFrontier;
    SparseBitVector unreachableBlocks_;

    bool IsUnreachable(const Block* block) {
        if(block->IsUnreachable()) return true;
        auto predecessorEnum = block->GetPredecessorEnum();
        int count = 0;
        
        // This is only an approximation...
        while(predecessorEnum.IsValid()) {
            auto predecessorBlock = predecessorEnum.Next();

            if(unreachableBlocks_.IsSet(predecessorBlock->Id())) {
                count++;
                continue;
            }
            else if(auto branch = predecessorBlock->BranchInstruction()) {
                if(branch->IsGoto()) return false;
                else if(auto ifInstr = branch->As<IfInstr>()) {
                    if(ifInstr->ConditionOp()->IsUndefinedConstant()) {
                        count++;
                    }
                    else if(auto intConst = ifInstr->ConditionOp()->As<IntConstant>()) {
                        if(intConst->IsZero() && 
                           (block == ifInstr->TrueTargetOp()->Target())) {
                            count++;
                        }
                        else if(block == ifInstr->FalseTargetOp()->Target()) {
                            count++;
                        }
                    }
                }
                else if(auto switchInstr = branch->As<SwitchInstr>()) {
                    if(switchInstr->ConditionOp()->IsUndefinedConstant()) {
                        break;
                    }
                    if(auto intConst = switchInstr->ConditionOp()->As<IntConstant>()) {
                        bool foundCase = false;

                        for(int i = 0; i < switchInstr->CaseCount(); i++) {
                            if(switchInstr->GetCase(i).Value == intConst->Value()) {
                                foundCase = true;
                                if(switchInstr->GetCase(i).Target->Target() != block) {
                                    count++;
                                }
                                break;
                            }
                        }

                        if((foundCase == false) && 
                           (block != switchInstr->DefaultTargetOp()->Target())) {
                            count++;
                        }
                    }
                }
            }
            else count++;
        }

        if((count == block->PredecessorCount()) &&
           (block->IsFunctionEntry() == false)) {
            unreachableBlocks_.SetBit(block->Id());
            return true;
        }
        else return false;
    }

	void NewBlockNode(const Block* block, bool markUnreachable = true,
                      bool onFrontier = false, bool frontierBlock = false) {
		const char* color = COLOR_LIGHT_GREY;
		const char* shape = SHAPE_RECT;

		if(auto branch = block->BranchInstruction()) {
			if(branch->IsIf()) {
				color = COLOR_LIGHT_BLUE;
				shape = SHAPE_HEXAGON;
			}
			else if(branch->IsSwitch()) {
				color = COLOR_DARK_YELLOW;
				shape = SHAPE_ELLIPSE;
			}
			else if(branch->IsReturn()) {
				color = COLOR_GREEN;
				shape = SHAPE_DIAMOND;
			}
		}

		// Mark the block if it's on the dominance frontier.
		if(onFrontier) {
			color = COLOR_LIGHT_PINK;
		}
		else if(frontierBlock) {
			color = COLOR_PINK;
		}

        if(markUnreachable && IsUnreachable(block)) {
            color = COLOR_DARK_RED;
        }

		// Create a node for this block, then for each successor.
		NewNode(block, block->HasName() ? *block->Name() : L"UNTITLED",
				color, shape);
	}

public:
	CFGPrinter(const string& file) : DotPrinterBase(file) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Prints the CFG of the specified function to a DOT file.
	// If 'immediateDoms' is 'true' then the blocks are linked to their immediate dom.
	void Print(const Function* function, bool markUnreachable = true,
               bool immediateDoms = false, Block* frontierBlock = nullptr) {
		shared<TDominatorTree> domTree;
		shared<TDominanceFrontier> frontier;

		if(immediateDoms || frontierBlock) {
			domTree = new TDominatorTree(function);
			domTree->Build();

			if(frontierBlock) {
				frontier = new TDominanceFrontier(domTree);
				frontier->Build();
			}
		}

		// Iterate over all successors.
		const Block* block = function->FirstBlock();

		while(block) {
			NewBlockNode(block, markUnreachable, false, block == frontierBlock);

			block->ForEachSuccessor([&](Block* succ, int index) -> bool {
				bool onFrontier = frontierBlock && 
                                  frontier->IsOnFrontier(succ, frontierBlock);
				this->NewBlockNode(succ, markUnreachable, onFrontier,
                                   succ == frontierBlock);

				// Link 'block' with 'succ'.
				string label = L"";
				auto branch = block->BranchInstruction();
				if(branch == nullptr) return true;

				if(auto ifInstr = branch->As<IfInstr>()) {
					if(ifInstr->TrueTargetOp()->Target() == succ) label = L"T";
					else label = L"F";
				}
				else if(auto switchInstr = branch->As<SwitchInstr>()) {
					// See which 'switch' branch this is.
					for(int i = 0; i < switchInstr->CaseCount(); i++) {
						if(switchInstr->GetCase(i).Target->Target() == succ) {
							label = string::Format(L"%d", switchInstr->GetCase(i).Value);
							break;
						}
					}
				}

				this->Link(block, succ, label);
				return true;
			});

			// Create a link to the immediate dominator, if requested.
			if(immediateDoms) {
				const Block* idom = domTree->GetImmediateDominator(block);
				if(idom) {
					this->NewBlockNode(idom, markUnreachable);
					this->Link(block, idom, L"", COLOR_RED, true);
				}
			}

			block = block->NextBlock();
		}

		Close();
	}
};

} // namespace Analysis
#endif