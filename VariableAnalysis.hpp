// VariableAnalysis.hpp
// Copyright (c) Lup Gratian
//
// Performs the variable analysis used by the SSA conversion pass.
// Computes the address-taken variable set, and the sets with the blocks
// where each variable is live and where it's assigned to.
// Note that array/record variables are considered not to have their
// address taken if they're used only by 'index'/'elem' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_VARIABLE_ANALYSIS_HPP
#define PC_ANALYSIS_VARIABLE_ANALYSIS_HPP

#include "../IR/Variable.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Operand.hpp"
#include "../IR/References.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/Unit.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/ObjectDumper.hpp"
#include "SparseBitVector.hpp"
#include "BitVector.hpp"
#include "CFGWalker.hpp"
#include "LanguageInfo.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace CompilationPass;

namespace Analysis {

class VariableAnalysis : public Pass {
private:
    MAKE_PAIR(OperandVariablePair, Operand*, PairOperand, Variable*, PairVariable);

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	typedef OperandVariablePair OVPair;
	typedef Dictionary<Operand*, OVPair> OperandVariableDict;
    typedef Function::VariableList VariableList;

	Function* funct_;
	Dictionary<int, SparseBitVector> defBlocks_;  // Definition block set for each variable.
	Dictionary<int, SparseBitVector> liveBlocks_; // Live block set for each variable.
    Dictionary<int, Block*> idToBlock_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	void MarkAddressTaken(Instruction* instr);

	void MarkDefinition(Instruction* instr);

	// Computes the sets of variables that are used 
    //and killed in the specified block.
	void ComputeBlockExposedAndKillSets(Block* block, BitVector& exposedSet,
										BitVector& killSet);

	// Computes the sets with blocks where each variable is used.
	void ComputeLiveBlocks(Dictionary<Block*, BitVector>& exposedSets);

	// Analyzes the function in order to mark aggregate variables
	// which have their address taken.
	void AnayzeAggregateOperations(OperandVariableDict& opDict);

    void AnalyzeAggregatesInIntruction(Instruction* instr, 
                                       OperandVariableDict& opDict);

	// Analyzes a function call that may use the address
    // of an aggregate variable.
	void AnalyzeAggregateCall(CallInstr* callInstr, 
                              OperandVariableDict& opDict);

public:
    // Determines the variables that have their address taken,
    // and the blocks where each variable gets a new definition.
    void ComputeAddressTakenAndDefinitions(Function* function);

    void ComputeScalarAddressTaken(Function* function);

    // Computes the sets with the blocks where each variable is live.
    // Uses data-flow algorithm based on a worklist.
    void ComputeLiveSets();

	// Returns 'true' if the variable has its address taken in this function.
	bool IsAddressTaken(Variable* variable);

	// Returns the first block in which the variable is defined.
	// If 'startBlock' is not 'nullptr' then only blocks after the specified 
    // ones are considered. If no block is found it returns 'nullptr'.
	Block* GetFirstDefinitionBlock(Variable* variable, Block* startBlock = nullptr);

	// Returns 'true' if the specified variable is used (live) 
	// at the beginning of the specified block or in any of its successors.
	bool IsLiveInBlock(Variable* variable, Block* block);

	// Computes the set of aggregate variables which have their address taken
	// and sets the 'IsAddressTaken' flag for each one.
	void AnnotateAggregateAddressTaken(Function* function);

    void InsertAggregateCandidates(OperandVariableDict& dict, 
                                   const VariableList& varList);

	void Dump();
};

} // namespace Analysis
#endif