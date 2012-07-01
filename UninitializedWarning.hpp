// UninitializedWarning.hpp
// Copyright (c) Lup Gratian
//
// Defines an analysis pass that determinates which variables are not initialized
// before their value is first used. To be able to report the warning, the variable
// needs to be annotated with a 'LocationTag'.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_UNINITIALIZED_WARNING_HPP
#define PC_ANALYSIS_UNINITIALIZED_WARNING_HPP

#include "BitVector.hpp"
#include "SparseBitVector.hpp"
#include "CFGWalker.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Variable.hpp"
#include "../Base/Queue.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Log.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class UninitializedWarning {
private:
	Function* funct_;
	Dictionary<Block*, BitVector> inSets_;
    Dictionary<Block*, BitVector> outSets_;
	Queue<Block*> worklist_;
	SparseBitVector handledVars_;
    SparseBitVector inWorklist_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	int BitIndex(Variable* variable) {
		return variable->Id();
	}

	Variable* VariableAtIndex(int index) {
		return funct_->Variables()[index];
	}

    int VariableCount() {
        return funct_->VariableCount() + 
               funct_->ParameterCount();
    }

    bool IsUndefined(Variable* variable, BitVector& inSet, 
                     BitVector& genSet, BitVector& killSet);

	// Sets the target of the specified 'store' instruction as "defined"
	// if all the conditions are met.
	void CheckDefinedByStore(StoreInstr* storeInstr, BitVector& inSet, 
							 BitVector& genSet, BitVector& killSet);

	// Marks as "defined" the variables whose address is passed to a function.
	void CheckDefinedByCall(CallInstr* callInstr, BitVector& inSet, 
							BitVector& genSet, BitVector& killSet);

	// Verifies if the specified load is from an "undefined" variable.
	void CheckUndefinedLoad(LoadInstr* loadInstr, BitVector& inSet, 
							BitVector& genSet, BitVector& killSet);

	// Sets the bits corresponding to the variables that are initialized in the block.
	void AnalyzeBlock(Block* block, BitVector& inSet, BitVector& genSet,
					  BitVector& killSet, bool handleLoads = false);


	// Emits an "uninitialized variable" warning regarding the specified variable.
	void EmitWarning(Variable* variable, Block* block);

	// Initializes the objects for the data-flow simulation.
	void Initialize();

    void UndefinedLoad(Variable* variable, Block* block);

public:
    UninitializedWarning() {}

	void Execute(Function* function);
};

} // namespace Analysis
#endif