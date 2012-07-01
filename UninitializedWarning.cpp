// UninitializedWarning.cpp
// Copyright (c) Lup Gratian
//
// Implementation of the UninitializedWarning analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "UninitializedWarning.hpp"

namespace Analysis {
    
void UninitializedWarning::Execute(Function* function) {
    DebugValidator::IsNotNull(function);

	// We have nothing to do if there are no blocks or variables in the function.
    // Note that we don't need to check the parameters, because we presume
    // that all of them have properly defined values.
    funct_ = function;
	if(funct_->BlockCount() == 0) return;
	if(funct_->VariableCount() == 0) return;

	// We use a worklist-driven algorithm to propagate the information.
	Initialize();

	while(worklist_.IsNotEmpty()) {
		Block* block = worklist_.Dequeue();
        inWorklist_.ResetBit(block->Id());

		// Combine the IN set has the values from the predecessors 
        // combined using an OR (join) operation. 
        // Note that for the entry block we don't do this, because there
        // are no predecessors; instead we use the bitvector defined by 'Initialize'.
		BitVector inSet(VariableCount());

        if(block == funct_->FirstBlock()) {
            inSet = inSets_[block];
        }
		else {
            auto predecessorEnum = block->GetPredecessorEnum();

		    while(predecessorEnum.IsValid()) {
			    Block* pred = predecessorEnum.Next();
			    inSet.Or(outSets_[pred]);
		    }
            
            // Update the IN block.
            inSets_[block] = inSet;
        }

		// Now analyze the block using the collected information.
		BitVector genSet(VariableCount());
		BitVector killSet(VariableCount());
		AnalyzeBlock(block, inSet, genSet, killSet);

		// The OUT set is the combination of the IN and GEN sets.
		BitVector outSet(inSet);
		outSet.Or(killSet);
		outSet.Difference(genSet);

		// If the sets are different then record the change, 
		// and add all the successors to the worklist.
		if(outSet != outSets_[block]) {
			outSets_[block] = outSet;
			auto successorEnum = block->GetSuccessorEnum();

			while(successorEnum.IsValid()) {
                auto successorBlock = successorEnum.Next();

                if(inWorklist_.IsSet(successorBlock->Id() == false)) {
    				worklist_.Enqueue(successorBlock);
                    inWorklist_.SetBit(successorBlock->Id());
                }
			}
		}
	}

    // Now verify if there are any loads from undefined variables.
    for(auto block = funct_->FirstBlock(); block; block = block->NextBlock()) {
        BitVector genSet(VariableCount());
        BitVector killSet(VariableCount());
        AnalyzeBlock(block, inSets_[block], genSet, killSet, true);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void UninitializedWarning::Initialize() {
    // Create a bit vector for each block. The blocks are added to the worklist
    // in reverse postorder to minimize the iteration count.
    CFGInfo<Block, Function> cfgInfo(funct_->FirstBlock(), false);
    auto& postorder = cfgInfo.PostorderList();

    for(int i = postorder.Count() - 1; i >= 0; i--) {
        auto block = const_cast<Block*>(postorder[i]);
        inSets_.Add(block, BitVector(VariableCount(), true));
        outSets_.Add(block, BitVector(VariableCount(), false));
        worklist_.Enqueue(block);
        inWorklist_.SetBit(block->Id());
    }

    // At the beginning the start node is in the list, and all variables
    // are considered to be "undefined", except the parameters.

    auto& parameters = funct_->Parameters();
    auto& entryBits = inSets_[funct_->FirstBlock()];

    for(int i = 0; i < parameters.Count(); i++) {
        entryBits.ResetBit(BitIndex(parameters[i]));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void UninitializedWarning::CheckDefinedByStore(StoreInstr* storeInstr, BitVector& inSet, 
											   BitVector& genSet, BitVector& killSet) {
    // If we store a variable into a pointer mark the variable as initialized,
    // because it's too complex to track what happens with the pointer.
    if(auto variableRef = storeInstr->SourceOp()->As<VariableReference>()) {
        if(variableRef->IsLocalVariableRef()) {
            int bitIndex = BitIndex(variableRef->GetVariable());
            genSet.SetBit(bitIndex);
            killSet.ResetBit(bitIndex);
       }   
    }

    // Check if a local variable is defined.
	if(auto variableRef = storeInstr->DestinationOp()->As<VariableReference>()) {
		// Consider only local variables. 
		if(variableRef->IsLocalVariableRef() == false) return;
		
		// If the stored value is from a variable that was not initialized
		// then we add the variable to the "kill" set.
		if(auto loadInstr = storeInstr->SourceOp()->DefiningInstrAs<LoadInstr>()) {
			auto loadedVar = loadInstr->SourceOp()->As<VariableReference>();
            
            if(loadedVar && loadedVar->IsLocalVariableRef()) {
		        int loadedBitIndex = BitIndex(loadedVar->GetVariable());
                
                if(IsUndefined(loadedVar->GetVariable(), inSet, genSet, killSet)) {
					// The stored value is undefined.
					int bitIndex = BitIndex(variableRef->GetVariable());
					killSet.SetBit(bitIndex);
					genSet.ResetBit(bitIndex);
					return;
				}
			}
		}

		// Mark it as initialized.
		int bitIndex = BitIndex(variableRef->GetVariable());
		genSet.SetBit(bitIndex);
        killSet.ResetBit(bitIndex);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool UninitializedWarning::IsUndefined(Variable* variable, BitVector& inSet, 
                                       BitVector& genSet, BitVector& killSet) {
    // A variable is undefined if it's not in the 'gen' set,
    // it's not in the 'in' set', or if it's in the 'in' set it's also
    // in the 'kill' set, meaning that it was killed before reaching this point.
    int index = BitIndex(variable);
    return (genSet.IsSet(index) == false) &&
           (inSet.IsSet(index) || killSet.IsSet(index));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void UninitializedWarning::CheckDefinedByCall(CallInstr* callInstr, BitVector& inSet, 
											  BitVector& genSet, BitVector& killSet) {
	if(callInstr->ArgumentCount() == 0) return;

	// If one the arguments is the address of a variable then we should consider
	// that its value is changed. It may not be always true, but it's 
    // a safe and conservative assumption. 
	const CallInstr::ArgumentList& arguments = *callInstr->Arguments();

	for(int i = 0; i < arguments.Count(); i++) {
		if(auto variableRef = arguments[i]->As<VariableReference>()) {
			if(variableRef->IsLocalVariableRef()) {
				// Mark it as initialized.
				int bitIndex = BitIndex(variableRef->GetVariable());
				genSet.SetBit(bitIndex);
                killSet.ResetBit(bitIndex);
				break;
			}
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void UninitializedWarning::CheckUndefinedLoad(LoadInstr* loadInstr, BitVector& inSet, 
											  BitVector& genSet, BitVector& killSet) {
	// Check if the variable has a value defined; if not we emit a warning.
	if(auto variableRef = loadInstr->SourceOp()->As<VariableReference>()) {
		// Consider only local variables.
		if(variableRef->IsLocalVariableRef() == false) return;
				
		if(IsUndefined(variableRef->GetVariable(), inSet, genSet, killSet)) {
			// The variable is not initialized.
			EmitWarning(variableRef->GetVariable(), loadInstr->ParentBlock());
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void UninitializedWarning::AnalyzeBlock(Block* block, BitVector& inSet, 
                                        BitVector& genSet, BitVector& killSet,
                                        bool handleLoads) {
	// Scan over all instructions in the block. Consider a direct 'store' to
	// a variable and variables whose address is passed to a function.
	for(Instruction* instr = block->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
		if(auto storeInstr = instr->As<StoreInstr>()) {
			CheckDefinedByStore(storeInstr, inSet, genSet, killSet);
		}
		else if(auto callInstr = instr->As<CallInstr>()) {
			CheckDefinedByCall(callInstr, inSet, genSet, killSet);
		}

        if(handleLoads && instr->IsLoad()) {
            CheckUndefinedLoad(instr->As<LoadInstr>(), inSet, genSet, killSet);
        }
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void UninitializedWarning::EmitWarning(Variable* variable, Block* block) {
	// Emit the warning only once per variable.
	if(handledVars_.IsSet(BitIndex(variable))) return;

	//! TODO: emit the warning

    UndefinedLoad(variable, block);
	handledVars_.SetBit(BitIndex(variable));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UninitializedWarning::UndefinedLoad(Variable* variable, Block* block) {
#if 1
    string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
    string varName = variable && variable->HasName() ? *variable->Name() : "UNTITLED";
    Log::Warning("Undefined load " + varName + ":" + blockName);
#endif
}

} // namespace Analysis