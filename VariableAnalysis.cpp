// VariableAnalysis.hpp
// Copyright (c) Lup Gratian
//
// Implements the VariableAnalysis class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "VariableAnalysis.hpp"

namespace Analysis {

void VariableAnalysis::ComputeAddressTakenAndDefinitions(Function* function) {
    DebugValidator::IsNotNull(function);

	// We consider that the variable has it's address taken
	// if it's used in any instruction that depends on it's address.
	// This includes storing the address, passing it as a parameter to a function,
	// converting it to an integer or another pointer type, etc.
    funct_ = function;

    // Initialize the map from block-id to block.
    for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
        idToBlock_.Add(block->Id(), block);
    }

	for(Block* block = funct_->FirstBlock(); block; block = block->NextBlock()) {
		for(auto instr = block->FirstInstruction(); instr;
            instr = instr->NextInstruction()) {
			MarkAddressTaken(instr);
			MarkDefinition(instr);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::ComputeScalarAddressTaken(Function* function) {
    funct_ = function;

    for(Block* block = funct_->FirstBlock(); block; block = block->NextBlock()) {
		for(auto instr = block->FirstInstruction(); instr; 
            instr = instr->NextInstruction()) {
			MarkAddressTaken(instr);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::MarkAddressTaken(Instruction* instr) {
	if(auto loadInstr = instr->As<LoadInstr>()) {
		// Volatile loads should not be eliminated.
		if(loadInstr->IsVolatile()) {
			if(auto variableRef = loadInstr->SourceOp()->As<VariableReference>()) {
				auto localVar = variableRef->GetVariable();
				
                if(localVar == nullptr) {
                    return;
                }
                
                localVar->SetIsAddresTaken(true);
			}
		}

		return; // We're done with this instruction.
	}
	else if(auto storeInstr = instr->As<StoreInstr>()) {
		// Volatile stores should not be eliminated.
		if(storeInstr->IsVolatile()) {
			if(auto variableRef = storeInstr->DestinationOp()->As<VariableReference>()) {
				auto localVar = variableRef->GetVariable();
				
                if(localVar == nullptr) {
                    return;
                }

				localVar->SetIsAddresTaken(true);
			}
		}

		// If the stored operand is a local variable we can't eliminate it.
		if(storeInstr->SourceOp()->IsLocalVariableRef()) {
			auto variableRef = storeInstr->SourceOp()->As<VariableReference>();
            variableRef->GetVariable()->SetIsAddresTaken(true);
		}

		return; // We're done with this instruction.
	}

	// Any other instruction that has a variable reference as it's operand
	// is considered to take it's address.
	for(int i = 0; i < instr->SourceOpCount(); i++) {
		if(auto variableRef = instr->GetSourceOp(i)->As<VariableReference>()) {
			auto localVar = variableRef->GetVariable();

			if(localVar == nullptr) {
                continue;
            }

			// We don't set the flag for pointers, arrays and records because
			// we have a separate step that takes care of this.
			if((localVar->IsPointer() || 
                localVar->IsRecord()  ||
				localVar->IsArray()) == false) {
                localVar->SetIsAddresTaken(true);
			}
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::AnnotateAggregateAddressTaken(Function* function) {
    DebugValidator::IsNotNull(function);

	// Try to determine which local variables of array and record type
	// have their address taken by different instruction than the ones
	// used to compute the offset into the object ('index', 'elem', 'addr', 'ptop')
	// or to load/store from/to elements of the objects.
	// First we analyze direct reference to the variables, then the result 
	// of the operations performed on them, and so on. As soon as we see an operation 
	// that is not safe we mark the related variable as address-taken.
    funct_ = function;
	OperandVariableDict opDict;
    InsertAggregateCandidates(opDict, funct_->Variables());
    InsertAggregateCandidates(opDict, funct_->Parameters());
	
	// Scan all instructions and determine if they take the address
	// of the variables found in the dictionary. Further operand could be
	// added to while scanning the instructions (consider multi-dimensional arrays).
	AnayzeAggregateOperations(opDict);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::InsertAggregateCandidates(OperandVariableDict& dict, 
                                                 const VariableList& varList) {
    auto unit = funct_->ParentUnit();

    for(int i = 0; i < varList.Count(); i++) {
        auto variable = varList[i];

        if((variable->IsArray()  || 
            variable->IsRecord() || 
            variable->IsPointer()) == false) {
            continue;
        }

        variable->SetIsAddresTaken(false); // Presume it's not.
        auto variableRefType = unit->Types().GetPointer(variable->GetType());
        auto variableRef = unit->References().GetVariableRef(variable, variableRefType);

        dict.Add(variableRef, OVPair(variableRef, variable));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::AnayzeAggregateOperations(OperandVariableDict& opDict) {
    // We use a reverse-postorder walk that guarantees that we visit
    // all predecessors of a block before we visit that block.
    CFGInfo<Block, Function> cfgInfo(funct_, false /* edgeInfoNeeded */);
    auto& postorderList = cfgInfo.PostorderList();

    for(int i = postorderList.Count() - 1; i >= 0; i--) {
        auto block = const_cast<Block*>(postorderList[i]);

        for(auto instr = block->FirstInstruction(); instr; 
            instr = instr->NextInstruction()) {
            AnalyzeAggregatesInIntruction(instr, opDict);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::AnalyzeAggregatesInIntruction(Instruction* instr,
                                                     OperandVariableDict& opDict) {
	switch(instr->GetOpcode()) {
		case Instr_Load: {
			auto loadInstr = instr->As<LoadInstr>();
			auto sourceOp = loadInstr->SourceOp();

			// If the load is volatile we mark the variable as address-taken.
			if(opDict.ContainsKey(sourceOp)) {
				if(loadInstr->IsVolatile()) {
					opDict[sourceOp].PairVariable->SetIsAddresTaken(true);
				}
			}
			break;
		}
		case Instr_Store: {
			auto storeInstr = instr->As<StoreInstr>();
			auto sourceOp = storeInstr->SourceOp();
			auto destOp = storeInstr->DestinationOp();

			// If the store is volatile we mark the variable as address-taken.
			if(opDict.ContainsKey(destOp)) {
				if(storeInstr->IsVolatile()) {
					opDict[destOp].PairVariable->SetIsAddresTaken(true);
				}
			}

			// If the operand that is stored is in the dictionary
			// mark the associated variable as address-taken.
			if(opDict.ContainsKey(sourceOp)) {
				opDict[sourceOp].PairVariable->SetIsAddresTaken(true);
			}
			break;
		}
		case Instr_Address:
		case Instr_Index: 
		case Instr_Element:
		case Instr_Ptop: {
			// All these instructions take the address only to compute
			// an offset. The resulting temporary is added to the worklist,
			// because it may be used in a way that renders the variable
			// as being address-taken.
			auto baseOp = instr->GetSourceOp(0);

			if(opDict.ContainsKey(baseOp) && instr->HasDestinationOp()) {
				auto destOp = instr->GetDestinationOp();
				opDict.Add(destOp, OVPair(destOp, opDict[baseOp].PairVariable));
			}
			break;
		}
		case Instr_Call: {
			AnalyzeAggregateCall(instr->As<CallInstr>(), opDict);
			break;
		}
		default: {
			// We need to presume that the instruction renders the variable
			// as being address-taken for all other instructions.
			for(int i = 0; i < instr->SourceOpCount(); i++) {
				auto op = instr->GetSourceOp(i);

				if(opDict.ContainsKey(op)) {
					opDict[op].PairVariable->SetIsAddresTaken(true);
				}
			}
			break;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::AnalyzeAggregateCall(CallInstr* callInstr, 
                                            OperandVariableDict& opDict) {
	// Check if any of the arguments is in the dictionary.
	// If we call a function from the standard library we may be able
	// to prove that the address is not taken.
    if(callInstr->HasArguments() == false) {
        return;
    }

    // Check if we have a 'copyMemory' or 'setMemory' intrinsic call.
    // These are frequently used for record copy operations.
    CallInstr::ArgumentList& arguments = *callInstr->Arguments();

    if(auto intrinsic = callInstr->GetIntrinsic()) {
        if(intrinsic->Is<CopyMemoryIntr>() ||
           intrinsic->Is<SetMemoryIntr>()) {
           return;
        }
    }

    if(callInstr->CalledFunctionType()->IsVarargs()) {
        // Mark all arguments as address taken.
        for(int i = 0; i < arguments.Count(); i++) {
            if(opDict.ContainsKey(arguments[i])) {
                opDict[arguments[i]].PairVariable->SetIsAddresTaken(true);
            }
        }

        return;
    }

	for(int i = 0; i < arguments.Count(); i++) {
		if(opDict.ContainsKey(arguments[i]) == false) {
            continue;
        }

        // Check if this is a record that is passed "by value",
        // meaning that we're guaranteed that the address
        // doesn't escape the called function.
        if(auto pointerType = arguments[i]->GetType()->As<PointerType>()) {
            if(pointerType->PointeeType()->IsRecord()) {
                // We should call a known function, so that we can
                // check the attributes of the parameters.
                if(auto function = callInstr->GetCalledFunction()) {
                    auto parameter = function->GetParameter(i);

                    if(parameter->IsNoEscape()) {
                        continue;
                    }
                }
            }
        }

		// Check if the behavior of the function is known
        // because it's part of the standard library.
        if(auto info = GetLanguageInfo()) {
            bool paramReturned = false;

            if(info->CallMayCaptureParameter(callInstr, i, &paramReturned)) {
                // Track the parameter if it's only returned.
                // Otherwise presume its address is taken.
                if(paramReturned) {
                    auto destOp = callInstr->GetDestinationOp();
                    opDict.Add(destOp, OVPair(destOp, opDict[arguments[i]].PairVariable));
                    continue;
                }
            }
            else continue;
        }

        // Presume address it's taken.
        opDict[arguments[i]].PairVariable->SetIsAddresTaken(true);
	}
}								

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::MarkDefinition(Instruction* instr) {
	// We have a new definition if we have a store to a variable
	// that is not address taken yet.
	auto storeInstr = instr->As<StoreInstr>();
    
    if(storeInstr == nullptr) {
        return;
    }

	if(storeInstr->DestinationOp()->IsLocalVariableRef()) {
		auto variableRef = storeInstr->DestinationOp()->As<VariableReference>();
		auto localVar = variableRef->GetVariable();
		unsigned localVarId = localVar->Id();

		if(localVar->IsAddressTaken() == false) {
			// Mark the store.
			unsigned blockId = instr->ParentBlock()->Id();

			if(defBlocks_.ContainsKey(localVarId) == false) {
				// Create the set now.
				defBlocks_.Add(localVarId, SparseBitVector());
			}
				
			defBlocks_[localVarId].SetBit(blockId);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::ComputeBlockExposedAndKillSets(Block* block, 
													  BitVector& exposedSet,
													  BitVector& killSet) {
	// Scan all the instructions in the block and look for 'load' and 'store'.
	for(auto instr = block->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
		if(auto storeInstr = instr->As<StoreInstr>()) {
			// A variable can be killed only if it's stored into.
			// Note that we don't care about the effects of function calls,
			// because in that case the variable is marked as "address taken"
			// and it isn't considered for SSA conversion anyway.
			if(auto variableRef = storeInstr->DestinationOp()->As<VariableReference>()) {
				auto localVar = variableRef->GetVariable();
				
                if(localVar == nullptr) {
                    continue;
                }

				killSet.SetBit(localVar->Id());
			}
		}
		else if(auto loadInstr = instr->As<LoadInstr>()) {
			// The only way a variable can be "used" is through a 'load' instruction.
			// All other kinds of uses will render the variable as being "address taken".
			if(auto variableRef = loadInstr->SourceOp()->As<VariableReference>()) {
				auto localVar = variableRef->GetVariable();
				
                if(localVar == nullptr) {
                    continue;
                }

				// Mark the variable as 'exposed' only if it's not in the 'kill' set.
				if(killSet.IsSet(localVar->Id()) == false) {
					exposedSet.SetBit(localVar->Id());
				}
			}
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::ComputeLiveSets() {
	// Compute the local information for each block.
	// We use a worklist-based algorithm to propagate the changes.
	Dictionary<Block*, BitVector> killSets;
	Dictionary<Block*, BitVector> exposedSets;
	List<Block*> worklist;
	
    // If we don't have any local variable or parameter
    // nothing needs to be done (we always load from global variables).
	int varCount = funct_->VariableCount() + funct_->ParameterCount();
    
    if(varCount == 0) {
        return;
    }

    int bitCount = funct_->PeekNextVariableId();

	for(auto block = funct_->FirstBlock(); block; block = block->NextBlock()) {
		killSets.Add(block, BitVector(bitCount));
		exposedSets.Add(block, BitVector(bitCount));
		ComputeBlockExposedAndKillSets(block, exposedSets[block], 
                                       killSets[block]);
		worklist.Add(block);
	}

	// Propagate the live sets to all related blocks.
	// The data-flow equations used:
	// LiveOut(block) = U LiveIn(predecessor)
	// LiveIn(block) = (LiveOut(block) - Killed(block)) U Exposed(block)
	int lastCount = 0;

	while(worklist.Count() != lastCount) {
		int currentCount = worklist.Count();

		for(int i = worklist.Count() - 1; i >= lastCount; i--) {
			BitVector liveOutSet(bitCount);
			auto block = worklist[i];
			auto successorEnum = block->GetSuccessorEnum();

			while(successorEnum.IsValid()) {
				auto successorBlock = successorEnum.Next();
				liveOutSet.Or(exposedSets[successorBlock]);
			}

			// Compute the new exposed variables set.
			liveOutSet.Difference(killSets[block]);
			liveOutSet.Or(exposedSets[block]);

			if(liveOutSet != exposedSets[block]) {
				// The information has changed and must be updated.
				// All the predecessors must be reprocessed.
				exposedSets[block] = liveOutSet;
				auto predecessorEnum = block->GetPredecessorEnum();

				while(predecessorEnum.IsValid()) {
					worklist.Add(predecessorEnum.Next());
				}
			}
		}

		lastCount = currentCount;
	}

	ComputeLiveBlocks(exposedSets);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::ComputeLiveBlocks(Dictionary<Block*,
                                            BitVector>& exposedSets) {
	// Each block that uses a variable will be added to the set.
	for(auto block = funct_->FirstBlock(); block; block = block->NextBlock()) {
		int varIndex = -1;
		BitVector& exposedSet = exposedSets[block];

		do {
			varIndex = exposedSet.FirstSetBit(varIndex + 1);

			if(varIndex != -1) {
				// Add the block to the set associated with the variable.
				if(liveBlocks_.ContainsKey(varIndex) == false) {
					liveBlocks_.Add(varIndex, SparseBitVector());
				}

				liveBlocks_[varIndex].SetBit(block->Id());
			}
		}
		while(varIndex != -1);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool VariableAnalysis::IsLiveInBlock(Variable* variable, Block* block) {
	DebugValidator::IsNotNull(variable);
	DebugValidator::IsNotNull(block);
	DebugValidator::IsTrue(variable->ParentTable() == &funct_->Symbols());
	DebugValidator::IsTrue(block->ParentFunction() == funct_);

	if(liveBlocks_.ContainsKey(variable->Id()) == false) {
		return false;
	}
	else return liveBlocks_[variable->Id()].IsSet(block->Id());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* VariableAnalysis::GetFirstDefinitionBlock(Variable* variable, Block* startBlock) {
	DebugValidator::IsNotNull(variable);
	DebugValidator::IsTrue(variable->ParentTable() == &funct_->Symbols());

    if(defBlocks_.ContainsKey(variable->Id()) == false) {
        // The variable is not defined in the function.
        return nullptr;
    }

	int startIndex = startBlock ? startBlock->Id() + 1 : 0;
	int id = defBlocks_[variable->Id()].FirstSetBit(startIndex);

    if(id != -1) {
        return idToBlock_[id];
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableAnalysis::Dump() {
	ObjectDumper("", "SSA Variable Analysis").Dump();
	auto function = funct_;
	string text = "";

	// Address taken info.
    for(int i = 0; i < funct_->VariableCount(); i++) {
        auto variable = funct_->Variables()[i];

	    if(variable->IsAddressTaken()) {
		    if(variable->HasName()) {
			    text = text + *variable->Name() + ", ";
            }
		}
    }

    for(int i = 0; i < funct_->ParameterCount(); i++) {
        auto variable = funct_->Parameters()[i];

        if(variable->IsAddressTaken()) {
            if(variable->HasName()) {
                text = text + *variable->Name() + ", ";
            }
        }
    }

	ObjectDumper(text, "Address taken: ").Dump();

	// Definition and live blocks info.
	string defText = "";
	string liveText = "";

	for(int i = 0; i < funct_->VariableCount(); i++) {
		auto variable = funct_->Variables()[i];

		if(defBlocks_.ContainsKey(variable->Id())) {
			auto& blockSet = defBlocks_[variable->Id()];
			defText = defText + "\n" + (variable->HasName() ? 
                      *variable->Name() : "UNTITLED") + ": ";

			blockSet.ForEachSetBit([&defText, function](int index) -> bool {
				auto block = function->GetBlock(index);
				if(block->HasName()) {
					defText = defText + *block->Name() + ", ";
				}

				return true;
			});
		}

		if(liveBlocks_.ContainsKey(variable->Id())) {
			auto& liveSet = liveBlocks_[variable->Id()];
			liveText = liveText + "\n" + (variable->HasName() ? 
                       *variable->Name() : "UNTITLED") + ": ";

			liveSet.ForEachSetBit([&liveText, function](int index) -> bool {
				auto block = function->GetBlock(index);
				if(block->HasName()) {
					liveText = liveText + *block->Name() + ", ";
				}

				return true;
			});
		}
	}

	ObjectDumper(defText, "Definition blocks: ").Dump();
	ObjectDumper(liveText, "Live blocks: ").Dump();
}

} // namespace Analysis