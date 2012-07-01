// SimpleDeadCodeElimination.hpp
// Copyright (c) Lup Gratian
//
// Implements the SimpleDeadCodeElimination class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "SimpleDeadCodeElimination.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

void SimpleDeadCodeElimination::Execute(Function* function) {
    StaticList<Instruction*, 512> worklist;
    Dictionary<Instruction*, bool> inWorklist; // If an instruction is in the worklist.

    // Try to remove 'store' instructions that have no effect.
    // The algorithm is really simple, but should catch cases like
    // arrays initialized with constants that were propagated already.
    RemoveDeadStores(function);

    // Try to remove copy/set operations that are unused,
    // because the aggregates they target are never referenced.
    RemoveDeadCopyOperations(function);

	// We process the blocks from last to first, and the instructions in the block
	// from last to first too; this allows removing more instructions on each
	// iteration that the usual first-last order.
	for(auto block = function->LastBlock(); block; block = block->PreviousBlock()) {
        // If the block is unreachable we remove all instructions from it,
        // but don't remove the block; this will be handled by the CFG Simplifier,
        // which knows how to repair the Dominator Tree.
        if(block->IsUnreachable() && (block->IsEmpty() == false)) {
            CleanUnreachableBlock(block);
            continue;
        }

        for(auto instr = block->LastInstruction(); instr; 
            instr = instr->PreviousInstruction()) {
            if(GetSafetyInfo()->IsDefinitelyDead(instr)) {
                worklist.Add(instr);
                inWorklist.Add(instr, true);
            }
        }
    }

	// Process while we have instructions in the worklist.
    while(worklist.IsNotEmpty()) {
        auto instr = worklist.RemoveLast();
        inWorklist.Remove(instr);

        // Remove the instruction if it's dead.
        if(GetSafetyInfo()->IsDefinitelyDead(instr)) {
            // All the instructions that where used by this one
            // may be dead now, add them to the worklist.
            for(int i = 0; i < instr->SourceOpCount(); i++) {
                auto sourceOp = instr->GetSourceOp(i);

                // Make sure we don't add an instruction in the worklist twice.
                if(auto definingInstr = sourceOp->DefiningInstruction()) {
                    if(inWorklist.ContainsKey(definingInstr) == false) {
                        worklist.Add(definingInstr);
                        inWorklist.Add(definingInstr, true);
                   }
                }
            }

            InstructionRemoved(instr);
            instr->RemoveFromBlock(true /* free */);
        }
    }

    // If there were removed stored we may now have variables
    // that are not used by any instruction.
    RemoveDeadVariables(function);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SimpleDeadCodeElimination::RemoveDeadStores(Function* function) {
    // This implements a flow-insensitive algorithm that scan the whole function 
    // and collect all the 'store' instructions that target an array/record
    // whose address is not taken. Analyzing this set we may be able to delete
    // dead 'store' instructions.
    auto& variables = function->Variables();
    storesRemoved_ = 0;

    // We process each variable, one by one, and use a very conservative approach
    // (any suspect operation renders all stores live, even if they probably aren't).
    for(int i = 0; i < variables.Count(); i++) {
        auto variable = variables[i];

        if((variable->IsArray() || variable->IsRecord()) == false) {
            continue;
        }
        else if(variable->IsAddressTaken()) {
            continue;
        }

        // Collect all 'store' instructions that write to this variable.
        StoreCollection stores;
        CollectStoreInstructions(variable, stores, function);

        if(stores.StoreList.Count() > 0) {
            DetermineLiveStores(variable, stores, function);

            // Remove all stores that are definitely dead.
            for(int j = 0; j < stores.StoreList.Count(); j++) {
                InstructionRemoved(stores.StoreList[j].Target);
                stores.StoreList[j].Target->RemoveFromBlock(true /* free */);
                storesRemoved_++;
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SimpleDeadCodeElimination::CollectStoreInstructions(Variable* variable, 
                                                         StoreCollection& stores, 
                                                         Function* function) {
    stores.HasVariableIndex = false;

    function->ForEachInstruction([&stores, variable, function, this]
                                  (Instruction* instr) -> bool {
        if(auto storeInstr = instr->As<StoreInstr>()) {
            LoadStoreInfo info;
            info.Target = storeInstr;
            info.HasVariableIndex = false;
            info.Valid = false;

            // Collect the 'index'/'elem' instruction indexes used as the base.
            this->AddIndexes(storeInstr->DestinationOp(), info, 
                             variable, true /* isStore */);

            if(info.Valid) {
                stores.StoreList.Add(info);
                stores.HasVariableIndex |= info.HasVariableIndex;
            }
        }
        return true;
    });

    stores.HasVariableIndex = stores.HasVariableIndex;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SimpleDeadCodeElimination::DetermineLiveStores(Variable* variable, 
                                                    StoreCollection& stores, 
                                                    Function* function) {
    // Any 'store' that might be live is removed from the list.
    // We need to check 'load' and 'call' instructions only.
    function->ForEachInstruction([&stores, variable, function, this]
                                (Instruction* instr) -> bool {
        if(auto loadInstr = instr->As<LoadInstr>()) {
            LoadStoreInfo info;
            info.Valid = false;
            info.HasVariableIndex = false;
            this->AddIndexes(loadInstr->SourceOp(), info, variable,
                             false /* isStore */);

            // Check if the 'load' is from our variable.
            if(info.Valid == false) {
                return true;
            }

            if(RemoveLiveStoresFromList(info, stores)) {
                // All stores are live, no reason to continue.
                return false;
            }
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            // Any local variable that appears as an argument is kept alive, 
            // because we don't know exactly if it's needed or not.
            for(int i = 0; i < callInstr->ArgumentCount(); i++) {
                auto argOp = callInstr->GetArgument(i);

                if(argOp->IsPointer()) {
                    LoadStoreInfo info;
                    info.Valid = true;
                    this->AddIndexes(argOp, info, variable, false /* isStore */);  
                    info.HasVariableIndex = true;

                    if(RemoveLiveStoresFromList(info, stores)) {
                        // All stores are live, no reason to continue.
                        return false;
                    }
                }
            }
        }
        else if(auto retInstr = instr->As<ReturnInstr>()) {
            if(retInstr->IsVoid() == false) {
                LoadStoreInfo info;
                info.Valid = false;
                info.HasVariableIndex = false;
                this->AddIndexes(retInstr->ReturnedOp(), info, variable, 
                                 false /* isStore */);  

                if(RemoveLiveStoresFromList(info, stores)) {
                    // All stores are live, no reason to continue.
                    return false;
                }
            }
        }
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SimpleDeadCodeElimination::RemoveLiveStoresFromList(LoadStoreInfo& info, 
                                                         StoreCollection& stores) {
    // Any 'store' that might be live is removed from the list.
    // If the index is not a constant, or we have 'addr' or 'ptop'
    // instructions involved we need to give up and presume
    // that all stores are live.
    if(info.HasVariableIndex) {
        stores.StoreList.Clear();
        return true;
    }

    // If we have a 'store' that uses a variable-indexed operand
    // we need to presume that all stores are live.
    if(stores.HasVariableIndex) {
        stores.StoreList.Clear();
        return true;
    }

    // Check if there is a 'store' to exactly the same address.
    // If there is it must be removed from the list, because it's live.
    for(int i = 0; i < stores.StoreList.Count(); i++) {
        if(this->IsSameLocation(stores.StoreList[i], info)) {
            stores.StoreList.RemoveAt(i);
            i--;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SimpleDeadCodeElimination::AddIndexes(Operand* op, LoadStoreInfo& info, 
                                           Variable* variable, bool isStore) {
    if(auto variableRef = op->As<VariableReference>()) {
        if(variableRef->GetVariable() == variable) {
            // Found the base variable.
            info.Valid = true;
        }
        return;
    }
    else if(auto indexInstr = op->DefiningInstrAs<IndexInstr>()) {
        // Check if we have a constant index.
        if(auto intConst = indexInstr->IndexOp()->As<IntConstant>()) {
            info.IndexList.Add(intConst->Value());
        }
        else info.HasVariableIndex = true;

        AddIndexes(indexInstr->BaseOp(), info, variable, isStore);
        return;
    }
    else if(auto elemInstr = op->DefiningInstrAs<ElementInstr>()) {
        info.IndexList.Add(elemInstr->GetFieldIndex());
        AddIndexes(elemInstr->BaseOp(), info, variable, isStore);
        return;
    }
    else if(isStore == false) {
        // Keep track of 'addr' and 'ptop' that lead to the variable,
        // because they keep alive any store.
        if(auto addrInstr = op->DefiningInstrAs<AddressInstr>()) {
            info.HasVariableIndex = true;
            AddIndexes(addrInstr->BaseOp(), info, variable, isStore);
        }
        else if(auto ptopInstr = op->DefiningInstrAs<PtopInstr>()) {
            info.HasVariableIndex = true;
            AddIndexes(ptopInstr->TargetOp(), info, variable, isStore);
        }
        return;
    }

    // Any other instruction renders the 'store' invalid.
    info.Valid = false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SimpleDeadCodeElimination::IsSameLocation(LoadStoreInfo& store, 
                                               LoadStoreInfo& load) {
    if(store.IndexList.Count() != load.IndexList.Count()) {
        return false;
    }
    
    auto& list1 = store.IndexList;
    auto& list2 = load.IndexList;

    for(int i = 0; i < list1.Count(); i++) {
        if(list1[i] != list2[i]) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SimpleDeadCodeElimination::RemoveDeadCopyOperations(Function* function) {
    // After other optimizations there often remain aggregate
    // copy/set operations that are not needed anymore,
    // because the aggregate is not referenced (besides by these calls).
    // This is a flow-insensitive algorithm that counts the number
    // of times a variable is referenced, and compares it with
    // the number of times it's referenced by a copy/set call.
    // If the numbers are equal the operations are dead and the variable
    // can later be removed.
    CopySetDict copySetInfo;

    for(int i = 0; i < function->VariableCount(); i++) {
        auto variable = function->GetVariable(i);

        if(variable->IsArray() || variable->IsRecord()) {
            copySetInfo.Add(variable, CopySetInfo(variable));
        }
    }

    // If there are no aggregate variables there is
    // no reason to scan the function body.
    if(copySetInfo.Count() == 0) {
        return;
    }

    function->ForEachInstruction([&copySetInfo, this](Instruction* instr) -> bool {
        // Most instructions can't reference a variable directly.
        if(instr->IsArithmetic() || instr->IsLogical()) {
            return true;
        }

        // Check if we have a 'call' to 'copyMemory'/'setMemory'.
        if(auto callInstr = instr->As<CallInstr>()) {
            if(AnalyzeDeadCopyCall(callInstr, copySetInfo)) {
                // Don't analyze the operands again.
                return true;
            }
        }

        // Check if any operands are variable references.
        for(int i = 0; i < instr->SourceOpCount(); i++) {
            if(auto variableRef = instr->GetSourceOp(i)->As<VariableReference>()) {
                if(auto variable = variableRef->GetVariable()) {
                    if(copySetInfo.ContainsKey(variable)) {
                        // Count the access.
                        copySetInfo[variable].References++;
                    }
                }
            }
        }

        return true;
    });

    // Delete any copy/set operation that is not needed.
    copySetInfo.ForEachValue([](CopySetInfo& info) -> bool {
        if(info.References == info.CopySetReferences) {
            for(int i = 0; i < info.CopySetCalls.Count(); i++) {
                info.CopySetCalls[i]->RemoveFromBlock(true);
            }
        }
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SimpleDeadCodeElimination::AnalyzeDeadCopyCall(CallInstr* callInstr, 
                                                    CopySetDict& copySetInfo) {
    auto intrinsic = callInstr->GetIntrinsic();
    if(intrinsic == nullptr) {
        return false;
    }

    // Check if a local aggregate is referenced.
    if(intrinsic->IsMemoryIntrinsic() == false) {
        return false;
    }

    // Analyze the destination operand, and for
    // 'copyMemory' the source also.
    bool hasDestVariable = false;

    if(auto destVar = GetBaseVariable(callInstr->GetArgument(0))) {
        if(copySetInfo.ContainsKey(destVar)) {
            // Count the access.
            CopySetInfo& info = copySetInfo[destVar];
            info.CopySetReferences++;
            info.CopySetCalls.Add(callInstr);
            hasDestVariable = true;
        }
    }

    if(auto sourceVar = GetBaseVariable(callInstr->GetArgument(1))) {
        if(copySetInfo.ContainsKey(sourceVar)) {
            // Count the access.
            CopySetInfo& info = copySetInfo[sourceVar];
            info.CopySetReferences++;

            if(hasDestVariable == false) {
                info.CopySetCalls.Add(callInstr);
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable* SimpleDeadCodeElimination::GetBaseVariable(Operand* op) {
    if(auto variableRef = op->As<VariableReference>()) {
        return variableRef->GetVariable();
    }
    else if(auto definingInstr = op->DefiningInstruction()) {
        if(definingInstr->IsAddress() ||
           definingInstr->IsIndex()   ||
           definingInstr->IsElement() ||
           definingInstr->IsPtop()) {
            return GetBaseVariable(definingInstr->GetSourceOp(0));
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SimpleDeadCodeElimination::RemoveDeadVariables(Function* function) {
    SparseBitVector liveVars;

    // Scan every instruction in the function and mark any local variable
    // that appears as an operand. All variables that are not marked are deleted.
    function->ForEachInstruction([&liveVars](Instruction* instr) -> bool {
        for(int i = 0; i < instr->SourceOpCount(); i++) {
            if(auto variableRef = instr->GetSourceOp(i)->As<VariableReference>()) {
                auto variable = variableRef->GetVariable(); // Only local variables.

                if(variable && (variable->IsParameter() == false)) {
                    liveVars.SetBit(variable->Id());
                }
            }
        }

        return true;
    });

    // Now removed all unused variables.
    for(int i = 0; i < function->VariableCount(); i++) {
        auto variable = function->Variables()[i];

        if(liveVars.IsSet(variable->Id()) == false) {
            function->RemoveVariable(variable);
            i--;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SimpleDeadCodeElimination::CleanUnreachableBlock(Block* block) {
	while(block->FirstInstruction()) {
		auto instr = block->FirstInstruction();
        if(instr->IsBranching()) break;
		block->RemoveInstruction(instr, true /* free */);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SimpleDeadCodeElimination::InstructionRemoved(Instruction* instr) {
#if 1
	auto block = instr->ParentBlock();
	auto function = instr->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string text = IRPrinter(instr).ToString();
	Log::Warning("Dead instr removed in " + functionName + ":" + blockName +
				 ": " + text);
#endif
}

} // namespace Analysis