// AggregateCopyPropagation.cpp
// Copyright (c) Lup Gratian
//
// Implements the Aggregate Copy Propagation pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "AggregateCopyPropagation.hpp"

namespace Optimization {

void AggregateCopyPropagation::Execute(Function* function) {
    // We use a worklist-driven, flow-sensitive and field-insensitive
    // data-flow algorithm, much like the one for scalar copy propagation
    // presented in Muchnick's optimization book.
    // The algorithm has 4 stages:
    // 1. Collect candidate 'copyMemory'/'setMemory' operations.
    //    This also does local copy propagation.
    // 2. Propagate to the block entries the available copies
    //    using a data-flow algorithm.
    // 3. Replace references to the copy to references to the original,
    //    where the copy is available.
    // 4. Remove copy operations that are dead (the copy is not used
    //    anymore; this is a flow-insensitive algorithm).
    funct_ = function;
    CollectCopySetFromFunction();

    if(infoList_.Count() > 0) {
        InitializeAnalysis();
        IterateToFixedpoint();
        PropagateCopies();
    }

    RemovedDeadCopySetCalls();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::CollectCopySetFromFunction() {
    // Traverse the CFG is reverse postorder and collect
    // the copy and set operations. At the same time we build
    // the copy and kill sets, which are used by the dataflow algorithm,
    // and also do local copy propagation.
    CFGInfo<Block, Function> cfgInfo(funct_->FirstBlock(), false);
    auto& postorderList = cfgInfo.PostorderList();

    for(int i = postorderList.Count() - 1; i >= 0; i--) {
        auto block = const_cast<Block*>(postorderList[i]);
        CollectCopySetFromBlock(block);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::CollectCopySetFromBlock(Block* block) {
    // We scan all instructions in the block and search for calls
    // to 'copyMemory' and 'setMemory' that target records or arrays. 
    // While scanning we also perform local copy propagation.
    copySets_.Add(block, BitVector(infoList_.Count() + 4));
    killSets_.Add(block, BitVector(infoList_.Count() + 4));
    auto& copySet = copySets_[block];
    auto& killSet = killSets_[block];
    TOperandToIdDict availableCopies;

    for(auto instr = block->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
        if(auto callInstr = instr->As<CallInstr>()) {
            // This might be a call that kills copies.
            AddToKillSet(callInstr, killSet, copySet);

            // Check if a copy is made by this call.
            Operand* destOp;
            int id = ExtractCopySetInfo(callInstr, destOp);

            if(id != -1) {
                // We found a copy/set operation, make it available
                // in this block.
                SetBit(copySet, id);
                auto& info = infoList_[infoList_.Count() - 1];
                availableCopies.Add(destOp, id);
            }
            else ReplaceWithOriginal(callInstr, availableCopies, killSet);
        }
        else if(auto storeInstr = instr->As<StoreInstr>()) {
            // Stores might kill available copies.
            AddToKillSet(storeInstr, killSet, copySet);
        }
        else if(auto loadInstr = instr->As<LoadInstr>()) {
            // Try to replace the operands with the original ones.
            ReplaceWithOriginal(loadInstr, availableCopies, killSet);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int AggregateCopyPropagation::ExtractCopySetInfo(CallInstr* instr, Operand*& destOp) {
    DebugValidator::IsNotNull(instr);

    // Check if we have a call to 'copyMemory' or
    // 'setMemory' that targets records or arrays.
    if(auto intrinsic = instr->GetIntrinsic()) {
        bool valid = false;
        bool isCopy;
        destOp = WithoutPointerCasts(CopyMemoryIntr::GetDestination(instr));

        if(intrinsic->Is<CopyMemoryIntr>()) {
            // The copy is valid only if the whole object
            // is copied using a constant size argument.
            auto destType = destOp->GetType()->As<PointerType>()->PointeeType();
            
            if(destType->IsRecord() || destType->IsArray()) {
                if(auto intConst = CopyMemoryIntr::GetLength(instr)->As<IntConstant>()) {
                    int size = TI::GetSize(destType, GetTarget());
                    valid = intConst->Value() == size;
                }
            }

            isCopy = true;
        }
        else if(intrinsic->Is<SetMemoryIntr>()) {
            // The copy is valid only if the whole object
            // is set using a constant size argument and a constant value.
            auto destType = destOp->GetType()->As<PointerType>()->PointeeType();

            if(destType->IsRecord() || destType->IsArray()) {
                if(auto intConst = SetMemoryIntr::GetLength(instr)->As<IntConstant>()) {
                    int size = TI::GetSize(destType, GetTarget());

                    // Check that the value that is set is a constant.
                    valid = (intConst->Value() == size) &&
                            (SetMemoryIntr::GetSource(instr)->IsIntConstant());
                }
            }

            isCopy = false;
        }

        if(valid) {
            return AddInfo(instr, isCopy);
        }
    }

    return -1;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int AggregateCopyPropagation::AddInfo(CallInstr* copySetCall, bool isCopy) {
    DebugValidator::IsTrue(copySetCall->IsIntrinsic());

    // Create the information object.
    int id = infoList_.Count();
    bool isFromConstant = isCopy && HasConstantSource(copySetCall);
    Operand* destOp = WithoutPointerCasts(copySetCall->GetArgument(0));
    Operand* sourceOp = WithoutPointerCasts(copySetCall->GetArgument(1));
    
    CopySetInfo info(copySetCall, destOp, sourceOp,
                     isCopy, isFromConstant, id);
    infoList_.Add(info);

    // Add the Id to the destination/source dictionaries.
    if(destToId_.ContainsKey(destOp) == false) {
        destToId_.Add(destOp, SparseBitVector());
    }

    SparseBitVector& destBits = destToId_[destOp];
    destBits.SetBit(id);
    return id;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::AddToKillSet(Instruction* instr, 
                                            BitVector& killSet,
                                            BitVector& copySet) {
    // Check if the instruction could write to the destination
    // or source operand of the copy/set operation.
    // Only 'store' and 'call' instructions can write to memory.
    if((instr->IsStore() || instr->IsCall()) == false) {
        return;
    }

    // Analyze each copy/set operation found until now.
    // If the destination or source operands might be written
    // by this instruction then we add its Id to the kill set.
    for(int i = 0; i < infoList_.Count(); i++) {
        auto& copyInfo = infoList_[i];

        if(MightWriteToOperand(instr, copyInfo.Destination)) {
            SetBit(killSet, copyInfo.Index);
            ResetBit(copySet, copyInfo.Index);
        }
        else if(copyInfo.IsCopy && (copyInfo.IsFromConstant == false)) {
            if(MightWriteToOperand(instr, copyInfo.Source)) {
                SetBit(killSet, copyInfo.Index);
                ResetBit(copySet, copyInfo.Index);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::ReplaceWithOriginal(LoadInstr* instr, 
                                                   TOperandToIdDict& availableCopies,
                                                   BitVector& killSet) {
    if(availableCopies.Count() == 0) {
        return;
    }

    // Try to replace each operand with the original one
    // (the one from which the copy was made).
    Instruction* lastInstr = nullptr;
    auto candidateOp = GetBaseOperand(instr->SourceOp(), &lastInstr);

    // Give up if we couldn't find a candidate,
    // or if the candidate is an instruction whose result
    // is used in more than one place.
    if((candidateOp == nullptr) || (lastInstr == nullptr) || 
       (lastInstr->GetDestinationOp()->HasSingleUser() == false)) {
        return;
    }
        
    // We can replace the operand if it's a copy
    // (found in 'availableCopies') and the source or copy
    // was not killed until this point.
    CopySetInfo info;

    if(GetOldestAvailable(candidateOp, availableCopies, killSet, info)) {
        if(info.IsCopy) {
            // Replace the operand with the original.
            auto originalOp = info.Source;
            lastInstr->ReplaceSourceOp(0, originalOp);
            info.Replacements++;
            CopyPropagated(instr);
        }
        else if(instr->HasDestinationOp()) {
            // The load itself is replaced by the value
            // to which the record/array was set.
            auto requiredType = instr->GetDestinationOp()->GetType();
            auto constantOp = CreateConstant(requiredType, info);
            instr->GetDestinationOp()->ReplaceWith(constantOp);
            info.Replacements++;
            CopyPropagated(instr);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::ReplaceWithOriginal(CallInstr* instr, 
                                                   TOperandToIdDict& availableCopies,
                                                   BitVector& killSet) {
    // Check if any of the arguments can be replaced 
    // with the original operand.
    for(int i = 0; i < instr->ArgumentCount(); i++) {
        auto argument = instr->GetArgument(i);
        Instruction* lastInstr = nullptr;
        auto candidateOp = GetBaseOperand(argument, &lastInstr);

        // Give up if we couldn't find a candidate,
        // or if the candidate is used by an instruction 
        // whose result is used in more than one place.
        if((candidateOp == nullptr) || 
           (lastInstr && (lastInstr->GetDestinationOp()->HasSingleUser() == false))) {
           return;
        }

        // Check if this is a copy.
        CopySetInfo info;

        if(GetOldestAvailable(candidateOp, availableCopies, killSet, info)) {
            if(info.IsCopy) {
                // Replace the operand with the original.
                auto originalOp = info.Source;
                info.Replacements++;
                CopyPropagated(instr);

                // If the copy is used by an instruction, we modify
                // that instruction, else we replace the call argument.
                if(lastInstr) {
                    lastInstr->ReplaceSourceOp(0, originalOp);
                }
                else instr->ReplaceArgument(i, originalOp);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AggregateCopyPropagation::GetOldestAvailable(Operand* candidateOp,
                                                  TOperandToIdDict& availableCopies,
                                                  BitVector& killSet, 
                                                  CopySetInfo& info) {
    // Check if an original source is available.
    int id;

    if(availableCopies.TryGetValue(candidateOp, &id)) {
        if(killSet.IsSet(id)) {
            // The original is dirty.
            return false;
        }

        info = infoList_[id];
        auto originalOp = info.Source;

        // Check if an older source is available. This is useful
        // for a chain of copies, when the copies are not modified.
        while(availableCopies.TryGetValue(originalOp, &id)) {
            if(killSet.IsSet(id)) {
                return originalOp;
            }

            info = infoList_[id];
            originalOp = info.Source;
        }

        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* AggregateCopyPropagation::CreateConstant(const Type* requiredType, 
                                                  CopySetInfo& info) {
    DebugValidator::IsNotNull(requiredType);
    DebugValidator::IsTrue(requiredType->IsInteger());
    DebugValidator::IsFalse(info.IsCopy);

    auto originalOp = info.CopySetCall->GetArgument(1);
    auto intConst = originalOp->As<IntConstant>();
    DebugValidator::IsNotNull(intConst);

    // Create a constant having the required number of bytes
    // by "multiplying" the constant byte ('setMemory works
    // at the byte level, so the constant is a byte).
    // For zero this is trivial.
    auto& constTable = funct_->ParentUnit()->Constants();

    if(intConst->IsZero()) {
        return constTable.GetInt(requiredType, 0);
    }
    else {
        __int64 value = intConst->Value();
        auto requiredIntType = requiredType->As<IntegerType>();

        for(int i = 1; i < requiredIntType->Size(); i++) {
            value <<= 8;
            value |= intConst->Value();
        }

        return constTable.GetInt(requiredType, value);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::InitializeAnalysis() {
    // Create the 'out' sets that are attached to the blocks.
    // The sets are initialized with the universal set
    // (all copies are available).
    for(auto block = funct_->FirstBlock(); block; block = block->NextBlock()) {
        outSets_.Add(block, BitVector(infoList_.Count(), true));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::IterateToFixedpoint() {
    // We use an iterative data-flow algorithm to propagate the copies
    // that are available on entry of each block. Add the blocks
    // in reverse-postorder, this minimizes the number of iterations.
    StaticList<Block*, 64> worklist;
    SparseBitVector inWorklist;

    CFGInfo<Block, Function> cfgInfo(funct_->FirstBlock(), false);
    auto& postorderList = cfgInfo.PostorderList();
    int copyCount = infoList_.Count();

    // Add all blocks to the worklist.
    for(int i = 0; i < postorderList.Count(); i++) {
        auto block = const_cast<Block*>(postorderList[i]);
        worklist.Add(block);
        inWorklist.SetBit(block->Id());
    }

    while(worklist.IsNotEmpty()) {
        // Extract a block from the worklist.
        auto block = worklist.RemoveLast();
        inWorklist.ResetBit(block->Id());

        // Compute the 'in' set, which is the intersection
        // out the 'out' sets of the predecessors.
        BitVector inSet(copyCount, false);
        auto predecessorEnum = block->GetPredecessorEnum();
        bool first = true;

        while(predecessorEnum.IsValid()) {
            auto predecessorBlock = predecessorEnum.Next();

            if(first) {
                inSet = outSets_[predecessorBlock];
                first = false;
            }
            else inSet.And(outSets_[predecessorBlock]);
        }

        // Save the 'in' set, it's needed later
        // when we want to eliminate the copies.
        inSets_.Add(block, inSet);

        // Now compute the new 'out' set, which is the union of the 'copy' set
        // with the 'in' set, from which the 'kill' set has been subtracted.
        // Out(B) = Copy(B) U (In(B) - Kill(B))
        BitVector outSet = copySets_[block];
        inSet.Difference(killSets_[block]);
        outSet.Or(inSet);

        if(outSets_[block] != outSet) {
            // The 'out' set must be updated, and all successors
            // must be added to the worklist and reprocessed.
            outSets_[block] = outSet;

            auto successorEnum = block->GetSuccessorEnum();

            while(successorEnum.IsValid()) {
                auto successorBlock = successorEnum.Next();

                if(inWorklist.IsSet(successorBlock->Id()) == false) {
                    worklist.Add(successorBlock);
                    inWorklist.IsSet(successorBlock->Id());
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::PropagateCopies() {
    TOperandToIdDict availableCopies;
    TCopyInfoList* infoList = &infoList_;

    for(auto block = funct_->FirstBlock(); block; block = block->NextBlock()) {
        // Based on the copies found in the 'in' set we build 
        // a dictionary with the available copies at the entry of the block.
        availableCopies.Clear();
        auto& inSet = inSets_[block];

        inSet.ForEachSet([&availableCopies, infoList](int index) -> bool {
            availableCopies.Add((*infoList)[index].Destination, index);
            return true;
        });

        // Run the local propagation algorithm if we have available copies.
        if(availableCopies.Count() > 0) {
            LocalCopyPropagation(block, availableCopies);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::LocalCopyPropagation(Block* block, 
                                                    TOperandToIdDict& availableCopies) {
    BitVector killSet(infoList_.Count());
    BitVector copySet(infoList_.Count()); // Unused here.

    // Try to use the original operand instead of the copy.
    // We need to recompute the 'kill' set, because copies available
    // at the block entry might be invalidated by 'store' and 'call'.
    for(auto instr = block->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
        if(auto callInstr = instr->As<CallInstr>()) {
            // This might be a call that kills copies.
            AddToKillSet(callInstr, killSet, copySet);
            ReplaceWithOriginal(callInstr, availableCopies, killSet);
        }
        else if(auto storeInstr = instr->As<StoreInstr>()) {
            AddToKillSet(storeInstr, killSet, copySet);
        }
        else if(auto loadInstr = instr->As<LoadInstr>()) {
            // Try to replace the operands with the original ones.
            ReplaceWithOriginal(loadInstr, availableCopies, killSet);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AggregateCopyPropagation::MightWriteToOperand(Instruction* instr, Operand* op) {
    //! TODO: THIS SHOULD USE ALIAS INFO!
    // Only 'store' and 'call' instructions can write to memory.
    if(auto storeInstr = instr->As<StoreInstr>()) {
        // If we store into a local/global variable we can write
        // over the source or destination only if it's exactly
        // the same variable.
        auto destOp = storeInstr->DestinationOp();

        if(auto baseOp = GetBaseOperand(destOp)) {
            if(auto variableRef = baseOp->As<VariableReference>()) {
                return op->IsVariableReference() &&
                       (op == variableRef);
            }
        }

        // We presume it might write.
        return true;
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        // For most intrinsics we know the behavior.
        if(auto intrinsic = callInstr->GetIntrinsic()) {
            if(intrinsic->IsMathIntrinsic() ||
               intrinsic->IsBitwiseIntrinsic() ||
               intrinsic->IsStackIntrinsic()) {
               return false;
            }
        }

        // If the destination/source is a variable whose address
        // is not taken, and we have a 'call' that doesn't refer
        // to these operands then the callee can't write to them.
        auto baseOp = GetBaseOperand(op);

        if(baseOp == nullptr) {
            return true;
        }

        auto variableRef = baseOp->As<VariableReference>();

        if(variableRef == nullptr) {
            return true;
        }

        if(variableRef->IsGlobalVariableRef()) {
            if(auto function = callInstr->GetCalledFunction()) {
                return function->IsNoWrite() == false;
            }

            return true;
        }
        
        // Check each pointer argument.
        for(int i = 0; i < callInstr->ArgumentCount(); i++) {
            auto argument = WithoutPointerCasts(callInstr->GetArgument(i));

            if(argument->IsPointer()) {
                // If the parameter is marked as 'noescape' and 'nowrite'
                // we know the called function doesn't modify the argument.
                if(auto function = callInstr->GetCalledFunction()) {
                    auto parameter = function->GetParameter(i);

                    if(parameter->IsNoEscape() && parameter->IsNoWrite()) {
                        continue;
                    }
                }

                if(auto argVariableRef = argument->As<VariableReference>()) {
                    return argVariableRef == variableRef;
                }

                // If the address of the variable is taken
                // we need to presume it might be written here.
                if(variableRef->IsAddressTaken()) {
                    return true;
                }
            }
        }

        return false;
    }

    return false;
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::RemovedDeadCopySetCalls() {
    // We count how many times each copy is used in the function.
    // All copies that are not used anymore can be removed,
    // allowing Dead Code Elimination to do it's job.
    List<int> copyCount(infoList_.Count());

    for(int i = 0; i < infoList_.Count(); i++) {
        copyCount.Add(0);
    }

    // Scan all instructions in the function.
    for(auto block = funct_->FirstBlock(); block; block = block->NextBlock()) {
        for(auto instr = block->FirstInstruction(); instr; 
            instr = instr->NextInstruction()) {
            // We check 'store', 'load' and 'call' instructions only,
            // because they're the ones that can access the copy.
            if(auto loadInstr = instr->As<LoadInstr>()) {
                VisitedDict visited;
                MarkUsedCopies(loadInstr->SourceOp(), 
                               copyCount, visited);
            }
            else if(auto storeInstr = instr->As<StoreInstr>()) {
                VisitedDict visited1;
                MarkUsedCopies(storeInstr->DestinationOp(), copyCount,
                               visited1, true /* ignoreLocals */);
                VisitedDict visited2;
                MarkUsedCopies(storeInstr->SourceOp(), copyCount,
                               visited2, true /* ignoreLocals */);
            }
            else if(auto callInstr = instr->As<CallInstr>()) {
                for(int i = 0; i < callInstr->ArgumentCount(); i++) {
                    VisitedDict visited;
                    MarkUsedCopies(callInstr->GetArgument(i), 
                                   copyCount, visited);
                }
            }
        }
    }

    // Remove any copy that is definitely dead.
    for(int i = 0; i < copyCount.Count(); i++) {
        if((copyCount[i] - infoList_[i].Replacements) < 2) {
            infoList_[i].CopySetCall->RemoveFromBlock(true /* free */);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::MarkUsedCopies(Operand* op, List<int>& copyCount,
                                              VisitedDict& visited, 
                                              bool ignoreLocals){
    Operand* candidateOp = nullptr;
                                                  
    if(auto variableRef = op->As<VariableReference>()) {
        // For 'store' we ignore local variables.
        if(variableRef->IsGlobalVariableRef() || (ignoreLocals == false)) {
            candidateOp = variableRef;
        }
    }
    else if(auto definingInstr = op->DefiningInstruction()) {
        // We mark the destination operand as visited
        // in order to prevent phi-cycles.
        if(definingInstr->HasDestinationOp()) {
            visited.Add(definingInstr->GetDestinationOp(), true);
        }

        // Mark each operand of the instruction.
        for(int i = 0; i < definingInstr->SourceOpCount(); i++) {
            auto sourceOp = definingInstr->GetSourceOp(i);

            // We process the operand only if it wasn't
            // already processed.
            if(visited.ContainsKey(sourceOp) == false) {
                MarkUsedCopies(sourceOp, copyCount,
                               visited, ignoreLocals);
            }
        }
    }
    else candidateOp = op;

    // Check if the candidate operand is a target of a copy
    // and mark the copy live if it's the case.
    if(candidateOp && destToId_.ContainsKey(candidateOp)) {
        auto& bits = destToId_[candidateOp];

        bits.ForEachSetBit([&copyCount](int index) -> bool {
            copyCount[index]++;
            return true;
        });
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AggregateCopyPropagation::HasConstantSource(CallInstr* instr) {
    // Check if we copy from a constant global variable.
    // This is often used to initialize local aggregates.
    DebugValidator::AreEqual(instr->ArgumentCount(), 3);
    auto sourceOp = WithoutPointerCasts(instr->GetArgument(1));

    if(auto variableRef = sourceOp->As<VariableReference>()) {
        auto globalVar = variableRef->GetGlobal();
        return globalVar &&
               globalVar->HasInitializer() &&
               globalVar->IsConstant();
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* AggregateCopyPropagation::WithoutPointerCasts(Operand* op) {
    while(auto ptopInstr = op->DefiningInstrAs<PtopInstr>()) {
        op = ptopInstr->TargetOp();
    }
    
    return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* AggregateCopyPropagation::GetBaseOperand(Operand* op,
                                                  Instruction** lastInstr) {
    if(auto variableRef = op->As<VariableReference>()) {
        return variableRef;
    }
    else if(auto definingInstr = op->DefiningInstruction()) {
        if(auto indexInstr = definingInstr->As<IndexInstr>()) {
            if(lastInstr) *lastInstr = indexInstr;
            return GetBaseOperand(indexInstr->BaseOp(), lastInstr);
        }
        else if(auto elemInstr = definingInstr->As<ElementInstr>()) {
            if(lastInstr) *lastInstr = elemInstr;
            return GetBaseOperand(elemInstr->BaseOp(), lastInstr);
        }
        else if(auto addrInstr = definingInstr->As<AddressInstr>()) {
            if(lastInstr) *lastInstr = addrInstr;
            return GetBaseOperand(addrInstr->BaseOp(), lastInstr);
        }
        else if(auto ptopInstr = definingInstr->As<PtopInstr>()) {
            if(lastInstr) *lastInstr = ptopInstr;
            return GetBaseOperand(ptopInstr->TargetOp(), lastInstr);
        }
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::SetBit(BitVector& vector, int bit) {
    if(vector.BitCount() == (bit + 1)) {
        vector.Resize(bit + 4);
    }
    
    vector.SetBit(bit);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::ResetBit(BitVector& vector, int bit) {
    if(vector.BitCount() == (bit + 1)) {
        vector.Resize(bit + 4);
    }
    
    vector.ResetBit(bit);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AggregateCopyPropagation::CopyPropagated(Instruction* instr) {
#if 1
	auto block = instr->ParentBlock();
	auto function = instr->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string text = IRPrinter(instr).ToString();
	Log::Warning("Aggregate copy propagated in " + functionName + ":" +
                 blockName + ": " + text);
#endif
}

} // namespace Optimization