// ValueNumbering.cpp
// Copyright (c) Lup Gratian
//
// Implements the ValueNumbering class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ValueNumbering.hpp"

namespace Optimization {

void ValueNumbering::Execute(Function* function) {
    // Initialize the constant folder.
    memoryVersion_ = 0;
    availInvalidated_ = false;
    availableMemOps_.Clear();
    IRGenerator irGen(function->ParentUnit());
    folder_ = ConstantFolder(&irGen, GetTarget());

    // Build the dominator tree.
    domTree_ = new IRDominatorTree(function);
    domTree_->Build();

    // Start from the root node of the dominator tree (the entry block).
    auto startNode = domTree_->GetRootNode();
    ScopedExpressionTable table(&valNumber_, startNode->Block(), nullptr);
    ProcessBlock(startNode, &table);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ValueNumbering::ProcessBlock(DominatorNode* blockNode, ScopedExpressionTable* table,
                                  ChildrenTableList* siblingsTables) {
    // Process each instruction, first trying to constant fold or simplify.
    // If we can't do that, then we search for an instruction with the same
    // value number, and use it instead of this one.
    // If such an instruction is not found, the instruction is made available.
    auto instr = blockNode->Block()->FirstInstruction();

    // If the block has more than one predecessor we need to be conservative
    // and invalidate any available memory value (consider a loop, for example).
    if(blockNode->Block()->PredecessorCount() > 1) {
        invalidatedVersion_ = memoryVersion_;
        memoryVersion_++;
        availInvalidated_ = true;
    }

    while(instr) {
        auto nextInstr = instr->NextInstruction();

        // There are some instructions ('load', 'phi' for ex.)
        // that need to be handled separately.
        if(HandleSpecialCases(instr, blockNode, table) == false) {
            // This instruction should be skipped.
            instr = nextInstr;
            continue;
        }

        // Try to find an available instruction that can be used
        // to replace this one using value numbering.
        Operand* result = table->Get(instr);

        // If nothing was found, it may be the case that the redundancy
        // is only partial. Try a simple form of PRE, it helps in some cases.
        if((result == nullptr) && siblingsTables) {
            result = PerformPRE(instr, siblingsTables);
        }

        if(result) {
            // This instruction is redundant.
            ReplaceAndRemove(instr, result);
        }
        else {
            // Make the instruction available.
            table->Insert(instr, instr->GetDestinationOp());
        }

        instr = nextInstr;
    }

    // Now process the children in the dominator tree.
    ProcessChildren(blockNode, table);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ValueNumbering::HandleSpecialCases(Instruction*& instr, DominatorNode* blockNode,
                                        ScopedExpressionTable* table) {
    // Search for available value that can be used instead of the 'load'.
    // This is very limited, but it helps in some cases.
    if(auto loadInstr = instr->As<LoadInstr>()) {
        if(auto result = ProcessLoad(loadInstr)) {
            ReplaceAndRemove(instr, result);
        }
    }
    else if(auto storeInstr = instr->As<StoreInstr>()) {
        // A 'store' kills the available 'load' instructions.
        ProcessStore(storeInstr);
    }

    // We ignore branching, 'load', and 'store' instructions here.
    // Instructions that have no destination operand should not be considered.
    if(ShouldSkipInstruction(instr)) {
        return false;
    }

    // If the instruction is dead, delete it now.
    if(GetSafetyInfo()->IsDefinitelyDead(instr)) {
        instr->RemoveFromBlock(true /* free */);
        return false;
    }

    // Try to simplify the instruction before doing value numbering.
    // This creates a canonical form, which exposes more opportunities.
    if(auto result = Simplify(instr)) {
        // If the result is an instruction we can check
        // if it can be value-numbered now.
        instr = result->DefiningInstruction();
        return instr != nullptr;
    }

    // 'phi' cannot be handled the usual way. We need to make sure that
    // all incoming operands have value numbers, which is not the case
    // for loops (and weird things resulted from 'goto').
    if(auto phiInstr = instr->As<PhiInstr>()) {
        if(auto result = ProcessPhi(phiInstr, blockNode, table)) {
            ReplaceAndRemove(phiInstr, result);
        }

        return false;
    }

    // Call instructions, in most cases, cannot be value-numbered.
    // An important exception are intrinsics like 'abs', 'min', etc.
    if(auto callInstr = instr->As<CallInstr>()) {
        if(auto result = ProcessCall(callInstr, blockNode, table)) {
            ReplaceAndRemove(instr, result);
        }

        return false;
    }

    // Canonicalize comparisons (creates more opportunities).
    if(auto cmpInstr = instr->As<CmpInstrBase>()) {
        if(auto result = CanonicalizeComparison(cmpInstr)) {
            instr->GetDestinationOp()->ReplaceWith(result);
            instr = result->DefiningInstruction();
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ValueNumbering::ProcessLoad(LoadInstr* instr) {
    int requiredVersion = memoryVersion_;

    if(availInvalidated_) {
        // We make an exception for small loops that consist of a single basic block;
        // in this case we search until the end of the block for a 'store' or 'call'.
        // If we don't find one, and there were no instruction that increased
        // the memory "version" from the moment we invalidated until now,
        // then we're allowed to use the available values.
        if((IsSmallLoopHeaderWithNoStore(instr) || 
            IsSmallLoopWithNoStore(instr)) &&
           (memoryVersion_ == (invalidatedVersion_ + 1))) {
            requiredVersion = invalidatedVersion_;
        }
        else return nullptr;
    }

    // Check if there is an operand available that was loaded/stored from/to
    // the address that this 'load' targets.
    // Note that we also need to check that the memory "version" is the same,
    // because we don't track exactly where a 'store' may write.
    AvailableOperand availOp;

    if(availableMemOps_.TryGetValue(instr->SourceOp(), &availOp)) {
        // Found an operand, now check the version.
        if(availOp.Version == requiredVersion) {
            return availOp.Operand;
        }
    }

    // Insert the loaded operand into the available table,
    // so that it can be reused by subsequent loads.
    availOp.Operand = instr->ResultOp();
    availOp.Version = memoryVersion_;
    availableMemOps_.Add(instr->SourceOp(), availOp);
    loadInstrs_.Add(instr);
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ValueNumbering::IsSmallLoopWithNoStore(LoadInstr* loadInstr) {
    // We have a loop if the block jumps (unconditionally) to itself,
    // or if it jumps to it's only predecessor (the loop header).
    auto block = loadInstr->ParentBlock();
    bool valid = false;

    auto gotoInstr = block->BranchInstruction()->As<GotoInstr>();
    if(gotoInstr == nullptr) {
        return nullptr;
    }

    if(gotoInstr->TargetOp()->Target() == block) {
        valid = true;
    }
    else if(block->HasSinglePredecessor()) {
        auto pred = block->PredecessorAt(0);
        valid = gotoInstr->TargetOp()->Target() == pred;
    }

    if(valid == false) {
        return false;
    }
    else {
        auto startInstr = loadInstr->NextInstruction();
        return BlockMayWriteToAddress(loadInstr, startInstr) == false;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ValueNumbering::IsSmallLoopHeaderWithNoStore(LoadInstr* loadInstr) {
    // The 'load's parent block is definitely a loop header
    // if one of it's two successors jumps back.
    // We check if there is any instruction that may write to the address
    // targeted by 'loadInstr' in both the header and the body of the loop.
    auto block = loadInstr->ParentBlock();
    auto ifInstr = block->BranchInstruction()->As<IfInstr>();
    if(ifInstr == nullptr) {
        return false;
    }

    // Check if the 'true' block is the loop body.
    bool valid = false;
    auto trueBlock = ifInstr->TrueTargetOp()->Target();
    auto falseBlock = ifInstr->FalseTargetOp()->Target();
    Block* loopBody;

    if(auto gotoInstr = trueBlock->BranchInstruction()->As<GotoInstr>()) {
        loopBody = trueBlock;
        valid = gotoInstr->TargetOp()->Target() == block;
    }

    // Check the 'false' block, if required.
    if(valid == false) {
        if(auto gotoInstr = falseBlock->BranchInstruction()->As<GotoInstr>()) {
            loopBody = falseBlock;
            valid = gotoInstr->TargetOp()->Target() == block;
        }
    }

    if(valid == false) {
        return false;
    }
    else return (BlockMayWriteToAddress(loadInstr, loadInstr->NextInstruction()) == false) &&
                (BlockMayWriteToAddress(loadInstr, loopBody->FirstInstruction())) == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ValueNumbering::BlockMayWriteToAddress(LoadInstr* instr, Instruction* startInstr) {
    // Scan all the instructions that follow and check if we have a 'store' or 'call'.
    // If we may need to presume it invalidates the 'load'.
    // We make an exception for variable references and parameters.
    auto variableRef = instr->SourceOp()->As<VariableReference>();
    auto parameter = instr->SourceOp()->As<Parameter>();

    for(auto instr = startInstr; instr; instr = instr->NextInstruction()) {
        if(auto storeInstr = instr->As<StoreInstr>()) {
            if(MayStoreWriteToVarOrParam(storeInstr, variableRef, parameter)) {
                return true;
            }
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            if(MayCallWriteToMemory(callInstr, variableRef)) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ValueNumbering::MayStoreWriteToVarOrParam(StoreInstr* storeInstr,
                                               VariableReference* variableRef,
                                               Parameter* parameter) {
    // We do here a few simple, conservative checks.
    auto destOp = storeInstr->DestinationOp();

    if(auto storeVariableRef = destOp->As<VariableReference>()) {
        if(variableRef) {
            // Check if the references are the same; 
            // if they're not we can ignore this 'store'.
            if(variableRef != storeVariableRef) {
                return false;
            }
        }
        else if(parameter) {
            // If we're storing into a local variable the load
            // from the parameter can't be affected.
            if(storeVariableRef->IsLocalVariableRef()) {
                return false;
            }
        }
    }
    else if(auto storeParam = destOp->As<Parameter>()) {
        if(variableRef) {
            // A store into a parameter can't access a local variable.
            // If we have a global variable we need to presume that it does.
            if(variableRef->IsLocalVariableRef()) {
                return false;
            }
        }
        else if(parameter) {
            // If both parameters are marked as 'restrict' the programmer
            // guarantees that they will never point to the same location.
            if(parameter->IsRestrict() && storeParam->IsRestrict()) {
                return false;
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ValueNumbering::MayCallWriteToMemory(CallInstr* callInstr,
                                          VariableReference* variableRef) {
    // Most intrinsics can't write to memory.
    if(auto intrinsic = callInstr->GetIntrinsic()) {
        if((intrinsic->IsMathIntrinsic() || intrinsic->IsBitwiseIntrinsic() ||
            intrinsic->IsStackIntrinsic()) == false) {
            return false;
        }
    }
    else {
        // Check if we have a call to a standard library function
        // for which we know that it can't write to (user) memory.
        StdlibCategory category;
        StdlibType type = StdlibRecognizer::Recognize(callInstr, &category);
        
        if(type != Stdlib_None) {
            if(category == StdLibCategory_Math) {
                return false;
            }

            switch(type) {
                case Stdlib_strlen:
                case Stdlib_strcmp:
                case Stdlib_strncmp:
                case Stdlib_strchr:
                case Stdlib_strstr:
                case Stdlib_isdigit:
                case Stdlib_isascii: {
                    return false;
                }
            }
        }
        else if(variableRef && variableRef->IsLocalVariableRef()) {
            // If we have a local variable and the called function
            // has no parameters we know it can't modify the variable.
            if(callInstr->HasArguments() == false) {
                return false;
            }

            // If none of the parameters is a pointer then
            // the local variable can't be modified.
            if(auto function = callInstr->GetCalledFunction()) {
                auto functionType = function->GetType();

                for(int i = 0; i < functionType->ParameterCount(); i++) {
                    if(functionType->Parameters()[i]->IsPointer()) {
                        return true;
                    }
                }

                return false;
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ValueNumbering::ProcessStore(StoreInstr* instr) {
    // We don't know exactly where the 'store' may write, so we invalidate
    // all previous available operands by increasing the memory "version".
    memoryVersion_++;

    // At the same time we make the stored operand available for subsequent loads.
    AvailableOperand availOp;
    availOp.Operand = instr->SourceOp();
    availOp.Version = memoryVersion_;
    availableMemOps_.Add(instr->DestinationOp(), availOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ValueNumbering::ShouldSkipInstruction(Instruction* instr) {
    return instr->IsBranching() || 
           instr->IsLoad() || 
           instr->IsStore();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ValueNumbering::ProcessChildren(DominatorNode* blockNode, 
                                     ScopedExpressionTable* table) {
    // We maintain the hash table of all children in the dominator tree,
    // because this allows us to perform a simple form of PRE. If we would remove
    // the table immediately after the child was visited, 
    // we couldn't do PRE in cases like the following:
    // (1)     if(c)
    //      /        \              In (4) we have access to the tables in (2) and (3),
    // (2) ...   t1 = a + b (3)     because they are siblings in the dominator tree;
    //      \        /              this allows us to do PRE on 'a + b'.
    // (4)  t3 = a + b
    auto& children = blockNode->Children();
    ChildrenTableList childrenTables;
    int prevMemoryVersion = memoryVersion_;
    bool prevAvailInvalidated = availInvalidated_;

    for(int i = 0; i < children.Count(); i++) {
        // Create a new scoped hash table for the child.
        auto childNode = children[i];
        childrenTables.Add(ScopedExpressionTable(&valNumber_, childNode->Block(), table));
        auto& childTable = childrenTables[childrenTables.Count() - 1];

        // Try to propagate some information about the branching instruction.
        // For 'if(a > 0) { ... if(a > 0) ...}', the second 'if' is redundant.
        ProcessBranching(blockNode, childNode, &childTable);
        ProcessBlock(childNode, &childTable, &childrenTables);

        // Restore the previous state. Any 'load' instruction result 
        // that was added to the available operands must now be removed.
        memoryVersion_ = prevMemoryVersion;
        availInvalidated_ = prevAvailInvalidated;

        while(loadInstrs_.Count() > 0) {
            int last = loadInstrs_.Count() - 1;
            availableMemOps_.Remove(loadInstrs_[last]->SourceOp());
            loadInstrs_.RemoveAt(last);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ValueNumbering::ProcessPhi(PhiInstr* instr, DominatorNode* blockNode,
                                    ScopedExpressionTable* table) {
    // Try to simplify and to value-number the 'phi'.
    // phi(a,a,a) -> a
    if(instr->SameOperands()) {
        return instr->GetOperand(0);
    }

    // We can try to value-number only if all incoming values have been
    // processed, which is not the case for incoming values on a loop back-edge.
    for(int i = 0; i < instr->OperandCount(); i++) {
        auto incomingBlock = instr->GetOperandBlock(i)->Target();

        if(domTree_->Dominates(incomingBlock, blockNode->Block()) == false) {
            // This is a back-edge, give up.
            return nullptr;
        }
    }

    // Check for available values. If all are the same we can replace
    // the 'phi' with one of the values.
    Operand* lastAvailable = nullptr;
    bool valid = true;

    for(int i = 0; i < instr->OperandCount(); i++) {
        // The operand should be an instruction.
        auto definingInstr = instr->GetOperand(i)->DefiningInstruction();

        if(definingInstr == nullptr) { 
            valid = false;
            break;
        }

        // There should be an instruction available, and be the same as the last one.
        auto available = table->GetOrInsert(definingInstr);

        if((available == nullptr) || 
           (lastAvailable && (available != lastAvailable))) {
            valid = false;
            break;
        }
        else lastAvailable = available;
    }

    if(valid) {
        // The 'phi' is not needed, replace with the available instruction.
        return lastAvailable;
    }

    // The 'phi' may be redundant, meaning that there is another 'phi'
    // that has the same incoming values as this one.
    return table->GetOrInsert(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ValueNumbering::ProcessCall(CallInstr* instr, DominatorNode* blockNode,
                                     ScopedExpressionTable* table) {
    // The 'call' may write to memory, so we need to increase the memory "version".
    if(MayCallWriteToMemory(instr)) {
        memoryVersion_++;
    }

    // Check if it's safe to value-number the call.
    if(ValueNumber::IsCallSafe(instr)) {
        return table->GetOrInsert(instr);
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ValueNumbering::ProcessBranching(DominatorNode* blockNode, DominatorNode* childNode,
                                      ScopedExpressionTable* childTable) {
    // If the block ends with an 'if' that tests the result of a comparison,
    // we can replace a subsequent comparison in a dominated block by
    // 1 or 0 (depends on which branch was taken). For example, in
    // 'if(a > 0) { ... if(a > 0) {} ... }', the second 'if' is redundant,
    // because we know that 'a > 0' is 'true' on the "then" branch.
    auto block = blockNode->Block();

    if(auto ifInstr = blockNode->Block()->BranchInstruction()->As<IfInstr>()) {
        if(auto cmpInstr = ifInstr->ConditionOp()->DefiningInstrAs<CmpInstrBase>()) {
            auto unit = blockNode->Block()->ParentFunction()->ParentUnit();
            auto resultType = cmpInstr->ResultOp()->GetType();

            // We need to make sure that control can flow into the target block
            // only through the true/false edge, else the propagation might not be valid.
            // An exception are back-edges, because control flows from inside the target.
            if((childNode->Block() == ifInstr->TrueTargetOp()->Target()) &&
                BlockUtils::DominatesItself(childNode->Block(), block, domTree_)) {
                auto oneConst = unit->Constants().GetInt(resultType, 1);
                childTable->Insert(cmpInstr, oneConst);
            }
            else if((childNode->Block() == ifInstr->FalseTargetOp()->Target()) &&
                     BlockUtils::DominatesItself(childNode->Block(), block, domTree_)) {
                auto oneConst = unit->Constants().GetInt(resultType, 0);
                childTable->Insert(cmpInstr, oneConst);
            }
        }

        return;
    }
    
    // We can do the same for 'switch'; if we test the result of any instruction,
    // and 'case X' is taken, then the instruction computes value 'X'. For example, in
    // 'switch(a / 3) { case 5: ... a / 3 + b }', the second 'a / 3' can be replaced by 5.
    if(auto switchInstr = blockNode->Block()->BranchInstruction()->As<SwitchInstr>()) {
        auto definingInstr = switchInstr->ConditionOp()->DefiningInstruction();
        if(definingInstr == nullptr) {
            return;
        }

        // See which 'case' target the child block is; if it's 'default'
        // then we know nothing about the instruction's value.
        auto& caseList = switchInstr->CaseList();
        auto childBlock = childNode->Block();

        for(int i = 0; i < caseList.Count(); i++) {
            if((caseList[i].Target->Target() == childBlock) &&
                BlockUtils::DominatesItself(childBlock, block, domTree_)) {
                auto unit = blockNode->Block()->ParentFunction()->ParentUnit();

                auto resultType = definingInstr->GetDestinationOp()->GetType();
                auto constantOp = unit->Constants().GetInt(resultType, caseList[i].Value);
                childTable->Insert(definingInstr, constantOp);
                break;
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ValueNumbering::CanonicalizeComparison(CmpInstrBase* instr) {
    // Transform > to < and >= to <=. This helps in eliminating redundancies
    // ('a > b' can be eliminated if 'b < a' is available).
    if(instr->IsGreater() || instr->IsGreaterOrEqual()) {
        instr->InvertOrder(true /* invertOperands */, false /* invertEquality */);
        return instr->ResultOp();
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ValueNumbering::Simplify(Instruction* instr) {
    // Try to constant-fold the instruction.
    if(auto result = folder_.Fold(instr)) {
        return result;
    }

    // Try to simplify using arithmetic identities.
    // We iterate, because one simplification could lead to another one.
    Operand* lastResult = nullptr;
    bool hasInstr = true;

    while(hasInstr) {
        hasInstr = false;

        if(auto result = peephole_.SimplifyInstruction(instr, true)) {
            ReplaceAndRemove(instr, result);
            lastResult = result;
            instr = result->DefiningInstruction();
            hasInstr = instr != nullptr;
        }
    }

    return lastResult;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ValueNumbering::PerformPRE(Instruction* instr, 
                                    ChildrenTableList* siblingsTables) {
    // Test for the following case, it's pretty common and easy to handle.
    //     if(c)                         if(c)
    //  /        \                    /        \      
    // ...   t1 = a + b   =>   t2 = a + b   t1 = a + b
    //  \        /                    \        /      
    //  t3 = a + b                 t3 = phi(t2, t1)
    auto block = instr->ParentBlock();
    if(block->PredecessorCount() != 2) {
        return nullptr;
    }

    // Make sure we have an 'if-then-else' construct.
    Block* preds[2];

    preds[0] = block->PredecessorAt(0);
    if((preds[0]->HasSinglePredecessor() &&
        preds[0]->HasSingleSuccessor()) == false) {
        return nullptr;
    }

    preds[1] = block->PredecessorAt(1);
    if((preds[1]->HasSinglePredecessor() &&
        preds[1]->HasSingleSuccessor()) == false) {
        return nullptr;
    }

    // Both should have the same predecessor (the 'if' block).
    if(preds[0]->PredecessorAt(0) != preds[1]->PredecessorAt(0)) {
        return nullptr;
    }

    // Check if the instruction is available in at least one of the predecessors.
    Operand* available[2] = {nullptr, nullptr};
    int availCount = 0;

    for(int i = 0; i < 2; i++) {
        // Usually there are only a few children in the tree, so it isn't
        // that bad that we do a linear search here.
        for(int j = 0; j < siblingsTables->Count(); j++) {
            auto& table = (*siblingsTables)[j];

            if(table.Owner() == preds[i]) {
                // Found the hash table of the predecessor, now see
                // if the instruction is available in it.
                available[i] = table.Get(instr);
                availCount += available[i] ? 1 : 0;
                break;
            }
        }
    }

    // Check the number of available expressions.
    if(availCount == 0) {
        // There is nothing to do.
        return nullptr;
    }
    else if(availCount == 2) {
        // The expression is fully-redundant (available in both predecessors).
        // Insert a 'phi' that "merges" the values.
        return CreatePhi(available[0], available[1], preds[0], preds[1], block);
    }
    else {
        // The expression is available in only one of the predecessors.
        // Clone it and insert it in the predecessor that doesn't contain it,
        // then "merge" the two values using a 'phi' instruction.
        auto availOp       = available[0] ? available[0] : available[1];
        auto availableBlock    = available[0] ? preds[0]     : preds[1];
        auto notAvailBlock = available[0] ? preds[1]     : preds[0];

        // The operands of the instruction must dominate the block where
        // a copy needs to be inserted.
        if(OperandsDominateBlock(instr, notAvailBlock)) {
#if 1
            Log::Error("PRE in block " + *notAvailBlock->Name());
#endif
            auto cloneOp = CloneInstruction(instr, notAvailBlock);
            return CreatePhi(availOp, cloneOp, availableBlock, notAvailBlock, block);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ValueNumbering::CreatePhi(Operand* opA, Operand* opB, 
                                   Block* opAParent, Block* opBParent, Block* parent) {
    DebugValidator::AreEqual(opA->GetType(), opB->GetType());
    DebugValidator::AreNotEqual(opAParent, opBParent);
    DebugValidator::AreNotEqual(opAParent, parent);
    DebugValidator::AreNotEqual(opBParent, parent);

    auto unit = parent->ParentFunction()->ParentUnit();
    auto temp = Temporary::GetTemporary(opA->GetType());
    auto phiInstr = PhiInstr::GetPhi(temp, 2);
    phiInstr->AddOperand(opA, unit->References().GetBlockRef(opAParent));
    phiInstr->AddOperand(opB, unit->References().GetBlockRef(opBParent));

    parent->InsertInstructionFirst(phiInstr);
    return temp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ValueNumbering::OperandsDominateBlock(Instruction* instr, Block* block) {
    DebugValidator::AreNotEqual(instr->ParentBlock(), block);
    bool valid = true;
    auto domTree = domTree_;

    instr->ForEachSourceOp([&valid, instr, block, domTree]
                           (Operand* op, int index) -> bool {
        // Constants and references dominate any block, so we need to test
        // for instruction results.
        if(auto instr = op->DefiningInstruction()) {
            if(domTree->Dominates(instr->ParentBlock(), block) == false) {
                valid = false;
                return false;
            }
        }
        
        return true;
    });

    return valid;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ValueNumbering::CloneInstruction(Instruction* instr, Block* parent) {
    auto temp = Temporary::GetTemporary(instr->GetDestinationOp()->GetType());
    auto clone = instr->Clone();
    clone->ReplaceDestinationOp(temp);

    // Set the source operands, since this is not done by 'Clone'.
    for(int i = 0; i < instr->SourceOpCount(); i++) {
        clone->ReplaceSourceOp(i, instr->GetSourceOp(i));
    }

    // Insert the clone in the parent as the last instruction 
    // (but before the branching one).
    parent->InsertInstructionBefore(clone, parent->BranchInstruction());
    return temp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ValueNumbering::ReplaceAndRemove(Instruction* instr, Operand* replacement) {
    instr->GetDestinationOp()->ReplaceWith(replacement);
    InstructionRemoved(instr);
    
    if(instr->GetDestinationOp()->HasNoUser()) {
        instr->RemoveFromBlock(true /* free */);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ValueNumbering::InstructionRemoved(Instruction* instr) {
#if 1
    auto block = instr->ParentBlock();
    auto function = instr->ParentFunction();
    string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
    string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
    string text = IRPrinter(instr).ToString();
    Log::Warning("Value numbering in " + functionName + ":" + blockName + ": " + text);
//    IRPrinter(instr->ParentFunction()).Dump();
#endif
}

} // namespace Optimization