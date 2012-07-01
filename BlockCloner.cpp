// BlockCloner.cpp
// Copyright (c) Lup Gratian
//
// Implements the BlockCloner class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "BlockCloner.hpp"

namespace Optimization {

void BlockCloner::CloneBlocks(BlockList& blockList, Function* destFunct,
                              Block* entryBlock) {
    if(blockList.Count() == 0) return;

    // If no destination function was specified use the function
    // from where the blocks to be cloned originate.
    if(destFunct == nullptr) {
        destFunct = blockList[0]->ParentFunction();
    }

    if(entryBlock == nullptr) {
        entryBlock = blockList[0];
    }

    // Clone the block in the list, then we patch the 'phi'
    // and branch instructions.
    destFunction_ = destFunct;
    entryBlock_ = entryBlock;
    insertionPoint_ = blockList[blockList.Count() - 1];

    for(int i = 0; i < blockList.Count(); i++) {
        CloneBlock(blockList[i]);
    }

    ProcessInstructions(blockList);
    ProcessBranchInstructions(blockList);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockCloner::CloneFunctionBody(Function* sourceFunct, Function* destFunct) {
    insertionPoint_ = nullptr;
    entryBlock_ = sourceFunct->FirstBlock();
    destFunction_ = destFunct;

    for(auto block = sourceFunct->FirstBlock(); block; block = block->NextBlock()) {
        CloneBlock(block);
    }

    for(auto block = sourceFunct->FirstBlock(); block; block = block->NextBlock()) {
        ProcessInstructions(block);
    }

    for(auto block = sourceFunct->FirstBlock(); block; block = block->NextBlock()) {
        ProcessBranchInstructions(block, blockMap_[block]);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* BlockCloner::CloneBlock(Block* block) {
    // We create an empty block having an unique name
    // and insert it into the destination after the insertion point.
    string name = GetUniqueName(block, &destFunction_->Symbols());
    Block* clonedBlock = Block::GetBlock(name);
 
    if(insertionPoint_) {
        destFunction_->InsertBlockAfter(clonedBlock, insertionPoint_);
    }
    else destFunction_->InsertBlock(clonedBlock);

    // Next clone block will be inserted after this one.
    insertionPoint_ = clonedBlock;

    // Iterate over all instructions and clone them,
    // except for the branch (it will be processed later,
    // after all blocks have been cloned).
    for(auto instr = block->FirstInstruction(); instr;
        instr = instr->NextInstruction()) {
        if((instr->IsBranching() == false) ||
            instr->IsReturn() /* not considered branching here */) {
            auto clonedInstr = CloneInstruction(instr);
            clonedBlock->InsertInstruction(clonedInstr);
        }
    }

    blockMap_.Add(block, clonedBlock);
    return clonedBlock;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* BlockCloner::CloneInstruction(Instruction* instr) {
    // The instruction is cloned without replacing any
    // of its operands yet, because some might be defined
    // in blocks that were not cloned yet.
    auto clonedInstr = instr->Clone();

    // If the instruction has a result operand we create
    // a new one for the clone and add it to the map.
    if(instr->HasDestinationOp()) {
        auto resultOp = instr->GetDestinationOp();
        auto clonedResultOp = Temporary::GetTemporary(resultOp->GetType());
        clonedInstr->ReplaceDestinationOp(clonedResultOp);
        operandMap_.Add(resultOp, clonedResultOp);
    }

    return clonedInstr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockCloner::ProcessInstructions(BlockList& blockList) {
    for(int i = 0; i < blockList.Count(); i++) {
        ProcessInstructions(blockMap_[blockList[i]]);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockCloner::ProcessInstructions(Block* clonedBlock) {
    for(auto instr = clonedBlock->FirstInstruction(); instr;
        instr = instr->NextInstruction()) {
        if(auto phiInstr = instr->As<PhiInstr>()) {
            ProcessPhiInstruction(phiInstr);
        }
        else {
            ReplaceSourceOperands(instr);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockCloner::ProcessPhiInstruction(PhiInstr* instr) {
    // If the 'phi' is found in the entry block we don't
    // test its incoming values.
    if(entryBlock_) {
        auto clonedEntryBlock = blockMap_[entryBlock_];
        if(instr->ParentBlock() == clonedEntryBlock) {
            return;
        }
    }

    for(int i = 0; i < instr->OperandCount(); i++) {
        // Check if the block of the incoming operand was also cloned:
        // if not we remove the incoming operand, because control
        // doesn't flow from that block. If it was cloned then we 
        // modify the 'phi' so that the operand is incoming from the clone.
        auto incomingBlock = instr->GetOperandBlock(i)->Target();

        if(blockMap_.ContainsKey(incomingBlock) == false) {
            instr->RemoveOperand(i);
            i--;
        }
        else {
            auto newIncomingBlock = blockMap_[incomingBlock];
            auto& refs = destFunction_->ParentUnit()->References();
            auto newIncomingBlockRef = refs.GetBlockRef(newIncomingBlock);
            instr->ReplaceOperandBlock(i, newIncomingBlockRef);

            // Replace the incoming operand with the (possible) clone.
            auto newIncomingOp = GetReplacementOperand(instr->GetOperand(i));
            instr->ReplaceOperand(i, newIncomingOp);
        }
    }

    // If the 'phi' has no incoming operands we replace it
    // with 'undef', although this might indicate an error elsewhere.
    if(instr->OperandCount() == 0) {
        DebugValidator::IsTrue(false);
        if(instr->ResultOp()) {
            auto type = instr->ResultOp()->GetType();
            auto undef = destFunction_->ParentUnit()->Constants().GetUndefined(type);
            instr->ResultOp()->ReplaceWith(undef);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockCloner::ReplaceSourceOperands(Instruction* instr) {
    for(int i = 0; i < instr->SourceOpCount(); i++) {
        auto replacement = GetReplacementOperand(instr->GetSourceOp(i));
        instr->ReplaceSourceOp(i, replacement);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* BlockCloner::GetReplacementOperand(Operand* op) {
    // References to global symbols (variables/functions)
    // and constants are valid for the whole unit. 
    if(op->IsConstant() ||
       op->IsGlobalVariableRef() ||
       op->IsFunctionReference()) {
        return op;
    }

    // Check if the operand should be replaced by another one
    // from the cloned blocks.
    Operand* replacement;
    if(operandMap_.TryGetValue(op, &replacement)) {
        return replacement;
    }
    
    return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockCloner::ProcessBranchInstructions(BlockList& blockList) {
    for(int i = 0; i < blockList.Count(); i++) {
        auto block = blockList[i];
        auto clonedBlock = blockMap_[block];
        ProcessBranchInstructions(block, clonedBlock);
        
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockCloner::ProcessBranchInstructions(Block* block, Block* clonedBlock) {
    ExternalBlockList externalBlocks;

    if(auto gotoInstr = block->BranchInstruction()->As<GotoInstr>()) {
        auto targetRef = GetReplacementBlock(gotoInstr->TargetOp(),
                                             externalBlocks);
        GotoInstr::GetGoto(targetRef, clonedBlock);
    }
    else if(auto ifInstr = block->BranchInstruction()->As<IfInstr>()) {
        auto trueTargetRef = GetReplacementBlock(ifInstr->TrueTargetOp(),
                                                 externalBlocks);
        auto falseTargetRef = GetReplacementBlock(ifInstr->FalseTargetOp(),
                                                  externalBlocks);
        auto newConditionOp = GetReplacementOperand(ifInstr->ConditionOp());
        IfInstr::GetIf(newConditionOp, trueTargetRef, falseTargetRef, clonedBlock);
    }
    else if(auto switchInstr = block->BranchInstruction()->As<SwitchInstr>()) {
        auto defaulTargetRef = GetReplacementBlock(switchInstr->DefaultTargetOp(),
                                                   externalBlocks);
        auto newConditionOp = GetReplacementOperand(switchInstr->ConditionOp());
        auto clonedSwitchInstr = SwitchInstr::GetSwitch(newConditionOp, 
                                                        switchInstr->CaseCount(),
                                                        defaulTargetRef, clonedBlock);

        for(int i = 0; i < switchInstr->CaseCount(); i++) {
            auto switchCase = switchInstr->GetCase(i);
            auto caseTargetRef = GetReplacementBlock(switchCase.Target,
                                                     externalBlocks);
            clonedSwitchInstr->AddCase(switchCase.Value, caseTargetRef);
        }
    }

    // For any of the successor blocks that were not cloned 
    // we need to check if there are any incoming 'phi' operands 
    // from 'block': if true then we need to make them incoming from
    // 'clonedBlock' too, because 'clonedBlock' is now a predecessor.
    for(int j = 0; j < externalBlocks.Count(); j++) {
        PatchIncomingOperands(externalBlocks[j], block, clonedBlock);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockCloner::PatchIncomingOperands(Block* block, Block* originalIncomingBlock, 
                                        Block* clonedIncomingBlock) {
    // If a 'phi' has an incoming operand from 'originalIncomingBlock'
    // we make it have such an operand incoming from 'clonedIncomingBlock' too.
    auto& refs = destFunction_->ParentUnit()->References();

    block->ForEachPhiInstruction([&, this](PhiInstr* instr) -> bool {
        if(instr->HasOperandFromBlock(originalIncomingBlock)) {
            auto incomingOp = instr->GetOperandFromBlock(originalIncomingBlock);
            auto newIncomingOp = GetReplacementOperand(incomingOp);

            auto clonedIncomingRef = refs.GetBlockRef(clonedIncomingBlock);
            instr->AddOperand(newIncomingOp, clonedIncomingRef);
        }
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* BlockCloner::GetReplacementBlock(BlockReference* blockRef,
                                                 ExternalBlockList& externalBlocks) {
    Block* clonedBlock;

    if(blockMap_.TryGetValue(blockRef->Target(), &clonedBlock)) {
        auto& refs = destFunction_->ParentUnit()->References();
        return refs.GetBlockRef(clonedBlock);
    }
    else {
        externalBlocks.Add(blockRef->Target());
        return blockRef;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string BlockCloner::GetUniqueName(Symbol* source, SymbolTable* symbolTable) {
    DebugValidator::IsNotNull(source);
    DebugValidator::IsNotNull(symbolTable);
    return GetUniqueName(*source->Name(), symbolTable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string BlockCloner::GetUniqueName(const string& startName, SymbolTable* symbolTable) {
    int count = 0;
    string name;

    // We append '_clone' to the name, but still check that it's
    // unique, and if not, we append a count, until it is unique.
    do {
        name = string::Format(L"%s_clone%d", startName.Chars(), count);
        count++;
    } while(symbolTable->Contains(&name));

    return name;
}

} // namespace Optimization