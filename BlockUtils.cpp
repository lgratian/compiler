// BlockUtils.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'BlockUtils' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "BlockUtils.hpp"

namespace Optimization {

Block* BlockUtils::MergeBlocks(Block* a, Block* b, bool removeB) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    DebugValidator::AreEqual(a->SuccessorCount(), 1);

    // The successors of 'b' are now the successors of 'a'.
    a->RemoveInstruction(a->BranchInstruction(), true /* free */);

    for(int i = 0; i < b->SuccessorCount(); i++) {
        b->SuccessorAt(i)->ReplacePredecessor(b, a);
    }

    // Remove the branching instruction from 'a', then copy 
    // all instructions from 'b' to 'a'.
    Instruction* instr = b->FirstInstruction();

    while(instr) {
        auto nextInstr = instr->NextInstruction();
        instr->RemoveFromBlock();
        a->InsertInstructionLast(instr);
        instr = nextInstr;
    }

    // Delete the block, if requested.
    if(removeB) {
        DebugValidator::IsNotNull(b->ParentFunction());
        b->ParentFunction()->RemoveBlock(b, true /* free */);
    }

    return a;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool BlockUtils::DominatesItself(Block* block, Block* exceptioBlock,
                                 IRDominatorTree* domTree) {
    DebugValidator::IsNotNull(block);
    DebugValidator::IsNotNull(exceptioBlock);
    DebugValidator::IsNotNull(domTree);

    // All predecessors, except 'exceptionBlock', need to be dominated
    // by 'block'. This can happen only for loop back-edges.
    auto predecessorEnum = block->GetPredecessorEnum();

    while(predecessorEnum.IsValid()) {
        auto predecessorBlock = predecessorEnum.Next();

        if((predecessorBlock != exceptioBlock) &&
            (domTree->Dominates(block, predecessorBlock) == false)) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool BlockUtils::ReplacePhiOperandsBlock(Block* phiBlock, Block* previousBlock,
                                         Block* newBlock) {
    // Check the 'phi' instructions in 'phiBlock'. If any of the incoming
    // operands comes from 'prevBlock', remove it and reinsert it, 
    // but this time coming from 'newBlock'.
    bool replaced = false;
    auto unit = newBlock->ParentFunction()->ParentUnit();

    for(auto instr = phiBlock->FirstInstruction(); instr;
        instr = instr->NextInstruction()) {
        auto phiInstr = instr->As<PhiInstr>();
        if(phiInstr == nullptr) break;

        int previousIndex = -1; // The position of the 'undef'.
        int undefIndex = -1;  // The position where we need to update.

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            if(phiInstr->GetOperandBlock(i)->Target() == previousBlock) {
                previousIndex = i;
            }
            else if(phiInstr->GetOperandBlock(i)->Target() == newBlock) {
                // There is already an operand incoming from the new block.
                // If it is 'undef' or the same operand that is incoming
                // from 'previousBlock' we delete it.
                auto op = phiInstr->GetOperand(i);

                if(op->IsUndefinedConstant() ||
                   (op == phiInstr->GetOperandFromBlock(previousBlock))) {
                    undefIndex = i;
                }
            }
        }

        // If nothing needs to be updated check the next 'phi'.
        if(previousIndex == -1) continue;

        // Remove the 'undef' incoming value, if present.
        if(undefIndex != -1) {
            phiInstr->RemoveOperand(newBlock);
        }

        // Update the incoming value.
        auto incomingOp = phiInstr->GetOperandFromBlock(previousBlock);
        auto newBlockRef = unit->References().GetBlockRef(newBlock);
        phiInstr->AddOperand(incomingOp, newBlockRef);
        phiInstr->RemoveOperand(previousBlock);
        replaced = true;
    }

    return replaced;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockUtils::InsertSameIncoming(Block* block, Block* oldBlock, Block* newBlock) {
    // The incoming 'phi' operand from 'oldBlock' is inserted again
    // as incoming from 'newBlock', for all 'phi' instructions in 'block'.
    block->ForEachPhiInstruction([oldBlock, newBlock](PhiInstr* instr) -> bool {
        if(auto incomingOp = instr->GetOperandFromBlock(oldBlock)) {
            instr->AddOperand(incomingOp, newBlock->GetReference());
        }

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string  BlockUtils::CreateUniqueName(const string& pattern, SymbolTable& symbols) {
    string name = pattern;
    int count = 0;

    while(symbols.Contains(&name)) {
        name = string::Format(L"%s_%d", pattern.Chars(), ++count);
    }

    return name;
}

} // namespace Optimization