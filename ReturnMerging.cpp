// ReturnMerging.cpp
// Copyright (c) Lup Gratian
//
// Implements the ReturnMerging pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ReturnMerging.hpp"

namespace Optimization {

Operand* ReturnMerging::MergeValues(ValueList& values, BlockList& blocks,
                                    Function* function) {
    DebugValidator::AreEqual(values.Count(), blocks.Count());
    DebugValidator::IsLarger(values.Count(), 1);

    // Create a 'phi' that merges all the incoming values.
    auto resultOp = Temporary::GetTemporary(values[0]->GetType());
    auto instr = PhiInstr::GetPhi(resultOp, values.Count());
    auto unit = function->ParentUnit();

    for(int i = 0; i < values.Count(); i++) {
        auto blockRef = unit->References().GetBlockRef(blocks[i]);
        instr->AddOperand(values[i], blockRef);
    }

    return resultOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
BlockReference* ReturnMerging::AppendReturnBlock(Function* function) {
    auto block = Block::GetBlock("#ret_block", function);
    return function->ParentUnit()->References().GetBlockRef(block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReturnMerging::RewriteReturnBlocks(BlockList& blocks, BlockReference* returnBlock) {
    // Replace each 'ret' instruction by a 'goto' to the return block.
    for(int i = 0; i < blocks.Count(); i++) {
        auto gotoInstr = GotoInstr::GetGoto(returnBlock);
        auto retInstr = blocks[i]->LastInstruction();
        blocks[i]->ReplaceInstructionWith(retInstr, gotoInstr);
        retInstr->Free();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReturnMerging::Execute(Function* function) {
    ValueList values;
    BlockList blocks;

    // If the return type is 'void' then we have nothing to do.
    if(function->IsVoid()) return;
    
    // Scan all the instructions and for each 'ret' record the value 
    // and the source block. The values will be merged by a 'phi' in a new
    // "return" block (placed at the end of the function), while the previous 'ret'
    // instructions will be replaced by 'goto' to the "return" block.
    function->ForEachInstruction([&values, &blocks](Instruction* instr) -> bool {
        if(auto retInstr = instr->As<ReturnInstr>()) {
            values.Add(retInstr->ReturnedOp());
            blocks.Add(retInstr->ParentBlock());
        }
        return true;
    });

    // If the number of returned values is greater than one merge them.
    if(values.Count() < 2) return;

    // Insert the 'phi' into the return block.
    auto mergedOp = MergeValues(values, blocks, function);
    auto retBlockRef = AppendReturnBlock(function);
    retBlockRef->Target()->InsertInstruction(mergedOp->DefiningInstruction());
    ReturnInstr::GetReturn(mergedOp, retBlockRef->Target());
    RewriteReturnBlocks(blocks, retBlockRef);
}

} // namespace Optimization