// CFGSimplifier.hpp
// Copyright (c) Lup Gratian
//
// Implements a helper class that knows how to clone a series
// of blocks in the same or a different function. Can be used by various
// optimizations (loop unrolling/peeling, region specialization, etc.).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_BLOCK_CLONER_HPP
#define PC_OPTIMIZATION_BLOCK_CLONER_HPP

#include "../Analysis/SparseBitVector.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../Base/Log.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/List.hpp"
using namespace IR;
using namespace Analysis;
using namespace Base;

namespace Optimization {

class BlockCloner {
public:
    typedef List<Block*> BlockList;
    typedef Dictionary<Block*, Block*> BlockMap;
    typedef StaticList<Block*, 4> ExternalBlockList;

private:
    Dictionary<Operand*, Operand*> operandMap_;
    SparseBitVector clonedBlocks_;
    Function* destFunction_;
    Block* entryBlock_;
    Block* insertionPoint_;
    BlockMap blockMap_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    void CreateBlockListForFunction(Function* function, BlockList& blockList);

    Block* CloneBlock(Block* block);

    Instruction* CloneInstruction(Instruction* instr);

    void ProcessInstructions(BlockList& blockList);

    void ProcessInstructions(Block* clonedBlock);

    void ProcessPhiInstruction(PhiInstr* instr);

    void ReplaceSourceOperands(Instruction* instr);

    void ProcessBranchInstructions(BlockList& blockList);

    void ProcessBranchInstructions(Block* block, Block* clonedBlock);

    void PatchIncomingOperands(Block* block, Block* originalIncomingBlock, 
                               Block* clonedIncomingBlock);

    Operand* GetReplacementOperand(Operand* op);

    BlockReference* GetReplacementBlock(BlockReference* blockRef,
                                        ExternalBlockList& externalBlocks);

    string GetUniqueName(Symbol* source, SymbolTable* symbolTable);
    string GetUniqueName(const string& startName, SymbolTable* symbolTable);

public:
    void CloneBlocks(BlockList& blockList, Function* destFunct = nullptr,
                     Block* entryBlock = nullptr);
                     
    void CloneFunctionBody(Function* sourceFunct, Function* destFunct);

    BlockMap& GetBlockMap() {
        return blockMap_;
    }
};

} // namespace Optimization
#endif