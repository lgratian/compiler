// SafetyInfo.hpp
// Copyright (c) Lup Gratian
//
// Provides information about the safety of certain IR transformations,
// most of all used during optimization.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SAFETY_INFO_HPP
#define PC_ANALYSIS_SAFETY_INFO_HPP

#include "../IR/Instructions.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/Block.hpp"
#include "IRDominators.hpp"
#include "OperandInfo.hpp"
using namespace IR;

namespace Analysis {

class SafetyInfo /* TODO : public Analysis */ {
private:
    IRDominatorTree* domTree_;

public:
    SafetyInfo(IRDominatorTree* domTree = nullptr) : domTree_(domTree) {}

    virtual ~SafetyInfo() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Returns 'true' if it is certain that the specified instruction is useless.
	// Can be used to perform a simple form of dead code elimination.
	virtual bool IsDefinitelyDead(Instruction* instr);
    
    // Returns 'true' if the specified instruction can be executed
    // without any information about the control flow (i.e it doesn't have
    // any side effects that could affect the execution of the program).
    virtual bool CanBeSpeculated(Instruction* instr, Block* toBlock = nullptr);

    // Returns 'true' if the definition point dominates the specified block.
    virtual bool DefinitionDominatesBlock(Operand* op, Block* block);

    // Returns 'true' if block 'a' dominates block 'b'.
    virtual bool Dominates(Block* a, Block* b);

    // Returns 'true' if the definition point of all operands 
    // of the instruction dominate the block.
    virtual bool OperandsDominateBlock(Instruction* instr, Block* block);

    // Returns 'true' if the specified instruction can be executed 
    // speculatively in the block. This can happen only if the instruction
    // has no side-effects and if the definition point of the operands used 
    // in the instruction dominate the block.
    virtual bool CanBeSpeculatedInBlock(Instruction* instr, Block* block);
};

} // namespace Analysis
#endif

//? CanBeMovedBefore/After
//? CanBeMovedToBlock (hoisted/forwarded)
