// ValueNumbering.hpp
// Copyright (c) Lup Gratian
//
// Implements a dominator-tree based value numbering algorithm.
// To improve the effectiveness of the redundancy elimination, it also does
// constant folding, algebraic simplification, branch conditional propagation,
// simple dead-code elimination and a simple form of PRE for 'if-then-else' constructs.
// In some simple cases it is able to eliminate redundant loads from memory.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_VALUE_NUMBERING_HPP
#define PC_OPTIMIZATION_VALUE_NUMBERING_HPP

#include "Peephole.hpp"
#include "BlockUtils.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/Temporary.hpp"
#include "../IR/References.hpp"
#include "../IR/IRGenerator.hpp"
#include "../IR/IRPrinter.hpp"
#include "../Analysis/IRDominators.hpp"
#include "../Analysis/ConstantFolder.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/StdlibRecognizer.hpp"
#include "../Analysis/ValueNumber.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/Log.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Analysis;
using namespace Base;
using namespace CompilationPass;

namespace Optimization {

class ValueNumbering : public Pass {
private:
    struct AvailableOperand {
        Operand* Operand;
        int Version;
    };

    typedef IRDominatorTree::TDominatorNode DominatorNode;
    typedef StaticList<ScopedExpressionTable, 4> ChildrenTableList;

    shared<IRDominatorTree> domTree_;
    Dictionary<Operand*, AvailableOperand> availableMemOps_;
    StaticList<LoadInstr*, 32> loadInstrs_;
    ConstantFolder folder_;
    ValueNumber valNumber_;
    Peephole peephole_;
    int memoryVersion_;
    bool availInvalidated_;
    int invalidatedVersion_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Processes all the instructions in the specified dominator tree node.
    void ProcessBlock(DominatorNode* blockNode, ScopedExpressionTable* table,
                      ChildrenTableList* siblingsTables = nullptr);

    // Checks if the instruction should be handled separately
    // and tries to process it. Returns 'true' if value numbering
    // should consider this instruction.
    bool HandleSpecialCases(Instruction*& instr, DominatorNode* blockNode,
                            ScopedExpressionTable* table);

    bool ShouldSkipInstruction(Instruction* instr);

    // Creates the scoped hash tables and processes all the children of the block.
    void ProcessChildren(DominatorNode* blockNode, ScopedExpressionTable* table);

    // Tries to value number/simplify a 'phi' instruction.
    // Note that this may not be possible if loops are involved.
    Operand* ProcessPhi(PhiInstr* instr, DominatorNode* blockNode,
                        ScopedExpressionTable* table);

    // Tries to value number the specified 'call' instructions.
    // This can be done only if the function is 'pure', or if
    // the function is a math/bitwise intrinsic.
    Operand* ProcessCall(CallInstr* instr, DominatorNode* blockNode,
                         ScopedExpressionTable* table);

    // Tries to propagate the condition expression to the children.
    void ProcessBranching(DominatorNode* blockNode, DominatorNode* childNode,
                          ScopedExpressionTable* childTable);

    // Rewrites comparisons so that 'greater' becomes 'less', for example.
    // Exposes more simplification opportunities.
    Operand* CanonicalizeComparison(CmpInstrBase* instr);

    // Tries to constant-fold and to apply arithmetic identities to the instruction.
    Operand* Simplify(Instruction* instr);

    // Tries to perform partial redundancy elimination of the specified instruction.
    // This only does a very simple form of PRE (for 'if-then-else' constructs).
    Operand* PerformPRE(Instruction* instr, ChildrenTableList* siblingsTables);

    // Creates a 'phi' instruction that "merges" the specified operands.
    Operand* CreatePhi(Operand* opA, Operand* opB, Block* opAParent, 
                       Block* opBParent, Block* parent);

    Operand* CloneInstruction(Instruction* instr, Block* parent);

    // Verifies if the blocks where the operands of the instruction
    // are defined dominate the specified block.
    bool OperandsDominateBlock(Instruction* instr, Block* block);

    // Methods for removing redundancy involving memory operations.
    Operand* ProcessLoad(LoadInstr* instr);
    void ProcessStore(StoreInstr* instr);

    // Returns 'true' if we're certain that nothing in the loop in which
    // the 'load' is found can modify the memory after we load from it.
    bool IsSmallLoopWithNoStore(LoadInstr* instr);

    bool IsSmallLoopHeaderWithNoStore(LoadInstr* instr);

    // Returns 'true' if there is a 'store'/'call' instruction in the block
    // that may write to the source operand of the specified 'load' instruction.
    // Note that only instructions starting with 'startInstr' are considered.
    bool BlockMayWriteToAddress(LoadInstr* instr, Instruction* startInstr);

    // Returns 'true' if the specified 'store' instruction
    // may write to the specified parameter.
    bool MayStoreWriteToVarOrParam(StoreInstr* storeInstr, 
                                   VariableReference* variableRef, 
                                   Parameter* parameter);

    // Returns 'true' if the specified 'call' instruction 
    // may write to the local/global variable.
    bool MayCallWriteToMemory(CallInstr* callInstr,
                              VariableReference* variableRef = nullptr);

    // Replaces all uses of the destination operand of the specified instruction
    // with another operand, then deletes the instruction.
    void ReplaceAndRemove(Instruction* instr, Operand* replacement);

    void InstructionRemoved(Instruction* instr);

public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif