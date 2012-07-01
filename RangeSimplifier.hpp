// RangeSimplifier.hpp
// Copyright (c) Lup Gratian
//
// Simplifies the instructions by taking advantage
// of the facts that can be deduced from value ranges.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_RANGE_SIMPLIFIER_HPP
#define PC_OPTIMIZATION_RANGE_SIMPLIFIER_HPP

#include "Peephole.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Operand.hpp"
#include "../IR/IRTypes.hpp"
#include "../IR/Constants.hpp"
#include "../IR/References.hpp"
#include "../IR/Instructions.hpp"
#include "../Analysis/OperandInfo.hpp"
#include "../Analysis/ConstantFolder.hpp"
#include "../Compilation Pass/Pass.hpp" 
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class RangeSimplifier : public Pass {
private:
    OperandInfo opInfo_;
    ConstantFolder folder_;
    IRGenerator irGen_;
    Peephole peephole_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    int GetMaximumScanLevel() const {
        //! TODO: This should be configurable.
        return 2;
    }
    
    // Tries to simplify the specified instruction
    // using range information.
    void ProcessInstruction(Instruction* instr);

    // Tries the replace the operands by constant that can be
    // deduced from ranges. Example:
    // if(a == 4) ... t = a + 2  ->  t = 4 + 2
    bool ReplaceSourcesByConstants(Instruction* instr);

    // Tries to re-evaluate the instructions that represent operands
    // for the specified instructions using information that now
    // can be deduced from ranges. Example:
    // t1 = a + 3;                       a + 3 is re-evaluated with a = 4
    // if(a == 4) ... t2 = t1 + 5  ->    t2 = 7 + 5
    bool ReevaluateSources(Instruction* instr);

    bool ReplaceIncomingValues(Instruction* instr);

    IntConstant* GetIncomingConstant(Operand* incomingOp, Block* incomingBlock,
                                     Block* block);

    IntConstant* GetIntConstant(__int64 value, Operand* op, Block* block);

    Operand* ReevaluatePointerComparison(UcmpInstr* instr, Block* testBlock);

    // Returns the constant that represents the specified operand,
    // or 'nullptr' if the operand is not a constant, even if
    // range information is used.
    Operand* GetConstantForSource(Instruction* instr, int sourceIndex, 
                                  Block* testBlock);

    Operand* GetConstantForOperand(Operand* op, Block* testBlock);

    // Tries to replace an operand with another one that is
    // identical , fact deduced from range information.
    // This is done only if a simplification may be enabled,
    // or if register pressure may be reduced. Example:
    // if(a == b) ... t = a + b  ->  t = a + a
    bool ReplaceWithIdentical(Instruction* instr);

    bool IsSymbolicEquality(Range& range) {
        return range.IsRange && range.HasBaseOperand() &&
               range.IsSingleValue() && (range.Low.Constant == 0);
    }

    // Try to constant-fold and to simplify the instruction
    // using arithmetic identities.
    Operand* SimplifyInstruction(Instruction* instr);

    // Returns 'true' if the operand is used in the specified block
    // or any of its successors. 'maxUserLevel' is set to the
    // maximum level at which the operand is used.
    bool IsUsedInBlockOrSuccessors(Operand* op, Block* block, 
                                   int level, int& maxUserLevel);

    // Returns 'true' if there is an instruction in the block
    // that uses the specified operand.
    bool IsUsedInBlock(Operand* op, Block* block);

    Operand* GetBool(bool value, Operand* op) {
        if(value) return irGen_.GetIntConst(op->GetType(), 1);
        else return irGen_.GetIntConst(op->GetType(), 0);
    }

    void InstructionSimplified(Instruction* instr);

public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif