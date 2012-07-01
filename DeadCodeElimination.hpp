// DeadCodeElimination.hpp
// Copyright (c) Lup Gratian
//
// Implements an aggressive dead-code elimination algorithm
// that presumes that all instructions are dead, keeping only
// the ones that can be proven to be required/essential.
// It is also able to eliminate dead blocks using control-dependence information.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_DEAD_CODE_ELIMINATION_HPP
#define PC_OPTIMIZATION_DEAD_CODE_ELIMINATION_HPP

#include "BlockUtils.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Intrinsics.hpp"
#include "../Analysis/ControlDependenceGraph.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Analysis/StdLibRecognizer.hpp"
#include "../Analysis/IntArithmetic.hpp"
#include "../Analysis/TypeInfo.hpp"
#include "../Analysis/OperandInfo.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/Log.hpp"
#include "../Base/List.hpp"
#include "../Base/Dictionary.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class DeadCodeElimination : public Pass {
private:
    typedef List<StoreInstr*> StoreList;
    typedef unsigned __int64 Mask;
    typedef IntArithmetic IA;
    typedef Analysis::TypeInfo TI;
    typedef DeadCodeElimination DCE;
    typedef Mask (DCE::*BitEstimator)(Operand* a, Operand* b);

    Function* funct_;
    List<Instruction*> worklist_;
    Dictionary<Instruction*, bool> processedInstrs_;
    Dictionary<VariableReference*, StoreList> potentiallyDeadStores_;
    Dictionary<VariableReference*, bool> loadedVariables_;
    ControlDependenceGraph dependenceGraph_;
    SparseBitVector activeBlocks_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Executes the algorithm that determines the required instructions,
    // then deletes the ones that are not required.
    void ProcessInstructions();

    // Deletes the instructions that have been determined to not be required.
    void DeleteDeadInstructions();

    // Deletes a branch which is dead. This happens when none of it's
    // successors is required. The branch is replaced with a 'goto'
    // to the immediate post-dominator.
    void DeleteDeadBranch(Instruction* instr);

    // Returns the immediate post-dominator of the specified block.
    Block* GetImmediatePostDominator(Block* block);

    // Returns 'true' if the specified instruction has a side-effect
    // that makes it essential for the correct execution of the function.
    bool IsEssentialInstruction(Instruction* instr);

    // Returns 'true' if the specified 'call' instruction is required
    // because it may affect the external state of the application.
    // Calls to some intrinsics are known to not be required.
    bool IsEssentialCall(CallInstr* instr);

    // Returns 'true' if the specified 'store' instruction is required.
    // A store into a non-address-taken local variable is presumed 
    // to not be required unless a load from that variable is found.
    bool IsEssentialStore(StoreInstr* instr);

    // All stores that target the specified variable are marked as being
    // required, because a load was found.
    void MarkStoresAlive(VariableReference* baseVariable);

    // Mark the fact that there is at least on 'load' that targets
    // the specified variable (it means that a 'store' that targets
    // the same variable is required).
    void MarkLoadFromVariable(VariableReference* baseVariable);

    // Add the specified block to the list of blocks that are required.
    // This also adds to the worklist all branching instructions
    // from the blocks that control its execution.
    void MarkBlockActive(Block* block);

    // Return the local variable that acts as the base for a series
    // of addressing instruction, or 'nullptr' if it's not the case.
    VariableReference* GetBaseVariable(Operand* op);

    // Returns 'true' if there is a 'load' instruction that
    // has the specified variable as a source.
    bool HasLoadFromVariable(VariableReference* variableRef) {
        DebugValidator::IsNotNull(variableRef);
        return loadedVariables_.ContainsKey(variableRef);
    }

    // Adds the 'store' instruction associated with the specified
    // base variable to the list of stores that might be dead.
    void AddPotentiallyDeadStore(VariableReference* baseVariable,
                                 StoreInstr* instr);

    // Adds to the worklist all the instructions that are required
    // (have a externally-visible side-effect).
    void CollectEssentialInstructions();

    // Adds to the worklist all instruction operands, because they
    // are required. If the instruction is a 'phi' the blocks
    // corresponding to the incoming values are activated.
    void AddOperandsToWorklist(Instruction* instr);

    // Adds to the worklist all incoming 'phi' operands
    // that are instructions, and activates the corresponding block.
    void AddPhiOperandsToWorklist(PhiInstr* instr);

    // Adds the specified instruction to the worklist.
    // It ensures that an instruction is processed a single time.
    void AddToWorklist(Instruction* instr);

    // Extracts an instruction from the worklist,
    // or returns 'nullptr' if no the worklist is empty.
    Instruction* RemoveFromWorklist();

    // Returns 'true' if the specified instruction has already been handled.
    bool WasInstructionProcessed(Instruction* instr) {
        return processedInstrs_.ContainsKey(instr);
    }

    // Estimates the bits that are discarded by the specified instruction.
    // For example, 'a >> 8' discards the lowest 8 bits.
    Mask EstimateDeadBits(Instruction* instr);

    // Tries to select the left or right operands of the specified
    // instruction based on the bits that remain alive after
    // some bits will be killed by the user.
    Operand* SelectOperand(Instruction* instr, Mask deadBits,
                           BitEstimator estimator);

    // Methods that estimate the bits that change when
    // an operation is applied between the operands.
    // or
    Mask EstimateSetBits(Operand* a, Operand* b);

    // and
    Mask EstimateResetedBits(Operand* a, Operand* b);

    // xor
    Mask EstimateInvertedBits(Operand* a, Operand* b);

    // add
    Mask EstimateAddedBits(Operand* a, Operand* b);

    // If the instruction performs an operation that affects
    // only bits that are killed by the user we can use directly
    // one of the operands. Returns the selected operand,
    // or 'nullptr' if the more bits than the killed ones are affected.
    Operand* SkipDeadInstruction(Instruction* instr, Mask deadBits);

    __int64 OperandBits(Operand* op) {
        return TI::GetSizeBits(op->GetType());
    }

    void InstructionEliminated(Instruction* instr);

public:
    DeadCodeElimination(Function* function) : 
            funct_(function), dependenceGraph_(function) {}

    void Execute();
};

} // namespace Optimization
#endif