// SSAConstruction.hpp
// Copyright (c) Lup Gratian
//
// Implements the classic algorithm that converts a function to SSA form,
// based on the paper "Efficiently Computing Static Single Assignment Form 
// and the Control Dependence Graph" by Cytron et al.
// We actually build "pruned" SSA, using live variable information.
// Optionally, expression simplifications and constant folding can be done.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_SSA_CONSTRUCTION_HPP
#define PC_OPTIMIZATION_SSA_CONSTRUCTION_HPP

#include "Peephole.hpp"
#include "BlockUtils.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Variable.hpp"
#include "../IR/Constants.hpp"
#include "../IR/References.hpp"
#include "../IR/Parameter.hpp"
#include "../IR/Unit.hpp"
#include "../IR/IRPrinter.hpp"
#include "../Analysis/VariableAnalysis.hpp"
#include "../Analysis/ConstantFolder.hpp"
#include "../Analysis/IRDominators.hpp"
#include "../Analysis/IntArithmetic.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Stack.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class SSAConversion : public Pass {
private:
    // Represents the type of conditional that controls
    // a branching instruction.
    enum ConditionalType {
        Condition_None,
        Condition_Single,
        Condition_Simple,
        Condition_Add,
        Condition_Sub
    };

    struct ConditionalInfo {
        ConditionalType Type;
        Variable* LeftComparisonVariable;
        Variable* RightComparisonVariable;
        Operand* ConstantOperand;
        Operand* AddSubConstantOperand;
        OrderType Order;

        ConditionalInfo() : 
                LeftComparisonVariable(nullptr), RightComparisonVariable(nullptr),
                ConstantOperand(nullptr), AddSubConstantOperand(nullptr) {}
    };

    struct ReplacedPhi {
        Variable* PhiVariable;
        Block* Parent;
        Operand* Replacement;

        ReplacedPhi() {}

        ReplacedPhi(Variable* variable, Block* parent, Operand* op) :
                PhiVariable(variable), Parent(parent), Replacement(op) {}
    };

    typedef StaticList<Instruction*, 64> InstructionWorklist;
    typedef StaticList<Block*, 64> BlockWorklist;
    typedef StaticList<Operand*, 8> RenameStack;
    typedef Function::VariableList VariableList;
    typedef IntArithmetic IA;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    StaticList<RenameStack, 64> renameStacks_;
    Dictionary<PhiInstr*, Variable*> phiToVar_;
    StaticList<Operand*, 16> removedInstructions_;
    StaticList<ReplacedPhi, 4> replacedPhis_;
    Function* funct_;
    shared<IRDominatorTree> domTree_;
    shared<IRDominanceFrontier> domFrontier_;
    VariableAnalysis varAnalysis_;
    ConstantFolder folder_;
    Peephole peephole_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns 'true' if instructions should be simplified 
    // when doing the renaming.
    bool ShouldSimplify() {
        //! TODO: this should consider the optimization level 
        // (false for debug).
        return true;
    }

    // Return the operand that should be used to replace
    // the specified variable. If such an operand doesn't exist 
    // (because the variable was not initialized, for example), 
    // an 'undef' operand is returned.
    Operand* GetActiveOperand(Variable* variable);

    // Returns 'true' if a 'phi' is required for 
    // the variable in the specified block.
    bool ShouldInsertPhi(Variable* variable, Block* block);

    // Performs the phi-insertion step of the SSA conversion algorithm.
    // Inserts 'phi' instructions on the dominance frontier of each block
    // where an assignment to a variable is found.
    void InsertPhiInstructions(const Function::VariableList& variables);

    // Returns 'true' if the specified variable should be considered 
    // by the SSA algorithm, replacing it by a temporary/parameter.
    bool IsCandidate(Variable* variable);

    // Returns 'true' if a load/store should be considered
    // by the SSA renaming algorithm.
    bool IsCandidate(Operand* op);

    // Returns the next 'load', 'store' or 'phi' instruction that follows.
    Instruction* GetNextLoadStorePhi(Instruction* instr);

    // Pushes the specified operand on the stack associated with the variable.
    void PushOnStack(Variable* variable, Operand* op, SparseBitVector& assignedVars);

    // Pops the operands that where pushed on the rename stacks.
    void PopFromStack(SparseBitVector& assignedVars);

    void PopFromStack(int variableId);

    // Replaces the result of the load instruction with the specified operand
    // (taken from a rename stack). This also does constant folding 
    // and some basic simplifications.
    void ReplaceResult(Temporary* resultOp, Operand* newOp);

    // Tries to simplify a 'phi' instruction that isn't necessary.
    Operand* SimplifyPhi(PhiInstr* instr);

    // Checks if the conditional used to branch to successor block
    // can be propagated to them, leading to some simplification opportunities.
    void CheckConditionalPropagation(Block* block, ConditionalInfo& info);

    // As above, but for 'switch' instructions.
    Variable* CheckSwitchPropagation(Block* block);

    // Propagates the conditional used to branch to the successor blocks.
    void PerformConditionalPropagation(Block* block, Block* childBlock,
                                       ConditionalInfo& info, 
                                       SparseBitVector& assignedVars);

    // Returns the local variable that is the target 
    /// of the specified 'load' instruction.
    Variable* GetLoadedLocal(LoadInstr* instr);

    // Returns the local variable that is the target
    // of the specified 'load' instruction,
    // which is added to a constant set into 'constantOp'.
    Variable* GetAddLoadedLocalConst(Operand* op, Operand*& constantOp);

    // Returns the local variable that is the target 
    // of the specified 'load' instruction, from which 
    // we subtract a constant set into 'constantOp'.
    Variable* GetSubLoadedLocalConst(Operand* op, Operand*& constantOp);

    // Returns 'true' if liveness information for all variables is computed.
    bool ShouldComputeLiveness() {
        //! TODO: this should consider the optimization level
        // (false for debug).
        return true;
    }

    // Adds the users of the specified temporary to the worklist.
    void AddUsersToWorklist(Temporary* temp, InstructionWorklist& worklist);

    // Performs the renaming step of the SSA algorithm.
    void RenameVariables(Block* block);

    // Inserts the incoming operands for the 'phi' instructions 
    // found in the successor blocks.
    void InsertPhiOperands(Block* block, SparseBitVector& assignedVars);

    // Removes all the variables that were promoted to temporaries.
    void RemoveDeadVariables();

    // Increments the user count of the specified reference.
    void IncrementReference(Operand* op);

    // Decrements the user count of the specified reference.
    void DecrementReference(Operand* op);

    // Returns 'true' the instruction (or any of it's operands that are
    // instructions) has a incomplete 'phi' as its operand.
    bool HasIncompletePhiOperands(Instruction* instr, bool phiAllowed = true);

    void InstructionSimplified(Instruction* instr);

public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif