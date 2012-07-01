// Inliner.hpp
// Copyright (c) Lup Gratian
//
// Implements a helper that inlines a callee into it's caller.
// It can work with arbitrarily complex function without problems,
// and does constant folding, algebraic simplifications, dead and
// unreachable code elimination on the fly.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_INLINER_HPP
#define PC_OPTIMIZATION_INLINER_HPP

#include "Peephole.hpp"
#include "BlockUtils.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Analysis/ConstantFolder.hpp"
#include "../Base/Log.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
using namespace IR;
using namespace Analysis;
using namespace Base;

namespace Optimization {

class Inliner {
private:
    // We use a series of maps that translate from and operand
    // in the callee to the one in the caller.
    StaticList<ReturnInstr*, 8> returnInstrs_;
    StaticList<PhiInstr*, 16> phiInstrs_;
    StaticList<Parameter*, 16> parameters_;
    Dictionary<Variable*, VariableReference*> paramVariableToCopyRef_;
    Dictionary<Operand*, Operand*> operandMap_;
    Dictionary<Block*, Block*> blockMap_;
    ConstantFolder folder_;
    Peephole peephole_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Copies all variables from the callee to the caller.
    // Because we're in SSA form it means that only the variables
    // whose address is taken will be copied, and they are a few.
    // It makes sure that the variable has an unique name.
    void CopyVariables(Function* source, Function* dest);
    
    // Copies the specified variable to the caller and
    // adds creates a mapping between the variable reference of the
    // original copy and the variable reference of the copy.
    VariableReference* CopyVariable(Variable* variable, Function* dest);

    // Creates a mapping between a parameter in the callee
    // and the corresponding argument. If the address of the parameter
    // is taken then it also create the corresponding variable in the caller.
    void AddParametersToMap(Function* source, Function* dest,
                            CallInstr::ArgumentList* argList);
    
    // Creates a name starting from the one of the source symbol
    // that is unique in the specified symbol table.
    string GetUniqueName(Symbol* source, SymbolTable* symbolTable);

    string GetUniqueName(const string& startName, SymbolTable* symbolTable);
    
    // Creates a block in the caller that inherits the name
    // of the source block and it's placed after 'afterBlock'.
    Block* CreateBlock(Block* source, Function* dest, Block* afterBlock);
    
    // Collects all the return instructions from the callee.
    void CollectReturnInstructions(Function* function);

    // Creates a special block in the caller where the returned values
    // are merged (using a 'phi'). All copied blocks that correspond
    // to return blocks in the callee are modified to branch
    // to this merge block using a 'goto'.
    Block* MergeReturnedValues(Function* dest, Block* afterBlock,
                               PhiInstr*& mergePhi);

    // For parameters whose address is taken we need to generate
    // an assignment of the corresponding argument to the variable
    // created in the caller for the parameter.
    // The assignments are placed into 'entryBlock'.
    void GenerateAddressTakenAssignments(CallInstr::ArgumentList* argList,
                                         Function* source, Block* entryBlock);
    
    // Tries to simplify a 'phi' that has the same incoming operand
    // or a single incoming operand.
    Operand* SimplifyPhi(PhiInstr* instr);

    // Tries to constant-fold/simplify the instruction using
    // the operands from the caller (the call arguments, but also
    // the result of other instructions that could be folded while inlining).
    Operand* ConstantFoldAndSimplify(Instruction* instr);

    // Tries to determine the outcome of the branching instruction
    // by evaluating the condition. This allows us to avoid copying block
    // that afterwards will be definitely removed by the CFG Cleaner.
    Block* ConstantFoldBranching(Instruction* instr);

    // Returns the operand that should be used by instructions
    // that are copied into the caller. Uses the operand map.
    Operand* GetReplacementOperand(Operand* op);

    // Clones the specified source block into the caller,
    // and places it after 'afterBlock'. This clones the instruction
    // (trying to fold/simplify them on the fly), then continues, recursively,
    // with the reachable successor blocks and connects the cloned blocks.
    BlockReference* CopyBlock(Block* source, Function* dest, Block* afterBlock);

    // Clones the specified instruction into the block 
    // part of the caller. Tries to constant-fold/simplify it
    // before creating a clone.
    void ProcessInstruction(Instruction* instr, Block* dest);

    // Creates a clone of the specified instruction and inserts it
    // at the end of the block. If requested, it replaced the operands
    // with the ones that should be used in the caller.
    Instruction* CreateAndInsertClone(Instruction* instr, Block* dest,
                                      bool replaceOps = true);

    // Clones the branching instruction and clones the successors
    // that are reachable. A successor might not be reachable if 
    // the condition operand could be folded to a constant 
    // and the outcome of the branch is known.
    void ProcessBranching(Block* source, Block* copy, Function* dest);

    // Removes from the cloned 'phi' instructions any operand
    // that is incoming from a block that was not cloned or
    // is no longer a predecessor. The other operands are updated
    // so that they use the one corresponding to the caller.
    void ProcessPhiInstructions(Function* dest);

    // Implements the driver of the inlining algorithm.
    // 'entryBlock' is the unique entry point in the cloned region,
    // 'exitBlock' the unique exit point, and 'returnedOp'
    // the operand that should be used instead of the 'call' result.
    void InlineFunction(Function* source, Function* dest,
                        CallInstr::ArgumentList* argList,
                        Block* afterBlock, Block*& entryBlock, 
                        Block*& exitBlock, Operand*& returnedOp);

public:
    // Inlines the specified function into the parent function
    // of 'splitInstr', before it. 'argList' is the list of arguments
    // that should be used to replace the parameters.
    Operand* InlineFunction(Function* source, CallInstr::ArgumentList* argList,
                            Instruction* splitInstr);

    // Inlines the function that is called by the specified
    // 'call' instruction. Presumes that the callee is known statically.
    void InlineCall(CallInstr* instr);
};

}; // namespace Optimization
#endif