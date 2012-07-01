// CFGSimplifier.hpp
// Copyright (c) Lup Gratian
//
// Identifies common CFG patterns and tries to simplify them.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_CFG_SIMPLIFIER_HPP
#define PC_OPTIMIZATION_CFG_SIMPLIFIER_HPP

#include "BlockUtils.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Analysis/CFGWalker.hpp"
#include "../Analysis/IRDominators.hpp"
#include "../Analysis/ConstantFolder.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Analysis/ValueNumber.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/Log.hpp"
#include "../Base/StaticList.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class CFGSimplifier : public Pass {
private:
    struct BlockHash {
        Block* HashedBlock;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        BlockHash() {}

        BlockHash(Block* block) : HashedBlock(block) {}

        BlockHash(const BlockHash& other) : HashedBlock(other.HashedBlock) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        unsigned GetHashCode() const;

        bool operator== (const BlockHash& other) const;

        bool operator< (const BlockHash& other) const {
            return false;
        }
    };

    typedef StaticList<__int64, 16> CmpValuesList;
    typedef Dictionary<__int64, bool> CmpValuesDict;
    typedef StaticList<Instruction*, 32> InstructionWorklist;
    typedef StaticList<Operand*, 4> OperandList;
    typedef StaticList<Block*, 4> BlockList;
    typedef StaticList<PhiInstr*, 4> PhiInstructionList;
    typedef StaticList<Constant*, 16> ConstantList;
    typedef StaticList<ConstantList, 8> SwitchConstantList;
    typedef StaticList<Instruction*, 8> InstructionList;

    //! TODO: THIS SHOULD BE A CONTROL
    static const int MAX_HOISTING_QUESTIONS = 2;

    ConstantFolder folder_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns the number of instructions that should be scanned
    // when searching for hoisting candidates in the successor blocks.
    int GetHoistingSearchDepth() const {
        //! TODO: THIS SHOULD TAKE INTO CONSIDERATION THE OPTIMIZATION LEVEL.
        return 5;
    }

    // Tries to simplify the specified block using one of the
    // available methods. Returns 'true' if the block could be simplified.
    bool SimplifyBlock(Block* block);

    // Tries to convert a long chain of 'or' instructions that involve
    // only integer constant to a 'switch', which can be lowered
    // more efficiently (jump-table, for example).
    bool ConvertOrChainToSwitch(Block* block);

    // Checks if this is a valid 'or' in an or-chain (sets 'valid').
    // Adds any found comparison instruction to the worklist,
    void HandleOrInOrChain(OrInstr* orInstr, CmpValuesList& cmpValues,
                           InstructionWorklist& worklist,
                           Operand*& compareOp, bool& valid);

    // Checks if the comparison instruction is part of an or-chain 
    // (sets 'valid' and adds the 'case' values to 'addedValues').
    // If 'compareOp' is specified then the comparison must involve
    // this operand, and in all cases other operand must be an integer constant.
    void HandleCmpInOrChain(CmpInstrBase* cmpInstr, CmpValuesList& cmpValues,
                            InstructionWorklist& worklist,
                            Operand*& compareOp, bool& valid, 
                            CmpValuesDict& addedVals);

    // Returns 'true' if the instructions have definitely
    // the same meaning (handles commutative instructions too, 
    // so that 'add a, b' and 'add b, a' are considered the same). 
    bool IsSameInstruction(Instruction* instrA, Instruction* instrB);

    // This tries to hoist the common instructions from the successor blocks.
    // This can be done only if it's safe to speculate the instructions.
    bool HoistCommonInstructions(Block* block);

    // Moves the instruction to 'block' and replaces all 
    // instructions in the list with its result.
    void HoistToBlock(Instruction* instr, Block* block,
                      InstructionList& replacedInstrs);

    bool CanBeHoistedToBlock(Instruction* instr, Block* block);

    bool HoistUsingQuestion(Instruction* instr, Block* block);

    bool CreateHoistingQuestion(Instruction* instr, Instruction* otherInstr, 
                                Operand* commonOp, Operand* otherOpTrue, 
                                Operand* otherOpFalse, Block* block);

    void DetectCommonOperand(Instruction* instr, Instruction* otherInstr, 
                             Operand* &commonOp, Operand* &otherOpTrue, 
                             Operand* &otherOpFalse );

    // Returns the first instruction found in the specified block
    // that is identical to the specified one ('sameInstr').
    // 'maxDepth' is used to limit the number of instructions that are searched.
    Instruction* GetSameInstruction(Block* block, Instruction* sameInstr, 
                                    int maxDepth);

    // Adds the specified value to the list of 'case' values.
    void AddCaseValue(__int64 value, CmpValuesList& cmpValues,
                      CmpValuesDict& addedVals);

    // Tries to convert a 'switch' with 'case' values that are
    // in increasing order and that target the same block
    // to a range test that uses an 'if'.
    bool ConvertSwitchToRangeTest(Block* block);

    // Creates the range tests and the 'if' instruction.
    void ReplaceSwitchByRangeTest(SwitchInstr* switchInstr, __int64 minValue,
                                  __int64 maxValue, Block* inRangeBlock,
                                  Block* notInRangeBlock);

    // Tries to replace a 'switch' that has a 'quest' as the condition,
    // and the 'quest' operands as its 'case' value with an 'if'.
    bool ConvertSwitchOnQuestion(Block* block);

    // Tries to perform a limited variant of jump-threading.
    // If this block ends with an 'if' and we have predecessors
    // for which we can determine the target block, we can force them 
    // to jump directly to the target.
    bool ThreadPhiBlocksToDestination(Block* block);

    // Returns 'true' if the block is a candidate for jump-threading,
    // meaning that its instruction are used only to compute the successor.
    bool IsThreadingCandidate(Block* block, PhiInstr*& phiInstr,
                              CmpInstrBase*& cmpInstr, IfInstr*& ifInstr);

    // Returns the block that will be the successor
    // if the specified constant is used as the condition operand.
    BlockReference* GetThreadingSuccessor(Operand* op, CmpInstrBase* cmpInstr,
                                          IfInstr* ifInstr);

    // Updates the 'phi' instruction in 'successor' so that
    // all values that are incoming from 'prevPred' should be
    // incoming from 'newPred' too.
    void PatchSuccessor(Block* successor, Block* prevPred, Block* newPred);

    // Tries to merge two 'switch' instructions, one of which
    // is nested inside the other one. This can be done only if
    // the 'case' values of the nested switch are not used by its parent.
    bool MergeNestedSwitch(Block* block);

    bool SwitchTargetsPhiBlock(SwitchInstr* switchInstr, Block* newDefaultBlock);

    // Returns 'true' if there is any incoming operand from 'fromBlock'.
    bool HasIncomingFromBlock(Block* fromBlock, Block* toBlock);

    // Any operand that is incoming from 'prevBlock' is modified
    // so that it appears that it is incoming from 'newBlock'.
    void ChangeIncomingBlock(Block* block, Block* prevBlock, Block* newBlock);

    // Tries to convert a 'switch' that only sets the value
    // of a variable to a load from a global variable that is
    // initialized with the values set by the 'switch'.
    bool ConvertSwitchToLoadFromGlobal(Block* block);

    // Adds all 'phi' instructions from 'defaultBlock' to the list.
    // Only 'phi's with integer and floating type are added.
    // Returns 'false' if there are too many 'phi' instructions.
    bool CollectPhiInstructions(Block* defaultBlock, PhiInstructionList& phiInstrs);

    // Adds to 'switchConsts' all constants that are assigned
    // to the variables in each 'case' block.
    // Returns 'false' if the 'switch' cannot be converted.
    bool CollectSwitchConstants(SwitchInstr* switchInstr, Block* defaultBlock,
                                SwitchConstantList& switchConsts, 
                                PhiInstructionList& phiInstrs, __int64& step);

    // Used during Debug builds to validate the collected constants.
    void ValidateSwitchConstants(SwitchConstantList& switchConsts,
                                 PhiInstructionList& phiInstrs);

    // Creates a variable with the values of all assigned variables.
    // We can use a single variable only if all constant have the same type.
    void CreateSameTypeGlobal(SwitchInstr* switchInstr, PhiInstructionList& phiInstrs,
                              SwitchConstantList& switchConsts, 
                              const Type* type, __int64 step,
                              Block*& loadBlock, OperandList& loadedOps);

    // Creates a global variable for each variable that is assigned
    // in the 'case' blocks. Multiple variables are used because 
    // their type is different.
    void CreateDifferentTypeGlobals(SwitchInstr* switchInstr, 
                                    PhiInstructionList& phiInstrs,
                                    SwitchConstantList& switchConsts, 
                                    __int64 step, Block*& loadBlock, 
                                    OperandList& loadedOps);

    // Creates a global variable whose name is unique.
    GlobalVariable* CreateUniqueVariable(const Type* type, Block* block, int count);

    // Creates a block whose name is unique.
    // The new block is inserted after 'block'.
    Block* CreateUniqueBlock(Block* block);

    // All the incoming operands are removed and instead
    // the operand loaded from the global variable is used.
    // Used by 'ConvertSwitchToLoadFromGlobal'.
    void PatchSwitchPhis(PhiInstructionList& phiInstrs, BlockReference* loadBlockRef,
                         OperandList& loadedOps, Block* defaultBlock,
                         Block* switchBlock);

    // Tries to convert a chain of 'if' instructions
    // to a 'switch', which can be lowered more efficiently 
    // (jump-table, for example). This can be done only if we have
    // equality comparison with an integer constant.
    bool ConvertIfChainToSwitch(Block* block);

    // Returns the integer constant that is compared by the
    // instruction that is the conditional operand of the 'if'
    // that ends the specified block. If 'op' is specified 
    // the comparison should target it.
    IntConstant* GetEqualsComparisonValue(Block* block, Operand*& op);

    // Tries to eliminate a 'goto'/'if' to a block that returns
    // from the function. The possible cases are handled by the methods below.
    bool SimplifyJumpToReturn(Block* block);

    // Tries to eliminate a 'goto' to a block that returns a value,
    // possibly a 'phi' instruction result.
    bool SimplifyGotoToReturn(Block* block, GotoInstr* gotoInstr);

    // Tries to eliminate a 'goto' to a block that returns nothing.
    bool SimplifyGotoToVoidReturn(Block* block, Block* targetBlock);

    // Tries to eliminate an 'if' that goes to two return blocks.
    // See the implementation for the cases it handles.
    bool SimplifyIfToReturn(Block* block, IfInstr* ifInstr);

    bool IsValidGotoReturn(Block* gotoBlock, Block* returnBlock);

    // Tries to eliminate an 'if' that goes to two blocks
    // that contain return instructions, possible a value
    // that is the result of a 'phi' instruction.
    bool SimplifyIfToReturnReturn(Block* block, IfInstr* ifInstr,
                                  Block* trueBlock, Block* falseBlock);

    // Scans the block and collects all 'phi' instructions, and an optional
    // "cheap" instruction which is assigned to 'other'. 
    // If "expensive" instructions remain it returns 'false'.
    bool CollectPhisAndOther(Block* returnBlock, PhiInstructionList& phis,
                             Instruction*& other);

    // Looks for blocks that are duplicate (have the same instructions
    // with the same operands) and removes the ones that are duplicates.
    // Duplicate blocks most often consist of a single return instruction.
    void RemoveDumplicateBlocks(Function* function);

    bool CheckIfBlockIsDumplicate(Block* block);

    Instruction* CloneInstruction(Instruction* instr, Block* block,
                                  Instruction* beforeInstr);

    void TryRemovePhi(PhiInstr* instr);

    bool IsCheapInstruction(Instruction* instr);

    void BlockSimplified(Block* block, int type);

public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif