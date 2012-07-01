// IfCombining.hpp
// Copyright (c) Lup Gratian
//
// Combines multiple simple 'if' instructions into a single one
// by hoisting the operands in the same block.
// It also tries to promote operations to 'quest' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_IF_COMBINING_HPP
#define PC_OPTIMIZATION_IF_COMBINING_HPP  

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Tag.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/ProfileInfo.hpp"
#include "../Analysis/SparseBitvector.hpp"
#include "../Analysis/IRDominators.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/String.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Log.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class IfCombining : public Pass {
private:
    // Represents the reason while an 'or' combining step failed.
    enum FailureReason {
        Failure_None,
        Failure_NoIf,
        Failure_InvalidIf,
        Failure_ManyInstrs
    };

    typedef StaticList<Instruction*, 8> HoistingList;
    typedef Dictionary<Instruction*, bool> VisitedDict;
    typedef StaticList<Instruction*, 64> CandidateList;
    typedef StaticList<IfInstr*, 64> CandidateIfList;
    typedef Dictionary<Operand*, Instruction*> TrappingDict;
    typedef IRDominatorTree::TDominatorNode DominatorNode;
    static const int QUESTION_PHI_LIMIT = 2;
    static const int HOISTING_LIMIT = 3;

    ProfileInfo* profile_;
    IRDominatorTree* domTree_;
    Dictionary<Block*, TrappingDict> trappingOps_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Searches for two blocks that can be combined into a 'quest' instruction.
    // This usually simplifies the CFG and exposes further optimization opportunities.
    bool DetectQuestion(Block* block);

    // Tries to convert the specified 'phi' instruction, together with its 
    // predecessor blocks, to a 'quest' instruction.
    // The 'quest' can be formed only if all instructions from the predecessors
    // can be hoisted into the common block.
    bool TryExtractQuestion(PhiInstr* instr);

    // Returns 'true' if the number of "complex" 'phi' instructions
    // that need to be speculated is small.
    bool ShouldTryExtractQuestion(PhiInstr* firstInstr);

    bool IsInLoop(Block* block);

    // Creates a 'quest' instruction and moves all instructions
    // to the specified hoisting blocks.
    void GenerateQuestion(PhiInstr* instr, Block* hoistingBlock, 
                          HoistingList& hoistingList, IfInstr* ifInstr);

    // Returns the 'if' instruction that terminates the specified block,
    // or 'nullptr' if there is a different type of terminator.
    IfInstr* GetIfFromBlock(Block* block);

    // Returns the maximum number of instructions that can be hoisted
    // form 'sourceBlock' to 'hoistingBlock'.
    // It takes profile info into consideration, if available.
    int ComputeHoistingLimit(Block* hoistingBlock, Block* sourceBlock,
                             bool isIfThenElse);

    // Returns 'true' if all instructions on which 'dependentOp' depends
    // can be hoisted to the specified block. All such instructions
    // are added to 'hoistingList'. Takes into consideration the estimated
    // cost of the instructions that should be hoisted.
    bool CanInstructionsBeHoisted(Operand* dependentOp, Block* toBlock,
                                  HoistingList& hoistingList, bool isIfThenElse);

    // Returns 'true' if the instruction can be executed speculatively.
    bool CanInstructionBeSpeculated(Instruction* instr);

    // Returns 'true' if all the instructions that use the result
    // of the specified instruction are marked in 'visited',
    // meaning that they are going to be hoisted too.
    bool IsUsedOnlyByHoisted(Instruction* instr, VisitedDict& visited,
                             Block* sourceBlock);

    // Adds all source operands of the specified instruction
    // to the worklist, so that they are processed too.
    void AddOperandsToWorklist(Instruction* instr, HoistingList& worklist,
                               VisitedDict& visited, Block* sourceBlock);

    // Returns 'true' if all instructions can be executed speculatively 
    // in the specified block (they retain their execution semantics).
    bool CanInstructionsBeSpeculated(HoistingList& hoistingList, Block* toBlock);

    // Returns 'true' if the specified 'load' can be safely moved
    // to one of its dominators. This happens only if we load from 
    // a local/global variable, or there is a trapping access 
    // to the same address in one of its dominators.
    bool CanLoadBeSpeculated(LoadInstr* instr);

    // Returns 'true' if we're certain that loading form the specified
    // address can't trap (valid for local/global variables).
    bool IsNonTrapping(Operand* addressOp);

    // Returns 'true' if any of the blocks that dominate a 'load'
    // from the specified address contains a trapping 'load'/'store' instruction.
    // This guarantees that this 'load' doesn't trap.
    bool IsTrappingInDominator(Operand* addressOp, Block* startBlock);

    // Adds all trapping 'load' and 'store' instructions found
    // in the specified block to the dictionary.
    void ComputeTrappingOperands(Block* block, TrappingDict& dict);

    // Moves all instructions to the specified block.
    void HoistInstructions(HoistingList& hoistingList, Block* toBlock);

    // Returns 'true' if the specified 'phi' can be considered "cheap"
    // (i.e all incoming operands are constants).
    bool IsCheapPhi(PhiInstr* instr);

    // Returns the estimated cost of the specified instruction.
    // The cost is estimated based on the latency found on modern CPUs.
    int EstimateInstructionCost(Instruction* instr);

    // Returns 'true' if there is a 'phi' instruction in 'block'
    // which has an operand incoming from 'fromBlock'.
    bool HasIncomingOperand(Block* block, Block* fromBlock);

    // Methods for obtaining the single successor/predecessor of a block.
    Block* GetSingleSuccessor(Block* block, bool stopOnGoto = false);
    Block* GetSinglePredecessor(Block* block, bool stopOnSingle = false);
    Block* GetTrueSuccessor(IfInstr* instr);
    Block* GetFalseSuccessor(IfInstr* instr);

    // Tries to combine two blocks that form a && or || pattern.
    // This simplifies the CFG and exposes further optimization opportunities.
    bool CombineIfWithAndOr(Block* block);

    // Combines the specified block, generating the necessary instructions,
    // depending on the pattern (&& or ||). Note that it may decide that the
    // combining should not be done (for example, the limit of instructions
    // that can be hoisted to 'predecessorBlock' has been reached).
    bool CombineIfBlocks(Block* block, Block* predecessorBlock, IfInstr* ifInstr,
                         IfInstr* predIfInstr, bool isAnd);

    // Makes sure that the type of the operands are the same 
    // before they are combined.
    void MakeCompatible(Operand*& opA, Operand*& opB, Unit* unit, 
                        Instruction* beforeInstr);

    Operand* GetZeroConstant(Operand* baseOp, Unit* unit);

    // Checks if the specified block ends a chain of comparisons that
    // follow an || pattern (for example, 'if(a == 1 || a == 3 || a==5').
    bool DetectOrChain(Block* block, SparseBitVector& processed);

    // Transforms a series of compare instructions spread in more blocks
    // into a linear sequence inserted at the end of 'block'.
    Operand* FlatternOrChain(Block* block, SparseBitVector& processed,
                             CandidateList& candidateCmps, 
                             CandidateIfList& candidateIfs);

    // Returns 'true' if the specified block contains only a comparison
    // that check the equality of an operand with a constant.
    // If 'cmpOp' is set then the comparison should involve this operand.
    FailureReason IsEqualityWithConstantBlock(Block* block, CmpInstrBase*& cmpInstr,
                                              IfInstr*& ifInstr, 
                                              Operand* cmpOp = nullptr);

    // Returns 'true' if there is no 'phi' instruction in 'block' that has
    // an incoming value from block B that isn't also incoming from block B.
    bool IsValidToMergeBlocks(Block* block, Block* fromBlockA, Block* fromBlockB);

    // Returns 'true' if the number of instructions that can be hoisted
    // in the parent block has not been reached.
    // We limit the number of instruction to avoid doing useless computations.
    bool ProfitableToHoistToBlock(Block* fromBlock, Block* hoistingBlock,
                                  IfInstr* toIfInstr, HoistingList& hoistingList);
                                  

    // Returns an adjusted limit if profile information is available.
    int AdjustLimit(int limit, Block* fromBlock, Block* toBlock);

    void InstructionSimplified(Instruction* instr);

public:
    IfCombining(ProfileInfo* profile = nullptr) : profile_(profile) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    void Execute(Function* function);
};

} // namespace Optimization
#endif