// IfCombining.cpp
// Copyright (c) Lup Gratian
//
// Implements the IfCombining class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IfCombining.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

void IfCombining::Execute(Function* function) {
    SparseBitVector processed;

#if 1
    //! TODO: THIS SHOULD USE A METHOD FROM 'PASS'!!!
    //! AND ONLY FOR HIGH OPTIMIZAITON LIEVEL
    domTree_ = new IRDominatorTree(function);
    domTree_->Build();
#endif

    // Process each block in the function.
    for(auto block = function->LastBlock(); block; block = block->PreviousBlock()) {
        if(processed.IsSet(block->Id())) continue;
        processed.SetBit(block->Id());

        if(DetectQuestion(block)) continue;
        if(DetectOrChain(block, processed)) continue;
        if(CombineIfWithAndOr(block)) continue;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::DetectQuestion(Block* block) {
    auto instr = block->FirstInstruction();
    bool checkedPhiCount = false;
    bool simplified = false;

    while(instr) {
        auto nextInstr = instr->NextInstruction();
        auto phiInstr = instr->As<PhiInstr>();

        if(phiInstr == nullptr) {
            instr = nextInstr;
            continue;
        }

        if(checkedPhiCount == false) {
            if(ShouldTryExtractQuestion(phiInstr) == false) {
                // There are too many 'phi' candidates, it isn't profitable
                // to create 'quest' instructions for all of them.
                break;
            }

            checkedPhiCount = true;
        }

        // Try to extract a 'quest' instruction.
        if(TryExtractQuestion(phiInstr)) {
            simplified = true;
        }

        instr = nextInstr;
    }

    return simplified;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::TryExtractQuestion(PhiInstr* instr) {
    // This tries to perform the following simplification:       
    // (1)                               (2)                        (3)
    // if c, B1, B2                          t1 = add a, 5            
    // label B1:                             t2 = add a, 3            
    //     t1 = add a, 5                     t3 = quest c, t1, t2     
    //     goto B3                           if c, B1, B2           t1 = add a, 5
    // label B2:                =>       label B1:             =>   t2 = add a, 3
    //     t2 = add a, 3                     goto B3                t3 = quest c, t1, t2
    //     goto B3                       label B2:                  t4 = mul t3, 8
    // label B3:                             goto B3
    //     t3 = phi {t1, B1}, {t2, B2}   label B3:
    //     t4 = mul t3, 8                    t4 = mul t3, 8
    //
    // (2) - after this step      (3) - after the CFG Simplifier pass is used
    // The 'phi' should have two operands, so we can match an
    // 'if-then' or an 'if-then-else' pattern.
    if(instr->OperandCount() != 2) {
        return false;
    }

    auto op1Block = instr->GetOperandBlock(0)->Target();
    auto op2Block = instr->GetOperandBlock(1)->Target();

    if((op1Block->PredecessorCount() > 1) && 
       (op2Block->PredecessorCount() > 1)) {
       return false;
    }

    // Check if the one of the supported pattern matches.
    Block* hoistingBlock = nullptr;
    Operand* incoming;
    HoistingList hoistingList;
    IfInstr* ifInstr;

    // Try to match first the 'if-then' pattern, it's more common,
    // especially after some simplifications have been performed.
    if(op1Block->PredecessorAt(0) == op2Block) {
        if(op1Block->PredecessorCount() > 1) {
            return false;
        }

        hoistingBlock = op2Block;
        incoming = instr->GetOperand(0);
    }
    else if(op2Block->PredecessorAt(0) == op1Block) {
        if(op2Block->PredecessorCount() > 1) {
            return false;
        }

        hoistingBlock = op1Block;
        incoming = instr->GetOperand(1);
    }

    if(hoistingBlock) {
        // Make sure that we really have an 'if-then' pattern.
        // All the instructions from 'otherBlock' need to be hoisted
        // in the common predecessor (this is done only if the instr. number is small).
        ifInstr = GetIfFromBlock(hoistingBlock);

        if(ifInstr == nullptr) {
            return false;
        }

        if(CanInstructionsBeHoisted(incoming, hoistingBlock,
                                    hoistingList, false) == false) {
            // There may be too many instructions, or the instructions
            // may have side-effects, so we give up.
            return false;
        }
    }
    else {
        // Check for the 'if-then-else' pattern.
        // 'op1Block' and 'op2Block' should have the same predecessors,
        // and that predecessors should have only two successors.
        if((op1Block->HasSinglePredecessor() &&
            op2Block->HasSinglePredecessor()) == false) {
            return false;
        }

        auto op1BlockPred = op1Block->PredecessorAt(0);
        auto op2BlockPred = op2Block->PredecessorAt(0);
        if(op1BlockPred != op2BlockPred) {
            return false;
        }

        hoistingBlock = op1BlockPred;
        ifInstr = GetIfFromBlock(hoistingBlock);

        if(ifInstr == nullptr) {
            return false;
        }

        // Now check that we can hoist the instructions from both blocks.
        if(CanInstructionsBeHoisted(instr->GetOperand(0), hoistingBlock,
                                    hoistingList, true) == false) {
            return false;
        }

        if(CanInstructionsBeHoisted(instr->GetOperand(1), hoistingBlock,
                                    hoistingList, true) == false) {
            return false;
        }
    }

    // If we reach this point we are certain we can create a 'quest' instruction.
    // Make sure that we don't hoist too many instructions though.
    if(ProfitableToHoistToBlock(instr->ParentBlock(), hoistingBlock,
                                ifInstr, hoistingList) == false) {
        return false;
    }

    GenerateQuestion(instr, hoistingBlock, hoistingList, ifInstr);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::IsInLoop(Block* block) {
    return block->IsInLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IfCombining::GenerateQuestion(PhiInstr* instr, Block* hoistingBlock, 
                                   HoistingList& hoistingList, IfInstr* ifInstr) {
    // First hoist the instructions to the common block.
    HoistInstructions(hoistingList, hoistingBlock);

    // Generate a 'quest' instructions that uses the the condition operand
    // from the 'if' instruction found at the end of the common block.
    Operand* trueOp;
    Operand* falseOp;

    if(instr->GetOperandBlock(0) == ifInstr->TrueTargetOp()) {
        trueOp = instr->GetOperand(0);
        falseOp = instr->GetOperand(1);
    }
    else if(instr->GetOperandBlock(1) == ifInstr->TrueTargetOp()) {
        trueOp = instr->GetOperand(1);
        falseOp = instr->GetOperand(0);
    }
    else if(instr->GetOperandBlock(0) == ifInstr->FalseTargetOp()) {
        trueOp = instr->GetOperand(1);
        falseOp = instr->GetOperand(0);
    }
    else if(instr->GetOperandBlock(1) == ifInstr->FalseTargetOp()) {
        trueOp = instr->GetOperand(0);
        falseOp = instr->GetOperand(1);
    }

    auto resultOp = Temporary::GetTemporary(trueOp->GetType());
    auto questInstr = QuestionInstr::GetQuestion(ifInstr->ConditionOp(),
                                                 trueOp, falseOp, resultOp);
    hoistingBlock->InsertInstructionBefore(questInstr, ifInstr);
                                       
    // Replace the 'phi' by the 'quest' result operand and 
    // propagate the changes in the whole function.
    InstructionSimplified(instr);
    instr->ResultOp()->ReplaceWith(resultOp);
    instr->RemoveFromBlock(true /* free */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IfInstr* IfCombining::GetIfFromBlock(Block* block) {
    if(auto ifInstr = block->BranchInstruction()->As<IfInstr>()) {
        return ifInstr;
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::CanInstructionsBeHoisted(Operand* dependentOp, Block* toBlock,
                                           HoistingList& hoistingList, 
                                           bool isIfThenElse) {
    if(auto temp = dependentOp->As<Temporary>()) {
        // We need to check if the instruction, together with all instructions
        // on which it depends, can be hoisted to 'toBlock'.
        VisitedDict visited;
        HoistingList worklist;
        int cost = 0;

        auto startInstr = temp->DefiningInstruction();
        auto sourceBlock = startInstr->ParentBlock();
        worklist.Add(startInstr);
        visited.Add(startInstr, true);

        // Compute the number of instructions we're allowed to hoist.
        int limit = ComputeHoistingLimit(toBlock, sourceBlock, isIfThenElse);

        while(worklist.IsNotEmpty()) {
            auto instr = worklist.RemoveLast();

            // Check if it's safe to speculate the instruction.
            if(CanInstructionBeSpeculated(instr) == false) {
                return false;
            }

            // This is a new hoisting candidate.
            hoistingList.Add(instr);
            cost += EstimateInstructionCost(instr);

            // Check that we didn't hit the limit of instruction
            // that can be hoisted.
            if(cost > limit) {
                return false;
            }

            // Check that it's result is used only by instructions 
            // that are also going to be hoisted.
            if(IsUsedOnlyByHoisted(instr, visited, sourceBlock) == false) {
                return false;
            }

            // Add the operands to the worklist, if needed.
            AddOperandsToWorklist(instr, worklist, visited, sourceBlock);
        }

        return CanInstructionsBeSpeculated(hoistingList, toBlock);
    }
    else {
        // Any other operand doesn't need hosting at all, because they
        // dominate all blocks in the function (consider constants, for example).
        return true;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::CanInstructionBeSpeculated(Instruction* instr) {
    if(GetSafetyInfo()->CanBeSpeculated(instr) == false) {
        // We allow hoisting of 'load' instructions,
        // if we can prove that they can't trap.
        if(auto loadInstr = instr->As<LoadInstr>()) {
            if(CanLoadBeSpeculated(loadInstr) == false) {
                return false;
            }
        }
        else return false;        
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::IsUsedOnlyByHoisted(Instruction* instr, VisitedDict& visited,
                                      Block* sourceBlock) {
    // Check that that the result operand is used only by instructions 
    // that are also going to be hoisted.
    auto userEnum = instr->GetDestinationOp()->GetUserEnumerator();

    while(userEnum.IsValid()) {
        auto user = userEnum.Next();

        if((user->ParentBlock() == sourceBlock) &&
            (visited.ContainsKey(user) == false)) {
            // An exception is for operands of an 'if'.
            if(auto ifInstr = sourceBlock->BranchInstruction()->As<IfInstr>()) {
                auto condInstr = ifInstr->ConditionOp()->DefiningInstruction();
                if(instr == condInstr) continue;
            }

            // The result is used by an instruction that
            // will not be hoisted, give up.
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IfCombining::AddOperandsToWorklist(Instruction* instr, HoistingList& worklist,
                                        VisitedDict& visited, Block* sourceBlock) {
    // Add the operands to the worklist, if needed.
    for(int i = 0; i < instr->SourceOpCount(); i++) {
        // We need to test only temporaries, and only if they're
        // defined inside this block.
        if(auto temp = instr->GetSourceOp(i)->As<Temporary>()) {
            auto definingInstr = temp->DefiningInstruction();

            if((definingInstr->ParentBlock() == sourceBlock) &&
                (visited.ContainsKey(definingInstr) == false)) {
                worklist.Add(definingInstr);
                visited.Add(definingInstr, true);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::CanLoadBeSpeculated(LoadInstr* instr) {
    // A 'load' can be speculated if we're certain that it can't trap.
    // This happens if we have a 'load' or a 'store' in a dominator that targets
    // the same address (if the address is invalid the operation will trap in the
    // dominator, so if control reaches this point we know the address is valid).

    // First we need to make sure there is no 'store' in block before this 'load'.
    // We try to disambiguate the addresses if we have variable references.
    auto addressOp = instr->SourceOp();
    auto variableRef = addressOp->As<VariableReference>();
    auto testInstr = instr->PreviousInstruction();

    while(testInstr) {
        if(auto storeInstr = testInstr->As<StoreInstr>()) {
            auto destOp = storeInstr->DestinationOp();

            if(auto storeVariableRef = destOp->As<VariableReference>()) {
                if(variableRef && (variableRef == storeVariableRef)) {
                    // The 'store' targets the same variable.
                    return false;
                }
            }
        }

        testInstr = testInstr->PreviousInstruction();
    }

    // If we're loading from a global or local variable
    // we definitely can't trap; do this check first, it's faster
    // than using the dominator tree.
    if(IsNonTrapping(addressOp)) {
        return true;
    }
    else return IsTrappingInDominator(addressOp, instr->ParentBlock());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::IsNonTrapping(Operand* addressOp) {
    // Loading from local and global variables can never trap, because
    // the pointer is never null, and because the application always has read access.
    if(addressOp->IsVariableReference()) {
        return true;
    }
    
    if(auto indexInstr = addressOp->DefiningInstrAs<IndexInstr>()) {
        return indexInstr->BaseOp()->IsVariableReference();
    }
    
    if(auto elemInstr = addressOp->DefiningInstrAs<ElementInstr>()) {
        return elemInstr->BaseOp()->IsVariableReference();
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::IsTrappingInDominator(Operand* addressOp, Block* startBlock) {
    // Check if there is a 'load' or 'store' that targets the address in
    // a block that dominates 'startBlock'. We use the dominator tree to do the check,
    // and walk up to the root as little as possible (i.e as soon as we found 
    // a 'load' or 'store' we stop computing further).
    // If we have no dominator tree give up.
    OperandInfo opInfo(startBlock->ParentFunction()->ParentUnit(),
                       GetTarget());

    // Try to answer using Operand Info, it's faster if the info 
    // is available, and it also uses range information
    if(opInfo.IsPointerNotNull(addressOp, startBlock)) {
        return true;
    }

    if(domTree_ == nullptr) {
        return false;
    }

    // Walk up the dominator tree.
    auto domBlock = domTree_->GetImmediateDominator(startBlock);

    while(domBlock) {
        if(trappingOps_.ContainsKey(domBlock) == false) {
            // This is the first time we're requesting trapping information
            // for this block; compute it now.
            trappingOps_.Add(domBlock, TrappingDict());
            ComputeTrappingOperands(domBlock, trappingOps_[domBlock]);
        }
        
        // Check the condition in this block.
        if(trappingOps_[domBlock].ContainsKey(addressOp)) {
            return true;
        }

        // Continue with the immediate dominator of this block.
        domBlock = domTree_->GetImmediateDominator(domBlock);
    }

    // We need to presume it may trap.
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IfCombining::ComputeTrappingOperands(Block* block, TrappingDict& dict) {
    // Determine which operands are targeted by 'load' and 'store' 
    // instructions in the block and add them to the dictionary.
    block->ForEachInstruction([&dict](Instruction* instr) -> bool {
        if(auto loadInstr = instr->As<LoadInstr>()) {
            dict.Add(loadInstr->SourceOp(), loadInstr);
        }
        else if(auto storeInstr = instr->As<StoreInstr>()) {
            dict.Add(storeInstr->DestinationOp(), storeInstr);
        }

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int IfCombining::ComputeHoistingLimit(Block* hoistingBlock, Block* sourceBlock,
                                      bool isIfThenElse) {
    // By default we allow only a small number of instructions to be executed.
    // There are two primary reasons:
    // - it's costly to speculatively execute instructions that are not needed
    // - branch prediction in modern CPUs is pretty good, so in most cases
    //   we are not penalized at all because we leave the branches
    int limit = isIfThenElse ? 2 : 3;

    // If we have profile information boost the limit.
    return AdjustLimit(limit, hoistingBlock, sourceBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::CanInstructionsBeSpeculated(HoistingList& hoistingList, 
                                              Block* toBlock) {
    // Traverse the list from last to first and check if each instruction
    // can safely be moved to 'toBlock'.
    if(hoistingList.Count() == 0) {
        return true;
    }

    Dictionary<Operand*, bool> processed;

    for(int i = hoistingList.Count() - 1; i >= 0; i--) {
        auto instr = hoistingList[i];
        processed.Add(instr->GetDestinationOp(), true);

        // If this is a 'load' we don't need to do further checks.
        if(instr->IsLoad()) continue;

        // The source operand must dominate the block; this also happens
        // if it's defined in the current block.
        for(int k = 0; k < instr->SourceOpCount(); k++) {
            auto source = instr->GetSourceOp(k);

            if(GetSafetyInfo()->DefinitionDominatesBlock(source, toBlock) == false) {
                if(processed.ContainsKey(source) == false) {
                    return false;
                }
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IfCombining::HoistInstructions(HoistingList& hoistingList, Block* toBlock) {
    // To hoist the instructions we simply need to remove them from
    // their source block and add the to the destination, before the branching one.
    // Note that we need to traverse the list from last to first in order
    // to maintain the definition-use relationship.
    if(hoistingList.Count() == 0) {
        return;
    }

    for(int i = hoistingList.Count() - 1; i >= 0; i--) {
        auto instr = hoistingList[i];
        instr->RemoveFromBlock();
        toBlock->InsertInstructionBefore(instr, toBlock->BranchInstruction());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::IsCheapPhi(PhiInstr* instr) {
    // We consider a 'phi' as being "cheap" if all it's operands
    // are constants of references.
    int cheapOps = 0;
        
    for(int i = 0; i < instr->OperandCount(); i++) {
        if(instr->GetOperand(i)->IsTemporary() == false) {
            cheapOps++;
        }
    }

    return cheapOps == instr->OperandCount();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::ShouldTryExtractQuestion(PhiInstr* firstInstr) {
    // We should try to create 'quest' instructions only if the number
    // of instructions that need to be hoisted is small (i.e we have
    // a small number of 'phi' that are not "cheap").
    int notCheap = 0;

    while(firstInstr) {
        if(IsCheapPhi(firstInstr) == false) {
            notCheap++;

            if(notCheap > QUESTION_PHI_LIMIT) {
                return false;
            }
        }

        firstInstr = firstInstr->NextInstruction()->As<PhiInstr>();
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int IfCombining::EstimateInstructionCost(Instruction* instr) {
    // Estimate the cost of the instruction based on the latencies usually found
    // on modern CPUs. If constants are involved some instructions may be cheaper,
    // because they can be replaced by more simple instructions. For example,
    // 'mul' has const 3, but for 't1 = mul a, 4' we define cost 2, because
    // 't1 = mul a, 5' -> 't1 = shl a, 2    t2 = add t1, a'.
    switch(instr->GetOpcode()) {
        // Cost 0
        case Instr_Ptop: return 0;

        // Cost 1
        case Instr_Add: 
        case Instr_Sub: 
        case Instr_And: 
        case Instr_Or:
        case Instr_Xor: 
        case Instr_Shl: 
        case Instr_Shr: 
        case Instr_Ushr:
        case Instr_Cmp: 
        case Instr_Ucmp:
        case Instr_Fcmp:
        case Instr_Zext:
        case Instr_Sext:
        case Instr_Trunc:
        case Instr_Ptoi:
        case Instr_Itop:
        case Instr_Element: return 1;

        // Cost 2
        case Instr_Fadd:
        case Instr_Fsub: 
        case Instr_Load: return 2;

        // Cost 2 or 3 (3 if none of the operands is a constant).
        case Instr_Mul: {
            return 2 + ((instr->GetSourceOp(0)->IsConstant() ||
                         instr->GetSourceOp(1)->IsConstant()) ? 0 : 1);
        }

        // Cost 1 or 3 (3 if the offset must be computed at runtime).
        case Instr_Address:
        case Instr_Index: return 1 + (instr->GetSourceOp(1)->IsConstant() ? 0 : 2);

        // Cost 3
        case Instr_Call: {
            auto callInstr = instr->As<CallInstr>();

            if(auto intrinsic = callInstr->GetIntrinsic()) {
                if(auto mathIntrinsic = intrinsic->As<MathIntrinsic>()) {
                    if(mathIntrinsic->IsMin() || 
                       mathIntrinsic->IsMax() || 
                       mathIntrinsic->IsAbs()) return 3;
                }
            }
            break;
        }
        case Instr_Question: return 3;

        // Cost 5
        case Instr_Fmul:
        case Instr_Ftrunc:
        case Instr_Fext:
        case Instr_Ftoi:
        case Instr_Ftoui:
        case Instr_Itof: return 5;

        // Cost 6 or 10 (10 if neither of the operands is a constant).
        case Instr_Div: 
        case Instr_Udiv: {
            return 6 + ((instr->GetSourceOp(0)->IsConstant() ||
                         instr->GetSourceOp(1)->IsConstant()) ? 0 : 4);
        }

        // Cost 10
        case Instr_Mod: 
        case Instr_Umod: return 10;
    }

    return 1000;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::CombineIfWithAndOr(Block* block) {
    // This tries to combine multiple blocks that end with an 'if' instruction
    // into a single block, where all conditions are evaluated. For example,
    // 'if(a == 4 && b == 6)' is converted to (1), and simplified to (2) and (3).
    // (1)                      (2)                          (3)
    //     t1 = cmp eq a, 4         t1 = cmp eq a, 4         t1 = cmp eq a, 4
    //     if t1, B1, B2            t2 = cmp eq b, 6         t2 = cmp eq b, 6
    // label B1:              =>    t3 = and t1, t2     =>   t3 = and t1, t2
    //     t2 = cmp eq b, 6         if t3, B1, B2            if t3, B3, B2
    //     if t2, B3, B2        label B1:               
    // ...                          if 1, B3, B2    
    // 1. The block needs to end with an 'if'.
    auto ifInstr = GetIfFromBlock(block);
    if(ifInstr == nullptr) {
        return false;
    }

    // If the condition operand is a constant we skip the block,
    // because it will be simplified later by the CFG cleaner.
    if(ifInstr->ConditionOp()->IsConstant()) {
        return false;
    }

    // 2. The block should have a single (reachable) predecessor.
    auto predecessorBlock = GetSinglePredecessor(block);
    if(predecessorBlock == nullptr) {
        return false;
    }

    // 3. The predecessor should also end with an 'if'.
    auto predIfInstr = GetIfFromBlock(predecessorBlock);
    if(predIfInstr == nullptr) {
        return nullptr;
    }

    // 4. Check if we have an && or || pattern by looking at the successors:
    //    || if predecessorBlock.trueSucc == block.trueSucc
    //    && if predecessorBlock.falseSucc == block.falseSucc
    bool valid = false;
    bool isAnd;
    Block* commonTarget;

    auto predTrueTarget = GetTrueSuccessor(predIfInstr);
    auto trueTarget = GetTrueSuccessor(ifInstr);

    if(predTrueTarget && (predTrueTarget == trueTarget)) {
        // ||
        valid = true;
        isAnd = false;
        commonTarget = trueTarget;
    }
    else {
        auto predFalseTarget = GetFalseSuccessor(predIfInstr);
        auto falseTarget = GetFalseSuccessor(ifInstr);

        if(predFalseTarget && (predFalseTarget == falseTarget)) {
            // &&
            valid = true;
            isAnd = true;
            commonTarget = falseTarget;
        }
    }

    // 5. Make sure that there are no incoming operands from the block
    //    that is not incoming from it's predecessor.
    //    If there is then we don't actually have an 'if'.
    if(valid) {
        if(IsValidToMergeBlocks(commonTarget, predecessorBlock, block) == false) {
            // Invalid case, give up.
            return false;
        }
        
        return CombineIfBlocks(block, predecessorBlock, 
                               ifInstr, predIfInstr, isAnd);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::CombineIfBlocks(Block* block, Block* predecessorBlock, IfInstr* ifInstr,
                                  IfInstr* predIfInstr, bool isAnd) {
    // We need to hoist any computation from 'block' into 'predecessorBlock',
    // then combine the conditionals according to 'isAnd'.
    HoistingList hoistingList;
    int instrCount = block->InstructionCount() - 1; // Don't count the branching one.

    if(CanInstructionsBeHoisted(ifInstr->ConditionOp(), predecessorBlock, 
                                hoistingList, false) == false) {
        return false;
    }

    // We give up if not all instructions in the block will be hoisted,
    // or the number of instructions hoisted in the block will be too large.
    if(hoistingList.Count() != instrCount) {
        return false;
    }

    if(ProfitableToHoistToBlock(block, predecessorBlock,
                                predIfInstr, hoistingList) == false) {
        return false;
    }

    // Hoist the instructions, then make sure that the condition operands
    // have the same type before we combine them.
    HoistInstructions(hoistingList, predecessorBlock);

    auto unit = block->ParentFunction()->ParentUnit();
    auto conditionOp = ifInstr->ConditionOp();
    auto predCondOp = predIfInstr->ConditionOp();

    // The condition operands must be combined only if this condition
    // is not a constant (it may be a constant because the CFG Cleaner
    // was not yet run, for example).
    if(conditionOp->IsConstant() == false) {
        MakeCompatible(predCondOp, conditionOp, unit, predIfInstr);

        // Combine the condition operands.
        auto combOp = Temporary::GetTemporary(conditionOp->GetType());
        Instruction* instr;

        if(isAnd) {
            instr = LogicalInstr::GetLogical(Instr_And, predCondOp, 
                                             conditionOp, combOp);
        }
        else instr = LogicalInstr::GetLogical(Instr_Or, predCondOp, 
                                              conditionOp, combOp);

        // Make this the new condition in the predecessor.
        predecessorBlock->InsertInstructionBefore(instr, predIfInstr);
        predIfInstr->SetConditionOp(combOp);

        // Fold the condition of 'ifInstr' to a constant so that the CFG Cleaner
        // knows that one of the blocks is now unreachable.
        if(isAnd) {
            auto oneConst = unit->Constants().GetInt(conditionOp->GetType(), 1);
            ifInstr->SetConditionOp(oneConst);
        }
        else {
            auto zeroConst = unit->Constants().GetInt(conditionOp->GetType(), 0);
            ifInstr->SetConditionOp(zeroConst);
        }
    }

    InstructionSimplified(ifInstr);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IfCombining::MakeCompatible(Operand*& opA, Operand*& opB, 
                                 Unit* unit, Instruction* beforeInstr) {
    // Check if both operands have a compatible type.
    // If not we insert comparisons to zero to make them compatible.
    auto defInstrA = opA->DefiningInstruction();
    auto defInstrB = opB->DefiningInstruction();
    auto block = beforeInstr->ParentBlock();

    if(defInstrA && defInstrB) {
        if(defInstrA->IsComparison() && 
           defInstrB->IsComparison()) {
           return;
        }
    }

    // Create the comparisons, they will use the same type.
    if((defInstrA == nullptr) || (defInstrA->IsComparison() == false)) {
        if(opA->GetType() != opB->GetType()) {
            Operand* zeroConstA = GetZeroConstant(opA, unit);
            auto resultOpA = Temporary::GetTemporary(IntegerType::GetInt32());
            auto instrA = CmpInstr::GetCmp(Order_NotEqual, opA, 
                                           zeroConstA, resultOpA);

            block->InsertInstructionBefore(instrA, beforeInstr);
            opA = resultOpA;
        }
    }

    if((defInstrB == nullptr) || (defInstrB->IsComparison() == false)) {
        if(opA->GetType() != opB->GetType()) {
            Operand* zeroConstB = GetZeroConstant(opB, unit);
            auto resultOpB = Temporary::GetTemporary(IntegerType::GetInt32());
            auto instrB = CmpInstr::GetCmp(Order_NotEqual, opB, 
                                           zeroConstB, resultOpB);

            block->InsertInstructionBefore(instrB, beforeInstr);
            opB = resultOpB;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* IfCombining::GetZeroConstant(Operand* baseOp, Unit* unit) {
    auto type = baseOp->GetType();

    if(type->IsInteger()) {
        return unit->Constants().GetInt(type, 0);
    }
    else if(type->IsFloating()) {
        return unit->Constants().GetFloating(type, 0.0);
    }
    else return unit->Constants().GetNull(type);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::DetectOrChain(Block* block, SparseBitVector& processed) {
    // We search for a "chain" of comparisons with a constant that are "linked"
    // by an 'or' relation. For example, this turns the series of blocks generated for
    // 'if(a == 1 || a == 3 || a == 4 || a == 5)' into a 'switch' instruction.
    CandidateList candidateCmps;
    CandidateIfList candidateIfs;
    CmpInstrBase* cmpInstr;
    IfInstr* ifInstr;
    
    // Check if this may be the beginning of a chain.
    if(IsEqualityWithConstantBlock(block, cmpInstr, ifInstr) != Failure_None) {
        return false;
    }

    candidateCmps.Add(cmpInstr);
    candidateIfs.Add(ifInstr);

    // This operand shall be used by all other comparisons.
    auto cmpOp = cmpInstr->LeftOp();
    int count = 0;
    bool valid = true;
    bool stopped = false;

    while(valid && !stopped && block->HasSinglePredecessor()) {
        auto predecessorBlock = block->PredecessorAt(0);
        auto prevIfInstr = ifInstr;

        // The predecessor should also contain an integer comparison.
        auto result = IsEqualityWithConstantBlock(predecessorBlock, cmpInstr, 
                                                  ifInstr, cmpOp);
        bool addCandidate = (result == Failure_None) || 
                            (result == Failure_ManyInstrs);

        // Make sure we have an 'or' relation between the blocks.
        // The 'true' target of the predecessor must match the true target of the block.
        if((result != Failure_NoIf) &&
           (prevIfInstr->TrueTargetOp() == ifInstr->TrueTargetOp())) {
            if(HasIncomingOperand(prevIfInstr->TrueTargetOp()->Target(), block)) {
                // There is an incoming operand from the current block
                // to the true target, and this means that we can't continue.
                valid = false;
                addCandidate = false;
            }
            else {
                // Continue with the predecessor.
                block = predecessorBlock;
                count++;
            }
        }
        else {
            stopped = true;
            addCandidate = false;
        }

        // Add the candidate to the list if it's valid.
        if(addCandidate) {
            candidateCmps.Add(cmpInstr);
            candidateIfs.Add(ifInstr);
        }

        if(result == Failure_ManyInstrs) {
            stopped = true;
        }
        else if((result != Failure_None) &&
                (result != Failure_NoIf)) {
            valid = false;
        }
    }

    // Check if we should combine the "chain".
    if((valid == false) || (count < 1)) return false;
    if(candidateCmps.Count() < 2) return false;
    if(candidateCmps.Count() > 512) return false;
    
    // Combine the "chain" and change the 'if' so that it considers
    // the resulting operand as being the branch condition.
    auto result = FlatternOrChain(block, processed, 
                                  candidateCmps, candidateIfs);
    auto lastInstr = block->BranchInstruction();
    lastInstr->As<IfInstr>()->SetConditionOp(result);
    InstructionSimplified(lastInstr);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::HasIncomingOperand(Block* block, Block* fromBlock) {
    for(auto instr = block->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
        if(auto phiInstr = instr->As<PhiInstr>()) {
            if(phiInstr->HasOperandFromBlock(fromBlock)) {
                return true;
            }
        }
        else break;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* IfCombining::FlatternOrChain(Block* block, SparseBitVector& processed,
                                      CandidateList& candidateCmps, 
                                      CandidateIfList& candidateIfs) {
    // Hoist all the comparisons in the block where we have stopped.
    // We insert an 'or' between each pair of comparisons.
    auto lastInstr = block->BranchInstruction();
    int last = candidateCmps.Count() - 1;
    auto result = candidateCmps[last];

    // All other 'if' instructions have their 'true' branch "deactivated".
    auto unit = block->ParentFunction()->ParentUnit();
    auto zeroConst = unit->Constants().GetInt(result->GetDestinationOp()->GetType(), 0);
    processed.SetBit(result->ParentBlock()->Id());

    for(int i = last - 1; i >= 0; i--) {
        auto cmp = candidateCmps[i];
        processed.SetBit(cmp->ParentBlock()->Id());
        block->InsertInstructionBefore(cmp, lastInstr);

        // result = or result, cmp
        auto tempOp = Temporary::GetTemporary(cmp->GetDestinationOp()->GetType());
        auto orInstr = LogicalInstr::GetLogical(Instr_Or,  result->GetDestinationOp(), 
                                                cmp->GetDestinationOp(), tempOp);
        block->InsertInstructionBefore(orInstr, lastInstr);

        // "Deactivate" the 'true' branch.
        candidateIfs[i]->SetConditionOp(zeroConst);
        result = orInstr;
    }

    return result->GetDestinationOp();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IfCombining::FailureReason 
IfCombining::IsEqualityWithConstantBlock(Block* block, CmpInstrBase*& cmpInstr,
                                         IfInstr*& ifInstr, Operand* cmpOp) {
    // We are searching for a block that ends in an 'if' whose condition
    // is the result of an 'equal' comparison with an integer constant.
    // No other instructions should appear in the block.
    ifInstr = block->BranchInstruction()->As<IfInstr>();
    if(ifInstr == nullptr) {
        return Failure_NoIf;
    }

    if(auto definingInstr = ifInstr->ConditionOp()->DefiningInstruction()) {
        if(definingInstr->IsCmp() || definingInstr->IsUcmp()) {
            cmpInstr = definingInstr->As<CmpInstrBase>();
            
            // Make sure we are comparing the same operand
            // as the previous comparison.
            if(cmpOp && (cmpInstr->LeftOp() != cmpOp)) {
                return Failure_InvalidIf;
            }

            bool validCmp = cmpInstr->IsEqual() ||
                            cmpInstr->IsLess() ||
                            cmpInstr->IsLessOrEqual();

            if(validCmp && cmpInstr->RightOp()->IsIntConstant()) {
                // Make sure that no other instructions are in the block.
                return block->InstructionCount() == 2 ? Failure_None :
                                                        Failure_ManyInstrs;
            }
        }
    }

    return Failure_InvalidIf;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::IsValidToMergeBlocks(Block* toBlock, Block* fromBlockA, 
                                       Block* fromBlockB) {
    // Check if there is a 'phi' instruction that has an incoming operand
    // from 'fromBlockB' that isn't also incoming from 'fromBlockB'.
    // If there is then we don't combine the 'if's, because we would
    // lose an incoming operand when 'fromBlockB' is removed.
    for(auto instr = toBlock->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
        if(auto phiInstr = instr->As<PhiInstr>()) {
            if(auto opB = phiInstr->GetOperandFromBlock(fromBlockB)) {
                // The 'phi' should have the same incoming operand from 'fromBlockA'.
                if(auto opA = phiInstr->GetOperandFromBlock(fromBlockA)) {
                    if(opA != opB) {
                        return false;
                    }
                }
                
                return false;
            }
        }
        else break;
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfCombining::ProfitableToHoistToBlock(Block* fromBlock, Block* hoistingBlock,
                                           IfInstr* toIfInstr, HoistingList& hoistingList) {
    
    // If one of the blocks is in a loop we don't do the transformation,
    // otherwise we could create a 'quest' inside the loop which is
    // not loop-invariant, slowing down the code.
    if(IsInLoop(hoistingBlock) != IsInLoop(fromBlock)) {
        return false;
    }

    // Compute the number of instructions on which the condition operand
    // used by the 'if' depends. If the limit is reached we don't hoist anymore.
    // We're interested only in comparison and logical instructions.
    StaticList<Instruction*, 4> worklist;
    Dictionary<Instruction*, bool> visited;
    auto toBlock = toIfInstr->ParentBlock();
    
    int limit = AdjustLimit(HOISTING_LIMIT, toBlock, fromBlock);
    int count = hoistingList.Count();

    // Add the starting instruction.
    if(auto definingInstr = toIfInstr->ConditionOp()->DefiningInstruction()) {
        worklist.Add(definingInstr);
        visited.Add(definingInstr, true);
    }

    while(worklist.IsNotEmpty()) {
        auto instr = worklist.RemoveLast();

        // Check if the limit has been reached.
        count++;

        if(count > HOISTING_LIMIT) {
            return false;
        }

        // Add all instructions on which this one depends on the worklist.
        for(int i = 0; i < instr->SourceOpCount(); i++) {
            if(auto definingInstr = instr->GetSourceOp(i)->DefiningInstruction()) {
                // We add only instructions defined inside the same block.
                if((definingInstr->IsComparison() ||
                    definingInstr->IsAnd() || 
                    definingInstr->IsOr() || 
                    definingInstr->IsXor()) &&
                   (definingInstr->ParentBlock() == toBlock) &&
                   (visited.ContainsKey(definingInstr) == false)) {
                    // Add it to the worklist.
                    worklist.Add(definingInstr);
                    visited.Add(definingInstr, true);
                }
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int IfCombining::AdjustLimit(int limit, Block* fromBlock, Block* toBlock) {
    // Try to use profile information to boost the hoisting limit.
    // If we don't have this info then we check if the frontend 
    // marked 'toBlock' as being part of a loop.
    if(profile_) {
        int frequency = profile_->GetExecutionFrequency(fromBlock, toBlock);

        if(frequency == ProfileInfo::NO_INFO) {
            if(toBlock->IsInLoop()) {
                if(toBlock->LoopDepth() >= 3) {
                    frequency = 90;
                }
                else if(toBlock->LoopDepth() == 2) {
                    frequency = 75;
                }
                else frequency = 55;
            }
        }

        if(frequency != ProfileInfo::NO_INFO) {
            // The frequency is expressed in percents.
            if(frequency >= 90)      limit *= 4;
            else if(frequency >= 75) limit *= 3;
            else if(frequency >= 55) limit *= 2;
            else if(frequency < 25)  limit = std::max(1, limit / 2); // Penalize.
            else if(frequency < 10)  limit = 0; // Don't hoist at all.
        }
    }

    return limit;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* IfCombining::GetSingleSuccessor(Block* block, bool stopOnGoto) {
    // This "constant folds" 'if' instructions when it searches for the successor,
    // else we would miss some opportunities (the condition operand of an 'if'
    // is set to 1 or 0 in this pass when two conditions are combined).
    if(auto gotoInstr = block->BranchInstruction()->As<GotoInstr>()) {
        if(stopOnGoto) {
            return block;
        }
        else return gotoInstr->TargetOp()->Target();
    }
    else if(auto ifInstr = block->BranchInstruction()->As<IfInstr>()) {
        if(auto intConst = ifInstr->ConditionOp()->As<IntConstant>()) {
            if(intConst->IsZero()) {
                // The 'false' branch is taken.
                return GetSingleSuccessor(ifInstr->FalseTargetOp()->Target(), true);
            }
            else return GetSingleSuccessor(ifInstr->TrueTargetOp()->Target(), true);
        }
    }
    else if(block->BranchInstruction()->IsReturn()) {
        return block; // Exit block.
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* IfCombining::GetSinglePredecessor(Block* block, bool stopOnSingle) {
    if((block->HasPredecessors() == false) && stopOnSingle) {
        return block; // Entry block.
    }
    if(block->HasSinglePredecessor()) {
        if(stopOnSingle) {
            return block;
        }
        else return block->PredecessorAt(0);
    }
    else if(block->PredecessorCount() == 2) {
        // We have two predecessors, check if one of them has an 'if'
        // with a constant condition that prevents it to jump to our block.
        IfInstr* ifInstr = nullptr;
        Block* other;

        if(auto temp = block->PredecessorAt(0)->BranchInstruction()->As<IfInstr>()) {
            if(temp->ConditionOp()->IsIntConstant()) {
                ifInstr = temp;
                other = block->PredecessorAt(1);
            }
        }

        if(ifInstr == nullptr) {
            if(auto temp = block->PredecessorAt(1)->BranchInstruction()->As<IfInstr>()) {
               if(temp->ConditionOp()->IsIntConstant()) {
                    ifInstr = temp;
                    other = block->PredecessorAt(0);
                }
            }
        }
        
        if(ifInstr) {
            auto intConst = ifInstr->ConditionOp()->As<IntConstant>();

            if(intConst->IsZero()) {
                // The block is not reachable from the the parent of the 'if'.
                if(ifInstr->TrueTargetOp()->Target() == block) {
                    return GetSinglePredecessor(other);
                }
            }
            else {
                // The block is not reachable from the the parent of the 'if'.
                if(ifInstr->FalseTargetOp()->Target() == block) {
                    return GetSinglePredecessor(other);
                }
            }
        }
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* IfCombining::GetTrueSuccessor(IfInstr* instr) {
    return GetSingleSuccessor(instr->TrueTargetOp()->Target(), true);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* IfCombining::GetFalseSuccessor(IfInstr* instr) {
    return GetSingleSuccessor(instr->FalseTargetOp()->Target(), true);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IfCombining::InstructionSimplified(Instruction* instr) {
#if 1
	auto block = instr->ParentBlock();
	auto function = instr->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string beforeText = IRPrinter(instr).ToString();
	Log::Warning("IfCombining " + functionName + ":" + blockName + ": " +  beforeText);
    //IRPrinter(function).Dump();
#endif
}

} // namespace Optimization