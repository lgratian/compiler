// DeadCodeElimination.hpp
// Copyright (c) Lup Gratian
//
// Implements the DeadCodeElimination pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "DeadCodeElimination.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

void DeadCodeElimination::Execute() {
    ProcessInstructions();
    DeleteDeadInstructions();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::ProcessInstructions() {
    // Add to the worklist all instructions that are essential
    // for the execution of the function (this includes instructions
    // with side effects, such as 'store', 'call' and 'ret').
    CollectEssentialInstructions();

    // Build the control dependence graph. It is used to activate
    // the blocks that control the execution of another block.
    dependenceGraph_.Build();
    dependenceGraph_.Dump();

    // Iterate until the worklist is empty. An instruction is taken
    // from the worklist and all it's operands that are instructions
    // too are added to the worklist, because they're required.
    while(auto instr = RemoveFromWorklist()) {
        // The block that contains is marked active
        // (this adds to the worklist the branching instructions 
        //  from all blocks that control its execution).
        MarkBlockActive(instr->ParentBlock());
        AddOperandsToWorklist(instr);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::DeleteDeadInstructions() {
    // Delete all instructions that are not marked.
    // If we have a branch to a block that is not active
    // we make the branch jump to the immediate post-dominator
    // of its parent block (the dead blocks are avoided).
    for(auto block = funct_->FirstBlock(); block; block = block->NextBlock()) {
        // Process all instructions and delete the dead ones.
        auto instr = block->FirstInstruction();

        while(instr) {
            auto nextInstr = instr->NextInstruction();

            // Delete the instruction if it's dead.
            // If it's a branch we handle it separately.
            if(WasInstructionProcessed(instr) == false) {
                if(instr->IsBranching() == false) {
                    InstructionEliminated(instr);
                    instr->RemoveFromBlock(true /* free */);
                }
                else DeleteDeadBranch(instr);
            }

            instr = nextInstr;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::DeleteDeadBranch(Instruction* instr) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsTrue(instr->IsBranching());

    // A branching instruction is dead if none of its successors
    // are required. In this case we can jump directly to the
    // immediate post-dominator.
    auto block = instr->ParentBlock();
    auto postDom = GetImmediatePostDominator(block);

    // Remove the current branching instruction,
    // replace it with a 'goto' to the postdominator.
    if(postDom) {
        block->LinkWith(postDom);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* DeadCodeElimination::GetImmediatePostDominator(Block* block) {
    auto& postdomTree = dependenceGraph_.GetPostdominatorTree();
    auto postDom = postdomTree.GetImmediateDominator(block);
    DebugValidator::IsNotNull(postDom);

    return postDom;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::CollectEssentialInstructions() {
    // Add to the worklist all instructions that are essential.
    // These form the initial set of alive instructions.
    funct_->ForEachInstruction([this](Instruction* instr) -> bool {
        if(IsEssentialInstruction(instr)) {
            AddToWorklist(instr);
        }
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* DeadCodeElimination::RemoveFromWorklist() {
    if(worklist_.IsEmpty()) {
        // Nothing left to process.
        return nullptr;
    }

    return worklist_.RemoveLast();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::MarkBlockActive(Block* block) {
    // The instruction's parent block becomes active,
    // the branching instructions in all blocks that control it's execution
    // need to be added to the worklist.
    if(activeBlocks_.IsSet(block->Id()) == false) {
        activeBlocks_.SetBit(block->Id());
        
        auto controllingBlocks = dependenceGraph_.GetControllingBlocks(block);
        if(controllingBlocks == nullptr) {
            return;
        }

        controllingBlocks->ForEach([this](Block* block) -> bool {
            AddToWorklist(block->BranchInstruction());
            return true;
        });
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::AddToWorklist(Instruction* instr) {
    DebugValidator::IsNotNull(instr);

    if(processedInstrs_.ContainsKey(instr) == false) {
        worklist_.Add(instr);
        processedInstrs_.Add(instr, true);

        // If this is a 'load' from a local variable
        // add all 'store's to the same variable to the worklist,
        // because they can no longer be considered dead.
        if(auto loadInstr = instr->As<LoadInstr>()) {
            if(auto baseVariable = GetBaseVariable(loadInstr->SourceOp())) {
                MarkLoadFromVariable(baseVariable);
                MarkStoresAlive(baseVariable);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::AddOperandsToWorklist(Instruction* instr) {
    DebugValidator::IsNotNull(instr);

    if(auto phiInstr = instr->As<PhiInstr>()) {
        AddPhiOperandsToWorklist(phiInstr);
        return;
    }

    // Estimate the bits that are killed by the instruction.
    Mask deadBits = EstimateDeadBits(instr);

    for(int i = 0; i < instr->SourceOpCount(); i++) {
        auto temp = instr->GetSourceOp(i)->As<Temporary>();
        if(temp == nullptr) continue;

        bool wasDead = false;

        if(deadBits) {
            // Check if the operand is actually dead because the bits
            // modified by it are "killed" by the instruction.
            // We iterate until no more dead operands are found. For example:
            // b = or a, 1   -> dead, sets bit killed by 'shr'
            // c = or b, 2   -> dead, the same
            // d = shr c, 8  -> d = shr a, 8
            auto defInstruction = temp->DefiningInstruction();

            while(auto replacementOp = SkipDeadInstruction(defInstruction, deadBits)) {
                // Replace the original operand.
                instr->ReplaceSourceOp(i, replacementOp);
                defInstruction = replacementOp->DefiningInstruction();
                wasDead = true;
            }
        }

        // The temporary might have been replaced by something
        // that isn't a temporary anymore.
        if(wasDead) {
            temp = instr->GetSourceOp(i)->As<Temporary>();
            if(temp == nullptr) continue;
        }

        AddToWorklist(temp->DefiningInstruction());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::AddPhiOperandsToWorklist(PhiInstr* instr) {
    DebugValidator::IsNotNull(instr);

    for(int i = 0; i < instr->OperandCount(); i++) {
        if(auto temp = instr->GetOperand(i)->As<Temporary>()) {
            AddToWorklist(temp->DefiningInstruction());
        }

        // The block from which the operand is incoming
        // must be processed, make its branching instruction active.
        auto incomingBlock = instr->GetOperandBlock(i)->Target();
        AddToWorklist(incomingBlock->BranchInstruction());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::MarkLoadFromVariable(VariableReference* baseVariable) {
    DebugValidator::IsNotNull(baseVariable);
    DebugValidator::IsTrue(baseVariable->IsLocalVariableRef());
    loadedVariables_.Add(baseVariable, true);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::MarkStoresAlive(VariableReference* baseVariable) {
    DebugValidator::IsNotNull(baseVariable);
    DebugValidator::IsTrue(baseVariable->IsLocalVariableRef());

    // Any store that targets the variable must be considered live,
    // because at least in one place the values are loaded.
    if(potentiallyDeadStores_.ContainsKey(baseVariable) == false) {
        return;
    }

    auto& storeList = potentiallyDeadStores_[baseVariable];

    for(int i = 0; i < storeList.Count(); i++) {
        AddToWorklist(storeList[i]);
    }

    // The list can now be removed.
    potentiallyDeadStores_.Remove(baseVariable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::IsEssentialInstruction(Instruction* instr) {
    DebugValidator::IsNotNull(instr);

    // An instruction is considered to be essential if it has
    // a side-effect visible externally (store to memory, function call, return).
    if(instr->IsArithmetic() || 
       instr->IsLogical()    ||
       instr->IsConversion()) {
        // The most often cases, handle it first.
        return false;
    }

    if(instr->IsReturn()) {
        // We consider all 'ret' instructions essential.
        return true;
    }
    else if(auto storeInstr = instr->As<StoreInstr>()) {
        return IsEssentialStore(storeInstr);
    }
    else if(auto loadInstr = instr->As<LoadInstr>()) {
        // Only volatile loads are considered essential.
        return loadInstr->IsVolatile();
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        return IsEssentialCall(callInstr);
    }

    // All other instructions don't have visible side-effects.
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::IsEssentialCall(CallInstr* instr) {
    // Calls to some intrinsics can be considered to not have side-effects.
    if(auto intrinsic = instr->GetIntrinsic()) {
        return (intrinsic->IsMathIntrinsic() ||
                intrinsic->IsBitwiseIntrinsic()) == false;
    }

    // Calls to pure functions (don't write to memory and their result
    // depends only on the parameters) have no side-effects.
    auto calledFunct = instr->GetCalledFunction();
    
    if(calledFunct == nullptr) {
        return true;
    }

    if(calledFunct->IsNoState() && calledFunct->IsNoWrite()) {
        return false;
    }

    // Try to use language-specific information.
    if(auto info = GetLanguageInfo()) {
        return info->CallMayHaveSideEffects(instr);
    }

    // All other calls may have side-effects.
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::IsEssentialStore(StoreInstr* instr) {
    // If the store is volatile it is essential.
    if(instr->IsVolatile()) {
        return true;
    }

    // We presume that a store to a local variable is dead,
    // unless we find a load from exactly the same variable.
    // A further condition is that the address of the variable should not be
    // taken, it's difficult to track the aliases exactly.
    if(auto baseVariable = GetBaseVariable(instr->DestinationOp())) {
        if(baseVariable->IsAddressTaken()) {
            return true;
        }

        // If we already found a load from this variable give up.
        if(HasLoadFromVariable(baseVariable)) {
            return true;
        }

        AddPotentiallyDeadStore(baseVariable, instr);
        return false;
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::AddPotentiallyDeadStore(VariableReference* baseVariable, 
                                                  StoreInstr* instr) {
    DebugValidator::IsNotNull(baseVariable);
    DebugValidator::IsNotNull(instr);

    if(potentiallyDeadStores_.ContainsKey(baseVariable) == false) {
        potentiallyDeadStores_.Add(baseVariable, StoreList());
    }

    potentiallyDeadStores_[baseVariable].Add(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
VariableReference* DeadCodeElimination::GetBaseVariable(Operand* op) {
    if(auto variableRef = op->As<VariableReference>()) {
        if(variableRef->IsLocalVariableRef()) {
            return variableRef;
        }
    }
    else if(auto definingInstr = op->DefiningInstruction()) {
        if(definingInstr->IsAddress() ||
           definingInstr->IsIndex()   ||
           definingInstr->IsElement() ||
           definingInstr->IsPtop()) {
            return GetBaseVariable(definingInstr->GetSourceOp(0));
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeadCodeElimination::Mask DeadCodeElimination::EstimateDeadBits(Instruction* instr) {
    Mask deadBits = 0;

    // We presume that the constants are always the right operand.
    // This is usually true after peephole optimization.
    switch(instr->GetOpcode()) {
        case Instr_Mul: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                if(IA::IsPowerOfTwo(intConst->Value())) {
                    // Multiplying by a power of two shifts the top bits.
                    // For example, 'a * 16' shifts the topmost 4 bits.
                    Mask shiftetBits = IA::Log2(intConst->Value());
                    deadBits = ((1ULL << shiftetBits) - 1) << 
                               (OperandBits(intConst) - shiftetBits);
                }
            }
            break;
        }
        case Instr_Div:
        case Instr_Udiv: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                if(IA::IsPowerOfTwo(intConst->Value())) {
                    // Division by a power of two shifts the bottom bits.
                    // For example, 'a / 8' shifts the bottom 3 bits.
                    Mask shiftetBits = IA::Log2(intConst->Value());
                    deadBits = (1ULL << shiftetBits) - 1;
                }
            }
            break;
        }
        case Instr_And: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                // The bits that are not set in the constant
                // will be reset, no matter what their value is.
                deadBits = ~intConst->Value();
            }
            break;
        }
        case Instr_Or: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                // The bits that are set in the constant will be set in the result.
                deadBits = intConst->Value();
            }
            break;
        }
        case Instr_Shr:
        case Instr_Ushr: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                // The bottom bits will be shifted away.
                deadBits = (1ULL << intConst->Value()) - 1;
            }
            break;
        }
        case Instr_Shl: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                // The topmost bits will be shifted away.
                deadBits = ((1ULL << intConst->Value()) - 1) <<
                           (OperandBits(intConst) - intConst->Value());
            }
            break;
        }
        case Instr_Trunc: {
            // The top bits are truncated, so their value doesn't matter.
            auto truncInstr = instr->As<TruncInstr>();
            Mask fromBits = OperandBits(truncInstr->TargetOp());
            Mask toBits = TI::GetSizeBits(truncInstr->CastType());
            deadBits = ((1ULL << (fromBits - toBits)) - 1) << toBits;
            break;
        }
    }

    return deadBits;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* DeadCodeElimination::SkipDeadInstruction(Instruction* instr, Mask deadBits) {
    DebugValidator::IsLarger(deadBits, 0);

    if(instr == nullptr) {
        return nullptr;
    }

    // We estimate the bits for some instructions and check
    // if they are among the ones that will be killed.
    switch(instr->GetOpcode()) {
        case Instr_Or: {
            // Check if the right operand doesn't set anything
            // that will not be killed afterwards.
            return SelectOperand(instr, deadBits, &DCE::EstimateSetBits);
        }
        case Instr_And: {
            // Check if the right operand doesn't reset anything
            // that will not be killed afterwards.
            return SelectOperand(instr, deadBits, &DCE::EstimateResetedBits);
        }
        case Instr_Xor: {
            // Check if the right operand doesn't invert anything
            // that will not be killed afterwards.
            return SelectOperand(instr, deadBits, &DCE::EstimateInvertedBits);
        }
        case Instr_Add: {
            // Check if the right operand doesn't add anything
            // that will not be killed afterwards.
            return SelectOperand(instr, deadBits, &DCE::EstimateAddedBits);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* DeadCodeElimination::SelectOperand(Instruction* instr, Mask deadBits,
                                            BitEstimator estimator) {
    DebugValidator::IsNotNull(instr);

    // Check if only the left operand should be saved.
    Mask estimatedBits = (this->*estimator)(instr->GetSourceOp(0), 
                                             instr->GetSourceOp(1));

    if((estimatedBits & deadBits) == estimatedBits) {
        return instr->GetSourceOp(0);
    }

    // Do the check for the right operand.
    estimatedBits = (this->*estimator)(instr->GetSourceOp(1), 
                                       instr->GetSourceOp(0));

    if((estimatedBits & deadBits) == estimatedBits) {
        return instr->GetSourceOp(1);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeadCodeElimination::Mask DeadCodeElimination::EstimateSetBits(Operand* a, Operand* b) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);

    // Estimate the bits that are zero. If we are not sure
    // if they're zero, we are conservative and assume they are.
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());
    Mask zeroBitsA;
    Mask oneBitsA;
    opInfo.EstimateZeroBits(a, zeroBitsA);
    opInfo.EstimateOneBits(a, oneBitsA);
    zeroBitsA = zeroBitsA | ~(zeroBitsA | oneBitsA);

    // Estimate the bits that are set in the other operand.
    // We are conservative and any bit that is not one or zero
    // is considered to be one.
    Mask oneBitsB;
    Mask zeroBitsB;
    opInfo.EstimateOneBits(b, oneBitsB);
    opInfo.EstimateZeroBits(b, zeroBitsB);
    oneBitsB = oneBitsB | ~(oneBitsB | zeroBitsB);

    // Any bit that was zero and after the 'or' is one
    // is considered to be set by the instruction.
    Mask typeLimit = IA::GetMinusOneMask(a->GetType()->As<IntegerType>());
    return (zeroBitsA & oneBitsB) & typeLimit;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeadCodeElimination::Mask DeadCodeElimination::EstimateResetedBits(Operand* a, Operand* b) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    
    // Estimate the bits that are zero. If we are not sure
    // if they're zero, we are conservative and assume they are.
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());
    Mask zeroBitsA;
    Mask oneBitsA;
    opInfo.EstimateZeroBits(a, zeroBitsA);
    opInfo.EstimateOneBits(a, oneBitsA);
    oneBitsA = oneBitsA | ~(zeroBitsA | oneBitsA);

    // Estimate the bits that are set in the other operand.
    // We are conservative and any bit that is not one or zero
    // is considered to be one.
    Mask oneBitsB;
    Mask zeroBitsB;
    opInfo.EstimateOneBits(b, oneBitsB);
    opInfo.EstimateZeroBits(b, zeroBitsB);
    zeroBitsB = zeroBitsB | ~(oneBitsB | zeroBitsB);

    // Any bit that was zero and after the 'or' is one
    // is considered to be set by the instruction.
    Mask typeLimit = IA::GetMinusOneMask(a->GetType()->As<IntegerType>());
    return (oneBitsA & zeroBitsB) & typeLimit;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeadCodeElimination::Mask DeadCodeElimination::EstimateInvertedBits(Operand* a, Operand* b) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);

    // Estimate the bits that are not set in operand 'b'.
    // Then loop over all bits and decide if it's possible
    // that they change. For example, if we know that a bit in 'b'
    // is definitely, the bit doesn't change after the 'xor'.
    Mask zeroBitsB;
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());
    opInfo.EstimateZeroBits(b, zeroBitsB);

    Mask typeLimit = IA::GetMinusOneMask(a->GetType()->As<IntegerType>());
    return ~zeroBitsB & typeLimit;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeadCodeElimination::Mask DeadCodeElimination::EstimateAddedBits(Operand* a, Operand* b) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);

    // Estimate the bits that are added. Any bit that is not
    // definitely zero is presumed to be one.
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());
    Mask zeroBitsB;
    Mask oneBitsB;
    opInfo.EstimateZeroBits(b, zeroBitsB);
    opInfo.EstimateOneBits(b, oneBitsB);
    oneBitsB = oneBitsB | ~(zeroBitsB | oneBitsB);

    // Estimate the bits that are zero in 'a'.
    Mask zeroBitsA;
    opInfo.EstimateZeroBits(a, zeroBitsA);
    Mask typeLimit = IA::GetMinusOneMask(a->GetType()->As<IntegerType>());

    if(((oneBitsB & zeroBitsA) & typeLimit) != (oneBitsB & typeLimit)) {
        // Not all bits set by 'b' are zero in 'a',
        // so we return a conservative result that will prevent
        // any (potential) invalid optimization.
        return typeLimit;
    }
        
    return (oneBitsB & zeroBitsA) & typeLimit;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::InstructionEliminated(Instruction* instr) {
#if 1
	auto block = instr->ParentBlock();
	auto function = instr->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string beforeText = IRPrinter(instr).ToString();
	Log::Warning("DCE in " + functionName + ":" + blockName + ": " +  beforeText);
    //IRPrinter(function).Dump();
#endif
}

} // namespace Optimization