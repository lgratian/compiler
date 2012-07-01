// SafetyInfo.cpp
// Copyright (c) Lup Gratian
//
// Implements the SafetyInfo class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "SafetyInfo.hpp"

namespace Analysis {

bool SafetyInfo::IsDefinitelyDead(Instruction* instr) {
    // An instruction is dead if its result is not used by any other instruction.
    // We need to take care of 'store', 'call' and volatile 'load' though.
    if(auto loadInstr = instr->As<LoadInstr>()) {
        // If it's marked as 'volatile' we can't eliminate it.
        if(loadInstr->IsVolatile()) {
            return false;
        }

        return (loadInstr->ResultOp() == nullptr) ||
               (loadInstr->ResultOp()->HasNoUser());
    }
    else if(auto storeInstr = instr->As<StoreInstr>()) {
        // We don't know where we write the result, so be conservative.
        // An exception is when we store an undefined value.
        if(storeInstr->IsVolatile()) {
            return false;
        }
        return storeInstr->SourceOp()->IsUndefinedConstant();
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        if(callInstr->ResultOp() && callInstr->ResultOp()->HasUsers()) {
            return false;
        }

        // A special case are intrinsics, for which we may know that they
        // don't have any side-effects.
        if(auto intrinsic = callInstr->GetIntrinsic()) {
            if(intrinsic->IsMathIntrinsic() || intrinsic->IsBitwiseIntrinsic()) {
                // min, max, sin, bswap, etc.
                return true;
            }
            else if(intrinsic->IsBoundsCheckIntrinsic()) {
                // A bounds checking from which we determined that the access
                // is always in range can be safely removed.
                return callInstr->GetArgument(0)->IsOneInt();
            }
        }

        // We need to presume that the 'call' has side-effects.
        return false;
    }
    else if(instr->IsBranching()) {
        // We presume that branching instructions are not dead.
        return false;
    }
    
    // For all other cases we check if the result is used by any other instruction.
    if(instr->HasDestinationOp() == false) {
        return true;
    }

    return instr->GetDestinationOp()->HasNoUser();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::CanBeSpeculated(Instruction* instr, Block* toBlock) {
    DebugValidator::IsNotNull(instr);

    // An instruction can safely be executed speculatively
    // if it doesn't have any visible side-effect on the program.
    // Most arithmetic and all logical instructions match this condition.
    if(instr->IsLogical()) {
        return true;
    }
    else if(instr->IsArithmetic()) {
        // Don't try to speculate some floating point instructions, 
        // they are expensive and it may not be worth it.
        if(instr->IsFloatArithmetic()) {
            auto arithInstr = instr->As<ArithmeticInstr>();
            
            // We can speculate only if the user indicates that
            // floating-point exceptions can be ignored.
            if(arithInstr->IsFast()) {
                return instr->IsFadd() || 
                       instr->IsFsub() || 
                       instr->IsFmul();
            }
        }

        switch(instr->GetOpcode()) {
            case Instr_Div:
            case Instr_Udiv:
            case Instr_Mod:
            case Instr_Umod: {
                // We can speculate div/mod only if we are certain that
                // the operation cannot trap (right operand is zero).
                // This can't happen if we have a constant that is not zero,
                // or if we know that at least one bit is set in the right operand.
                if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                    return intConst->IsZero() == false;
                }
                //! TODO: OPERAND-INFO!!!
                auto unit = instr->ParentBlock()->ParentFunction()->ParentUnit();
                OperandInfo opInfo(unit);
                return opInfo.IsNotZero(instr->GetSourceOp(1), toBlock);
            }
            default: {
                // All other arithmetic instruction can be speculated.
                return true;
            }
        }
    }
    else if(instr->IsComparison() ||
            instr->IsConversion() ||
            instr->IsAddressing()) {
        return true;
    }
    else if(instr->IsControl()) {
        if(instr->IsIf()) {
            return true;
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            // Some intrinsics can be speculated.
            auto intrinsic = callInstr->GetIntrinsic();

            if(intrinsic == nullptr) {
                return false;
            }

            // Allow only cheap intrinsics to be speculated.
            if(auto mathIntrinsic = intrinsic->As<MathIntrinsic>()) {
                return mathIntrinsic->IsMin() ||
                       mathIntrinsic->IsMax() || 
                       mathIntrinsic->IsAbs();
            }
            else return intrinsic->IsBitwiseIntrinsic();
        }
    }
    else if(instr->IsQuestion()) {
        return true;
    }
    else if(auto loadInstr = instr->As<LoadInstr>()) {
        // Volatile loads should not be touched.
        if(loadInstr->IsVolatile()) {
            return false;
        }

        // Check if we know that this 'load' can't trap.
        // This happens when there is a 'load' or 'store' from/to
        // the same address in a block that dominates this one
        // (if there would have been an exception control 
        //  wouldn't have reached this point).
        auto unit = instr->ParentBlock()->ParentFunction()->ParentUnit();
        return OperandInfo(unit).IsPointerNotNull(loadInstr->SourceOp(), toBlock);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::DefinitionDominatesBlock(Operand* op, Block* block) {
    DebugValidator::IsNotNull(block);
    DebugValidator::IsNotNull(op);

    // This is intended to work even if the dominator tree is not available,
    // but then, of course, in a much more limited fashion.
    // Constants and parameters dominate all blocks.
    if(op->IsConstant() || op->IsParameter()) {
        return true;
    }

    // Variables and functions dominate all blocks too.
    if(op->IsVariableReference() || op->IsFunctionReference()) {
        return true;
    }

    // Now check for temporaries.
    if(auto temp = op->As<Temporary>()) {
        auto definingInstr = temp->DefiningInstruction();
        return Dominates(definingInstr->ParentBlock(), block);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::Dominates(Block* a, Block* b) {
    // A block dominates itself.
    if(a == b) {
        return true;
    }

    // The entry b dominates all other ones.
    if(a->IsFunctionEntry()) {
        return true;
    }

    // Try to use the dominator tree.
    if(domTree_) {
        return domTree_->Dominates(a, b);
    }

    // If no dominator tree is available try to handle the common cases
    // of 'if-then' and 'if-then-else' patterns.
    if(b->HasSinglePredecessor()) {
        return b->PredecessorAt(0) == a;
    }
    else if(b->PredecessorCount() == 2) {
        if(a == b->PredecessorAt(0)) {
            // We may have 'if-then'.
            auto otherPred = b->PredecessorAt(1);
            return otherPred->HasSinglePredecessor() &&
                    (otherPred->PredecessorAt(0) == a);
        }
        else if(a == b->PredecessorAt(1)) {
            // We may have 'if-then'.
            auto otherPred = b->PredecessorAt(0);
            return otherPred->HasSinglePredecessor() &&
                   (otherPred->PredecessorAt(0) == a);
        }

        // Check if we have the diamond-shape pattern that is specific for
        // 'if-then-else' (both predecessors should have the same predecessor).
        auto pred1 = b->PredecessorAt(0);
        auto pred2 = b->PredecessorAt(1);

        if((pred1->PredecessorCount() != 1) ||
           (pred2->PredecessorCount() != 1)) {
           return false;
        }
            
        auto pred1Pred = pred1->PredecessorAt(0);
        auto pred2Pred = pred2->PredecessorAt(0);
        return (pred1Pred == pred2Pred) && (pred1Pred == a);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::OperandsDominateBlock(Instruction* instr, Block* block) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(block);

    for(int i = 0; i < instr->SourceOpCount(); i++) {
        if(DefinitionDominatesBlock(instr->GetSourceOp(i), block) == false) {
            // Give up, no reason to continue.
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::CanBeSpeculatedInBlock(Instruction* instr, Block* block) {
    // An instruction can be executed speculatively in a different block
    // if it can be speculated and if the definition points of all source
    // operands dominate the block. This guarantees that all source operands 
    // have been evaluated when the block is reached.
    if(CanBeSpeculated(instr) == false) {
        return false;
    }
    
    return OperandsDominateBlock(instr, block);
}

} // namespace Analysis