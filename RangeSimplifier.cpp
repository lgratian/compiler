// RangeSimplifier.cpp
// Copyright (c) Lup Gratian
//
// Implements the Range Simplifier pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "RangeSimplifier.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

void RangeSimplifier::Execute(Function* function) {
    DebugValidator::IsNotNull(function);

    opInfo_ = OperandInfo(function->ParentUnit(), GetTarget());
    irGen_ = IRGenerator(function->ParentUnit());
    folder_ = ConstantFolder(&irGen_, GetTarget());
    
    // Process all instructions in the function.
    auto instrEnum = function->GetInstructionEnum();

    while(instrEnum.IsValid()) {
        ProcessInstruction(instrEnum.Next());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RangeSimplifier::ProcessInstruction(Instruction* instr) {
    // First we try to replace operands by a constant,
    // then to re-evaluate an instruction that is an operand.
    // If there was at least one replaced operand then we try
    // to constant-fold or simplify the "new" instruction.
    bool replaced = ReplaceSourcesByConstants(instr);
    replaced |= ReevaluateSources(instr);
    replaced |= ReplaceWithIdentical(instr);
    replaced |= ReplaceIncomingValues(instr);

    // If no replacement was done check if we have a pointer comparison
    // used by an 'if' - we might be able to determine the taken branch.
    if((replaced == false) && instr->IsIf() &&
       instr->GetSourceOp(0)->DefiningInstrIs<UcmpInstr>()) {
        auto cmpInstr = instr->GetSourceOp(0)->DefiningInstrAs<UcmpInstr>();

        if(cmpInstr->LeftOp()->IsPointer()) {
            if(auto result = ReevaluatePointerComparison(cmpInstr, instr->ParentBlock())) {
                instr->ReplaceSourceOp(0, result);
                replaced = true;
            }
        }
    }

    if(replaced) {
        InstructionSimplified(instr);

        if(instr->HasDestinationOp()) {
            if(auto result = SimplifyInstruction(instr)) {
                instr->GetDestinationOp()->ReplaceWith(result);
                instr->RemoveFromBlock(true /* free */);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeSimplifier::ReplaceSourcesByConstants(Instruction* instr) {
    // If the instruction doesn't target integer
    // values we have nothing to do.
    if(instr->IsGoto()) {
        return false;
    }
    else if(instr->SourceOpCount() == 0) {
        return false;
    }

    // Try to replace each of the source operands.
    auto block = instr->ParentBlock();
    bool replaced = false;

    // For 'if' and 'switch' we consider only the first operand.
    int opCount = instr->IsBranching() ? std::min(1, instr->SourceOpCount()) :
                                         instr->SourceOpCount();

    for(int i = 0; i < opCount; i++) {
        auto sourceOp = instr->GetSourceOp(i);

        if(sourceOp->IsInteger() == false) {
            return false;
        }
        else if(sourceOp->IsConstant()) {
            continue;
        }

        if(auto intConst = opInfo_.GetIntConstant(sourceOp, block)) {
            instr->ReplaceSourceOp(i, intConst);
            replaced = true;
        }
    }

    return replaced;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* RangeSimplifier::ReevaluatePointerComparison(UcmpInstr* instr, Block* testBlock) {
    // This handles that case when we have a comparison like 'cmp neq p, nullptr'
    // and now we know that 'p' is definitely not null.
    if((instr->IsEqual() || instr->IsNotEqual()) == false) {
        return nullptr;
    }
    
    Operand* otherOp = nullptr;
    Operand* nullOp = nullptr;

    if(opInfo_.IsPointerNull(instr->LeftOp(), testBlock)) {
        nullOp = instr->LeftOp();
        otherOp = instr->RightOp();
    }
    else if(opInfo_.IsPointerNull(instr->RightOp(), testBlock)) {
        nullOp = instr->RightOp();
        otherOp = instr->LeftOp();
    }
    else return nullptr;

    // Check what we know about the other operand.
    if(opInfo_.IsPointerNotNull(otherOp, testBlock)) {
        // cmp eq notNull, nullptr -> 0
        // cmp neq notNull, nullptr -> 1
        return GetBool(instr->IsNotEqual(), instr->GetDestinationOp());
    }
    else if(opInfo_.IsPointerNull(otherOp, testBlock)) {
        // cmp eq nullptr, nullptr -> 1
        // cmp neq nullptr, nullptr -> 0
        return GetBool(instr->IsEqual(), instr->GetDestinationOp());
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeSimplifier::ReevaluateSources(Instruction* instr) {
    // This tries to re-evaluate the source operands, in the hope 
    // that now they can be simplified using range information.
    // t = a + b;
    // if(a == 3) {
    //    if(b == 5) ... t2 = t + 10;  ->  t2 = (3 + 5) + 10
    if(instr->IsGoto()) {
        return false;
    }

    auto block = instr->ParentBlock();
    bool replaced = false;

    // For 'if' and 'switch' we consider only the first operand.
    int opCount = instr->IsBranching() ? std::min(1, instr->SourceOpCount()) :
                                         instr->SourceOpCount();

    for(int i = 0; i < opCount; i++) {
        auto definingInstr = instr->GetSourceOp(i)->DefiningInstruction();
        if(definingInstr == nullptr) {
            continue;
        }

        // Candidates are arithmetic, logical, comparison and address instructions.
        if((definingInstr->IsArithmetic() || definingInstr->IsLogical() ||
            definingInstr->IsComparison() || definingInstr->IsAddress()) == false) {
            continue;
        }

        // Check if at least one of the operand is now a constant.
        // If it is we try to constant-fold the "new" instruction.
        Operand* constOps[2] = {nullptr, nullptr};
        constOps[0] = GetConstantForSource(definingInstr, 0, block);
        constOps[1] = GetConstantForSource(definingInstr, 1, block);

        if(constOps[0] || constOps[1]) {
            // There is an opportunity for simplification.
            Operand* op1 = constOps[0] ? constOps[0] : 
                                         definingInstr->GetSourceOp(0);
            Operand* op2 = constOps[1] ? constOps[1] : 
                                         definingInstr->GetSourceOp(1);
            Operand* result = nullptr;

            // First try to constant-fold.
            if(definingInstr->IsAddress()) {
                result = folder_.FoldAddress(op1, op2);
            }
            else if(auto cmpInstr = definingInstr->As<CmpInstrBase>()) {
                result = folder_.FoldCompare(cmpInstr->GetOpcode(), op1, op2,
                                             cmpInstr->Order());
            }
            else result = folder_.FoldBinary(definingInstr->GetOpcode(), op1, op2);

            if(result == nullptr) {
                // Try to simplify, but only if we're sure that we 
                // don't lengthen the live range of the operand that
                // is not a constant.
                Operand* notConst = op1->IsConstant() ? op2 : op1;

                if((notConst->IsTemporary() == false) ||
                    notConst->HasSingleUser()) {
                    auto unit = block->ParentFunction()->ParentUnit();
                    result = peephole_.SimplifyInstruction(definingInstr->GetOpcode(),
                                                           op1, op2, unit);
                }
                                            
            }

            if(result) {
                // Replace the source operand by the new one.
                instr->ReplaceSourceOp(i, result);
                replaced = true;
            }
        }
    }

    return replaced;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeSimplifier::ReplaceIncomingValues(Instruction* instr) {
    // Try to replace the incoming operands of a 'phi' instructions
    // by constants. For example, 'phi t1, -1' -> 'phi 2, -1'.
    // This reduces register pressure and might enable other optimizations.
    auto phiInstr = instr->As<PhiInstr>();

    if(phiInstr == nullptr) {
        return false;
    }

    auto block = instr->ParentBlock();
    bool replaced = false;

    for(int i = 0; i < phiInstr->OperandCount(); i++) {
        auto incomingOp = phiInstr->GetOperand(i);

        // If the incoming operand is already a constant we skip it.
        if(incomingOp->IsConstant()) {
            continue;
        }

        // Check if the incoming operand is known to be a constant.
        if(auto constOp = GetConstantForOperand(incomingOp, block)) {
            phiInstr->ReplaceOperand(i, constOp);
            replaced = true;
        }
        else {
            // We might know that the incoming operand is a constant
            // even though this information is not valid for the entire block
            // (it's valid only when coming from the associated predecessor).
            auto incomingBlock = phiInstr->GetOperandBlock(i)->Target();
            auto rangeTag = incomingBlock->GetTag<RangeTag>();

            if(rangeTag == nullptr) {
                continue;
            }

            Range range;

            if(rangeTag->GetRangeForSuccessor(incomingOp, block, range)) {
                // Check if the found range is a constant.
                if(auto intConst = GetIncomingConstant(incomingOp, incomingBlock, block)) {
                    phiInstr->ReplaceOperand(i, intConst);
                    replaced = true;
                }
            }
        }
    }

    return replaced;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IntConstant* RangeSimplifier::GetIncomingConstant(Operand* incomingOp, 
                                                  Block* incomingBlock, Block* block) {
    // Check if from 'incomingBlock' we have a value that is a constant
    // according to the range information.
    auto rangeTag = incomingBlock->GetTag<RangeTag>();
    
    if(rangeTag == nullptr) {
        return nullptr;
    }

    Range range;

    if(rangeTag->GetRangeForSuccessor(incomingOp, block, range)) {
        if(range.IsRange && range.IsSingleValue() &&
           (range.HasBaseOperand() == false)) {
            return GetIntConstant(range.Low.Constant, incomingOp, block);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IntConstant* RangeSimplifier::GetIntConstant(__int64 value, Operand* op, Block* block) {
    auto unit = block->ParentFunction()->ParentUnit();
    auto type = op->GetType()->IsPointer() ?
                IntegerType::GetInt32() : op->GetType();
    
    return unit->Constants().GetInt(type, value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* RangeSimplifier::GetConstantForSource(Instruction* instr, 
                                               int sourceIndex, 
                                               Block* testBlock) {
    auto op = instr->GetSourceOp(sourceIndex);
    return GetConstantForOperand(op, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* RangeSimplifier::GetConstantForOperand(Operand* op, Block* testBlock) {
    auto unit = testBlock->ParentFunction()->ParentUnit();

    if(op->IsConstant() == false) {
        if(op->IsPointer()) {
            // Check if we know that the pointer is NULL.
            if(opInfo_.IsPointerNull(op, testBlock)) {
                return unit->Constants().GetUndefined(op->GetType());
            }
        }
        else if(auto intConst = opInfo_.GetIntConstant(op, testBlock)) {
            return intConst;
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* RangeSimplifier::SimplifyInstruction(Instruction* instr) {
    if(instr->HasDestinationOp() == false) {
        return false;
    }

    // First try to constant-fold.
    if(auto result = folder_.Fold(instr)) {
        return result;
    }
    
    // Try to simplify the instruction otherwise.
    if(auto result = peephole_.SimplifyInstruction(instr, true)) {
        return result;
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeSimplifier::ReplaceWithIdentical(Instruction* instr) {
    // Try to replace an operand by one that is identical,
    // if this enables some simplifications or if it may 
    // reduce register pressure. For example,
    // if(a == b) t = a - b  ->  t = a - a
    auto block = instr->ParentBlock();
    bool replaced = false;

    // We do this only for binary instructions.
    if(instr->SourceOpCount() != 2) {
        return false;
    }

    for(int i = 0; i < instr->SourceOpCount(); i++) {
        auto sourceOp = instr->GetSourceOp(i);
        if(sourceOp->IsConstant()) {
            continue;
        }

        // Try to get the range for the operand.
        Range range;

        if(opInfo_.GetRange(sourceOp, block, range) == false) {
            // We don't have a known range.
            continue;
        }

        // The range should be symbolic and express equality.
        if(IsSymbolicEquality(range)) {
            auto otherOp = instr->GetSourceOp(1 - i);
            auto baseOp = range.Low.Base;

            if(baseOp == otherOp) {
                // Same operands, may enable some simplifications
                // if we have an arithmetic/logical instruction.
                instr->ReplaceSourceOp(i, baseOp);
                replaced = true;
                break;
            }
            else {
                int baseLevel;
                int sourceLevel;
                IsUsedInBlockOrSuccessors(baseOp, block, 0, baseLevel);
                IsUsedInBlockOrSuccessors(sourceOp, block, 0, sourceLevel);

                if(baseLevel > sourceLevel) {
                    // Register pressure may be reduced.
                    instr->ReplaceSourceOp(i, baseOp);
                    replaced = true;
                    break;
                }
            }
        }
    }

    return replaced;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeSimplifier::IsUsedInBlockOrSuccessors(Operand* op, Block* block, 
                                                int level, int& maxUserLevel) {
    // If this is the first level initialize the maximum level.
    if(level == 0) {
        maxUserLevel = -1;
    }

    // Check if the operand is used in the block, then
    // check if it's used in any of it's successors.
    if(IsUsedInBlock(op, block) && (level > maxUserLevel)) {
        maxUserLevel = level;
    }

    if(level < GetMaximumScanLevel()) {
        for(int i = 0; i < block->SuccessorCount(); i++) {
            IsUsedInBlockOrSuccessors(op, block->SuccessorAt(i),
                                      level + 1, maxUserLevel);
        }
    }

    return maxUserLevel != -1;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RangeSimplifier::IsUsedInBlock(Operand* op, Block* block) {
    // Scan all instructions in the block and check if at least
    // one of the uses the operand as a source.
    for(auto instr = block->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
        for(int i = 0; i < instr->SourceOpCount(); i++) {
            if(instr->GetSourceOp(i) == op) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RangeSimplifier::InstructionSimplified(Instruction* instr) {
#if 1
	auto block = instr->ParentBlock();
	auto function = instr->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string text = IRPrinter(instr).ToString();
	Log::Warning("RangeSimplifier in " + functionName + ":" + blockName +
				 ": " + text);
#endif
}

}// namespace Optimization