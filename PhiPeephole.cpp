// PhiPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 'phi' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

Operand* Peephole::HandlePhi(PhiInstr* instr) {
    // Try to constant fold.
    // phi(C, C, ..., C) -> C
    if(auto result = folder_.Fold(instr)) {
        return LOG(result);
    }

    // phi(a, a, a, a) -> a
    if(instr->SameOperands()) {
        return LOG(instr->GetOperand(0));
    }

    // phi(a, undef, b, undef) -> phi(a, b)
    // The undefined operands can be safely ignored.
    if(auto result = HandlePhiWithUndef(instr)) {
        return result;
    }

    if(auto result = HandlePhiOnLoad(instr)) {
        return result;
    }
    
    if(auto result = HandlePhiOnInstructions(instr)) {
        return result;
    }

    if(HandlePhiCopyCycle(instr)) {
        // 'HandlePhiCopyCycle' handles the replacing itself.
        return LOG(nullptr);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::HandlePhiCopyCycle(PhiInstr* instr) {
    // t1 = phi(t1, a, t1, a, t1) -> a, if 'a' dominates the 'phi'
    // This sometimes happens for loops that contain variable copies.
    // The condition that 'a' dominates the 'phi' assures us
    // that its value is always available, because it dominates the loop header.
    Operand* diffOp = nullptr;
    auto resultOp = instr->ResultOp();

    for(int i = 0; i < instr->OperandCount(); i++) {
        auto op = instr->GetOperand(i);

        if(op == resultOp) continue;
        else if(diffOp == nullptr) diffOp = op;
        else if(op == diffOp) continue;
        else return false;
    }

    if(diffOp) {
        // Because we may not have the dominator tree available
        // we make some conservative estimates.
        if(GetSafetyInfo()->DefinitionDominatesBlock(diffOp, instr->ParentBlock())) {
            instr->ResultOp()->ReplaceWith(diffOp);
            instr->RemoveFromBlock(true);
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandlePhiWithUndef(PhiInstr* instr) {
    // phi(a, undef, b, undef) -> phi(a, b)
    // The undefined operands can be safely ignored.
    // Check if we have undefined operands.
    bool hasUndef = false;

    for(int i = 0; i < instr->OperandCount(); i++) {
        if(instr->GetOperand(i)->IsUndefinedConstant()) {
            hasUndef = true;
            break;
        }
    }

    if(hasUndef) {
        // Create a new 'phi' that excludes the undefined operands.
        auto phiOp = GetTemporary(instr->ResultOp());
        auto phiInstr = irGen_.GetPhi(phiOp, instr->OperandCount() - 1);

        for(int i = 0; i < instr->OperandCount(); i++) {
            auto op = instr->GetOperand(i);

            if(op->IsUndefinedConstant() == false) {
                phiInstr->AddOperand(op, instr->GetOperandBlock(i));
            }
        }

        return phiOp;
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandlePhiOnLoad(PhiInstr* instr) {
    // B1: t1 = load p
    //     goto B3
    // B2: t2 = load q
    //     goto B3
    // B3: phi(t1, t2)  ->  load phi(p, q)
    // This simplification reduces code size, but also may reduce
    // register pressure. Note that it is safe only if the blocks 
    // containing the 'load' instructions are predecessors of the one 
    // with the 'phi', and if there is no 'store' until the branch is reached.
    if((instr->OperandCount() < 2) || (instr->OperandCount() > 32)) {
        return nullptr;
    }

    // Check if all incoming operands are from 'load' instructions.
    auto phiBlock = instr->ParentBlock();
    StaticList<LoadInstr*, 32> loads;

    for(int i = 0; i < instr->OperandCount(); i++) {
        auto incomingOp = instr->GetOperand(i);

        if(auto loadInstr = incomingOp->DefiningInstrAs<LoadInstr>()) {
            // The 'load' should have a single user (i.e it's only by the 'phi').
            if(loadInstr->ResultOp()->HasSingleUser()) {
                loads.Add(loadInstr);
            }
            else return nullptr;
        }
        else if(incomingOp->IsUndefinedConstant()) {
            // Undefined operands can be ignored.
        }
        else return nullptr; // No reason to continue.
    }

    // Nothing to do if we have a single 'load'.
    if(loads.Count() < 2) {
        return nullptr;
    }

    // Check the conditions stated above.
    for(int i = 0; i < loads.Count(); i++) {
        auto loadBlock = loads[i]->ParentBlock();
        if(phiBlock->HasPredecessor(loadBlock) == false) {
           return nullptr;
        }
        else if(loads[i]->NextInstruction() != loadBlock->LastInstruction()) {
            return nullptr;
        }
    }

    // Create a 'phi' that merges the source operands,
    // then the new 'load'; the source operands of all other loads
    // are changed to 'undef' so that these loads will be later removed.
    auto phiOp = GetTemporary(loads[0]->SourceOp());
    auto newPhiInstr = irGen_.GetPhi(phiOp, loads.Count());

    for(int i = 0; i < loads.Count(); i++) {
        auto blockRef = irGen_.GetBlockRef(loads[i]->ParentBlock());
        newPhiInstr->AddOperand(loads[i]->SourceOp(), blockRef);
        loads[i]->SetSourceOp(GetUndefined(loads[i]->SourceOp()));
    }

    // Make sure that the 'load' is placed after all 'phi's.
    Instruction* insertionPoint = instr->ParentBlock()->FirstNonPhi();
    insertionPoint = insertionPoint->PreviousInstruction();
    DebugValidator::IsNotNull(insertionPoint);

    auto previousInsertionPoint = irGen_.GetInsertionPoint();
    irGen_.SetInsertionPoint(insertionPoint);
    
    auto loadOp = GetTemporary(instr->ResultOp());
    irGen_.GetLoad(phiOp, loadOp);
    irGen_.SetInsertionPoint(previousInsertionPoint);
    return LOG(loadOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandlePhiOnInstructions(PhiInstr* instr) {
    // Check if the 'phi' "combines" nearly identical instructions
    // (same type, one of the operands the same). This applies to arithmetic,
    // logical, conversion and address instructions. For example, 
    // B1: t1 = mul a, 12
    //     goto B3
    // B2: t2 = mul b, 12
    //     goto B3
    // B2: phi(t1, t2)   ->  mul phi(a,b), 12
    if((instr->OperandCount() < 2) || (instr->OperandCount() > 32)) {
        return nullptr;
    }
    
    // Try to make a list of instructions of the same type.
    InstructionList instrs;
    BlockList incomingBlocks;
    Opcode lastOpcode;

    if(CollectPhiInstructionOps(instr, instrs, incomingBlocks,
                                lastOpcode) == false) {
        return nullptr;
    }

    // If we have conversion instructions we can do the simplification.
    // For all other types we need check if all instructions have
    // at least one operand in common.
    if(Instruction::IsConversion(lastOpcode)) {
        auto firstInstr = instrs[0]->As<ConversionInstr>();
        auto phiOp = GetTemporary(firstInstr->TargetOp());
        auto phiInstr = irGen_.GetPhi(phiOp, instrs.Count());

        for(int i = 0; i < instrs.Count(); i++) {
            auto blockRef = irGen_.GetBlockRef(incomingBlocks[i]);
            phiInstr->AddOperand(instrs[i]->GetSourceOp(0), blockRef);
        }

        // Note that we need to insert the instruction after all other 'phi's.
        auto previousInsertionPoint = irGen_.GetInsertionPoint();
        auto insertionPoint = phiInstr->ParentBlock()->FirstNonPhi();

        insertionPoint = insertionPoint->PreviousInstruction();
        DebugValidator::IsNotNull(insertionPoint);
        irGen_.SetInsertionPoint(insertionPoint);

        auto resultOp = irGen_.GetTemporary(firstInstr->CastType());
        irGen_.GetConversion(lastOpcode, phiOp, firstInstr->CastType(), resultOp);
        irGen_.SetInsertionPoint(previousInsertionPoint);
        return LOG(resultOp);
    }

    // Check for the common operand.
    OperandList diffOps;
    bool commonOnRight = true;
    Operand* commonOp = GetCommonOperand(instrs, diffOps, 
                                         lastOpcode, commonOnRight);
    if(commonOp == nullptr) {
        return nullptr;
    }

    // Try to determine a common type by looking at the first
    // two instructions. If a type was found, check the rest too.
    for(int i = 2; i < instrs.Count(); i++) {
        // Check if it has the same common operand.
        // If it's a commutative instruction it doesn't matter the side.
        if(Instruction::IsCommutative(lastOpcode)) {
            if(instrs[i]->GetSourceOp(0) == commonOp) {
                diffOps.Add(instrs[i]->GetSourceOp(1));
            }
            else if(instrs[i]->GetSourceOp(1) == commonOp) {
                diffOps.Add(instrs[i]->GetSourceOp(0));
            }
            else return nullptr;
        }
        else if(commonOnRight) {
            if(instrs[i]->GetSourceOp(1) == commonOp) {
                // We have a common operator.
                diffOps.Add(instrs[i]->GetSourceOp(0));
            }
            else return nullptr;
        }
        else if(instrs[i]->GetSourceOp(0) == commonOp) {
                // We have a common operator.
                diffOps.Add(instrs[i]->GetSourceOp(1));
        }
        else return nullptr;
    }

    // We know that all instructions have a common operands.
    // Create a 'phi' that merges the other operands, then the appropriate
    // instruction that applies the common operand on the 'phi'.
    auto previousInsertionPoint = irGen_.GetInsertionPoint();
    irGen_.SetInsertionPoint(instr->ParentBlock()->FirstInstruction());

    auto phiOp = GetTemporary(diffOps[0]);
    auto phiInstr = irGen_.GetPhi(phiOp, diffOps.Count());

    for(int i = 0; i < diffOps.Count(); i++) {
        auto blockRef = irGen_.GetBlockRef(incomingBlocks[i]);
        phiInstr->AddOperand(diffOps[i], blockRef);
    }

    // Note that we need to insert the instruction after all other 'phi's.
    Instruction* insertionPoint = phiInstr->ParentBlock()->FirstNonPhi();
    insertionPoint = insertionPoint->PreviousInstruction();
    DebugValidator::IsNotNull(insertionPoint);

    if(commonOnRight) {
        return LOG(CreateBinaryInstruction(lastOpcode, phiOp, commonOp,
                                           phiOp->GetType(), insertionPoint, 
                                           previousInsertionPoint));
    }
    else return LOG(CreateBinaryInstruction(lastOpcode, commonOp, phiOp,
                                            phiOp->GetType(), insertionPoint, 
                                            previousInsertionPoint));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::CollectPhiInstructionOps(PhiInstr* instr, InstructionList& instrs,
                                        BlockList& incomingBlocks, Opcode& lastOpcode) {
    for(int i = 0; i < instr->OperandCount(); i++) {
        auto op = instr->GetOperand(i);

        if(auto definingInstr = op->DefiningInstruction()) {
            if(definingInstr->IsArithmetic() || 
               definingInstr->IsLogical()    ||
               definingInstr->IsConversion()) {
                // This is a valid instruction. Now check that its type
                // is exactly the type of the previous instruction.
                Opcode opcode = definingInstr->GetOpcode();

                if((i > 0) && (opcode != lastOpcode)) {
                    return false;
                }
                else lastOpcode = opcode;

                // Add the instruction to the list and test the next operand.
                instrs.Add(definingInstr);
                incomingBlocks.Add(instr->GetOperandBlock(i)->Target());
                continue;
            }
        }
        
        return false;
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::GetCommonOperand(InstructionList& instrs, OperandList& diffOps,
                                    Opcode lastOpcode, bool& commonOnRight) {
    // Try to determine the common operand; 
    // the other ones are added to the list. Note that this also works for cases
    // like 't1 = add a, b' and 't2 = add b, c', considering 'b' the common operand.
    if(Instruction::IsCommutative(lastOpcode)) {
        if(instrs[0]->GetSourceOp(0) == instrs[1]->GetSourceOp(0)) {
            diffOps.Add(instrs[0]->GetSourceOp(1));
            diffOps.Add(instrs[1]->GetSourceOp(1));
            return instrs[0]->GetSourceOp(0);
        }
        else if(instrs[0]->GetSourceOp(0) == instrs[1]->GetSourceOp(1)) {
            diffOps.Add(instrs[0]->GetSourceOp(1));
            diffOps.Add(instrs[1]->GetSourceOp(0));
            return instrs[0]->GetSourceOp(0);
        }
        else if(instrs[0]->GetSourceOp(1) == instrs[1]->GetSourceOp(1)) {
            diffOps.Add(instrs[0]->GetSourceOp(0));
            diffOps.Add(instrs[1]->GetSourceOp(0));
            return instrs[0]->GetSourceOp(1);
        }
        else if(instrs[0]->GetSourceOp(1) == instrs[1]->GetSourceOp(0)) {
            diffOps.Add(instrs[0]->GetSourceOp(0));
            diffOps.Add(instrs[1]->GetSourceOp(1));
            return instrs[0]->GetSourceOp(1);
        }
        else return nullptr;
    }

    // For operators that are not commutative we need to remember
    // on which side we saw the common operand.
    if(instrs[0]->GetSourceOp(0) == instrs[1]->GetSourceOp(0) &&
       (lastOpcode != Instr_Element)) {
        diffOps.Add(instrs[0]->GetSourceOp(1));
        diffOps.Add(instrs[1]->GetSourceOp(1));
        commonOnRight = false;
        return instrs[0]->GetSourceOp(0);
    }
    else if(instrs[0]->GetSourceOp(1) == instrs[1]->GetSourceOp(1)) {
        diffOps.Add(instrs[0]->GetSourceOp(0));
        diffOps.Add(instrs[1]->GetSourceOp(0));
        commonOnRight = true;
        return instrs[0]->GetSourceOp(1);
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleInstructionOnPhi(Opcode opcode, Operand* opA, Operand* opB) {
    DebugValidator::IsTrue(Instruction::IsArithmetic(opcode) ||
                           Instruction::IsLogical(opcode));
    // add phi(2,3), 5 -> phi (7,8)
    // Make sure that we have a 'phi' as one of the operands.
    // For commutative operators it doesn't matter on which side it is.
    // Note that we're not allowed to have a 'phi' on both sides.
    PhiInstr* phiInstr = nullptr;
    Operand* otherOp = nullptr;

    if(auto instr = opA->DefiningInstrAs<PhiInstr>()) {
        phiInstr = instr;
    }
    else if(Instruction::IsCommutative(opcode) == false) {
        return nullptr;
    }
    else otherOp = opA;

    if(auto instr = opB->DefiningInstrAs<PhiInstr>()) {
        if(phiInstr) return nullptr;
        else phiInstr = instr;
    }
    else otherOp = opB;

    // The 'phi' should have a single user and few incoming operands, 
    // else we will most probably increase the register pressure. 
    if((phiInstr == nullptr) ||
       (ShouldApplyOperatorOnPhi(phiInstr, otherOp) == false)) {
        return nullptr;
    }

    // Check if each incoming operand simplifies.
    StaticList<Operand*, 8> simplifiedOps;

    for(int i = 0; i < phiInstr->OperandCount(); i++) {
        auto op = phiInstr->GetOperand(i);
        auto opBlock = phiInstr->GetOperandBlock(i);
        
        // Don't try to do anything if the 'phi' has cycles,
        // or if the operand is itself a 'phi' instruction.
        if(auto definingInstr = op->DefiningInstruction()) {
            if(definingInstr->IsPhi()) {
                return nullptr;
            }
        }

        // It is very important that any created instruction to be found
        // in the parent block of the incoming operand.
        auto previousInsertionPoint = irGen_.GetInsertionPoint();
        irGen_.SetInsertionPoint(opBlock->Target()->BranchInstruction()
                                                  ->PreviousInstruction());
        bool simplified = false;
        
        if(auto result = SimplifyBinary(opcode, op, otherOp, 
                                        nullptr, FP_Exact, false)) {
            simplifiedOps.Add(result);
            simplified = true;
        }

        // Restore the previous insertion point.
        irGen_.SetInsertionPoint(previousInsertionPoint);

        if(simplified == false) {
            return nullptr;
        }
    }

    // Create the new 'phi' instruction and place it
    // where the previous one was found.
    auto resultOp = GetTemporary(opA);
    auto previousInsertionPoint = irGen_.GetInsertionPoint();
    irGen_.SetInsertionPoint(phiInstr->ParentBlock()->FirstInstruction());
    auto newPhiInstr = irGen_.GetPhi(resultOp, simplifiedOps.Count());

    for(int i = 0; i < simplifiedOps.Count(); i++) {
        newPhiInstr->AddOperand(simplifiedOps[i], 
                                phiInstr->GetOperandBlock(i));
    }

    irGen_.SetInsertionPoint(previousInsertionPoint);
    return LOG(resultOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::ShouldApplyOperatorOnPhi(PhiInstr* instr, Operand* otherOp) {
    // We really don't want to increase the register pressure.
    if(instr->ResultOp()->HasSingleUser() == false) {
        return false;
    }

    // There is an advantage for doing this transformation
    // only if all operands are constant, else we create new
    // instructions that only increase code size.
    return instr->HasOnlyConstants() && 
           otherOp->IsConstant();
}

} // namespace Optimization