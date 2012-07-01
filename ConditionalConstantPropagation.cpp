// LatticePropagator.cpp
// Copyright (c) Lup Gratian
//
// Implements the SCCP pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConditionalConstantPropagation.hpp"

namespace Optimization {

ConstantLatticeCellCache::ConstantLatticeCellCache() {
    // We create the TOP and BOTTOM cells now,
    // the rest will be created on demand.
    topCell_ = new ConstantLatticeCell(Cell_Top);
    bottomCell_ = new ConstantLatticeCell(Cell_Bottom);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ConstantLatticeCellCache::~ConstantLatticeCellCache() {
    delete topCell_;
    delete bottomCell_;

    constCells_.ForEachValue([](ConstantLatticeCell* cell) -> bool {
        delete cell;
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ConstantLatticeCell* 
ConstantLatticeCellCache::GetCellForConstant(Constant* constant) {
    DebugValidator::IsNotNull(constant);

    // Check if the cell is already created;
    // if not we create it now and cache it.
    ConstantLatticeCell* constCell;

    if(constCells_.TryGetValue(constant, &constCell) == false) {
        constCell = new ConstantLatticeCell(constant);
        constCells_.Add(constant, constCell);
    }

    return constCell;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LatticeCell* ConstantLattice::GetCellForParameter(Parameter* parameter) {
    DebugValidator::IsNotNull(parameter);
    OperandInfo opInfo(funct_->ParentUnit(), target_);

    if(auto intConst = opInfo.GetIntConstant(parameter, nullptr)) {
        return GetCellForConstant(intConst);
    }
    else return GetBottomCell();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::AreCellsEqual(LatticeCell* cellA, LatticeCell* cellB) {
    if(Lattice::AreCellsEqual(cellA, cellB)) {
        return true;
    }

    // We know that the cells aren't TOP/TOP or BOTTOM/BOTTOM.
    // We can have equality only if the cells represent the same constant.
    auto constCellA = static_cast<ConstantLatticeCell*>(cellA);
    auto constCellB = static_cast<ConstantLatticeCell*>(cellB);

    // Because the constants are unique we can test the pointers only.
    return constCellA->IsConstant() && 
           constCellB->IsConstant() &&
           (constCellA->AsConstant() == constCellB->AsConstant());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LatticeCell* ConstantLattice::MergeCells(LatticeCell* cellA, LatticeCell* cellB) {
    if(auto merged = Lattice::MergeCells(cellA, cellB)) {
        // Some TOP/BOTTOM combination.
        return merged;
    }

    auto constCellA = static_cast<ConstantLatticeCell*>(cellA);
    auto constCellB = static_cast<ConstantLatticeCell*>(cellB);
    DebugValidator::IsTrue(constCellA->IsConstant());
    DebugValidator::IsTrue(constCellB->IsConstant());

    // If the constant are the same we return the constant
    // lattice cell, else BOTTOM.
    if(constCellA->AsConstant() == constCellB->AsConstant()) {
        return constCellA;
    }
    else return GetBottomCell();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::Dominates(Block* a, Block* b) {
    return parent_->GetSafetyInfo()->Dominates(a, b);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantLattice::Transfer(Instruction* instr) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsFalse(instr->IsPhi());
    bool result = false;

    // We simulate the instruction by trying to evaluate it
    // if all operands are constants. For certain operations
    // (mul, and, shl, etc.) we know that the result is a constant
    // even if only one operand is constant.
    switch(instr->Category()) {
        case Instr_Arithmetic:
        case Instr_Logical: {
            result = TransferArithmeticLogical(instr);
            break;
        }
        case Instr_Conversion: {
            result = TransferConversion(instr->As<ConversionInstr>());
            break;
        }
        default: {
            if(auto cmpInstr = instr->As<CmpInstrBase>()) {
                result = TransferComparison(cmpInstr);
                break;
            }
            else if(auto loadInstr = instr->As<LoadInstr>()) {
                result = TransferLoad(loadInstr);
                break;
            }
            else if(auto callInstr = instr->As<CallInstr>()) {
                result = TransferCall(callInstr);
                break;
            }
            else if(auto questInstr = instr->As<QuestionInstr>()) {
                result = TransferQuestion(questInstr);
                break;
            }
        }
    }

    // Any other instruction, if it has a destination operand,
    // is varying (BOTTOM).
    if((result == false) && instr->HasDestinationOp()) {
        MarkBottom(instr);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LatticeCell* ConstantLattice::TransferPhi(PhiInstr* instr, bool isOptimistic) {
    // First we check if we should consider that this 'phi'
    // is a constant in all cases (this is used when evaluating 
    // the regions candidate for specialization).
    if(HasFixedPhiConstants()) {
        if(auto constantCell = GetFixedPhiConstant(instr)) {
            return constantCell;
        }
    }

    // Use the default behavior to handle all other cases.
    return Lattice::TransferPhi(instr, isOptimistic);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferArithmeticLogical(Instruction* instr) {
    // If both operands are constants we evaluate them.
    // There are cases when we have a constant even if
    // only one of the operands is a constant.
    auto constCellA = GetCell(instr->GetSourceOp(0));
    auto constCellB = GetCell(instr->GetSourceOp(1));

    if(constCellA->IsTop() && constCellB->IsTop()) {
        // Do nothing, the lattice will be lowered later.
        return true;
    }

    if(constCellA->IsConstant() && constCellB->IsConstant()) {
        auto result = folder_.FoldBinary(instr->GetOpcode(),
                                         constCellA->AsConstant(),
                                         constCellB->AsConstant());
        DebugValidator::IsNotNull(result);
        UpdateCell(instr, GetCellForConstant(result->As<Constant>()));
        return true;
    }

    // mul a, 0 -> 0
    if(instr->IsMul()) {
        if(constCellA->IsZeroInt() || 
           constCellB->IsZeroInt()) {
            UpdateCell(instr, GetCellForZero(instr->GetSourceOp(0)));
            return true;
        }
    }

    // div (quest c, 2, 3), 2 -> 1
    // mul (phi 2,10), (phi 10, 2) -> 20
    // mul (quest c, 2,10), (quest c, 10, 2) -> 20
    return (instr->IsArithmetic() &&
            TransferArithmetic(instr, constCellA, constCellB)) ||
           (instr->IsLogical() &&
            TransferLogical(instr, constCellA, constCellB)) ||
           TransferArithmeticLogicalPhi(instr, constCellA, constCellB) ||
           TransferArithmeticLogicalQuest(instr, constCellA, constCellB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferArithmetic(Instruction* instr,
                                         ConstantLatticeCell* constCellA,
                                         ConstantLatticeCell* constCellB) {
    // div 0, a -> 0
    // div (quest c, 2, 3), 2 -> 1
    // div (phi 2,3,4), 5 -> 0
    // We try pretty hard to fold division because it's expensive.
    if((instr->IsDiv() || instr->IsUdiv()) == false) {
        return false;
    }
    else if(constCellA->IsZeroInt()) {
        UpdateCell(instr, GetCellForZero(instr->GetSourceOp(0)));
        return true;
    }

    // mod/umod a, 1 -> 0
    if((instr->IsMod() || instr->IsUmod()) &&
       constCellB->IsMinusOneInt()) {
        UpdateCell(instr, GetCellForZero(instr->GetSourceOp(0)));
        return true;
    }

    auto intConstB = constCellB->AsIntConstant();
    if(intConstB == nullptr) {
        return false;
    }

    if(auto questInstr = instr->GetSourceOp(0)->DefiningInstrAs<QuestionInstr>()) {
        // div (quest c, 2, 3), 2 -> 1
        auto trueIntConst = GetCell(questInstr->TrueOp())->AsIntConstant();
        auto falseIntConst = GetCell(questInstr->FalseOp())->AsIntConstant();
        if((trueIntConst && falseIntConst) == false) {
            return false;
        }

        // If we get the same result we have a constant.
        auto trueResult = folder_.FoldBinary(instr->GetOpcode(),
                                             trueIntConst, intConstB);
        auto falseResult = folder_.FoldBinary(instr->GetOpcode(),
                                              falseIntConst, intConstB);
        DebugValidator::IsNotNull(trueResult);
        DebugValidator::IsNotNull(falseResult);

        if(trueResult == falseResult) {
            // We have a constant.
            UpdateCell(instr, GetCellForConstant(trueResult->As<Constant>()));
            return true;
        }
        else return false;
    }
    else if(auto phiInstr = instr->GetSourceOp(0)->DefiningInstrAs<PhiInstr>()) {
        // div (phi 2,3,4), 5 -> 0
        auto block = instr->ParentBlock();
        IntConstant* result = nullptr;

        // This transformation is not valid in case of loops,
        // so we give up as soon as we have an unexecutable edge,
        // a TOP incoming operand, or the parent block of the incoming
        // operand doesn't dominate the block where the 'phi' is found.
        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            auto incomingBlock = phiInstr->GetOperandBlock(i)->Target();
            FlowEdge edge(incomingBlock, block);

            if((Propagator()->IsExecutableEdge(edge) &&
                Dominates(incomingBlock, block)) == false) {
                return false;
            }

            auto incomingCell = GetCell(phiInstr->GetOperand(i));
            if(incomingCell->IsBottom() || 
               incomingCell->IsTop()){
                return false;
            }

            auto incomingIntConst = incomingCell->AsIntConstant();
            if(incomingIntConst == nullptr) {
                return false;
            }

            // Divide the incoming constant and check
            // if it's the same as the previous one.
            auto newResult = folder_.FoldBinary(instr->GetOpcode(),
                                                incomingIntConst, intConstB);
            DebugValidator::IsNotNull(newResult);

            if(result == nullptr) {
                // The first constant we see.
                result = newResult->As<IntConstant>();
            }
            else if(newResult != result) {
                // Conflicting constants.
                return false;
            }
        }

        if(result) {
            // The 'phi' actually folds to a constant.
            // We need to re-evaluate the users of the 'phi', because in case
            // of loops this optimization is usually invalid.
            UpdateCell(instr, GetCellForConstant(result));
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferArithmeticLogicalPhi(Instruction* instr,
                                                   ConstantLatticeCell* constCellA,
                                                   ConstantLatticeCell* constCellB) {
    // mul (phi 2, 10), (phi 10, 2) -> 20
    auto phiInstrA = instr->GetSourceOp(0)->DefiningInstrAs<PhiInstr>();
    auto phiInstrB = instr->GetSourceOp(1)->DefiningInstrAs<PhiInstr>();
    if((phiInstrA && phiInstrB) == false) {
        return false;
    }

    // The 'phi's must be found in the same block,
    // else we can't match the incoming operands.
    if(phiInstrA->ParentBlock() != phiInstrB->ParentBlock()) {
        return false;
    }

    auto block = phiInstrA->ParentBlock();
    Constant* result = nullptr;

    for(int i = 0; i < phiInstrA->OperandCount(); i++) {
        FlowEdge edge(phiInstrA->GetOperandBlock(i)->Target(), block);
        if(Propagator()->IsExecutableEdge(edge) == false) {
            continue;
        }

        auto incomingCellA = GetCell(phiInstrA->GetOperand(i));
        auto incomingCellB = GetCell(phiInstrB->GetOperand(i));

        if(incomingCellA->IsBottom() || 
           incomingCellB->IsBottom()) {
           return false;
        }
        else if(incomingCellA->IsTop() && 
                incomingCellB->IsTop()) {
            continue;
        }

        // Evaluate the instruction and check if the result
        // is the same as the previous one.
        auto incomingConstA = incomingCellA->AsConstant();
        auto incomingConstB = incomingCellB->AsConstant();
        if((incomingCellA && incomingCellB) == false) {
            return false;
        }

        auto newResult = folder_.FoldBinary(instr->GetOpcode(),
                                            incomingConstA, incomingConstB);
        if(newResult == nullptr) {
            return nullptr;
        }

        if(result == nullptr) {
            // The first constant we see.
            result = newResult->As<Constant>();
        }
        else if(newResult != result) {
            // Conflicting constants.
            return false;
        }
    }

    if(result) {
        // The 'phi's actually fold to a constant.
        UpdateCell(instr, GetCellForConstant(result));
        return true;
    }
 
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferArithmeticLogicalQuest(Instruction* instr,
                                                     ConstantLatticeCell* constCellA,
                                                     ConstantLatticeCell* constCellB) {
    // This is equivalent with what is done in 'TransferArithmeticLogicalPhi'.
    // mul (quest c, 2, 10), (quest c, 10, 2) -> 20
    auto questInstrA = instr->GetSourceOp(0)->DefiningInstrAs<QuestionInstr>();
    auto questInstrB = instr->GetSourceOp(1)->DefiningInstrAs<QuestionInstr>();
    if((questInstrA && questInstrB) == false) {
        return false;
    }

    // The condition operand must be the same.
    if(questInstrA->ConditionOp() != questInstrB->ConditionOp()) {
        return false;
    }

    auto trueConstA = GetCell(questInstrA->TrueOp())->AsConstant();
    auto falseConstA = GetCell(questInstrA->FalseOp())->AsConstant();
    if((trueConstA && falseConstA) == false) {
        return false;
    }

    auto trueConstB = GetCell(questInstrB->TrueOp())->AsConstant();
    auto falseConstB = GetCell(questInstrB->FalseOp())->AsConstant();
    if((trueConstB && falseConstB) == false) {
        return false;
    }

    // Evaluate both sides and check if the result is the same.
    auto trueResult = folder_.FoldBinary(instr->GetOpcode(),
                                         trueConstA, trueConstB);
    auto falseResult = folder_.FoldBinary(instr->GetOpcode(),
                                          falseConstA, falseConstB);
    if((trueResult && falseResult) == false) {
        return false;
    }
        
    if(trueResult == falseResult) {
        // We have a constant.
        UpdateCell(instr, GetCellForConstant(trueResult->As<Constant>()));
        return true;
    }
 
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferLogical(Instruction* instr,
                                      ConstantLatticeCell* constCellA,
                                      ConstantLatticeCell* constCellB) {
    // Here we implement some logical identities.
    // and a, 0 -> 0
    if(instr->IsAnd() &&
       (constCellA->IsZeroInt() || constCellB->IsZeroInt())) {
        UpdateCell(instr, GetCellForZero(instr->GetSourceOp(0)));
        return true;
    }

    // or a, -1 -> -1
    if(instr->IsOr() &&
       (constCellA->IsMinusOneInt() || constCellB->IsMinusOneInt())) {
        UpdateCell(instr, GetCellForMinusOne(instr->GetSourceOp(0)));
        return true;
    }

    // shr -1, a -> -1
    if(instr->IsShr() && constCellA->IsMinusOneInt()) {
        UpdateCell(instr, GetCellForMinusOne(instr->GetSourceOp(0)));
        return true;
    }

    // shl/shr/ushr 0, a -> 0
    if(instr->IsShift() && constCellA->IsZeroInt()) {
        UpdateCell(instr, GetCellForZero(instr->GetSourceOp(0)));
        return true;
    }

    // shl/shr/ushr a, C -> 0, if C >= bits(a)
    if(instr->IsShift() && constCellB->IsConstant()) {
        auto intType = instr->GetSourceOp(0)->GetType()->As<IntegerType>();
        int bits = intType->SizeInBits();
        auto intConst = constCellB->AsIntConstant();

        if((intConst == nullptr) || (intConst->Value() < bits)) {
            return false;
        }

        // For 'shr' we need to know if the shifter operand
        // is positive or negative. If we don't we need to give up,
        // because we don't know what kind of bit is inserted.
        if(instr->IsShr()) {
            OperandInfo opInfo(funct_->ParentUnit(), target_);

            if(opInfo.IsPositive(instr->GetSourceOp(0), instr->ParentBlock())) {
                // shr a, 35 -> 0
                UpdateCell(instr, GetCellForZero(instr->GetSourceOp(0)));
                return true;
            }
            else if(opInfo.IsNegative(instr->GetSourceOp(0), instr->ParentBlock())) {
                // shr a, 35 -> -1
                UpdateCell(instr, GetCellForMinusOne(instr->GetSourceOp(0)));
                return true;
            }
        }
        else {
            // The result is definitely zero.
            UpdateCell(instr, GetCellForZero(instr->GetSourceOp(0)));
            return true;
        }
    }
    
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferConversion(ConversionInstr* instr) {
    // Try to evaluate as a constant.
    auto constCell = GetCell(instr->TargetOp());
    
    if(constCell->IsTop()) {
        // Do nothing , the lattice will be lowered later.
        return true;
    }
    else if(auto constant = constCell->AsConstant()) {
        auto result = folder_.FoldConversion(instr->GetOpcode(), constant,
                                             instr->CastType());
        if(result) {
            UpdateCell(instr, GetCellForConstant(result->As<Constant>()));
            return true;
        }
    }
    
    return false; // BOTTOM.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferComparison(CmpInstrBase* instr) {
    // We try pretty hard to fold comparisons, because they are
    // mostly used in branching instructions.
    auto constCellA = GetCell(instr->LeftOp());
    auto constCellB = GetCell(instr->RightOp());

    if(constCellA->IsTop() && constCellB->IsTop()) {
        // Do nothing, the lattice will be lowered later.
        return true;
    }
    
    if(constCellA->IsConstant() && constCellB->IsConstant()) {
        auto result = folder_.FoldCompare(instr->GetOpcode(),
                                          constCellA->AsConstant(), 
                                          constCellB->AsConstant(),
                                          instr->Order());
        UpdateCell(instr, GetCellForConstant(result->As<Constant>()));
        return true;
    }

    // If we have a 'quest' or 'phi' we may know the result
    // of the comparison even if they are not constant.
    // cmp lt (quest c, 5, 7), 3 -> 0
    // cmp gt (phi 6, 7), (phi 4, 2) -> 1
    return TransferComparisonOneConst(instr, constCellA, constCellB) ||
           TransferComparisonPhi(instr, constCellA, constCellB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferComparisonOneConst(CmpInstrBase* instr,
                                                 ConstantLatticeCell* constCellA,
                                                 ConstantLatticeCell* constCellB) {
    // cmp lt (quest c, 5, 7), 3 -> 0
    // cmp gt (phi 6, 7), 2 -> 1
    bool inverted = false;
    IntConstant* intConst = constCellB->AsIntConstant();

    if((intConst == nullptr) && 
       (instr->LeftOp()->DefiningInstrIs<PhiInstr>() ||
        instr->LeftOp()->DefiningInstrIs<QuestionInstr>()) == false) {
        intConst = constCellA->AsIntConstant();
        inverted = true;
    }

    if(intConst == nullptr) {
        return false;
    }

    // If the constant is on the left side we need
    // to invert the comparison order.
    auto otherOp = inverted ? instr->RightOp() : instr->LeftOp();
    auto order = inverted ? CmpInstrBase::InvertedOrder(instr->Order(), false) :
                            instr->Order();

    if(auto questInstr = otherOp->DefiningInstrAs<QuestionInstr>()) {
        // Check if we have the same result for both sides.
        auto trueConst = GetCell(questInstr->TrueOp())->AsIntConstant();
        auto falseConst = GetCell(questInstr->FalseOp())->AsIntConstant();
        if((trueConst && falseConst) == false) {
            return false;
        }

        auto trueResult = folder_.FoldCompare(instr->GetOpcode(),
                                              trueConst, intConst, order);
        auto falseResult = folder_.FoldCompare(instr->GetOpcode(),
                                               falseConst, intConst, order);
        if(trueResult == falseResult) {
            // We have a constant.
            UpdateCell(instr, GetCellForConstant(trueResult->As<Constant>()));
            return true;
        }
    }
    else if(auto phiInstr = otherOp->DefiningInstrAs<PhiInstr>()) {
        auto block = phiInstr->ParentBlock();
        Constant* result = nullptr;

        // This transformation is not valid in case of loops,
        // so we give up as soon as we have an unexecutable edge
        // a TOP incoming operand, or the parent block of the incoming
        // operand doesn't dominate the block where the 'phi' is found.
        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            auto incomingBlock = phiInstr->GetOperandBlock(i)->Target();
            FlowEdge edge(incomingBlock, block);

            if((Propagator()->IsExecutableEdge(edge) &&
               (Dominates(incomingBlock, block)) == false)) {
                return false;
            }

            auto incomingCell = GetCell(phiInstr->GetOperand(i));
            if(incomingCell->IsBottom() || incomingCell->IsTop()) {
                return false;
            }

            // Evaluate the instruction and check if the result
            // is the same as the previous one.
            auto incomingConst = incomingCell->AsIntConstant();
            if(incomingCell == nullptr) {
                return false;
            }

            auto newResult = folder_.FoldCompare(instr->GetOpcode(),
                                                 incomingConst, intConst, order);
            if(newResult == nullptr) {
                return nullptr;
            }

            if(result == nullptr) {
                // The first constant we see.
                result = newResult->As<Constant>();
            }
            else if(newResult != result) {
                // Conflicting constants.
                return false;
            }
        }

        if(result) {
            // The comparison has the same result for all incoming operands.
            UpdateCell(instr, GetCellForConstant(result));
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferComparisonPhi(CmpInstrBase* instr,
                                            ConstantLatticeCell* constCellA,
                                            ConstantLatticeCell* constCellB) {
    // cmp gt (phi 6, 7), (phi 4, 2) -> 1
    auto phiInstrA = instr->LeftOp()->DefiningInstrAs<PhiInstr>();
    auto phiInstrB = instr->RightOp()->DefiningInstrAs<PhiInstr>();
    if((phiInstrA && phiInstrB) == false) {
        return false;
    }

    // The 'phi's must be found in the same block,
    // else we can't match the incoming operands.
    if(phiInstrA->ParentBlock() != phiInstrB->ParentBlock()) {
        return false;
    }

    auto block = phiInstrA->ParentBlock();
    Constant* result = nullptr;

    for(int i = 0; i < phiInstrA->OperandCount(); i++) {
        FlowEdge edge(phiInstrA->GetOperandBlock(i)->Target(), block);
        if(Propagator()->IsExecutableEdge(edge) == false) {
            continue;
        }

        auto incomingCellA = GetCell(phiInstrA->GetOperand(i));
        auto incomingCellB = GetCell(phiInstrB->GetOperand(i));

        if(incomingCellA->IsBottom() || 
           incomingCellB->IsBottom()) {
           return false;
        }
        else if(incomingCellA->IsTop() && 
                incomingCellB->IsTop()) {
            continue;
        }

        // Evaluate the comparison and check if the result
        // is the same as the previous one.
        auto incomingConstA = incomingCellA->AsConstant();
        auto incomingConstB = incomingCellB->AsConstant();
        if((incomingCellA && incomingCellB) == false) {
            return false;
        }

        auto newResult = folder_.FoldCompare(instr->GetOpcode(),
                                             incomingConstA, incomingConstB,
                                             instr->Order());
        if(newResult == nullptr) {
            return false;
        }

        if(result == nullptr) {
            // The first constant we see.
            result = newResult->As<Constant>();
        }
        else if(newResult != result) {
            // Conflicting constants.
            return false;
        }
    }

    if(result) {
        // The comparison has the same result for all incoming operands.
        UpdateCell(instr, GetCellForConstant(result));
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferLoad(LoadInstr* instr) {
    // Try to fold a load from a constant global variable
    // that has an initializer. Such variables are usually exposed
    // after interprocedural optimizations.
    __int64 offset;
    auto globalVariableRef = GetBaseGlobal(instr->SourceOp(), offset);

    if(globalVariableRef == nullptr) {
        return false;
    }

    // We let the Constant Folder to validate and load the constant.
    auto resultType = instr->ResultOp()->GetType();
    auto result = folder_.FoldLoad(globalVariableRef, resultType, offset);

    if((result != nullptr) && result->IsConstant()) {
        UpdateCell(instr, GetCellForConstant(result->As<Constant>()));
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
VariableReference* ConstantLattice::GetBaseGlobal(Operand* op, __int64& offset) {
    offset = 0;

    // We try to determine the global variable that is
    // accessed, and the offset into the variable.
    while(true) {
        if(auto variableRef = op->As<VariableReference>()) {
            if(variableRef->IsGlobalVariableRef()) {
                return variableRef;
            }
            else return nullptr;
        }
        else if(auto indexInstr = op->DefiningInstrAs<IndexInstr>()) {
            // We can continue only if the the index is a constant.
            auto indexCell = GetCell(indexInstr->IndexOp());
            if(indexCell->IsTop() || indexCell->IsBottom()) {
                break;
            }

            if(auto intConst = indexCell->AsIntConstant()) {
                __int64 elementSize = TI::GetSize(indexInstr->GetElementType(), target_);
                offset += elementSize * intConst->Value();
                op = indexInstr->BaseOp();
            }
            else break;
        }
        else if(auto elemInstr = op->DefiningInstrAs<ElementInstr>()) {
            int field = elemInstr->GetFieldIndex();
            offset += elemInstr->GetRecordType()->GetFieldOffset(field);
            op = elemInstr->BaseOp();
        }
        else break;
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferCall(CallInstr* instr) {
    // We handle calls to the min/max/abs intrinsics,
    // because they appear frequently.
    auto mathIntrinsic = instr->GetIntrinsicAs<MathIntrinsic>();
    if(mathIntrinsic == nullptr) {
        return false;
    }
    
    if(mathIntrinsic->IsAbs()) {
        auto argumentCell = GetCell(instr->GetArgument(0));
        if(argumentCell->IsTop()) {
            return true;
        }
        else if(argumentCell->IsBottom()) {
            return false;
        }

        auto intConst = argumentCell->AsIntConstant();
        if(intConst == nullptr) {
            return false;
        }

        // Compute the absolute value and set it.
        __int64 abs = intConst->Value();
        if(abs < 0) abs = -abs;
        
        auto& consts = funct_->ParentUnit()->Constants();
        auto absConst = consts.GetInt(instr->GetArgument(0)->GetType(), abs);

        UpdateCell(instr, GetCellForConstant(absConst));
        return true;
    }
    else if(mathIntrinsic->IsMin() ||
            mathIntrinsic->IsMax()) {
        auto argumentCellA = GetCell(instr->GetArgument(0));
        auto argumentCellB = GetCell(instr->GetArgument(1));

        if(argumentCellA->IsBottom() || argumentCellB->IsBottom()) {
            return false;
        }
        else if(argumentCellA->IsTop() || argumentCellB->IsTop()) {
            return true;
        }

        auto intConstA = argumentCellA->AsIntConstant();
        auto intConstB = argumentCellB->AsIntConstant();
        if((intConstA && intConstB) == false) {
            return false;
        }

        // Compute the minimum/maximum, taking the sign into consideration.
        bool isSigned = instr->GetArgument(2)->IsOneInt();
        __int64 result;
    
        if(isSigned) {
            if(mathIntrinsic->IsMax()) {
                result = std::max(intConstA->Value(), intConstB->Value());
            }
            else result = std::min(intConstA->Value(), intConstB->Value());
        }
        else if(mathIntrinsic->IsMax()) {
            result = std::max((unsigned __int64)intConstA->Value(), 
                              (unsigned __int64)intConstB->Value());
        }
        else result = std::min((unsigned __int64)intConstA->Value(), 
                               (unsigned __int64)intConstB->Value());

        auto& consts = funct_->ParentUnit()->Constants();
        auto resultConst =  consts.GetInt(instr->GetArgument(0)->GetType(), result);
        UpdateCell(instr, GetCellForConstant(resultConst));
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantLattice::TransferQuestion(QuestionInstr* instr) {
    // If the condition operand is a constant
    // we know which side is selected.
    auto conditionCell = GetCell(instr->ConditionOp());

    if(conditionCell->IsTop()) {
        // If both operands are varying there is no reason
        // to continue tracking the 'quest'.
        return (GetCell(instr->TrueOp())->IsBottom()) ||
               (GetCell(instr->FalseOp())->IsBottom()) == false;
    }

    if(auto intConst = conditionCell->AsIntConstant()) {
        auto selectedCell = intConst->IsZero() ? GetCell(instr->FalseOp()) :
                                                 GetCell(instr->TrueOp());
        if(selectedCell->IsTop()) {
            return true;
        }
        else if(selectedCell->IsBottom()) {
            return false;
        }
        
        UpdateCell(instr, GetCellForConstant(selectedCell->AsConstant()));
        return true;
    }

    // quest c, 4, 4 -> 4
    auto trueConst = GetCell(instr->TrueOp())->AsConstant();
    auto falseConst = GetCell(instr->FalseOp())->AsConstant();

    if((trueConst && falseConst) &&
       (trueConst == falseConst)) {
        UpdateCell(instr, GetCellForConstant(trueConst));
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ConstantLattice::AddFeasibleSuccessors(ControlInstr* instr, 
                                           TSuccessorList& successorList) {
    DebugValidator::IsNotNull(instr);

    // We try to fold the branch, often because this leads
    // to more constant to be discovered.
    if(instr->IsGoto()) {
        // The successor is always executed.
        successorList.Add(instr->ParentBlock()->SuccessorAt(0));
        return 1;
    }

    // Check if the condition operand is a constant.
    // If it is we let the Constant Folder determine 
    // the taken branch. If it isn't we may still be able
    // to determine the result for 'phi' and 'quest' instructions.
    auto conditionCell = GetCell(instr->GetSourceOp(0));

    if(conditionCell->IsTop()) {
        // We don't know which successor is taken in this case.
        return AddAllSuccessors(instr->ParentBlock(), successorList);
    }
    else if(auto constant = conditionCell->AsConstant()) {
        auto destination = folder_.FoldBranching(instr, constant);
        DebugValidator::IsNotNull(destination);
        successorList.Add(destination->Target());
        return 1;
    }

    // The condition is varying, but if we have an if
    // we may determine that the 'true' branch is taken.
    // if (phi 2, 5), B1, B2 -> B1
    if(instr->IsIf() && instr->GetSourceOp(0)->HasDefiningInstruction()) {
        auto definingInstr = instr->GetSourceOp(0)->DefiningInstruction();
        bool notZero = true;
        
        if(auto phiInstr = definingInstr->As<PhiInstr>()) {
            // This transformation is not valid in case of loops,
            // so we give up as soon as we have an unexecutable edge,
            // a TOP incoming operand, or the parent block of the incoming
            // operand doesn't dominate the block where the 'phi' is found.
            auto block = phiInstr->ParentBlock();

            for(int i = 0; i < phiInstr->OperandCount(); i++) {
                auto incomingBlock = phiInstr->GetOperandBlock(i)->Target();
                FlowEdge edge(incomingBlock, block);

                if((Propagator()->IsExecutableEdge(edge) &&
                    Dominates(incomingBlock, block)) == false) {
                    notZero = false;
                    break;
                }

                auto incomingCell = GetCell(phiInstr->GetOperand(i));

                if(incomingCell->IsBottom() ||
                   incomingCell->IsTop()) {
                    // We need to give up now.
                    notZero = false;
                    break;
                }

                // We should have a constant that is not zero.
                if((incomingCell->IsConstant() == false) ||
                    incomingCell->AsConstant()->IsZeroInt()) {
                    notZero = false;
                    break;
                }
            }

            if(notZero) {
                // The 'true' successor is executed.
                successorList.Add(instr->ParentBlock()->SuccessorAt(0));
                return 1;
            }
        }
        else if(auto questInstr = definingInstr->As<QuestionInstr>()) {
            // Check if both operands are not zero.
            auto trueConst = GetCell(questInstr->TrueOp())->AsConstant();
            auto falseConst = GetCell(questInstr->FalseOp())->AsConstant();

            if((trueConst && falseConst) &&
               ((trueConst->IsZeroInt() || falseConst->IsZeroInt()) == false)) {
                // The 'true' successor is executed.
                successorList.Add(instr->ParentBlock()->SuccessorAt(0));
                return 1;
            }
        }
    } 

    // We really need to add all successors.
    return AddAllSuccessors(instr->ParentBlock(), successorList);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ConstantLattice::AddAllSuccessors(Block* block, TSuccessorList& successorList) {
    auto succEnum = block->GetSuccessorEnum();

    while(succEnum.IsValid()) {
        successorList.Add(succEnum.Next());
    }

    return block->SuccessorCount();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantLattice::AddFixedPhiConstant(PhiInstr* instr, Constant* constant) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(constant);
    DebugValidator::IsTrue(instr->HasDestinationOp());

    auto cell = GetCell(constant);
    fixedPhiConsts_.Add(instr->GetDestinationOp(), cell);
    UpdateCell(instr, cell);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ConstantLatticeCell* ConstantLattice::GetFixedPhiConstant(PhiInstr* instr) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsTrue(hasFixedPhiConsts_);

    if(auto destOp = instr->GetDestinationOp()) {
        ConstantLatticeCell* cell;
        if(fixedPhiConsts_.TryGetValue(destOp, &cell)) {
            return cell;
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::Initialize(Function* function) {
    funct_ = function;
    lattice_ = new ConstantLattice(function, cache_, GetTarget(), this);
    lattice_->SetPropagator(&propagator_);
    propagator_.SetLattice(lattice_);

    //! TODO: obtain!
    domTree_ = new IRDominatorTree(function);
    domTree_->Build();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::Execute(Function* function) {
    // Execute the SCCP algorithm, then try to specialize regions.
    Initialize(function);
    IterateToFixedpoint(false /* limited */);

#if 0
    if(FindConstantRegions() > 0) {
        // Select the most profitable regions and clone them.
        RegionList selectedRegions;
        SelectConstantRegions(selectedRegions);

#if 1
        DumpRegions();
#endif
        if(selectedRegions.Count()) {
            S+= selectedRegions.Count();
            DumpRegions();
            }

        CloneConstantRegions(selectedRegions);

        // Resume the SCCP algorithm.
        IterateToFixedpoint(false /* limited */, true /* resume */);
    }
#endif

    PatchFunction(function);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::IterateToFixedpoint(bool limited, bool resume) {
    propagator_.SetLimitIterations(limited);
    propagator_.SetMaximumIterationCount(1);

    if(resume) {
        propagator_.Resume();
    }
    else propagator_.Start(funct_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::PatchFunction(Function* function) {
    // After the SCCP algorithm is run we try to replace
    // each instruction with the determined constant.
    // Branching instructions are modified to branch
    // to the single executable successor, if it's the case.
    constantsFound_ = 0;
    constantBranchesFound_ = 0;

    function->ForEachInstruction([this](Instruction* instr) -> bool {
        PatchInstruction(instr);
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::PatchInstruction(Instruction* instr) {
    // 'if' and 'switch' instructions are handled separately.
    if(instr->IsIf() || instr->IsSwitch()) {
        PatchBranching(instr);
        return;
    }

    // If the instruction has a result operand,
    // and it was determined that it is a constant, we replace
    // it with the constant. This will update all users.
    if(instr->HasDestinationOp() == false) {
        return;
    }

    auto resultOp = instr->GetDestinationOp();
    auto resultCell = lattice_->GetCell(resultOp);

    if(resultCell->IsBottom()) {
        // We couldn't prove that the instruction is a constant.
        return;
    }
    else if(resultCell->IsConstant()) {
        // Replace the instruction by the determined constant.
        InstructionSimplified(instr);
        resultOp->ReplaceWith(resultCell->AsConstant());
        instr->RemoveFromBlock(true);
        constantsFound_++;
    }
    else if(resultCell->IsTop()) {
        // Undefined value.
        InstructionSimplified(instr);
        resultOp->ReplaceWith(GetUndefined(resultOp));
        instr->RemoveFromBlock(true);
    }
    else DebugValidator::Unreachable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::PatchBranching(Instruction* instr) {
    // It may be the case that only one of the successors
    // was determined to be executable. In this case we need
    // to replace the condition with a constant that forces
    // the CFG Cleaner to remove the unreachable blocks.
    if(lattice_->GetCell(instr->GetSourceOp(0))->IsConstant()) {
        // The constant has been propagated already.
        InstructionSimplified(instr);
        constantBranchesFound_++;
        return;
    }

    if(auto ifInstr = instr->As<IfInstr>()) {
        bool trueExec = lattice_->IsEdgeExecutable(instr->ParentBlock(),
                                                   ifInstr->TrueTargetOp()->Target());
        bool falseExec = lattice_->IsEdgeExecutable(instr->ParentBlock(),
                                                    ifInstr->FalseTargetOp()->Target());
        if((trueExec && falseExec) == false) {
            if(trueExec) {
                instr->ReplaceSourceOp(0, GetOneInt());
            }
            else instr->ReplaceSourceOp(0, GetZeroInt());

            InstructionSimplified(instr);
            constantBranchesFound_++;
        }
    }
    else if(auto switchInstr = instr->As<SwitchInstr>()) {
        // If the condition operand is not a constant it still might
        // be the case that the Constant Folder decided that only one
        // of the successors is executable; in this case we scan the entire
        // successor list and check which ones are marked as being executable.
        Block* singleSuccessor = nullptr;

        for(int i = 0; i < switchInstr->CaseCount(); i++) {
            auto successorBlock = switchInstr->GetCase(i).Target->Target();

            if(lattice_->IsEdgeExecutable(switchInstr->ParentBlock(),
                                          successorBlock)) {
                // Check if we have a single successor; if not we immediately
                // give up, because we can't have a single successor.
                if(singleSuccessor) return;
                else singleSuccessor = successorBlock;
            }
        }

        // Try to get the value associated with the single successor;
        // note that this is not possible if the successor is the default target.
        __int64 value;

        if(switchInstr->GetValueForTarget(singleSuccessor, value)) {
            auto type = switchInstr->ConditionOp()->GetType();
            switchInstr->SetConditionOp(GetIntConstant(type, value));
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::InstructionSimplified(Instruction* instr) {
#if 1
	auto block = instr->ParentBlock();
	auto function = instr->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string text = IRPrinter(instr).ToString();
    string branching = instr->IsIf() ? "branching " : "";
	Log::Warning("SCCP constant " + branching + functionName + ":" + blockName +
				 ": " + text);
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::DumpRegions() {
    string text = string::Format(L"Region count: %d\n\n", regions_.Count());

    regions_.ForEach([&text](ConstantRegion* region) -> bool {
        text += string::Format(L"\nRegion %X\n", region);
        text += string::Format(L"\tParent: %X\n", region->Parent());
        text += string::Format(L"\tTotal Instrs: %d\n", region->ConstantInstructions());
        text += string::Format(L"\tConstant Instrs: %d\n", region->ConstantAfterInstructions());
        text += string::Format(L"\ttBlocks: %d\n", region->BlockCount());

        for(int i = 0; i < region->BlockCount(); i++) {
            text += "\t\t" + *region->GetBlock(i)->Name();
        }
        return true;
    });

    if(regions_.Count() > 0){
        int a=8;
        a=7;
    }
     ObjectDumper(text, "SCCP Constant Regions").Dump();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::Dump() {
    string text = string::Format(L"Consts: %d     Const branches: %d",
                                 constantsFound_, constantBranchesFound_);
    ObjectDumper(text, "SCCP").Dump();
}

} // namespace Optimization