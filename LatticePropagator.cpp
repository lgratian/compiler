// LatticePropagator.cpp
// Copyright (c) Lup Gratian
//
// Implements the SSA Propagator.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "LatticePropagator.hpp"

namespace Analysis {

LatticeCell* Lattice::GetInitializationCell(Operand* op) {
    if(auto constantOp = op->As<Constant>()) {
        return GetCellForConstant(constantOp);
    }
    else if(auto parameter = op->As<Parameter>()) {
        return GetCellForParameter(parameter);
    }
    else if(op->IsVariableReference() ||
            op->IsFunctionReference() ||
            op->IsBlockReference()) {
        return GetBottomCell();
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Lattice::AreCellsEqual(LatticeCell* cellA, LatticeCell* cellB) {
    DebugValidator::IsNotNull(cellA);
    DebugValidator::IsNotNull(cellB);

    if(cellA == cellB) {
        return true;
    }
    else if((cellA->IsTop() && cellB->IsTop()) ||
            (cellA->IsBottom() && cellB->IsBottom())) {
        return true;
    }
    else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LatticeCell* Lattice::MergeCells(LatticeCell* cellA, LatticeCell* cellB) {
    // We use the rules of the usual lattice meet operator.
    // BOTTOM + ANY -> BOTTOM
    // TOP + ANY -> ANY
    if(cellA->IsBottom() || cellB->IsBottom()) {
        return GetBottomCell();
    }
    else if(cellA->IsTop()) {
        return cellB;
    }
    else if(cellB->IsTop()) {
        return cellA;
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LatticeCell* Lattice::TransferPhi(PhiInstr* instr, bool isOptimistic) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(propagator_);

    // If the 'phi' has a large number of incoming operands
    // the chance that all of them are the same is very small,
    // so we give up in this case.
    if(instr->OperandCount() > PhiLimit()) {
        return MarkBottom(instr);
    }

	// We merge all incoming operands of the 'phi' instruction.
    // If all operands have the same lattice value then
    // the result of the 'phi' is that value, else BOTTOM.
    // In "optimistic" mode we're allowed to ignore TOP values.
    auto block = instr->ParentBlock();
    LatticeCell* value = nullptr;

    for(int i = 0; i < instr->OperandCount(); i++) {
        // If the edge associated with the operand
        // is not executable we can ignore it in "optimistic" mode.
        if(IsExecutableEdge(instr->GetOperandBlock(i)->Target(), block) == false) {
            if(isOptimistic) continue;
            else return MarkBottom(instr);
        }

        // Check the associated cell and give up as soon as possible.
        LatticeCell* other = GetCell(instr->GetOperand(i));

        if(other->IsBottom()) {
            return MarkBottom(instr);
        }
        else if(other->IsTop()) {
            // An undefined value, ignore it in "optimistic" mode.
            if(isOptimistic) continue;
            else return MarkBottom(instr);
        }

        if(value == nullptr) {
            // This is the first time we see the value.
            value = other;
        }
        else {
            // Check if the merged value is the same as the previous one.
            // If not we give up and set BOTTOM.
            LatticeCell* merged = MergeCells(value, other);

            if(AreCellsEqual(merged, value) == false) {
                return MarkBottom(instr);
            }
        }
    }

    if(value) {
        UpdateCell(instr, value);
        return value;
    }
    else return MarkTop(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Lattice::AddFeasibleSuccessors(ControlInstr* instr, 
                                   TSuccessorList& successorList) {
    // Add all successors of the parent block to the list.
    // This is always a safe assumption, albeit not optimal.
    auto successorEnum = instr->ParentBlock()->GetSuccessorEnum();

    while(successorEnum.IsValid()) {
        successorList.Add(successorEnum.Next());
    }

    return successorList.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LatticeCell* Lattice::MarkBottom(Instruction* instr) {
    auto bottom = GetBottomCell();
    propagator_->UpdateCell(instr, bottom);
    return bottom;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LatticeCell* Lattice::MarkTop(Instruction* instr) {
    auto top = GetTopCell();
    propagator_->UpdateCell(instr, top);
    return top;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LatticeCell* Lattice::GetCell(Instruction* instr) {
    return propagator_->GetCell(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LatticeCell* Lattice::GetCell(Operand* op) {
    if(auto constantOp = op->As<Constant>()) {
        return GetCellForConstant(constantOp);
    }
    else return propagator_->GetCell(op);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lattice::UpdateCell(Instruction* instr, LatticeCell* cell) {
    propagator_->UpdateCell(instr, cell);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Lattice::IsExecutableEdge(Block* fromBlock, Block* toBlock) {
    return propagator_->IsExecutableEdge(FlowEdge(fromBlock, toBlock));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::Start(Function* function, bool optimistic) {
    DebugValidator::IsNotNull(function);
    DebugValidator::IsNotNull(lattice_);
    optimistic_ = optimistic;
    
    lattice_->PropagationStarted();
    Initialize(function);
    Process();
    lattice_->PropagationEnded();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::Resume() {
    lattice_->PropagationStarted();
    Process();
    lattice_->PropagationEnded();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::Initialize(Function* function) {
    // The only thing we need to do is to add the entry block
    // to the worklist and to create lattice cells for the parameters.
    // The lattice cells for the instruction results will be
    // created on demand.
    blockWorklist_.Add(function->FirstBlock());
    inBlockWorklist_.SetBit(function->FirstBlock()->Id());

    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameter = function->GetParameter(i);
        SetCell(parameter, lattice_->GetInitializationCell(parameter));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LatticeCell* LatticePropagator::GetCell(Operand* op) {
    DebugValidator::IsNotNull(op);
    LatticeCell* cell;

    if(cells_.TryGetValue(op, &cell) == false) {
        cell = lattice_->GetInitializationCell(op);
        cells_.Add(op, cell);
    }

    return cell;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LatticeCell* LatticePropagator::GetCell(Instruction* instr) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsTrue(instr->HasDestinationOp());
    return GetCell(instr->GetDestinationOp());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::SetCell(Operand* op, LatticeCell* cell) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsNotNull(cell);
    cells_.Add(op, cell);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::SetCell(Instruction* instr, LatticeCell* cell) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(cell);
    DebugValidator::IsTrue(instr->HasDestinationOp());
    SetCell(instr->GetDestinationOp(), cell);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LatticePropagator::UpdateCell(Instruction* instr, LatticeCell* cell) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(cell);
    DebugValidator::IsTrue(instr->HasDestinationOp());

    // Check if the cell is different from the old one.
    // If it is all users need to be added to the instruction
    // worklist and reprocessed later.
    auto resultOp = instr->GetDestinationOp();
    auto oldCell = GetCell(resultOp);

    if(lattice_->AreCellsEqual(cell, oldCell) == false) {
        SetCell(resultOp, cell);
        AddUsersToWorklist(resultOp);
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::AddUsersToWorklist(Temporary* temp) {
    for(int i = 0; i < temp->UserCount(); i++) {
        auto user = temp->GetUser(i);

        // Check if we reached the iteration limit
        // for this instruction.
        if(limitIterations_) {
            int iterations;

            if(instructionInterations_.TryGetValue(user, &iterations) &&
               (iterations >= maxIterationCount_)) {
                // Notify the client that the limit has been reached
                // (it might want to change the cell to the most pessimistic case).
                lattice_->CellLimitReached(temp);
                continue;
            }
        }

        if(inInstructionWorklist_.ContainsKey(user) == false) {
            instructionWorklist_.Add(user);
            inInstructionWorklist_.Add(user, true);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::AddExecutableSuccessors(Block* block, TSuccessorList& successorList) {
    // Add to the worklist all blocks that are not already added.
    // The executable flag will be set the when the block is processed.
    for(int i = 0; i < successorList.Count(); i++) {
        auto successorBlock = successorList[i];

        // Mark the edge as executable.
        executableEdges_.Add(FlowEdge(block, successorBlock), true);

        if(inBlockWorklist_.IsSet(successorBlock->Id()) == false) {
            blockWorklist_.Add(successorBlock);
            inBlockWorklist_.SetBit(successorBlock->Id());
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::ProcessBlock(Block* block) {
    // If this is the first time that we process the block
    // we simulate all instructions, else only the 'phi's.
    auto instr = block->FirstInstruction();

    while(auto phiInstr = instr->As<PhiInstr>()) {
        lattice_->TransferPhi(phiInstr, optimistic_);
        instr = instr->NextInstruction();
    }

    if(wasBlockProcessed_.IsSet(block->Id())) {
        // The instruction worklist will update the rest
        // of the block, if required.
        return;
    }
    else wasBlockProcessed_.SetBit(block->Id());
    
    // Process the rest of the block.
    while(instr) {
        ProcessInstruction(instr);
        instr = instr->NextInstruction();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::ProcessInstruction(Instruction* instr) {
    UpdateIterationCount(instr);

    if(auto phiInstr = instr->As<PhiInstr>()) {
        lattice_->TransferPhi(phiInstr, optimistic_);
    }
    else if(instr->IsGoto() ||
            instr->IsIf() ||
            instr->IsSwitch()) {
        // We let the lattice decide which successor blocks
        // are really executable.
        TSuccessorList successorList;
        lattice_->AddFeasibleSuccessors(instr->As<ControlInstr>(), successorList);
        AddExecutableSuccessors(instr->ParentBlock(), successorList);
    }
    else lattice_->Transfer(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::UpdateIterationCount(Instruction* instr) {
    if(limitIterations_) {
        if(instructionInterations_.ContainsKey(instr)) {
            instructionInterations_[instr]++;
        }
        else instructionInterations_.Add(instr, 0);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::AddBlockToWorklist(Block* block, Block* predecessorBlock) {
    DebugValidator::IsNotNull(block);

    if(inBlockWorklist_.IsSet(block->Id()) == false) {
        blockWorklist_.Add(block);
        inBlockWorklist_.SetBit(block->Id());
    }

    if(predecessorBlock) {
        executableEdges_.Add(FlowEdge(predecessorBlock, block), true);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LatticePropagator::Process() {
    // The algorithm runs until both worklists are empty.
    while(blockWorklist_.IsNotEmpty() ||
          instructionWorklist_.IsNotEmpty()) {
        // We process the block worklist first, because the
        // algorithm converges sometimes faster this way.
        while(blockWorklist_.IsNotEmpty()) {
            auto block = blockWorklist_.RemoveLast();
            inBlockWorklist_.ResetBit(block->Id());
            ProcessBlock(block);
        }

        // Now process the instructions.
        while(instructionWorklist_.IsNotEmpty()) {
            auto instr = instructionWorklist_.RemoveLast();
            inInstructionWorklist_.Remove(instr);
            ProcessInstruction(instr);
        }
    }
}

} // namespace Analysis