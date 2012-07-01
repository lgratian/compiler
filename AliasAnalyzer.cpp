// AliasAnalyzer.cpp
// Copyright (c) Lup Gratian
//
// Implements the AliasAnalyzer class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "AliasAnalyzer.hpp"

namespace Analysis {

bool AliasAnalyzer::RangesOverlap(AliasLocation& locationA, 
                                  AliasLocation& locationB) {
    DebugValidator::IsTrue(locationA.HasKnownSize());
    DebugValidator::IsTrue(locationB.HasKnownSize());

    __int64 lowA = locationA.Offset();
    __int64 highA = lowA + locationA.Size();
    __int64 lowB = locationB.Offset();
    __int64 highB = lowB + locationB.Size();
    
    return (lowA < highB) && (highA > lowB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::IsAllocCall(CallInstr* instr) {
    DebugValidator::IsNotNull(instr);

    // We consider allocation functions to be functions
    // marked as acting like 'malloc' and calls
    // to the stack allocation intrinsic 'stackTop'.
    if(auto calledFunction = instr->GetCalledFunction()) {
        if(auto tag = calledFunction->GetTag<CFamilyTag>()) {
            return tag->IsAllocLike() || 
                   tag->IsCheckedAllocLike();
        }

        return calledFunction->As<StackTopIntr>() != nullptr;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* AliasAnalyzer::GetBaseOperand(Operand* op) {
    if(auto indexInstr = op->DefiningInstrAs<IndexInstr>()) {
        return GetBaseOperand(indexInstr->BaseOp());
    }
    else if(auto addrInstr = op->DefiningInstrAs<AddressInstr>()) {
        return GetBaseOperand(addrInstr->BaseOp());
    }
    else if(auto elemInstr = op->DefiningInstrAs<ElementInstr>()) {
        return GetBaseOperand(elemInstr->BaseOp());
    }
    else if(auto ptopInstr = op->DefiningInstrAs<PtopInstr>()) {
        return GetBaseOperand(ptopInstr->TargetOp());
    }
    else return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::BaseIsVariable(Operand* op, VariableReference** variableRef) {
    if(auto varRef = GetBaseOperand(op)->As<VariableReference>()) {
        if(variableRef) {
            *variableRef = varRef;
        }

        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::OriginatesFromAlloc(Operand* op, AllocCallList* allocList, int level) {
    DebugValidator::IsNotNull(op);

    // Don't recourse too much.
    if(level > 3) {
        return false;
    }

    // Look through addressing instructions and pointer casts.
    auto baseOp = GetBaseOperand(op);

    // Check if we have a call to an allocation function.
    if(auto callInstr = baseOp->DefiningInstrAs<CallInstr>()) {
        bool isAlloc = IsAllocCall(callInstr);

        // Add the call to the list if desired.
        if(isAlloc && allocList) {
            if(allocList->Contains(callInstr) == false) {
                allocList->Add(callInstr);
            }
        }

        return isAlloc;
    }

    if(auto phiInstr = baseOp->DefiningInstrAs<PhiInstr>()) {
        // It's unlikely that all incoming operands of a large 'phi'
        // originate from an allocation call.
        if(phiInstr->OperandCount() > 4) {
            return false;
        }

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            if(OriginatesFromAlloc(phiInstr->GetOperand(i), 
                                   allocList, level + 1) == false) {
                return false;
            }
        }

        return true;
    }
    else if(auto questInstr = baseOp->DefiningInstrAs<QuestionInstr>()) {
        return OriginatesFromAlloc(questInstr->TrueOp(), allocList, level + 1) &&
               OriginatesFromAlloc(questInstr->FalseOp(), allocList, level + 1);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::AreAllocCallsIndependent(AllocCallList* listA, 
                                             AllocCallList* listB) {
    DebugValidator::IsNotNull(listA);
    DebugValidator::IsNotNull(listB);

    // Because we usually have only one/two calls in each list
    // we do a quadratic search.
    for(int i = 0; i < listB->Count(); i++) {
        if(listA->Contains((*listB)[i])) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::IsAddressNotTaken(Operand* op, VariableReference** variableRef) {
    DebugValidator::IsNotNull(op);

    // We look through address instructions until we find
    // the variable that acts as the base.
    auto baseOp = GetBaseOperand(op);

    if(auto varRef = baseOp->As<VariableReference>()) {
        if(varRef->IsAddressTaken() == false) {
            if(variableRef) {
                *variableRef = varRef;
            }

            return true;
        }
    }

    // Presume the address may be taken.
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::IsAddressNotTaken(AliasLocation location, 
                                      VariableReference** variableRef) {
    if(location.AddressTaken() != AddressTaken_Unknown) {
        return location.AddressTaken() == AddressTaken_No;
    }

    bool isNotTaken = IsAddressNotTaken(location.Base());
    location.SetAddressTaken(isNotTaken ? AddressTaken_No : AddressTaken_Yes);
    return isNotTaken;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::IsUnion(const RecordType* recordType) {
    DebugValidator::IsNotNull(recordType);

    // Check if two adjacent fields have the same offset.
    for(int i = 1; i < recordType->FieldCount(); i++) {
        if(recordType->GetFieldOffset(i - 1) ==
           recordType->GetFieldOffset(i)) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::IsNonAggregatePointer(Operand* op) {
    op = GetBaseOperand(op);
    
    if(op->DefiningInstrIs<LoadInstr>() || 
       op->DefiningInstrIs<CallInstr>()) {
        return op->GetType()->IsPointer();
    }
    else return op->IsParameter() && 
                op->GetType()->IsPointer();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::AreParametersIndependent(Parameter* parameterA, 
                                             Parameter* parameterB) {
    if(parameterA == parameterB) {
        return false;
    }

    // If both parameters are marked 'restrict' we know 
    // that the programmer guarantees that they will never
    // point to the same memory locations.
    if(parameterA->IsRestrict() && parameterB->IsRestrict()) {
        return true;
    }

    //! TODO: consider interprocedural analysis result.

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* AliasAnalyzer::WithoutPointerCasts(Operand* op) {
    while(auto ptopInstr = op->DefiningInstrAs<PtopInstr>()) {
        op = ptopInstr->TargetOp();
    }

    return op;
}

} // namespace Analysis