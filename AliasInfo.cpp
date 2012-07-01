// AliasInfo.cpp
// Copyright (c) Lup Gratian
//
// Implements the AliasInfo class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "AliasInfo.hpp"

namespace Analysis {

void AliasInfo::RegisterAnalyzer(shared<AliasAnalyzer> analyzer) {
    DebugValidator::IsNotNull(analyzer.Raw());
    DebugValidator::IsFalse(analyzers_.Contains(analyzer));

    // Add the analyzer to the list, and to the list of analyzers 
    // able to handle 'call' instructions, if it's the case.
    analyzers_.Add(analyzer);

    if(analyzer->HandlesCalls()) {
        callAnalyzers_.Add(analyzer);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult AliasInfo::ComputeAlias(AliasLocation& locationA,
                                    AliasLocation& locationB) {
    // Query the available analyzers.
    for(int i = 0; i < analyzers_.Count(); i++) {
        // We stop as soon as we get a result that is not "May";
        auto result = analyzers_[i]->ComputeAlias(locationA, locationB);

        if(result != Alias_May) {
            return result;
        }
    }

    return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult AliasInfo::ComputeAlias(Instruction* instr, Operand* op) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(op);

    // First remove the pointer casts, else we might incorrectly
    // assume that an instruction doesn't access memory when it does.
    if(auto ptopInstr = instr->As<PtopInstr>()) {
        auto withoutPtop = WithoutPointerCasts(ptopInstr->TargetOp());

        // If the operand is not an instruction we compute the alias
        // between the two operands.
        if(withoutPtop->HasDefiningInstruction() == false) {
            return ComputeAlias(withoutPtop, op);
        }
        else instr = withoutPtop->DefiningInstruction();
    }

    if(auto loadInstr = instr->As<LoadInstr>()) {
        return ComputeAlias(loadInstr, op);
    }
    else if(auto storeInstr = instr->As<StoreInstr>()) {
        return ComputeAlias(storeInstr, op);
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        return ComputeAlias(callInstr, op);
    }

    // Any other instruction can't access memory.
    return Alias_None;
}


} // namespace Analysis