// AliasInfo.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_ALIAS_INFO_HPP
#define PC_ANALYSIS_ALIAS_INFO_HPP

#include "AliasAnalyzer.hpp"
#include "TypeInfo.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/DebugValidator.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Instructions.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace CompilationPass;

namespace Analysis {

class AliasInfo : public Pass {
private:
    List<shared<AliasAnalyzer>> analyzers_;
    List<shared<AliasAnalyzer>> callAnalyzers_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns the operand that results after removing all pointer casts.
    // ptop (ptop a, PT1), PT2 -> a
    Operand* WithoutPointerCasts(Operand* op) {
        while(auto ptopInstr = op->DefiningInstrAs<PtopInstr>()) {
            op = ptopInstr->TargetOp();
        }

        return op;
    }

    // Returns the size of the element pointed by the operand.
    __int64 GetPointeeSize(Operand* op) {
        DebugValidator::IsTrue(op->IsPointer());
        auto pointee = op->GetType()->As<PointerType>()->PointeeType();
        return TypeInfo::GetSize(pointee, GetTarget());
    }

    // Various methods for creating alias location objects from operands.
    AliasLocation CreateLocation(Operand* op) {
        auto withoutPtop = WithoutPointerCasts(op);
        return AliasLocation(withoutPtop, 0, GetPointeeSize(op));
    }

    AliasLocation CreateLocationWithUnknownSize(Operand* op) {
        auto withoutPtop = WithoutPointerCasts(op);
        return AliasLocation(withoutPtop, 0, AliasLocation::GetUnknown());
    }

    AliasLocation CreateLocation(Operand* op, __int64 offset, __int64 size) {
        auto withoutPtop = WithoutPointerCasts(op);
        return AliasLocation(withoutPtop, offset, size);
    }

    AliasLocation CreateLocation(LoadInstr* instr) {
        auto withoutPtop = WithoutPointerCasts(instr->SourceOp());
        return AliasLocation(withoutPtop, 0, GetPointeeSize(instr->SourceOp()));
    }

    AliasLocation CreateLocation(StoreInstr* instr) {
        auto withoutPtop = WithoutPointerCasts(instr->DestinationOp());
        return AliasLocation(withoutPtop, 0, GetPointeeSize(instr->DestinationOp()));
    }

public:
    // Registers the specified analyzer.
    // The analyzers are called in the order they were registered.
    void RegisterAnalyzer(shared<AliasAnalyzer> analyzer);

    // Returns the number of registered analyzers.
    int AnalyzerCount() const {
        return analyzers_.Count();
    }

    // Returns the number of registered analyzers
    // that can handle 'call' instructions.
    int CallAnalyzerCount() const {
        return callAnalyzers_.Count();
    }

    // Returns true if at least one analyzer that can handle
    // 'call' instructions was registered.
    bool HasCallAnalyzers() const {
        return callAnalyzers_.Count() > 0;
    }

    // Returns the analyzer found at the specified position.
    shared<AliasAnalyzer> GetAnalyzer(int index) {
        return analyzers_[index];
    }

    //
    AliasResult ComputeAlias(AliasLocation& locationA,
                             AliasLocation& locationB);

    //
    AliasResult ComputeAlias(Operand* opA, Operand* opB) {
        DebugValidator::IsNotNull(opA);
        DebugValidator::IsNotNull(opB);
        
        return ComputeAlias(CreateLocation(opA),
                            CreateLocation(opB));
    }

    AliasResult ComputeAliasWithUnknownSize(Operand* opA, Operand* opB) {
        DebugValidator::IsNotNull(opA);
        DebugValidator::IsNotNull(opB);

        return ComputeAlias(CreateLocationWithUnknownSize(opA),
                            CreateLocationWithUnknownSize(opB));
    }

    //
    AliasResult ComputeAlias(Operand* opA, __int64 offsetA, __int64 sizeA, 
                             Operand* opB, __int64 offsetB, __int64 sizeB) {
        DebugValidator::IsNotNull(opA);
        DebugValidator::IsNotNull(opB);
        
        return ComputeAlias(CreateLocation(opA, offsetA, sizeA),
                            CreateLocation(opB, offsetB, sizeB));
    }

    //
    AliasResult ComputeAlias(LoadInstr* instr, Operand* op) {
        DebugValidator::IsNotNull(instr);
        DebugValidator::IsNotNull(op);
        return ComputeAlias(instr->SourceOp(), op);
    }

    AliasResult ComputeAlias(LoadInstr* instrA, LoadInstr* instrB) {
        DebugValidator::IsNotNull(instrA);
        DebugValidator::IsNotNull(instrB);
        return ComputeAlias(instrA->SourceOp(),
                            instrB->SourceOp());
    }

    //
    AliasResult ComputeAlias(StoreInstr* instr, Operand* op) {
        DebugValidator::IsNotNull(instr);
        DebugValidator::IsNotNull(op);
        return ComputeAlias(instr->DestinationOp(), op);
    }

    //
    AliasResult ComputeAlias(CallInstr* instr, Operand* op) {
        DebugValidator::IsNotNull(instr);
        DebugValidator::IsNotNull(op);
        //return ComputeAlias(instr->SourceOp(), op);
        return Alias_May;
    }

    //
    AliasResult ComputeAlias(StoreInstr* instrA, StoreInstr* instrB) {
        DebugValidator::IsNotNull(instrA);
        DebugValidator::IsNotNull(instrB);
        return ComputeAlias(instrA->DestinationOp(),
                            instrB->DestinationOp());
    }

    //
    AliasResult ComputeAlias(StoreInstr* storeInstr, LoadInstr* loadInstr) {
        DebugValidator::IsNotNull(storeInstr);
        DebugValidator::IsNotNull(loadInstr);
        return ComputeAlias(storeInstr->DestinationOp(),
                            loadInstr->SourceOp());
    }

    AliasResult ComputeAlias(LoadInstr* loadInstr, StoreInstr* storeInstr) {
        DebugValidator::IsNotNull(storeInstr);
        DebugValidator::IsNotNull(loadInstr);
        return ComputeAlias(loadInstr->SourceOp(),
                            storeInstr->DestinationOp());
    }

    //
    bool HasNoAlias(Operand* opA, Operand* opB) {
        return ComputeAlias(opA, opB) == Alias_None;
    }

    bool HasAlias(Operand* opA, Operand* opB) {
        return ComputeAlias(opA, opB) != Alias_None;
    }

    bool HasMustAlias(Operand* opA, Operand* opB) {
        return ComputeAlias(opA, opB) == Alias_Must;
    }

    bool HasMayAlias(Operand* opA, Operand* opB) {
        return ComputeAlias(opA, opB) == Alias_May;
    }

    //
    AliasResult ComputeAlias(Instruction* instr, Operand* op);

    //
    bool ReadsFromGlobalConstant(Operand* op) {
        return false;
    }

    TargetInfo* GetTarget() {
        return Pass::GetTarget();
    }

    // IsPureFunction
};

} // namespace Analysis
#endif