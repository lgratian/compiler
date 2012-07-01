// SimpleAliasAnalysis.cpp
// Copyright (c) Lup Gratian
//
// Implements the SimpleAliasAnalysis class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "SimpleAliasAnalysis.hpp"

namespace Analysis {

AliasResult SimpleAliasAnalysis::ComputeAlias(AliasLocation locationA, 
                                              AliasLocation locationB) {
    // Check the case when the base operands are the same.
    if(locationA.Base() == locationB.Base()) {
        // If the locations have the same base operand and offset
        // we definitely have a "Must" alias.
        if(locationA.Offset() == locationB.Offset()) {
            return Alias_Must;
        }
        else if(locationA.HasKnownSize() && locationB.HasKnownSize() &&
                (RangesOverlap(locationA, locationB) == false)) {
            return Alias_None;
        }

        return Alias_Must;
    }

    // Simple rules that apply for variables.
    auto result = ComputeVariableAlias(locationA, locationB);

    if(result != Alias_May) {
        return result;
    }

    // Simple rules that apply for at least one parameter.
    result = ComputeParameterAlias(locationA, locationB);
    
    if(result != Alias_May) {
        return result;
    }

    // Simple rules that apply for array/record access.
    return ComputeAggregateAlias(locationA, locationB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MemoryResult SimpleAliasAnalysis::ComputeCallAlias(CallInstr* instr, 
                                                   AliasLocation location) {
    // Here we handle calls to built-in intrinsics.
    if(auto intrinsic = instr->GetIntrinsic()) {
        // Math/bitwise/stack/prefetch intrinsics don't access memory at all.
        if(intrinsic->IsMathIntrinsic()    ||
           intrinsic->IsBitwiseIntrinsic() ||
           intrinsic->IsStackIntrinsic()   ||
           intrinsic->Is<PrefetchIntr>()) {
            return MemoryResult::GetNone();
        }
        else if(intrinsic->Is<SetMemoryIntr>() ||
                intrinsic->Is<CopyMemoryIntr>()) {
            // Aliasing depends only on the destination location
            // and on the source location for 'copyMemory'.
            __int64 size = AliasLocation::GetUnknown();

            if(auto intConst = instr->GetArgument(2)->As<IntConstant>()) {
                // If a constant number of bytes is copied
                // we might get a better result. For example, if we have
                // int a[8]; memset(a, 0, 4); a[2] = 1;
                // the 'memset' and 'a[2]' don't alias.
                size = intConst->Value();
            }

            auto result = Parent()->ComputeAlias(instr->GetArgument(0), 0, size,
                                                 location.Base(), location.Offset(), 
                                                 location.Size());
            if(result != Alias_None) {
                return MemoryResult::GetParametersWrite();
            }
            else if(intrinsic->Is<SetMemoryIntr>()) {
                return MemoryResult::GetNone();
            }

            // The 'copyMemory' intrinsic might read from the location.
            result = Parent()->ComputeAlias(instr->GetArgument(1), 0, size,
                                            location.Base(), location.Offset(), 
                                            location.Size());
            if(result != Alias_None) {
                return MemoryResult::GetParametersRead();
            }
            else return MemoryResult::GetNone();
        }
    }

    // Presume the call might modify the location.
    return MemoryResult::GetUnknown();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeSingleVariableAlias(VariableReference* variableRef, 
                                                            Operand* other) {
    if(other->IsParameter()) {
        // A local variable can't alias a parameter, even if we have
        // recursive calls (the variable has multiple "instances" on the stack).
        if(variableRef->IsLocalVariableRef()) {
            return Alias_None;
        }

        // A global variable can't alias a parameter if it's marked
        // as being non-address-taken.
        if(variableRef->IsGlobalVariableRef() && 
           (variableRef->IsAddressTaken() == false)) {
               return Alias_None;
        }
    }

    // A variable can't alias the address returned
    // by a call to a memory allocation routine ('malloc', for example).
    if(OriginatesFromAlloc(other)) {
        return Alias_None;
    }

    return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeVariableAlias(AliasLocation& locationA, 
                                                      AliasLocation& locationB) {
    // If both base operands are local/global variables we can determine
    // the alias relation of the locations. Note that the offsets
    // don't matter because no language standard guarantees an order
    // in memory for the variables, making it (theoretically) impossible
    // to access a variable knowing the address of another one.
    auto variableRefA = locationA.Base()->As<VariableReference>();
    auto variableRefB = locationB.Base()->As<VariableReference>();

    if(variableRefA && variableRefB) {
        // If the variables are different there can't be any alias
        // under standard-conforming code. This also handles the case
        // of local/global variables.
        if(variableRefA != variableRefB) {
            return Alias_None;
        }

        // Even if the variable are the same, if the accessed regions
        // don't overlap then we have no alias.
        if(locationA.HasKnownSize() && locationB.HasKnownSize()) {
            if(RangesOverlap(locationA, locationB)) {
                return Alias_Must;
            }
            else return Alias_None;
        }

        return Alias_May;
    }

    // If only one of the base operand is a variable, depending
    // on the other operand we might conclude there is no alias.
    auto variableRef = variableRefA ? variableRefA : variableRefB;
    auto other = variableRefA ? locationB.Base() : locationA.Base();

    if(variableRef) {
        return ComputeSingleVariableAlias(variableRef, other);
    }
        
    return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeParameterAlias(AliasLocation& locationA, 
                                                       AliasLocation& locationB) {
    // If both base operands are parameters marked 'restrict'
    // we know that the programmer guarantees that they will never
    // point to the same memory locations.
    auto parameterA = locationA.Base()->As<Parameter>();
    auto parameterB = locationB.Base()->As<Parameter>();

    if(parameterA && parameterB) {
        if(AreParametersIndependent(parameterA, parameterB)) {
            return Alias_None;
        }
    }

    // If one of the base operand is a parameter and the other one
    // is a global constant we know we have no alias
    // (the parameter cannot point to the global constant,
    //  otherwise the variable would have not been considered a constant).
    auto parameter = parameterA ? parameterA : parameterB;
    auto other = parameterA ? locationB.Base() : locationA.Base();

    if(parameter) {
        if(Parent()->ReadsFromGlobalConstant(other)) {
            return Alias_None;
        }
    }

    return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeAggregateAlias(AliasLocation& locationA, 
                                                       AliasLocation& locationB) {
    // If both locations originate from different non-address-taken 
    // aggregates we can't have any alias.
    VariableReference* variableRefA;
    VariableReference* variableRefB;
    bool notAddressTakenA = IsAddressNotTaken(locationA.Base(), &variableRefA);
    bool notAddressTakenB = IsAddressNotTaken(locationB.Base(), &variableRefB);

    if(notAddressTakenA && notAddressTakenB) {
        if(variableRefA != variableRefB) {
            return Alias_None;
        }

        // If the variables are the same we're sure there is no alias
        // if a location is a record field and the other an array element.
        // Note that the record must not be a C 'union'.
        auto indexInstr = locationA.Base()->DefiningInstrIs<IndexInstr>() ?
                          locationA.Base()->DefiningInstrAs<IndexInstr>() :
                          locationB.Base()->DefiningInstrAs<IndexInstr>();
        auto elemInstr = locationA.Base()->DefiningInstrIs<ElementInstr>() ?
                         locationA.Base()->DefiningInstrAs<ElementInstr>() :
                         locationB.Base()->DefiningInstrAs<ElementInstr>();

        if((indexInstr && elemInstr) &&
            (IsUnion(elemInstr->GetRecordType()) == false)) {
            return Alias_None;
        }
    }

    // If one of the location is a non-address-taken variable and
    // the other one is a non-aggregate pointer we have no alias
    // (it is not possible for the pointer to point to any part
    //  of the non-address-taken variable, otherwise it would have 
    //  been marked as address-taken).
    if((notAddressTakenA && IsNonAggregatePointer(locationB.Base())) ||
       (notAddressTakenB && IsNonAggregatePointer(locationA.Base()))) {
        return Alias_None;
    }

    return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeAllocAlias(AliasLocation& locationA, 
                                                   AliasLocation& locationB) {
    // Check if the locations originated from different
    // dynamic allocation function calls. Example:
    // RECORD* p1 = (RECORD*)malloc(sizeof(RECORD));
    // RECORD* p2 = (RECORD*)malloc(sizeof(RECORD));
    // 'p1->a' does not alias 'p2->a' (we're allowed to presume
    // the memory allocator is not faulty).
    auto baseA = GetBaseOperand(locationA.Base());
    auto baseB = GetBaseOperand(locationB.Base());

    List<CallInstr*> callsA;
    List<CallInstr*> callsB;
    bool fromAllocA = OriginatesFromAlloc(baseA, &callsA);
    bool fromAllocB = OriginatesFromAlloc(baseB, &callsB);

    if(fromAllocA && fromAllocB) {
        if(AreAllocCallsIndependent(&callsA, &callsB)) {
            return Alias_None;
        }

        // Even if the calls are not independent there is no alias
        // if the accessed ranges do not overlap.
        if(locationA.HasKnownSize() && locationB.HasKnownSize()) {
           if(RangesOverlap(locationA, locationB)) {
                return Alias_Must;
           }
           else return Alias_None;
        }
    }

    return Alias_May;
}

} // namespace Analysis