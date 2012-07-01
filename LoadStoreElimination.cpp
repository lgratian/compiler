// LoadStoreElimination.cpp
// Copyright (c) Lup Gratian
//
// Implements the Basic Load/Store Elimination pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "LoadStoreElimination.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

void LoadStoreElimination::Execute(Function* function) {
    DebugValidator::IsNotNull(function);
    funct_ = function;

    // We try to eliminate redundant 'load' and 'store' instructions
    // from all the blocks in the function.
    for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
        EliminateRedundantLoads(block);
    }

    for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
        EliminateDeadStores(block);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::CallMayWriteToAddress(CallInstr* callInstr, Operand* addressOp) {
    // We presume that all called functions may write to memory, unless
    // it's an intrinsic or well-known standard library function,
    // or we are certain that the function definitely doesn't write to memory.
    auto functionRef = callInstr->TargetOp()->As<FunctionReference>();

    // If we are not calling the function directly we don't know anything
    // about it, so we presume it writes to memory.
    if(functionRef == nullptr) {
        // We may still be able to prove that the call doesn't use 
        // the address if we're dealing with a local variable.
        return IsAddressUsedByCall(callInstr, addressOp);
    }

    // If the callee is marked as 'nowrite' then we're done.
    if(functionRef->Target()->IsNoWrite()) {
        return true;
    }

    // A local variable that has its address not taken
    // cannot be modified by a call.
    if(auto variableRef = addressOp->As<VariableReference>()) {
        if(variableRef->IsLocalVariableRef() && IsAddressNotTaken(addressOp)) {
            return false;
        }
    }

    // Test for intrinsics. All "memory" intrinsics write to memory,
    // but we may prove they definitely write do a different variable
    // than the one from which we load.
    if(auto intrinsic = functionRef->Target()->As<Intrinsic>()) {
        return IntrinsicMayWriteToAddress(intrinsic, callInstr, addressOp);
    }

    // Check if this is a call to a well-known standard library function
    // (important especially for math functions).
    if(auto info = GetLanguageInfo()) {
        if(info->CallMayWriteToAddress(callInstr, addressOp, this) == false) {
            return false;
        }
    }

    // Assume the worst case.
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::IntrinsicMayWriteToAddress(Intrinsic* intrinsic, 
                                                      CallInstr* callInstr, 
                                                      Operand* addressOp) {
    // All "memory" intrinsics write to memory,
    // but we may prove they definitely write do a different variable
    // than the one from which we load.
     if(auto memIntr = intrinsic->As<MemoryIntrinsic>()) {
        // The 'prefetch' intrinsic doesn't affect program memory.
        if(memIntr->IsPrefetchIntr()) {
            return false;
        }

        // Check if both are references to different variables.
        if(AreReferencesDifferent(addressOp, callInstr->GetArgument(0))) {
            return false;
        }

        return true;
    }
    else if(intrinsic->DoesWriteToMemory() == false) {
        // The intrinsic guarantees that it doesn't write to user memory.
        return false;
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::IsAddressUsedByCall(CallInstr* callInstr, Operand* addressOp) {
    // If the local variable has it's address NOT taken,
    // there is no way it could be modified by another function.
    if(IsAddressNotTaken(addressOp)) {
        return false;
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::AreReferencesEqual(Operand* opA, Operand* opB) {
    auto variableRefA = opA->As<VariableReference>();
    auto variableRefB = opB->As<VariableReference>();

    // Both operands must be variable references.
    if((variableRefA && variableRefB) == false) {
        return false;
    }

    // Check if both are references to different variables.
    // Because references are unique it's enough to do this simple test.
    return variableRefA == variableRefB;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::AreReferencesDifferent(Operand* opA, Operand* opB) {
    // Try to disambiguate array/record accesses. For example,
    // 'a[4]' and 'a[5]' definitely access different memory locations,
    // but so does 'a[i]' and 'a[i + 1]' and 'a[i]' and 'b[i]'.
    // Note that for all the above cases we request that the address
    // of the arrays/records should not be taken.
    VariableReference* variableRefA;
    VariableReference* variableRefB;

    if(IsAddressNotTaken(opA, &variableRefA)) {
       if(IsAddressNotTaken(opB, &variableRefB)) {
            return AreArrayRecordRefsDifferent(opA, opB, variableRefA, variableRefB);
       }
       else if(variableRefB = WithoutPointerCasts(opB)->As<VariableReference>()) {
           if((variableRefB->IsAddressTaken() == false) &&
              (variableRefA != variableRefB)) {
              return true;
           }
       }
    }
    else if(IsAddressNotTaken(opB, &variableRefB)) {
        if(variableRefA = WithoutPointerCasts(opA)->As<VariableReference>()) {
           if((variableRefA->IsAddressTaken() == false) &&
              (variableRefB != variableRefA)) {
                return true;
           }
        }
    }

    // Check if we have pointers computed by 'addr' instructions.
    if(ArePointersDifferent(opA, opB)) {
        return true;
    }

    // Check if we work with variable references.
    variableRefA = WithoutPointerCasts(opA)->As<VariableReference>();
    variableRefB = WithoutPointerCasts(opB)->As<VariableReference>();

    // Both operands must be variable references.
    if((variableRefA && variableRefB) == false) {
        return false;
    }

    // Check if both are references to different variables.
    return variableRefA != variableRefB;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::ArePointersDifferent(Operand* opA, Operand* opB) {
    // Try to disambiguate accesses involving pointers. Because we don't have
    // alias information, we restrict the tests to the following:
    // - the base of the 'addr' is the same
    // - the base, in both cases, is a parameter marked 'restrict'
    //   (the programmer guarantees that the pointed locations are independent).
    // - the base, in both cases, is a global variable marked 'constant'.
    auto baseOpA = GetBaseOperand(opA);
    auto baseOpB = GetBaseOperand(opB);

    // 'index' not supported here, and if both operands are defined by instructions,
    // then the instructions should have the same opcode.
    if(opA->DefiningInstrIs<IndexInstr>() ||
       opB->DefiningInstrIs<IndexInstr>()) {
        return false;
    }
    else if(opA->HasDefiningInstruction() && 
            opB->HasDefiningInstruction() &&
           (opA->DefiningInstruction()->SameKind(opB->DefiningInstruction()) == false)) {
        return false;
    }

    if(baseOpA && baseOpB) {
        // We have two 'addr' instructions; check the rules stated above.
        bool valid = false;

        if(baseOpA == baseOpB) {
            valid = true;
        }
        else if(auto paramA = baseOpA->As<Parameter>()) {
            if(auto paramB = baseOpB->As<Parameter>()) {
				// Only if marked 'restrict'.
                valid = paramA->IsRestrict() && paramB->IsRestrict();
            }
        }
        else if(auto variableRefA = baseOpA->As<VariableReference>()) {
            if(auto variableRefB = baseOpB->As<VariableReference>()) {
                auto globalA = variableRefA->GetGlobal();
                auto globalB = variableRefB->GetGlobal();

                // No one is able to write to constant globals
                // that have array or record type. Note that we don't allow
                // pointers, because even if they're constant, they may still
                // point to the same location.
                valid = globalA && globalB && 
                        globalA->IsConstant() && globalB->IsConstant() &&
                        (globalA->IsArray() || globalA->IsRecord()) &&
                        (globalB->IsArray() || globalB->IsRecord());
            }
        }

        if(valid) {
            auto addrInstrA = static_cast<AddressInstr*>(opA->DefiningInstruction());
            auto addrInstrB = static_cast<AddressInstr*>(opB->DefiningInstruction());

            if(addrInstrA->IsAddress()) {
                return AreArrayRefsDifferent(addrInstrA, addrInstrB,
                                             baseOpA, baseOpB, 1 /* level */);
            }
            else return AreFieldsDifferent(addrInstrA->As<ElementInstr>(),
                                           addrInstrB->As<ElementInstr>(),
                                           baseOpA, baseOpB);
        }
    }
    else if(baseOpA || baseOpB) {
        // Only one of the operands is defined by an 'addr' instruction.
        // Handle the case when the base operand is the same as the other one.
        // store @p, 5     store (addr @p, 1), 6
        auto baseOp = baseOpA ? baseOpA : baseOpB;
        auto otherOp = baseOpA ? opB : opA;

        if(baseOp == otherOp) {
            // 'addr p, 1' and 'p' don't interfere, but for
            // 'addr p, a' and 'p' we must presume they do, because 'a' might be 0.
            auto indexOp = baseOpA ? opA->DefiningInstrAs<AddressInstr>()->IndexOp() :
                                     opB->DefiningInstrAs<AddressInstr>()->IndexOp();
            if(indexOp == nullptr) {
                return false; // 'elem' not valid here.
            }
            else return indexOp->IsConstant() && 
                       (indexOp->IsZeroInt() == false);
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::AreArrayRecordRefsDifferent(Operand* opA, Operand* opB,
                                                       Operand* baseOpA, Operand* baseOpB, 
                                                       int level) {
    auto instrA = opA->DefiningInstruction();
    auto instrB = opB->DefiningInstruction();
    
    if(((instrA && instrB) == false) || (level == 0)) {
        // References to different local variables 
        // can't access the same memory location.
        return baseOpA != baseOpB;
    }

    // If we have a combination of array and record access,
    // we know they target different memory locations.
    if(instrA->SameKind(instrB) == false) {
        return true;
    }

    if(auto indexInstrA = instrA->As<IndexInstr>()) {
        auto indexInstrB = instrB->As<IndexInstr>();

        // We check for the following cases:
        // 1. same base, constant index (&a[5] != &a[6])
        // 2. same base, variable index (&a[i] != &a[i + 1])
        // 3. different base (a[i] != b[i])
        auto intConstA = indexInstrA->IndexOp()->As<IntConstant>();
        auto intConstB = indexInstrB->IndexOp()->As<IntConstant>();

        if(intConstA && intConstB) {
            // If the constant is the same we're not sure if the referenced
            // locations are different, but we can use the result
            // from the base operands (consider 'a[1][2]' and 'a[3][2]').
            if(intConstA != intConstB) {
                return true;
            }
            else return AreArrayRecordRefsDifferent(indexInstrA->BaseOp(),
                                                    indexInstrB->BaseOp(),
                                                    baseOpA, baseOpB, level - 1);
        }

        // Test for more complicated cases.
        return AreArrayRefsDifferent(indexInstrA, indexInstrB, 
                                     baseOpA, baseOpB, level);
    }
    else if(auto elemInstrA = instrA->As<ElementInstr>()) {
        auto elemInstrB = instrB->As<ElementInstr>();
        return AreFieldsDifferent(elemInstrA, elemInstrB, 
                                  baseOpA, baseOpB, level);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::AreArrayRefsDifferent(AddressInstr* addrInstrA, 
                                                 AddressInstr* addrInstrB, 
                                                 Operand* baseOpA, Operand* baseOpB, 
                                                 int level) {
    // Test for cases like 'a[i +/- C1]' and 'a[i +/- C1]'.
    // If the operations are different ('+/-' or '-/+'), we know
    // the resulting addresses cannot be equal.
    // If the operations are the same we require the constants to be different.
    bool different = false;
    auto instrA = addrInstrA->IndexOp()->DefiningInstruction();
    auto instrB = addrInstrB->IndexOp()->DefiningInstruction();

    if((instrA && (instrA->IsAdd() || instrA->IsSub()) == false) ||
       (instrB && (instrB->IsAdd() || instrB->IsSub()) == false)) {
        return false; // Only 'add' and 'sub' supported.
    }

    // Canonicalize 'C + i' to 'i + C', so we have fewer cases to test for.
    if(instrA && instrA->IsAdd() && 
       instrA->GetSourceOp(0)->IsConstant()) {
        instrA->As<AddInstr>()->SwapOperands();
    }

    if(instrB && instrB->IsAdd() && 
       instrB->GetSourceOp(0)->IsConstant()) {
        instrB->As<AddInstr>()->SwapOperands();
    }

    // Make sure that the instructions have a constant operand on the right side.
    // We don't know enough about the values to handle cases like 'a[i + j]'.
    if(instrA && instrA->GetSourceOp(1)->IsConstant() == false) {
        return false;
    }
    else if(instrB && instrB->GetSourceOp(1)->IsConstant() == false) {
        return false;
    }

    // Make sure that the left operands are the same. Again, we don't know
    // enough about the values to handle cases like 'a[i + 1]' and 'a[j + 2]'.
    if(instrA && instrB && 
       (instrA->GetSourceOp(0) != instrB->GetSourceOp(0))) {
        return AreArrayRecordRefsDifferent(addrInstrA->BaseOp(),
                                           addrInstrB->BaseOp(), 
                                           baseOpA, baseOpB, level - 1);
    }

    if(instrA && instrB) {
        if(instrA->SameKind(instrB) == false) {
            // 'a[i + C1]' and 'a[i - C2].
            // Different if 'i + C1 != i - C2 -> C1 != -C2'
            if(instrB->IsSub()) {
                auto C1 = instrA->GetSourceOp(1)->As<IntConstant>();
                auto C2 = instrB->GetSourceOp(1)->As<IntConstant>();
                different = C1->Value() != -C2->Value();
            }
            // 'a[i - C1]' and 'a[i + C2].
            // Different if 'i - C1 != i + C2 -> -C1 != C2'
            else {
                auto C1 = instrA->GetSourceOp(1)->As<IntConstant>();
                auto C2 = instrB->GetSourceOp(1)->As<IntConstant>();
                different = -C1->Value() != C2->Value();
            }
        }
        else {
            // The operations is the same. To be different, the constants
            // involved need to be different.
            different = instrA->GetSourceOp(1) != 
                        instrB->GetSourceOp(1);
        }
    }
    else {
        // We have something like 'a[i]' and 'a[i + 1]' or 'a[i]' and 'a[i]'.
        // Note that we cannot handle 'a[i + 1]' and 'a[j]', for example.
        auto instr = instrA ? instrA : instrB;
        auto other = instrA ? addrInstrB->IndexOp() : 
                              addrInstrA->IndexOp();

        if(instr == nullptr) {
            // We don't know anything about the values of 'i' and 'j' in
            // '&a[i] != &a[j]', so we need to presume that they may be equal.
            different = false;
        }
        else if(instr->GetSourceOp(0) == other) {
            // The memory location is different if the constant is not 0.
            different = instr->GetSourceOp(1)->IsZeroInt() == false;
        }
    }

    // If not different yet try to use the result from the base operands.
    if(different) {
        return true;
    }
    else return AreArrayRecordRefsDifferent(addrInstrA->BaseOp(),
                                            addrInstrB->BaseOp(),
                                            baseOpA, baseOpB, level - 1);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::AreFieldsDifferent(ElementInstr* elemInstrA, 
                                              ElementInstr* elemInstrB,
                                              Operand* baseOpA, Operand* baseOpB, 
                                              int level) {
    if(elemInstrA->GetRecordType() == elemInstrB->GetRecordType()) {
        // Test based on the offset in the record, because we may have 
        // an "union", where all fields start at offset 0.
        auto fieldA = elemInstrA->GetField();
        auto fieldB = elemInstrB->GetField();
        
        if(fieldA.FieldOffset != fieldB.FieldOffset) {
            // Definitely different memory locations.
            return true;
        }
        else {
            // Try to see if the base operands are different,
            // then the accessed memory locations are also different,
            // even if the field is the same.
            return AreArrayRecordRefsDifferent(elemInstrA->BaseOp(), 
                                               elemInstrB->BaseOp(),
                                               baseOpA, baseOpB, level - 1);
        }
    }

    // We may have a record that is nested into the other. For example,
    // record ABC {             record DEF {             
    //    int32 offset(0)          int32 offset(0)       var p <DEF>
    //    int32 offset(4)          <ABC> offset(4)
    // }                        }
    // 'elem p, 0' and 'elem (elem p, 1), 0' are independent.
    if(auto ancestorA = GetAncestor(elemInstrA, elemInstrB)) {
        return AreFieldsDifferent(ancestorA, elemInstrB, 
                                  baseOpA, baseOpB, level);
    }
    else if(auto ancestorB = GetAncestor(elemInstrB, elemInstrA)) {
        return AreFieldsDifferent(ancestorB, elemInstrA,
                                  baseOpA, baseOpB, level);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ElementInstr* LoadStoreElimination::GetAncestor(ElementInstr* elemInstrA, 
                                                ElementInstr* elemInstrB) {
    // Test if 'elemInstrB' is an ancestor for 'elemInstrA'.
    while(elemInstrA) {
        if(elemInstrA->BaseOp() == elemInstrB->BaseOp()) {
            return elemInstrA;
        }
        else elemInstrA = elemInstrA->BaseOp()->DefiningInstrAs<ElementInstr>();
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* LoadStoreElimination::FindNearestSource(Instruction* startInstr, 
                                                 Operand* addressOp, bool& notFound) {
    Instruction* instr = startInstr;

    // Scan backwards in the block, until we find the last instruction,
    // or we find a 'phi' instruction. We stop when we find a 'phi' because
    // all the other instructions before it are also 'phi'.
    while(instr) {
        if(auto storeInstr = instr->As<StoreInstr>()) {
            // We found a 'store'; if it stores exactly to the address from
            // which we load we can reuse the stored operand instead of reloading it.
            if(IsSameAddress(addressOp, storeInstr->DestinationOp())) {
                return storeInstr->SourceOp();
            }
            
            // We can't reuse the value, but we may continue scanning the block
            // if we prove that this 'store' definitely writes to a variable that is
            // not the one addressed by 'addressOp'.
            if(MayStoreWriteToAddress(storeInstr, addressOp) == false) {
                instr = instr->PreviousInstruction();
                continue;
            }

            notFound = false; // Mark clobbering.
            return nullptr;
        }
        else if(auto loadInstr = instr->As<LoadInstr>()) {
            // Check if this loads from the same address; if it does, we can
            // reuse the loaded operand instead of reloading it.
            if(IsSameAddress(addressOp, loadInstr->SourceOp())) {
                return loadInstr->ResultOp();
            }
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            // Special case for 'setMemory' calls. We might be able
            // to determine that the entire read memory is set here.
            if(callInstr->IntrinsicIs<SetMemoryIntr>()) {
                if(auto value = ExtractFromSetMemory(callInstr, addressOp)) {
                    return value;
                }
            }

            // By default we presume that calls write to memory arbitrary values,
            // but we may prove that this call does not, in which case we can continue.
            if(CallMayWriteToAddress(callInstr, addressOp)) {
                notFound = false;
                return nullptr;
            }
        }
        else if(instr->IsPhi()) {
            // No reasons to continue.
            break;
        }

        instr = instr->PreviousInstruction();
    }

    // No suitable 'store' or 'load' found.
    notFound = true;
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* LoadStoreElimination::ExtractFromSetMemory(CallInstr* instr, Operand* addressOp) {
    // To be able to eliminate a 'load' from a location is target by a 'setMemory'
    // we need to have a complete overlapping:
    // - the base operand should be the same
    // - the offset of the 'setMemory' should be <= the offset of the loaded location
    // - the length of the 'setMemory' should be >= the size of the loaded location
    __int64 setOffset;
    auto destinationOp = SetMemoryIntr::GetDestination(instr);
    auto setBaseOp = ExtractBaseAndOffset(destinationOp, setOffset);

    if(setBaseOp == nullptr) {
        return nullptr;
    }

    // Try to obtain the base and offset of the loaded location.
    __int64 loadOffset;
    auto loadBaseOp = ExtractBaseAndOffset(addressOp, loadOffset);

    if(setBaseOp != loadBaseOp) {
        return nullptr;
    }

    // Check if the offset is valid.
    if(setOffset > loadOffset) {
        return nullptr;
    }

    // Check if the set region covers the entire loaded location.
    auto pointeeType = addressOp->GetType()->As<PointerType>()->PointeeType();
    __int64 loadSize = GetSize(pointeeType);

    if(IsSetMemoryOfSize(instr, setOffset, loadOffset, loadSize)) {
        // Create the loaded value by merging 'loadSize' 
        // instances of the set value.
        return GetIntConstant(0, IntegerType::GetInt32());
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::IsSetMemoryOfSize(CallInstr* instr, __int64 setOffset, 
                                             __int64 loadOffset, __int64 loadSize) {
    // We need to check if the size of the set region is at least as large
    // as the loaded location. The following expression must be true:
    // setOffset + setLength >= loadOffset + loadSize
    // (note that we already know that 'setOffset <= loadOffset').
    auto lengthOp = SetMemoryIntr::GetLength(instr);

    if(auto intConst = lengthOp->As<IntConstant>()) {
        return (setOffset + intConst->Value()) >=
               (loadOffset + loadSize);
    }

    // Even if the length of the set region is not a constant we might be able
    // to tell that at least 'loadSize' bytes are set using Operand Information.
    __int64 requiredLength = (loadOffset - setOffset) + loadSize;
    auto requiredConst = GetIntConstant(requiredLength, IntegerType::GetInt64());
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());

    return opInfo.IsGreaterOrEqual(lengthOp, requiredConst) == Result_Yes;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::MayStoreWriteToAddress(StoreInstr* storeInstr, 
                                                  Operand* addressOp) {
    // Check if the 'store' definitely writes to a variable that is
    // not the one addressed by 'addressOp'.
    if(AreReferencesDifferent(addressOp, storeInstr->DestinationOp())) {
        return false;
    }

    // If we're loading from a local array/record that has it's address NOT taken,
    // we can't write over the location indicated by 'addressOp'
    // by writing through a pointer. Consider this simple example:
    // int a[10]; ... *p = 5; -> 'a' can be modified only if 'p = a + ?'.
    if(IsAddressNotTaken(addressOp) && 
       StoresIntoPointer(storeInstr->DestinationOp())) {
        return false;
    }

    // If 'addressOp' is a parameter that hasn't been modified until this point,
    // and the 'store' targets a local array, then it can't write into 'addressOp'.
    if(addressOp->IsParameter() && 
       IsAddressNotTaken(storeInstr->DestinationOp())) {
        return false;
    }

    // Even if the address is taken, if we write through a pointer
    // parameter that wasn't modified until this point, we can't
    // modify the location indicated by 'addressOp'.
    // Note that this works even for recursive functions, because the pointer
    // still can't point to arrays/records local to this function instance.
    if(storeInstr->DestinationOp()->IsParameter() &&
        (addressOp->IsLocalVariableRef())) {
        // Because we're in SSA form it's enough to test that we have a parameter.
        return false;
    }

    // If we have two parameters, and both are marked as being 'restrict',
    // the programmer guarantees that they will never point 
    // to the same memory location.
    if(auto paramA = storeInstr->DestinationOp()->As<Parameter>()) {
        if(paramA->IsRestrict()) {
            auto paramB = addressOp->As<Parameter>();

            if(paramB && paramB->IsRestrict()) {
                // Both parameters are 'restrict'.
                return false;
            }
        }
    }

    // We need to presume it may write.
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::StoresIntoPointer(Operand* op) {
    if(auto baseOp = GetBaseOperand(op)) {
        return StoresIntoPointer(baseOp);
    }
    else if(op->DefiningInstrIs<LoadInstr>() || 
            op->DefiningInstrIs<CallInstr>()) {
        return op->GetType()->IsPointer();
    }
    else return op->IsParameter() && 
                op->GetType()->IsPointer();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* LoadStoreElimination::FindNearestSourceGlobal(Instruction* startInstr, 
                                                       Operand* addressOp,
                                                       bool checkCurrent, 
                                                       bool& notFound, int level) {
    // Try to find a block where 'addressOp' is available.
    // We check for the 'if-then' and 'if-then-else' patterns only.
    // If the value is not available in all predecessors we give up.
    auto parentBlock = startInstr->ParentBlock();
    auto unit = parentBlock->ParentFunction()->ParentUnit();

    // Check if we need to look into the current block.
    if(checkCurrent) {
        if(auto result = FindNearestSource(startInstr, addressOp, notFound)) {
            return result;
        }
        else if(notFound == false) {
            // Something is blocking the 'load'.
            return nullptr;
        }
    }

    if(parentBlock->PredecessorCount() == 1) {
        // This case is simple, just continue searching in the predecessor.
        auto predecessorBlock = parentBlock->PredecessorAt(0);
        return FindNearestSourceGlobal(predecessorBlock->LastInstruction(), 
                                       addressOp, true, notFound, level + 1);
    }
    else if(parentBlock->PredecessorCount() == 2) {
        // Test for the following situations, which are simple enough
        // to handle without data-flow information.
        //     if                  if
        //   /    \              /  |
        // then  else         then  |
        //   \    /              \  |
        //     A                   A
        notFound = false; // Presume it's invalid.
        bool isIfThenElse = true;
        bool recognized = false;
        Block* B = parentBlock->PredecessorAt(0);
        Block* C = parentBlock->PredecessorAt(1);
        Block* ifBlock = nullptr;
        Block* thenBlock = nullptr;
        Block* elseBlock = nullptr;

        // First test for 'if-then'.
        if(B->HasSuccessor(C) && (B->SuccessorCount() == 2) &&
           C->HasSinglePredecessor()) {
            // 'if-then', 'B' contains the 'if' instruction.
            ifBlock = B;
            thenBlock = C;
            isIfThenElse = false;
            recognized = true;
        }
        else if(C->HasSuccessor(B) && (C->SuccessorCount() == 2) &&
                B->HasSinglePredecessor()) {
            // 'if-then', 'C' contains the 'if' instruction.
            ifBlock = C;
            thenBlock = B;
            isIfThenElse = false;
            recognized = true;
        }
        
        // Now test for 'if-then-else'.
        if(recognized == false) {
            if(B->HasSinglePredecessor() == false) {
                return nullptr;
            }
            else ifBlock = B->PredecessorAt(0);

            if((C->HasSinglePredecessor() == false) ||
               (C->PredecessorAt(0) != ifBlock)) {
                return nullptr;
            }

            isIfThenElse = true;
            thenBlock = B;
            elseBlock = C;
        }

        // Now search for a source.
        if(isIfThenElse) {
           return FindNearestSourceIfThenElse(ifBlock, thenBlock, elseBlock,
                                              addressOp, parentBlock, 
                                              notFound, level);
        }
        else return FindNearestSourceIfThen(ifBlock, thenBlock,
                                            addressOp, parentBlock, 
                                            notFound, level);
    }
    else {
        // There is no practical reason to try for blocks with more predecessors.
        notFound = false;
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* 
LoadStoreElimination::FindNearestSourceIfThenElse(Block* ifBlock, Block* thenBlock,
                                                  Block* elseBlock, Operand* addressOp,
                                                  Block* parentBlock, bool& notFound, 
                                                  int level) {
    // Both the 'then' and 'else' blocks should provide the same
    // value, or at least they shouldn't interfere with the load.
    auto thenResult = FindNearestSource(thenBlock->LastInstruction(), 
                                        addressOp, notFound);
    if((thenResult == nullptr) && (notFound == false)) {
        return nullptr;
    }

    auto elseResult = FindNearestSource(elseBlock->LastInstruction(), 
                                        addressOp, notFound);
    if((elseResult == nullptr) && (notFound == false)) {
        return nullptr;
    }

    if(thenResult && elseResult) {
        // Both predecessors load/store from/to the address.
        // The values are "combined" using a 'phi' instruction.
        // if(c) a[i] = 5; else a[i] = 8; ... a[i] + 3
        // Insert 't = phi {5, Then}, {8, Else}'.
        return CombineSources(thenResult, elseResult, 
                              thenBlock, elseBlock, parentBlock);
    }
    else if((thenResult || elseResult) && (level == 0)) {
        auto available = thenResult ? thenResult : elseResult;
        auto availableBlock = thenResult ? thenBlock : elseBlock;
        auto unavailableBlock = thenResult ? elseBlock : thenBlock;
        return EliminatePartialLoad(ifBlock, availableBlock, unavailableBlock, 
                                    parentBlock, available, addressOp);
    }

    // The source was not found, or it's partial (found only in one
    // of the predecessors). Check if the 'if' block (or any of it's
    // predecessors) contains a valid source.            
    if(auto ifResult = FindNearestSourceGlobal(ifBlock->LastInstruction(), addressOp,
                                               true, notFound, level + 1)) {
        // Found a source, check if it must be "combined"
        // with a source from the 'then' or 'else' blocks.
        auto otherResult = thenResult ? thenResult : elseResult;
        auto otherBlock = thenResult ? thenBlock : elseBlock;
        auto ifIncomingBlock = thenResult ? elseBlock : thenBlock;

        if(otherResult) {
            return CombineSources(ifResult, otherResult, 
                                  ifIncomingBlock, otherBlock, parentBlock);
        }
        else return ifResult;
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* 
LoadStoreElimination::EliminatePartialLoad(Block* ifBlock, Block* availableBlock, 
                                           Block* unavailableBlock, Block* parentBlock, 
                                           Operand* availableOp, Operand* addressOp) {
    // We have a 'load' that is only partially available. Here we try to
    // perform PRE, so that 'unavailableBlock' also gets a 'load'.
    //     if                  if    
    //   /    \              /    \  
    // ...   load p   =>  load p   load p
    //   \    /              \    /  
    //    load p             load p (fully redundant now)

    // The 'load' is inserted as the last instruction, then both loaded operands
    // are "combined" using a 'phi' instruction.
#if 1
    Log::Error("PRE of load in " + *unavailableBlock->Name());
#endif
    auto resultOp = Temporary::GetTemporary(availableOp->GetType());
    auto loadInstr = LoadInstr::GetLoad(addressOp, resultOp);
    auto positionInstr = unavailableBlock->BranchInstruction();

    unavailableBlock->InsertInstructionBefore(loadInstr, positionInstr);
    return CombineSources(resultOp, availableOp, unavailableBlock, 
                          availableBlock, parentBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::IsSafeToSpeculateLoad(LoadInstr* loadInstr, Block* toBlock) {
    // If the source operand doesn't dominate the block
    // where the 'load' is moved give up.
    auto sourceOp = loadInstr->SourceOp();

    if(GetSafetyInfo()->DefinitionDominatesBlock(sourceOp, toBlock) == false) {
        return false;
    }

    // Scan all instructions that precede the load in it's block.
    // If we have a 'store', a 'load' or a 'call' that may write/read from
    // the same address as the 'load' then we can't speculate it.
    auto addressOp = loadInstr->SourceOp();
    auto instr = loadInstr->PreviousInstruction();

    while(instr && (instr->IsPhi() == false)) {
        if(auto storeInstr = instr->As<StoreInstr>()) {
            // If we know that this 'store' definitely doesn't write
            // to the address we can continue.
            if(MayStoreWriteToAddress(storeInstr, addressOp)) {
                return false;
            }
        }
        else if(auto loadInstr = instr->As<LoadInstr>()) {
            // If it loads from the same address then we can't move the 'store'
            // past it, because then it may load a different value.
            if(IsSameAddress(addressOp, loadInstr->SourceOp())) {
                return false;
            }

            if(AreReferencesDifferent(addressOp, loadInstr->SourceOp()) == false) {
                return false;
            }
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            // We give up if the called function may write or read to the address.
            if(CallMayWriteToAddress(callInstr, addressOp) ||
               CallMayReadFromAddress(callInstr, addressOp)) {
                return false;
            }
        }

        instr = instr->PreviousInstruction();
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* 
LoadStoreElimination::FindNearestSourceIfThen(Block* ifBlock, Block* thenBlock,
                                              Operand* addressOp, Block* parentBlock, 
                                              bool& notFound, int level) {
    // Both the 'then' and 'if' blocks should provide the same
    // value, or at least 'then' shouldn't interfere with the load.
    auto thenResult = FindNearestSource(thenBlock->LastInstruction(), addressOp, notFound);
    if((thenResult == nullptr) && (notFound == false)) {
        return nullptr;
    }

    if(auto ifResult = FindNearestSourceGlobal(ifBlock->LastInstruction(), addressOp,
                                               true, notFound, level + 1)) {
        // Found a source, check if it must be "combined"
        // with the source from the 'then' block.
        if(thenResult) {
            return CombineSources(ifResult, thenResult, 
                                  ifBlock, thenBlock, parentBlock);
        }
        else return ifResult;
    }

    // If there is no available operand at all we must give up.
    if(thenResult == nullptr) {
        return nullptr;
    }

    if(auto loadInstr = thenResult->DefiningInstrAs<LoadInstr>()) {
        // Even if we don't have a source in both predecessors, we may be able
        // to move the available load to the 'if' block using speculation.
        //                   load p
        //     if              if      
        //     |  \            |  \    
        //     | load p   =>   | ...
        //     |  /            |  /    
        //    load p          load p   (redundant)
        // Testing that 'level' is 0 assures us that we have
        // exactly the situation depicted above.
        if((level == 0) && IsSameAddress(loadInstr->SourceOp(), addressOp) &&
            IsSafeToSpeculateLoad(loadInstr, ifBlock)) {
            // Move the load from 'thenBlock' to 'ifBlock'.
#if 1
            Log::Error("Load speculated to " + *ifBlock->Name());
#endif
            loadInstr->RemoveFromBlock();
            ifBlock->InsertInstructionBefore(loadInstr, ifBlock->BranchInstruction());
            return thenResult;
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* LoadStoreElimination::CombineSources(Operand* sourceA, Operand* sourceB, 
                                              Block* blockA, Block* blockB,
                                              Block* parent) {
    DebugValidator::AreNotEqual(blockA, blockB);
    DebugValidator::AreEqual(sourceA->GetType(), sourceB->GetType());

    // We use a 'phi' to "combine" the values.
    auto unit = parent->ParentFunction()->ParentUnit();
    auto temp = Temporary::GetTemporary(sourceA->GetType());
    auto phiInstr = PhiInstr::GetPhi(temp, 2);

    phiInstr->AddOperand(sourceA, unit->References().GetBlockRef(blockA));
    phiInstr->AddOperand(sourceB, unit->References().GetBlockRef(blockB));

    parent->InsertInstructionFirst(phiInstr);
    return temp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::IsAddressNotTaken(Operand* op, VariableReference** variable) {
    if(auto variableRef = op->As<VariableReference>()) {
        return variableRef->IsAddressTaken() == false;
    }

    // Try to find the base variable by looking through address instructions.
    Operand* baseOp = op;
    
    do {
        baseOp = GetBaseOperand(baseOp);

        if(baseOp && baseOp->IsLocalVariableRef()) {
            auto variableRef = baseOp->As<VariableReference>();
            
            if(variableRef->IsAddressTaken() == false) {
                // Found the base variable, and it's address is not taken.
                if(variable) *variable = variableRef;
                return true;
            }
        }
    } while(baseOp);

    // Presume the address may be taken.
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* LoadStoreElimination::GetBaseOperand(Operand* op) {
    // Skip over 'ptop' casts.
    while(op->DefiningInstrIs<PtopInstr>()) {
        op = op->DefiningInstrAs<PtopInstr>()->TargetOp();
    }

    if(auto instr = op->DefiningInstruction()) {
        if(instr->IsAddress() || instr->IsIndex() || instr->IsElement()) {
            auto addrInstr = static_cast<AddressInstr*>(instr);
            return addrInstr->BaseOp();
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::IsSameAddress(Operand* opA, Operand* opB) {
    // If both operands are equal the addresses are equal.
    if(opA == opB) return true;

    // References to the same variable always have the same address.
    if(AreReferencesEqual(opA, opB)) {
        return true;
    }

    // 'index/elem/addr' instructions that have the same operands
    // compute the same memory address, because we're in SSA form.
    if(opA->HasDefiningInstruction() && 
      (opA->DefiningInstruction()->IsIndex()   ||
       opA->DefiningInstruction()->IsElement() ||
       opA->DefiningInstruction()->IsAddress())) {
        auto addressInstrA = static_cast<AddressInstr*>(opA->DefiningInstruction());
        
        if(opB->HasDefiningInstruction() && 
           (opB->DefiningInstruction()->GetOpcode() == 
           addressInstrA->GetOpcode())) {
            auto addressInstrB = static_cast<AddressInstr*>(opB->DefiningInstruction());
            
            // Because we're in SSA form we know that if the operands
            // are the same the result of the instructions will be the same.
            // This will return 'true' for things like
            // index a, 2   ==  index a, 2, but also for
            // index a, t1  ==  index a, t1
            if((addressInstrA->BaseOp() == addressInstrB->BaseOp()) &&
                AreIndexOpsEqual(addressInstrA->IndexOp(), 
                                 addressInstrB->IndexOp())) {
                return true;
            }
        }
    }
    
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::AreIndexOpsEqual(Operand* opA, Operand* opB) {
    // Test for the obvious case first.
    if(opA == opB) return true;

    // It's possible that we have the same integer value,
    // but the constants use a different type.
    if(auto intConstA = opA->As<IntConstant>()) {
        if(auto intConstB = opB->As<IntConstant>()) {
            return intConstA->Value() == intConstB->Value();
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoadStoreElimination::Remove(Instruction* instr) {
    // Remove the 'load'/'store' instruction and all the instructions
    // it depends on, if they were used only to compute the address.
    int sources = instr->SourceOpCount();

    for(int i = 0; i < sources; i++) {
        auto source = instr->GetSourceOp(i);

        if(source->HasDefiningInstruction() && 
           source->DefiningInstruction()->GetDestinationOp()->HasSingleUser()) {
            // The operand is an instruction that was used only to compute
            // the address of the 'load'/'store', so it can be eliminated.
            Remove(source->DefiningInstruction());
        }
    }
    
    instr->RemoveFromBlock(true /* free */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoadStoreElimination::EliminateRedundantLoads(Block* block) {
    // Scan each instruction in the block and try to find 
    // a 'store' or 'load' instruction from which to take the operand,
    // instead of reloading it again. This basically a store forwarding and
    // a common-subexpression elimination fro loads.
    Instruction* instr = block->FirstInstruction();

    while(instr) {
        if(auto loadInstr = instr->As<LoadInstr>()) {
            // If the 'load' is marked as 'volatile' we're not allowed to remove it!
            if(loadInstr->IsVolatile()) {
                instr = instr->NextInstruction();
                continue;
            }

            // Try to find a source by looking to all the preceding 
            // instructions in this block.
            bool notFound;
            Operand* source = FindNearestSource(loadInstr->PreviousInstruction(),
                                                loadInstr->SourceOp(), notFound);
            if(source == nullptr) {
                if(notFound == false) {
                    // An operation may interfere with this 'load',
                    // so there is no reason to try more complex
                    // methods for elimination.
                    instr = instr->NextInstruction();
                    continue;
                }

                // Try to find a source in the predecessor blocks.
                source = FindNearestSourceGlobal(loadInstr, loadInstr->SourceOp(), 
                                                 false, notFound, 0 /* level */);
            }

            if(source) {
                // Found an available source; remove this load instruction,
                // and replace it's result with the found source operand.
                // We also do dead-code elimination when we eliminate the 'load'.
                instr = instr->NextInstruction();
                loadInstr->ResultOp()->ReplaceWith(source);
                LogLoadRemoved(loadInstr);
                Remove(loadInstr);
                continue;
            }
        }

        instr = instr->NextInstruction();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoadStoreElimination::EliminateRedundantLoads(Function* function) {
    // We try to eliminate the loads from all reachable blocks.
    for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
        if(block->PredecessorCount() > 0) {
            EliminateRedundantLoads(block);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::CallMayReadFromAddress(CallInstr* callInstr, 
                                                  Operand* addressOp) {
    // We presume that calls read from the address, but we try to eliminate some
    // obvious cases like calls to intrinsics and standard library functions.
    auto functionRef = callInstr->TargetOp()->As<FunctionReference>();

    // If we are not calling the function directly we don't know anything
    // about it, so we presume it could read from the address.
    if(functionRef == nullptr) {
        // We may still be able to prove that the call doesn't use 
        // the address if we're dealing with a local variable.
        return IsAddressUsedByCall(callInstr, addressOp);
    }

    // Test for intrinsics. All 'memory' intrinsics could read from the address,
    // but we may prove they read from a different variable than the one
    // referenced by the address operand.
    if(auto intrinsic = functionRef->Target()->As<Intrinsic>()) {
        if(intrinsic->Is<CopyMemoryIntr>()) {
            if(AreReferencesDifferent(addressOp, callInstr->GetArgument(1))) {
                return false;
            }
        }
        else if(intrinsic->Is<SetMemoryIntr>()) {
            // 'SetMemory' uses only constants.
            return false;
        }
        else if(intrinsic->DoesReadFromMemory() == false) {
            // The intrinsic guarantees that it doesn't read user memory.
            return false;
        }
    }

    // Check if this is a call to a well-known standard library function
    // (important especially for math functions).
    if(auto info = GetLanguageInfo()) {
        if(info->CallMayReadFromAddress(callInstr, addressOp, this) == false) {
            return false;
        }
    }

    // We may still be able to prove that the call doesn't use 
    // the address if we're dealing with a local variable.
    return IsAddressUsedByCall(callInstr, addressOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::IsStoreDead(Instruction* startInstr, 
                                       Operand* addressOp, int level) {
    // Scan the instructions that follow a 'store' and try to determine if it's dead,
    // meaning that the stored value is not used before another 'store' writes
    // to the exact same memory address.
    Instruction* instr = startInstr;

    while(instr) {
        if(auto storeInstr = instr->As<StoreInstr>()) {
            // If we're writing exactly at the same address the previous store is dead.
            if(IsSameAddress(addressOp, storeInstr->DestinationOp())) {
                return true;
            }
        }
        else if(auto loadInstr = instr->As<LoadInstr>()) {
            // If we're loading from the same address the previous store isn't dead.
            if(IsSameAddress(addressOp, loadInstr->SourceOp())) {
                return false;
            }

            // We couldn't decide that the store isn't dead, but we could show
            // that this loads from a different variable and may continue scanning.
            if(AreReferencesDifferent(addressOp, loadInstr->SourceOp())) {
                instr = instr->NextInstruction();
                continue;
            }
            
            return false; // Presume store is not dead.
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            // We presume that calls read from the address, but we may be able
            // to prove that they not, in which case we continue.
            if(CallMayReadFromAddress(callInstr, addressOp)) {
                return false; // Presume store is not dead.
            }
        }

        instr = instr->NextInstruction();
    }

    // If we have a single successor block we scan it to see
    // if the store might be dead there.
    auto block = startInstr->ParentBlock();

    if(block->HasSuccessors() && (level < MAX_DEATH_STORE_LEVEL)) {
        return IsStoreDeadInSuccessors(block, addressOp, level);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoadStoreElimination::IsStoreDeadInSuccessors(Block* block, 
                                                   Operand* addressOp, int level) {
    // We don't test blocks with too many successors, it's unlikely the store
    // will be dead in all of them.
    if(block->SuccessorCount() <= MAX_DEATH_STORE_SUCCESSORS) {
        for(int i = 0; i < block->SuccessorCount(); i++) {
            auto successorBlock = block->SuccessorAt(i);

            if(IsStoreDead(successorBlock->FirstInstruction(), 
                           addressOp, level + 1) == false) {
                // No reason to continue, the store must be dead
                // in all successors to be eliminated.
                return false;
            }
        }

        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoadStoreElimination::EliminateDeadStores(Block* block) {
    // Scan each instruction in the block and try to find 
    // 'store' instructions that are dead.
    Instruction* instr = block->FirstInstruction();

    while(instr) {
        if(auto storeInstr = instr->As<StoreInstr>()) {
            // If the 'store' is marked as 'volatile' we're not allowed to remove it!
            if(storeInstr->IsVolatile()) {
                instr = instr->NextInstruction();
                continue;
            }

            if(IsStoreDead(storeInstr->NextInstruction(), 
                           storeInstr->DestinationOp())) {
                // Remove the instruction, together with any instruction that
                // was used only to compute the address of the 'store'.
                instr = instr->NextInstruction();
                LogStoreRemoved(storeInstr);
                Remove(storeInstr);
                continue;
            }
        }

        instr = instr->NextInstruction();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoadStoreElimination::EliminateDeadStores(Function* function) {
    // We try to eliminate the stores from all reachable blocks.
    for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
        if(block->PredecessorCount() > 0) {
            EliminateDeadStores(block);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* LoadStoreElimination::WithoutPointerCasts(Operand* op) {
    while(auto ptopInstr = op->DefiningInstrAs<PtopInstr>()) {
        op = ptopInstr->TargetOp();
    }
    
    return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* LoadStoreElimination::ExtractBaseAndOffset(Operand* destOp, __int64& offset) {
    // Compute the offset relative to the base operand.
    // This looks through 'index', 'elem' and 'addr' instructions,
    // and stops as soon as the index operand is not a constant.
    offset = 0;

    while(destOp->HasDefiningInstruction()) {
        if(auto indexInstr = destOp->DefiningInstrAs<IndexInstr>()) {
            if(auto intConst = indexInstr->IndexOp()->As<IntConstant>()) {
                offset += GetMultipledSize(indexInstr->GetElementType(), intConst);
                destOp = indexInstr->BaseOp();
            }
            else return nullptr;
        }
        else if(auto elemInstr = destOp->DefiningInstrAs<ElementInstr>()) {
            // The index is always a constant.
            auto recordType = elemInstr->GetRecordType();
            offset += recordType->GetFieldOffset(elemInstr->GetFieldIndex());
            destOp = elemInstr->BaseOp();
        }
        else if(auto addrInstr = destOp->DefiningInstrAs<AddressInstr>()) {
            if(auto intConst = addrInstr->IndexOp()->As<IntConstant>()) {
                offset += GetMultipledSize(addrInstr->GetPointeeType(), intConst);
                destOp = addrInstr->BaseOp();
            }
            else return nullptr;
        }
        else if(auto ptopInstr = destOp->DefiningInstrAs<PtopInstr>()) {
            destOp = ptopInstr->TargetOp();
        }
        else return nullptr;
    }

    return destOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoadStoreElimination::LogStoreRemoved(StoreInstr* instr) {
#if 1
    auto block = instr->ParentBlock();
    auto function = block->ParentFunction();
    string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
    string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
    string text = IRPrinter(instr).ToString();
    Log::Warning("Store removed from " + functionName + ":" + blockName + " - " + text);
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoadStoreElimination::LogLoadRemoved(LoadInstr* instr) {
#if 1
    auto block = instr->ParentBlock();
    auto function = block->ParentFunction();
    string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
    string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
    string text = IRPrinter(instr).ToString();
    Log::Warning("Load removed from " + functionName + ":" + blockName + " - " + text);
#endif
}

} // namespace Optimization