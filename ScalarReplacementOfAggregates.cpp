// ScalarReplacementOfAggregates.cpp
// Copyright (c) Lup Gratian
//
// Implements the Aggregate Copy Propagation pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ScalarReplacementOfAggregates.hpp"

namespace Optimization {

void ScalarReplacementOfAggregates::Execute(Function* function) {
    funct_ = function;
    replacementsCreated_ = 0;

    // Try to improve the function so that more aggregates
    // can be scalarized. This especially handles 'phi' and 'quest'.
    ImproveFunction();

    // Run the aggregate address-taken pass again,
    // it may mark fever variables after the improvements.
    VariableAnalysis varAnalysis;
    varAnalysis.AnnotateAggregateAddressTaken(function);

    // Mark the variables the are candidates
    // for scalar replacement (records and arrays).
    if(MarkCandidates() == 0) {
        return;
    }

    // Collect all accesses to the candidate
    // by processing each instruction in the function.
    for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
        ProcessBlock(block);
    }

#if 1
    Dump();
#endif

    // Now try to scalarize the aggregates. Large aggregates
    // are only partial scalarized (the "hottest" accesses).
    ReplaceAggregates();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ScalarReplacementOfAggregates::MarkCandidates() {
    // Check if we have variables that are eligible
    // for scalar replacement and mark them in the bit vector.
    auto& variables = funct_->Variables();
    int count = 0;

    for(int i = 0; i < variables.Count(); i++) {
        auto variable = variables[i];

        if(IsEligibleCandidate(variable)) {
            // We start by presuming the variable is eligible.
            MakeEligible(variable);
            count++;

            // Create an information object for the candidate.
            int index = candidates_.Count();
            varToCandidateIndex_.Add(variable, index);

            candidates_.Add(Candidate());
            auto& candidate = candidates_[index];

            // Reset the object.
            candidate.Base = variable;
            candidate.HasLoad = false;
            candidate.HasStore = false;
            candidate.NeedsTree = false;
            candidate.ComponentCount = 0;
            candidate.Data = nullptr;
        }
    }

    return count;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::IsEligibleCandidate(Variable* variable) {
    // The variable should be a record or an array
    // whose address is not taken.
    if((variable->IsRecord() || variable->IsArray()) == false) {
        return false;
    }

    // The address of the variable should not be taken.
    // If it is we might miss a place where it is modified,
    // so we prefer not to optimize them. Note that this
    // presumes that calls to the 'setMemory'/'copyMemory' intrinsics
    // don't take the address, exposing more optimization opportunities.
    if(variable->IsAddressTaken()) {
        return false;
    }

    // If we have a record it shouldn't be the equivalent of a C union
    // (at least two fields having the same offset).
    if(auto recordType = variable->GetType()->As<RecordType>()) {
        for(int i = 1; i < recordType->FieldCount(); i++) {
            if(recordType->GetFieldOffset(i) ==
               recordType->GetFieldOffset(i - 1)) {
                // It is a "union", give up.
                return false;
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ScalarReplacementOfAggregates::ComputeRequiredVariables(const Type* type) {
    if(auto recordType = type->As<RecordType>()) {
        // For records we sum up the number of required variables
        // for all fields (this handles record inside record, for example).
        auto& fields = recordType->Fields();
        int count = 0;

        for(int i = 0; i < fields.Count(); i++) {
            auto fieldType = fields[i].FieldType;

            if(fieldType->IsRecord() || fieldType->IsArray()) {
                count += ComputeRequiredVariables(fieldType);
            }
            else count++;
        }

        return count;
    }
    else if(auto arrayType = type->As<ArrayType>()) {
        // We create a variable for each element.
        int elemVars = ComputeRequiredVariables(arrayType->ElementType());
        return arrayType->Size() * elemVars;
    }
    else return 1;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::ProcessBlock(Block* block) {
    // We're interested in 'load', 'store' and 'call' instructions
    // that call the 'copyMemory' and 'setMemory' intrinsics.
    // Any other instruction can't affect the values in an aggregate.
    for(auto instr = block->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
        if(auto loadInstr = instr->As<LoadInstr>()) {
            ProcessLoadStore(loadInstr, loadInstr->SourceOp(),
                             true /* isLoad */, loadInstr->IsVolatile());
        }
        else if(auto storeInstr = instr->As<StoreInstr>()) {
            ProcessLoadStore(storeInstr, storeInstr->DestinationOp(),
                             false /* isLoad */, storeInstr->IsVolatile());
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            ProcessCall(callInstr);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::ProcessLoadStore(Instruction* instr, Operand* op,
                                                     bool isLoad, bool isVolatile) {
    Access access(instr);
    Variable* accessedVar = nullptr;
    int offset; // Not used here.

    if(CreateAccessPath(op, access.Path, accessedVar, offset)) {
        DebugValidator::IsNotNull(accessedVar);

        // If there are no indexes in the path
        // then we can't have an access to an aggregate.
        if(access.Path.Indices.Count() == 0) {
            return;
        }

        // If the 'load' is volatile disqualify the variable.
        if(isVolatile) {
            ResetEligible(accessedVar);
            return;
        }

        // Attach the access to the candidate.
        int candidateIndex = varToCandidateIndex_[accessedVar];
        auto& candidate = candidates_[candidateIndex];
        access.Type = isLoad ? Access_Load : Access_Store;
        candidate.Accesses.Add(access);

        if(isLoad) candidate.HasLoad = true;
        else candidate.HasStore = true;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::ProcessCall(CallInstr* instr) {
    // Check if this calls a 'copyMemory' or 'setMemory' intrinsic
    // that has a local aggregate as it's destination.
    if(auto intrinsic = instr->GetIntrinsic()) {
        if(auto copyMemoryIntr = intrinsic->As<CopyMemoryIntr>()) {
            ProcessCopyMemory(instr);
            return;
        }
        else if(auto setMemoryIntr = intrinsic->As<SetMemoryIntr>()) {
            ProcessSetMemory(instr);
            return;
        }
    }

    // Check if any argument represents a record
    // that is passed by value.
    for(int i = 0; i < instr->ArgumentCount(); i++) {
        auto argument = instr->GetArgument(i);

        if(argument->IsPointer() == false) {
            continue;
        }

        AccessPath path;
        Variable* variable = nullptr;
        int offset; // Not used here.
        
        if(CreateAccessPath(argument, path, variable, offset)) {
            // Mark that the candidate is used by a 'call',
            // because all replaced components need to be
            // written back before making the call.
            if(IsEligible(variable) == false) {
                continue; // Skip this variable;
            }

            int candidateIndex = varToCandidateIndex_[variable];
            auto& candidate = candidates_[candidateIndex];
            CallAccess access(instr, path);

            // Check if the parameter guarantees that the aggregate
            // is not read from/written to (knowing this reduces the number
            // of scalarized components that need to be written/reloaded).
            if(auto function = instr->GetCalledFunction()) {
                auto paramVar = function->GetParameterVariable(i);
                access.IsNoRead = paramVar->IsNoRead();
                access.IsNoWrite = paramVar->IsNoWrite();
            }

            candidate.CallAccesses.Add(access);
            candidate.NeedsTree = true;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::ProcessCopyMemory(CallInstr* instr) {
    // The size of the copy should be an integer constant,
    // else we can't determine which scalarized components are required.
    auto intConst = CopyMemoryIntr::GetConstantLength(instr);
    bool valid = intConst != nullptr;
    
    // Check if the copy targets variables that are candidates.
    AccessPath destPath;
    AccessPath sourcePath;
    Variable* destVariable = nullptr;
    Variable* sourceVariable = nullptr;
    int destOffset;
    int sourceOffset;
    
    bool hasDestPath = CreateAccessPath(SetMemoryIntr::GetDestination(instr),
                                        destPath, destVariable, destOffset);
    bool hasSourcePath = CreateAccessPath(SetMemoryIntr::GetSource(instr),
                                          sourcePath, sourceVariable, sourceOffset);
    if((hasDestPath || hasSourcePath) == false) {
        // No local variable are accessed, we can ignore the call.
        return;
    }

    // Make sure that the destination/source variables are still candidates.
    for(int i = 0; i < 2; i++) {
        Variable* variable = i == 0 ? (hasDestPath ? destVariable : nullptr) :
                                      (hasSourcePath ? sourceVariable : nullptr);
        int offset = i == 0 ? destOffset : sourceOffset;

        if(variable) {
            if(IsEligible(variable) == false) {
                // No reason to continue.
                return;
            }

            if(valid) {
                // The operation shall not write outside
                // the bounds of the variable.
                int varSize = TI::GetSize(variable->GetType(), GetTarget());
                valid = (offset + intConst->Value()) <= varSize;
            }

            if(valid == false) {
                // Something isn't right, don't optimize the variable at all.
                ResetEligible(variable);
                return;
            }
        }
    }

    // Attach the access to the candidate.
    if(valid && hasDestPath) {
        CopyAccess access(instr);
        access.Type = CopyAccess_Copy;
        access.DestinationPath = destPath;
        access.SourcePath = sourcePath;
        access.DestinationVariable = destVariable;
        access.SourceVariable = sourceVariable;
        access.Size = intConst->Value();
            
        // Add the candidate to the list.
        int candidateIndex = varToCandidateIndex_[destVariable];
        auto& candidate = candidates_[candidateIndex];
        candidate.CopyAccesses.Add(access);
        candidate.NeedsTree = true;
    }

    if(valid && hasSourcePath) {
        // Mark that we're copying from this candidate.
        int candidateIndex = varToCandidateIndex_[sourceVariable];
        auto& candidate = candidates_[candidateIndex];
        candidate.NeedsTree = true;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::ProcessSetMemory(CallInstr* instr) {
    // The size of the copy should be an integer constant,
    // else we can't determine which scalarized components are required.
    auto intConst = SetMemoryIntr::GetConstantLength(instr);
    bool valid = intConst != nullptr;

    // Check if the copy targets variables that are candidates.
    AccessPath destPath;
    Variable* destVariable = nullptr;
    int offset;
    bool hasDestPath = CreateAccessPath(instr->GetArgument(0),
                                        destPath, destVariable, offset);
    if(hasDestPath == false) {
        // No local variable are accessed, we can ignore the call.
        return;
    }

    // Make sure that the variable is still a candidate.
    if(IsEligible(destVariable) == false) {
        // No reason to continue.
        return;
    }

    if(valid) {
        // The operation shall not write outside
        // the bounds of the variable.
        int destVariableSize = TI::GetSize(destVariable->GetType(), GetTarget());
        valid = (offset + intConst->Value()) <= destVariableSize;
    }

    if(valid == false) {
        // Something isn't right, don't optimize the variable at all.
        ResetEligible(destVariable);
        return;
    }

    CopyAccess access(instr);
    access.Type = CopyAccess_Set;
    access.DestinationPath = destPath;
    access.DestinationVariable = destVariable;
    access.SourceVariable = nullptr;
    access.Size = intConst->Value();
            
    // Add the candidate to the list.
    int candidateIndex = varToCandidateIndex_[destVariable];
    auto& candidate = candidates_[candidateIndex];
    candidate.CopyAccesses.Add(access);
    candidate.NeedsTree = true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::CreateAccessPath(Operand* op, AccessPath& path,
                                                     Variable*& variable, int& offset,
                                                     bool usedByAddr) {
    // An access path is composed from a series of 'index' and 'elem'
    // instructions that finally target a variable reference.
    // The path is valid if the variable is marked as a candidate,
    // and for 'index' instructions, if the index operand is a constant.
    // If the path is not valid we mark the variable as not eligible
    // for scalar replacement.
    if(auto variableRef = op->As<VariableReference>()) {
        // We're interested only in local variables. If there is a 'addr'
        // user then the candidate isn't eligible anymore.
        if(variable = variableRef->GetVariable()) {
            if(IsEligible(variable)) {
                if(usedByAddr) {
                    ResetEligible(variable);
                    return false;
                }
                else {
                    offset = 0;
                    return true;
                }
            }
        }

        return false;
    }
    else if(auto elemInstr = op->DefiningInstrAs<ElementInstr>()) {
        return CreateAccessPathForElem(elemInstr, path, variable, 
                                       offset, usedByAddr);
    }
    else if(auto indexInstr = op->DefiningInstrAs<IndexInstr>()) {
       return CreateAccessPathForIndex(indexInstr, path, variable, 
                                       offset, usedByAddr);
    }
    else if(auto addrInstr = op->DefiningInstrAs<AddressInstr>()) {
        // We can't track 'addr' instructions, so we give up.
        // (most of them are folded by Peephole anyway).
        return CreateAccessPath(addrInstr->BaseOp(), path, 
                                variable, offset, true);
    }
    else if(auto ptopInstr = op->DefiningInstrAs<PtopInstr>()) {
        // Pointer casts are ignored, they are used
        // for aggregate copies.
        return CreateAccessPath(ptopInstr->TargetOp(),  path, 
                                variable, offset, usedByAddr);
    }
    
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::CreateAccessPathForElem(ElementInstr* elemInstr, 
                                                            AccessPath& path, 
                                                            Variable*& variable, 
                                                            int& offset, bool usedByAddr) {
    // An access to a record is valid as long as
    // it's valid to access it's base operand.
    if(CreateAccessPath(elemInstr->BaseOp(), path, 
                        variable, offset, usedByAddr)) {
        auto intConst = elemInstr->IndexOp()->As<IntConstant>();
        path.Indices.Add(intConst->Value());

        // Update the offset into the variable.
        auto recordType = elemInstr->GetRecordType();
        offset += recordType->GetFieldOffset(intConst->Value());
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::CreateAccessPathForIndex(IndexInstr* indexInstr, 
                                                             AccessPath& path, 
                                                             Variable*& variable, 
                                                             int& offset, bool usedByAddr) {
    // An access to an array is valid if the index operand
    // is an integer constant within the array bounds,
    // or when it's a temporary and the array has size 1.
    if(CreateAccessPath(indexInstr->BaseOp(), path,
                        variable, offset, usedByAddr)) {
        auto arrayType = indexInstr->GetArrayType();
        DebugValidator::IsNotNull(arrayType);

        if(auto intConst = indexInstr->IndexOp()->As<IntConstant>()) {
            if((intConst->Value() >= 0) &&
                (intConst->Value() < arrayType->Size())) {
                    path.Indices.Add(intConst->Value());

                    // Update the offset into the variable.
                    int elemSize = TI::GetSize(arrayType->ElementType(), GetTarget());
                    offset += elemSize * intConst->Value();
                    return true;
            }
        }
        else if(arrayType->Size() == 1) {
            // Even if we have 'a[n] = ...' we can presume that
            // the the first element is accessed.
            path.Indices.Add(0);
            return true;
        }

        // The candidate is not eligible anymore.
        DebugValidator::IsNotNull(variable);
        ResetEligible(variable);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::ReplaceAggregates() {
    // First we process the load/store operations
    // for all candidates, then the copy/set and call operations.
    for(int i = 0; i < candidates_.Count(); i++) {
        auto& candidate = candidates_[i];
        auto variable = candidate.Base;
        bool completeReplacement = false;
        PathDict replaced;

        // If the candidate is not eligible skip it.
        if(IsEligible(variable) == false) {
            CreateTree(candidate);
            continue;
        }

        // If there is no read or write to the variable
        // there is really no reason to replace it, it will be
        // removed by Dead Code Elimination anyway.
        if((candidate.HasLoad || candidate.HasStore) == false) {
            CreateTree(candidate);
            continue;
        }

        // Check if it's profitable to replace this aggregate.
        // This takes into consideration register pressure
        // and other heuristics that estimate the profitability.
        if(ShouldDoCompleteReplacement(candidate)) {
            completeReplacement = true;
        }
        else if(ShouldDoPartialReplacement(candidate, replaced) == false) {
            CreateTree(candidate);
            continue;
        }

        // Create the variables that are used to replace the aggregate.
        // We may create unnecessary variables (because the associated
        // component is never accessed), but they will be eliminated
        // during the SSA conversion, and this happens rarely anyway.
        CreateReplacements(candidate, completeReplacement, replaced);

        // Replace each access of the aggregate with the appropriate 
        // 'load'/'store' instruction of the replacements.
        for(int i = 0; i < candidate.Accesses.Count(); i++) {
            auto& access = candidate.Accesses[i];
            ReplaceAccess(access, candidate.Data);
        }

        // If there are copy/set or call operations that target this candidate
        // (either as a source or as a destination) we create a tree
        // that represents each component with it's offset and size.
        CreateTree(candidate);
    }

    // Now process each copy/set and call operation.
    for(int i = 0; i < candidates_.Count(); i++) {
        if(candidates_[i].CopyAccesses.Count() > 0) {
            ReplaceCopyAccess(candidates_[i]);
        }

        if(candidates_[i].CallAccesses.Count()) {
            PatchCallAccess(candidates_[i]);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::ShouldDoCompleteReplacement(Candidate& candidate) {
    // Check how many variables need to be generated in order
    // to replace the aggregate completely. If the number
    // is small enough we replace the aggregate completely.
    auto candidateType = candidate.Base->GetType();
    candidate.ComponentCount = ComputeRequiredVariables(candidateType);

    if(candidateType->IsRecord()) {
        if(candidate.ComponentCount <= RECORD_REPLACEMENT_LIMIT) {
            return true;
        }
    }
    else {
        DebugValidator::IsTrue(candidateType->IsArray());
        if(candidate.ComponentCount <= ARRAY_REPLACEMENT_LIMIT) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::ShouldDoPartialReplacement(Candidate& candidate, 
                                                               PathDict& replaced) {
    DebugValidator::IsLarger(candidate.Accesses.Count(), 0);

    // The aggregate has many components, make a decision based on
    // profitability estimate (using profile info, if possible).
    List<ComponentHottness> hottnessList;

    // Limit the number of replacements, else register
    // pressure increases too much.
    int maxReplacements = GetTarget()->AvailableRegisters() *
                          REPLACEMENT_LIMIT_FACTOR;                        

    // If there are many candidates we want to limit further
    // the number of scalarized components.
    int candidateCount = EligibleCount();
    int penality = 0;

    if(candidateCount > CANDIDATE_NUMBER_LIMIT) {
        penality = (candidateCount - CANDIDATE_NUMBER_LIMIT) *
                    CANDIDATE_NUMBER_PENALIZATION;
    }

    // We create a list of the access paths that are unique
    // and attach a 'hotness' attribute to each one, which
    // is computed from the other accesses with the same path.
    auto& accesses = candidate.Accesses;
    accesses.Sort();

    for(int i = 0; i < accesses.Count(); i++) {
        // Compute the hotness of this access.
        int hotness = EstimateHottness(accesses[i]) - penality;
        bool isStore = accesses[i].Type == Access_Store;
        int last = hottnessList.Count() - 1;

        if((last >= 0) && 
           (accesses[i].Path == hottnessList[last].Path)) {
            // This access has the same path as the previous one,
            // just update the hotness.
            hottnessList[last].Hottness += hotness;
            hottnessList[last].HasOnlyStore &= isStore;
        }
        else {
            // This access has a path not seen before.
            // Penalize the access if the aggregate is used by a 'call'
            // because the scalarized components must be written back.
            hotness -= candidate.CallAccesses.Count() * CALL_PENALIZATION;
            hottnessList.Add(ComponentHottness(accesses[i].Path, 
                                            hotness, isStore));
        }
    }

    // If we have an access that is only a store, and the aggregate
    // is copied to another one, try to scalarize it because
    // it's highly probable to be loaded from the copy.
    if(candidate.NeedsTree) {
        for(int i = 0; i < hottnessList.Count(); i++) {
            if(hottnessList[i].HasOnlyStore) {
                hottnessList[i].Hottness += LOAD_ACCESS_HOTTNESS / 2;
            }
        }
    }

    // Sort the list based on the hotness and pick the first
    // 'maxReplacements' ones that met the minimum hotness criteria.
    int position = 0;

    while((position < hottnessList.Count()) &&
          (replaced.Count() < maxReplacements)) {
        auto& access = hottnessList[position];
        position++;

        if(access.Hottness >= MINIMUM_REPLACEMENT_HOTTNESS) {
            // The access to this component should be replaced.
            replaced.Add(access.Path, true);
        }
    }

    return replaced.Count() > 0;
 }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ScalarReplacementOfAggregates::EstimateHottness(Access& access) {
    int hotness = access.Type == Access_Load ? 
                   LOAD_ACCESS_HOTTNESS :
                   STORE_ACCESS_HOTTNESS;

    // If possible, use the dynamic execution frequency 
    // from the profile information.
    //! TODO: PROFILE INFO
    ProfileInfo profile;
    int executionFreq = profile.GetDynamicExecutionFrequency(access.Source);

    if(executionFreq != ProfileInfo::NO_INFO) {
        // Use the profile info, it's the most precise.
        return hotness * executionFreq;
    }

    // If the access is inside a loop it gets a boost.
    auto parentBlock = access.Source->ParentBlock();

    if(parentBlock->IsInLoop()) {
        // The hotness is greater for deeply-nested loops.
        hotness *= LOOP_ACCESS_HOTTNESS_BOOST * parentBlock->LoopDepth();
    }

    return hotness;
 }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::CreateReplacements(Candidate& candidate,
                                                       bool completeReplacement,
                                                       PathDict& replaced) {
    // Create unnamed local variables that are used to replace
    // each component of the aggregate. We also create a mapping
    // from access path to the variable (this speeds up other tasks).
    if(candidate.Data == nullptr) {
        candidate.Data = new CandidateData();
    }

    AccessPath path;
    CreateVariable(candidate.Base->GetType(), path, 
                   candidate.Data, completeReplacement, replaced);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::CreateVariable(const Type* type, 
                                                   AccessPath& path,
                                                   CandidateData* data,
                                                   bool completeReplacement,
                                                   PathDict& replaced) {
    // Variables are created only for basic types,
    // for records and array we call this method recursively.
    if(auto recordType = type->As<RecordType>()) {
        // Create a variable for each field;
        int indexPosition = path.Indices.Count();

        for(int i = 0; i < recordType->FieldCount(); i++) {
            path.Indices.Add(i);
            CreateVariable(recordType->GetFieldType(i), path, 
                           data, completeReplacement, replaced);
            path.Indices.RemoveAt(indexPosition);
        }
    }
    else if(auto arrayType = type->As<ArrayType>()) {
        // Create a variable for each element.
        int indexPosition = path.Indices.Count();

        for(int i = 0; i < arrayType->Size(); i++) {
            path.Indices.Add(i);
            CreateVariable(arrayType->ElementType(), path, 
                           data, completeReplacement, replaced);
            path.Indices.RemoveAt(indexPosition);
        }
    }
    else {
        // For a basic type we create an unnamed variable,
        // but only if all components are replaced, or the access
        // path is among the ones who should be replaced.
        if(completeReplacement || replaced.ContainsKey(path)) {
#if 1
            // For debugging purposes.
            string name = string::Format(L"#SRA_%d", replacementsCreated_++);
            auto variable = Variable::GetVariable(type, name);
#else
            auto variable = Variable::GetVariable(type, nullptr);
#endif
            funct_->AddVariable(variable);
            data->Replacements.Add(path, variable);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::ReplaceAccess(Access& access, 
                                                  CandidateData* data) {
    DebugValidator::IsNotNull(data);

    // We need to generate a 'load' or 'store' instruction,
    // depending on the type of the access. We use the access path
    // to identify the corresponding variable.
    auto& refs = funct_->ParentUnit()->References();
    auto& types = funct_->ParentUnit()->Types();
    Variable* replacementVar;

    // Check if we should replace this access.
    if(data->Replacements.TryGetValue(access.Path, &replacementVar)) {
        auto replacementType = types.GetPointer(replacementVar->GetType());
        auto replacementVariableRef = refs.GetVariableRef(replacementVar, 
                                                     replacementType);
    
        if(access.Type == Access_Load) {
            DebugValidator::IsTrue(access.Source->IsLoad());
            auto loadInstr = access.Source->As<LoadInstr>();
            loadInstr->SetSourceOp(replacementVariableRef);
        }
        else {
            DebugValidator::IsTrue(access.Source->IsStore());
            auto storeInstr = access.Source->As<StoreInstr>();
            storeInstr->SetDestinationOp(replacementVariableRef);
        }

        AccessReplaced(access);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::ReplaceCopyAccess(Candidate& candidate) {
    // Scalarized source components that can't be copied directly
    // to the scalarized destination components are first written
    // back to the aggregate, then loaded by the destination components.
    //
    // For both the destination and source we create a list 
    // of the scalarized components that are affected by the copy.
    // The destination list is processed twice: once to mark
    // the components which can be initialized by copying directly
    // from the corresponding scalarized source component
    // (the code is generated now), and a second time to generate
    // the code that copies the rest of the aggregate and
    // initializes the remaining scalarized components.
    auto& copies = candidate.CopyAccesses;

    for(int i = 0; i < copies.Count(); i++) {
        auto& copyAccess = copies[i];

        // If the access is "set" we handle it separately.
        if(copyAccess.Type == CopyAccess_Set) {
            ReplaceSetAccess(candidate, copyAccess);
            continue;
        }

        // Obtain the list of scalarized components
        // that are affected by the copy.
        NodeList destList;
        NodeList sourceList;
        int startDestOffset = 0;
        int startSourceOffset = 0;

        if(copyAccess.DestinationVariable) {
            int index = varToCandidateIndex_[copyAccess.DestinationVariable];
            auto startNode = GetNodeForAccessPath(candidates_[index],
                                                  copyAccess.DestinationPath);
            AddInRegionToList(candidates_[index].Data->TreeRoot,
                              startNode->Offset, copyAccess.Size, destList);
            startDestOffset = startNode->Offset;
        }

        if(copyAccess.SourceVariable) {
            int index = varToCandidateIndex_[copyAccess.SourceVariable];
            auto startNode = GetNodeForAccessPath(candidates_[index],
                                                  copyAccess.SourcePath);
            AddInRegionToList(candidates_[index].Data->TreeRoot, 
                              startNode->Offset, copyAccess.Size, sourceList);
            startSourceOffset = startNode->Offset;
        }

        // If both lists are empty it means that there are
        // no components in the regions that were scalarized,
        // hence we have nothing to do for this aggregate.
        if((destList.Count() == 0) &&
           (sourceList.Count() == 0)) continue;

        // Copy the common scalarized components.
        CopyCommonComponents(destList, sourceList,
                             startDestOffset, startSourceOffset,
                             copyAccess);

        // Copy the components that were not scalarized
        // in both aggregates, and the spece between the components
        // (happens in case of records - alignment).
        int finalSourceOffset = startSourceOffset + copyAccess.Size;
        CopyRemainingComponents(sourceList, startDestOffset, 
                                startSourceOffset, finalSourceOffset, 
                                copyAccess);

        // Create code that loads destination scalarized components
        // that could not be processed by the first step.
        for(int i = 0; i < destList.Count(); i++) {
            if(destList[i]->Processed == false) {
                ReloadReplacement(candidate.Base, 
                                  destList[i]->Path,
                                  destList[i]->Replacement,
                                  copyAccess.IntrinsicCall);
            }
        }

        CopyAccessReplaced(copyAccess);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::CopyCommonComponents(NodeList& destList, 
                                                         NodeList& sourceList,
                                                         int startDestOffset, 
                                                         int startSourceOffset,
                                                         CopyAccess& copyAccess) {
    // Identify all common scalarized components
    // and copy directly from the source to the destination.
    int destNodeIndex = 0;
    int sourceNodeIndex = 0;

    TreeNode* destNode = AdvanceToNextComponent(destList, destNodeIndex);
    TreeNode* sourceNode = AdvanceToNextComponent(sourceList, sourceNodeIndex);

    do {
        // Skip source components until we find one whose
        // offset is at least equal to a destination component.
        while(sourceNode) {
            if((destNode == nullptr) ||
                ((sourceNode->Offset - startSourceOffset) <
                (destNode->Offset - startDestOffset))) {
                // Advance the source component.
                sourceNode = AdvanceToNextComponent(sourceList, sourceNodeIndex);
            }
            else break;
        }

        // Skip destination components until we find one whose
        // offset is at least equal to a source component.
        while(destNode) {
            if((sourceNode == nullptr) ||
                ((destNode->Offset - startDestOffset) <
                (sourceNode->Offset - startSourceOffset))) {
                destNode = AdvanceToNextComponent(destList, destNodeIndex);
            }
            else break;
        }

        // Check if we have a source and destination component
        // that overlap completely (meaning that they start
        // at the same offset - relative to the starting offset -
        // and have the same type).
        if(destNode && sourceNode && 
            ((destNode->Offset - startDestOffset) ==
             (sourceNode->Offset - startSourceOffset))) {
            if(destNode->ComponentType == sourceNode->ComponentType) {
                // Copy from one variable to the other,
                // because they overlap completely.
                MakeReplacementCopy(destNode->Replacement,
                                    sourceNode->Replacement,
                                    copyAccess.IntrinsicCall);
                destNode->Processed = true;
                sourceNode->Processed = true;
            }

            // Advance to the next components.
            destNode = AdvanceToNextComponent(destList, destNodeIndex);
            sourceNode = AdvanceToNextComponent(sourceList, sourceNodeIndex);
        }
    } while(destNode || sourceNode);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::CopyRemainingComponents(NodeList& sourceList, 
                                                            int startDestOffset, 
                                                            int startSourceOffset, 
                                                            int finalSourceOffset,
                                                            CopyAccess& copyAccess) {
    // Any source component that is not marked as 'processed'
    // is copied to the destination aggregate.
    // The unused space between the components (in case
    // of records - alignment) is also copied, because
    // the program may use it.
    int sourceOffset = startSourceOffset;
    int destOffset = startDestOffset;

    for(int i = 0; i < sourceList.Count(); i++) {
        auto sourceNode = sourceList[i];

        // Is there unused space?
        if(sourceNode->Offset > sourceOffset) {
            //
            int copyAmount = (sourceNode->Offset - sourceOffset);
            GenerateMemoryCopy(copyAccess, sourceOffset, 
                               destOffset, copyAmount,
                               copyAccess.IntrinsicCall);
        }

        // Copy the component if required
        if(sourceNode->Processed == false) {
            WriteReplacementToMemory(sourceNode->Replacement, copyAccess,
                                     destOffset, copyAccess.IntrinsicCall);
        }

        // Update the current offset.
        int difference = (sourceNode->Offset - sourceOffset) +
                          sourceNode->Size;
        sourceOffset += difference;
        destOffset += difference;
    }

    // There is space at the end of the aggregate that
    // may have not been covered by the components, copy it now.
    if(sourceOffset != finalSourceOffset) {
        int copyAmount = finalSourceOffset - sourceOffset;
        GenerateMemoryCopy(copyAccess, sourceOffset, 
                           destOffset, copyAmount,
                           copyAccess.IntrinsicCall);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::ReplaceSetAccess(Candidate& candidate, 
                                                     CopyAccess& setAccess) {
    DebugValidator::IsTrue(setAccess.Type == CopyAccess_Set);
    DebugValidator::IsNotNull(setAccess.DestinationVariable);
    
    // Each scalarized component is initialized directly
    // with the appropriate value, while the rest
    // of the aggregate is initialized using 'setMemory'.
    NodeList destList;

    // Obtain the list of scalarized components
    // that are affected by the set.
    int index = varToCandidateIndex_[setAccess.DestinationVariable];
    auto startNode = GetNodeForAccessPath(candidates_[index],
                                          setAccess.DestinationPath);
    AddInRegionToList(candidates_[index].Data->TreeRoot,
                      startNode->Offset, setAccess.Size, destList);

    // If there are no scalarized components affected
    // we have nothing to do.
    if(destList.Count() == 0) {
        return;
    }

    IRGenerator irGen(funct_->ParentUnit());
    auto block = setAccess.IntrinsicCall->ParentBlock();
    auto setConst = setAccess.IntrinsicCall->GetArgument(1)->As<IntConstant>();

    // Process the scalarized components.
    for(int i = 0; i < destList.Count(); i++) {
        // Create the constant that is used to initialize
        // the scalarized components.
        auto replacement = destList[i]->Replacement;
        auto initConst = CreateConstant(replacement->GetType(), 
                                        setConst->Value());

        // Store the value into the replacement variable.
        auto replacementRef = irGen.GetVariableRef(destList[i]->Replacement);
        auto storeInstr = irGen.GetStore(replacementRef, initConst);
        block->InsertInstructionBefore(storeInstr, setAccess.IntrinsicCall);
    }

    // Now initialize the regions that are not covered 
    // by the scalarized components.
    int destOffset = startNode->Offset;
    int finalDestOffset = destOffset + setAccess.Size;

    for(int i = 0; i < destList.Count(); i++) {
        if(destList[i]->Offset > destOffset) {
            GenerateMemorySet(irGen.GetVariableRef(candidate.Base),
                              setConst, destOffset,
                              destList[i]->Offset - destOffset,
                              setAccess.IntrinsicCall);
        }

        destOffset += (destList[i]->Offset - destOffset) +
                       destList[i]->Size;
    }

    // There is space at the end of the aggregate that
    // may have not been covered by the components, set it now.
    if(destOffset != finalDestOffset) {
        GenerateMemorySet(irGen.GetVariableRef(candidate.Base),
                          setConst, destOffset,
                          finalDestOffset - destOffset,
                          setAccess.IntrinsicCall);
    }

    // The call can now be deleted.
    CopyAccessReplaced(setAccess);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ScalarReplacementOfAggregates::CreateConstant(const Type* requiredType, 
                                                       __int64 value) {
    // Create a constant having the required of bytes
    // by "multiplying" the constant byte ('setMemory works
    // at the byte level, so the constant is a byte).
    auto& constTable = funct_->ParentUnit()->Constants();
    value = IntArithmetic::LimitToType(value, IntegerType::GetInt8());

    if(value == 0) {
        return constTable.GetInt(requiredType, 0);
    }
    else {
        auto requiredIntType = requiredType->As<IntegerType>();
        __int64 newValue = value;

        for(int i = 1; i < requiredIntType->Size(); i++) {
            newValue <<= 8;
            newValue |= value;
        }

        return constTable.GetInt(requiredType, value);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::GenerateMemorySet(Operand* destAddress, 
                                                      Operand* valueOp,
                                                      int toOffset, int size, 
                                                      Instruction* beforeInstr) {
    DebugValidator::IsNotNull(destAddress);
    DebugValidator::IsNotNull(valueOp);
    DebugValidator::IsLarger(size, 0);

    // First we convert the address to an 'int8' pointer,
    // increment them with the corresponding offsets, then call 
    // the 'setMemory' intrinsic.
    IRGenerator irGen(funct_->ParentUnit());
    auto block = beforeInstr->ParentBlock();
    auto pointerType = irGen.GetInt8Pointer();

    // Convert to 'int8' pointer.
    auto destAddressPtop = irGen.GetTemporary(pointerType);
    auto ptopInstr1 = irGen.GetPtop(destAddress, pointerType,
                                    destAddressPtop);
    block->InsertInstructionBefore(ptopInstr1, beforeInstr);

    // Increment the addresses using 'addr'.
    auto toOffsetConst = irGen.GetInt32Const(toOffset);
    auto destAddressAddr = irGen.GetTemporary(pointerType);
    auto addrInstr1 = irGen.GetAddress(destAddressPtop, toOffsetConst,
                                       destAddressAddr);
    block->InsertInstructionBefore(addrInstr1, beforeInstr);

    // Call 'setMemory'.
    auto sizeConst = irGen.GetInt32Const(size);
    auto callInstr = irGen.GetSetMemoryCall(destAddressAddr,
                                            valueOp, sizeConst);
    block->InsertInstructionBefore(callInstr, beforeInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ScalarReplacementOfAggregates::TreeNode* 
ScalarReplacementOfAggregates::AdvanceToNextComponent(NodeList& list, 
                                                      int& nodeIndex) {
    // Check if there are still components in the list.
    if(nodeIndex < list.Count()) {
        auto node = list[nodeIndex];
        node->Processed = false;
        nodeIndex++;
        return node;
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::PatchCallAccess(Candidate& candidate) {
    // Before the call is made we need to make sure that
    // the replaced components are written back to the aggregate,
    // because the callee may access them. Note that this is done
    // only if the callee may read from the aggregate.
    // If the callee may write to the aggregate, we need to reload
    // all components (we don't know exactly which components are written).
    auto& calls = candidate.CallAccesses;

    for(int i = 0; i < calls.Count(); i++) {
        // If only a part of the aggregate is an argument
        // then we write only the replacements that are affected.
        if(calls[i].IsNoRead == false) {
            auto node = GetNodeForAccessPath(candidate, calls[i].Path);
            WriteReplacementsBack(node, candidate.Base, calls[i].Call);
        }

        if(calls[i].IsNoWrite == false) {
            auto node = GetNodeForAccessPath(candidate, calls[i].Path);
            ReloadAllReplacements(node, candidate.Base, 
                                  calls[i].Call->NextInstruction());
        }

        CallAccessReplaced(calls[i]);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::CreateTree(Candidate& candidate) {
    if(candidate.NeedsTree) {
        if(candidate.Data == nullptr) {
            candidate.Data = new CandidateData();
        }

        auto candidateType = candidate.Base->GetType();
        AccessPath path;
        candidate.Data->TreeRoot = CreateTree(candidateType, 0,
                                              candidate.Data, path);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ScalarReplacementOfAggregates::TreeNode* 
ScalarReplacementOfAggregates::CreateTree(const Type* type, int offset, 
                                          CandidateData* data, AccessPath& path) {
    DebugValidator::IsNotNull(type);
    DebugValidator::IsNotNull(data);

    // Create the node for this component, then for the
    // child components if we have a record or an array.
    TreeNode* node = new TreeNode(type);
    node->Offset = offset;
    node->Size = TI::GetSize(type, GetTarget());
    node->Path = path;

    // Check if we have a replacement variable
    // for this component.
    if(data->Replacements.ContainsKey(path)) {
        node->Replacement = data->Replacements[path];
        node->ScalarizedChildren = 1;
    }

    if(auto recordType = type->As<RecordType>()) {
        // Construct the access path for the elements.
        int last = path.Indices.Count();
        path.Indices.Add(0);

        for(int i = 0; i < recordType->FieldCount(); i++) {
            path.Indices[last] = i;
            int childOffset = offset + recordType->GetFieldOffset(i);
			
            TreeNode* childNode = CreateTree(recordType->GetFieldType(i),
                                             childOffset, data, path);
            node->Children.Add(childNode);
            node->ScalarizedChildren += childNode->ScalarizedChildren;
        }

        // Restore the access path.
        path.Indices.RemoveAt(last);
    }
    else if(auto arrayType = type->As<ArrayType>()) {
        // Construct the access path for the elements.
        int elemSize = TI::GetSize(arrayType->ElementType(), GetTarget());
        int last = path.Indices.Count();
        path.Indices.Add(0);

        for(int i = 0; i < arrayType->Size(); i++) {
            path.Indices[last] = i;
            int childOffset = offset + (elemSize * i);
			
            TreeNode* childNode = CreateTree(arrayType->ElementType(),
                                             childOffset, data, path);
            node->Children.Add(childNode);
            node->ScalarizedChildren += childNode->ScalarizedChildren;
        }

        // Restore the access path.
        path.Indices.RemoveAt(last);
    }

    return node;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::AddInRegionToList(TreeNode* node, int offset, 
                                                      int size, NodeList& list) {
    // Check if the component intersects with the range
    // [offset, offset + size]. If it does not then we don't
    // need to check any child components, because they will
    // definitely not intersect too.
    if(HasCommonRegion(node, offset, size) == false) {
        return;
    }

    // If this is a component that has been replaced 
    // (a leaf in the tree), add it to the list.
    if(node->Children.Count() == 0) {
        if(node->Replacement) {
            list.Add(node);
        }
    }
    else {
        // Process each child component in the tree.
        for(int i = 0; i < node->Children.Count(); i++) {
            AddInRegionToList(node->Children[i], offset, size, list);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::HasCommonRegion(TreeNode* node,
                                                    int offset, int size) {
    // Check if the range covered by the component
    // intersect with the range [offset, offset + size].
    return (((offset + size) < node->Offset) ||
           (offset > (node->Offset + node->Size))) == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ScalarReplacementOfAggregates::TreeNode* 
ScalarReplacementOfAggregates::GetNodeForAccessPath(Candidate& candidate, 
                                                    AccessPath& path) {
    DebugValidator::IsNotNull(candidate.Data);
    DebugValidator::IsNotNull(candidate.Data->TreeRoot);
    TreeNode* node = candidate.Data->TreeRoot;

    // Apply the access path on the tree nodes.
    for(int i = 0; i < path.Indices.Count(); i++) {
        node = node->Children[path.Indices[i]];
    }

    return node;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::WriteReplacementsBack(TreeNode* node, 
                                                          Variable* variable,
                                                          Instruction* beforeInstr) {
    DebugValidator::IsNotNull(node);
    DebugValidator::IsNotNull(variable);
    DebugValidator::IsNotNull(beforeInstr);

    // If this is a replaced variable (a leaf in the tree)
    // we generate code that writes it back to the aggregate.
    // Else we process each child that has replacements.
    if(node->Children.Count() == 0) {
        if(node->Replacement) {
            // Load the replacement variable, generate the
            // address of the component, then store the
            // replacement at that address.
            auto block = beforeInstr->ParentBlock();
            auto loadedOp = LoadReplacement(node->Replacement, beforeInstr);
            auto addressOp = GetAddressForPath(node->Path, variable,
                                                     beforeInstr);
            auto instr = StoreInstr::GetStore(addressOp, loadedOp);
            block->InsertInstructionBefore(instr, beforeInstr);
        }
    }
    else {
        // Write the replacements of the children.
        for(int i = 0; i < node->Children.Count(); i++) {
            auto child = node->Children[i];

            if(child->ScalarizedChildren > 0) {
                WriteReplacementsBack(child, variable, beforeInstr);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ScalarReplacementOfAggregates::GetAddressForPath(AccessPath& path, 
                                                          Variable* variable,
                                                          Instruction* beforeInstr) {
    DebugValidator::IsNotNull(variable);
    DebugValidator::IsNotNull(beforeInstr);

    // Create instructions that compute the address
    // of the component accessed by the path.
    IRGenerator irGen(funct_->ParentUnit());
    auto block = beforeInstr->ParentBlock();

    auto& indexes = path.Indices;
    auto variableRef = irGen.GetVariableRef(variable);
    auto lastType = variable->GetType();
    Operand* addressOp = variableRef;

    for(int i = 0; i < indexes.Count(); i++) {
        int index = indexes[i];
        auto indexConst = irGen.GetIntConst(IntegerType::GetInt32(), index);

        if(auto recordType = lastType->As<RecordType>()) {
            // The result's type is 'pointer to field type'.
            auto fieldType = recordType->GetFieldType(indexes[i]);
            auto addressType = irGen.GetPointer(fieldType);
            auto newAddressOp = irGen.GetTemporary(addressType);

            auto instr = irGen.GetElement(addressOp, indexConst, 
                                          newAddressOp);
            block->InsertInstructionBefore(instr, beforeInstr);
            
            addressOp = newAddressOp;
            lastType = fieldType;
        }
        else if(auto arrayType = lastType->As<ArrayType>()) {
            // The result's type is 'pointer to element type'.
            auto elementType = arrayType->ElementType();
            auto addressType = irGen.GetPointer(elementType);
            auto newAddressOp = irGen.GetTemporary(addressType);

            auto instr = irGen.GetIndex(addressOp, indexConst, 
                                        newAddressOp);
            block->InsertInstructionBefore(instr, beforeInstr);
            
            addressOp = newAddressOp;
            lastType = elementType;
        }
        else DebugValidator::Unreachable();
    }

    return addressOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ScalarReplacementOfAggregates::LoadReplacement(Variable* replacement,
                                                        Instruction* beforeInstr) {
    // Generate code that loads the variable.
    IRGenerator irGen(funct_->ParentUnit());
    auto variableRef = irGen.GetVariableRef(replacement);
    auto loadedOp = irGen.GetTemporary(replacement->GetType());

    auto instr = irGen.GetLoad(variableRef, loadedOp);
    beforeInstr->ParentBlock()->InsertInstructionBefore(instr, beforeInstr);
    return loadedOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::GenerateMemoryCopy(CopyAccess& access, 
                                                       int fromOffset, int toOffset, 
                                                       int size, Instruction* beforeInstr) {
    // If we're copying to/from scalarization candidates
    // then we need to create variable references, else
    // we use the operands of the operation directly.
    IRGenerator irGen(funct_->ParentUnit());
    Operand* destAddress = access.DestinationVariable ?
                           irGen.GetVariableRef(access.DestinationVariable) :
                           access.IntrinsicCall->GetArgument(0);
    Operand* sourceAddress = access.SourceVariable ?
                             irGen.GetVariableRef(access.SourceVariable) :
                             access.IntrinsicCall->GetArgument(1);

    GenerateMemoryCopy(destAddress, sourceAddress, 
                       fromOffset, toOffset, size, beforeInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::GenerateMemoryCopy(Operand* destAddress,
                                                       Operand* sourceAddress,
                                                       int fromOffset, int toOffset, 
                                                       int size, Instruction* beforeInstr) {
    DebugValidator::IsNotNull(destAddress);
    DebugValidator::IsNotNull(sourceAddress);
    DebugValidator::IsLarger(size, 0);

    // First we convert both addresses to 'int8' pointers,
    // increment them with the corresponding offsets, then call 
    // the 'copyMemory' intrinsic. It's possible to produce more
    // efficient code, but we leave it for other optimization passes.
    IRGenerator irGen(funct_->ParentUnit());
    auto block = beforeInstr->ParentBlock();
    auto pointerType = irGen.GetInt8Pointer();

    // Convert to 'int8' pointer.
    auto destAddressPtop = irGen.GetTemporary(pointerType);
    auto ptopInstr1 = irGen.GetPtop(destAddress, pointerType,
                                    destAddressPtop);
    block->InsertInstructionBefore(ptopInstr1, beforeInstr);

    auto sourceAddressPtop = irGen.GetTemporary(pointerType);
    auto ptopInstr2 = irGen.GetPtop(sourceAddress, pointerType,
                                    sourceAddressPtop);
    block->InsertInstructionBefore(ptopInstr2, beforeInstr);

    // Increment the addresses using 'addr'.
    auto toOffsetConst = irGen.GetInt32Const(toOffset);
    auto destAddressAddr = irGen.GetTemporary(pointerType);
    auto addrInstr1 = irGen.GetAddress(destAddressPtop, toOffsetConst,
                                       destAddressAddr);
    block->InsertInstructionBefore(addrInstr1, beforeInstr);

    auto fromOffsetConst = irGen.GetInt32Const(fromOffset);
    auto sourceAddressAddr = irGen.GetTemporary(pointerType);
    auto addrInstr2 = irGen.GetAddress(sourceAddressPtop, fromOffsetConst,
                                       sourceAddressAddr);
    block->InsertInstructionBefore(addrInstr2, beforeInstr);

    // Call 'copyMemory'.
    auto callInstr = irGen.GetCopyMemoryCall(destAddressAddr,
                                             sourceAddressAddr,
                                             irGen.GetInt32Const(size));
    block->InsertInstructionBefore(callInstr, beforeInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::ReloadReplacement(Variable* variable, 
                                                      AccessPath& path,
                                                      Variable* replacement, 
                                                      Instruction* beforeInstr) {
    DebugValidator::IsNotNull(variable);
    DebugValidator::IsNotNull(replacement);

    // First we generate code that computes the address
    // of the replaced component base on the path,
    // then we load the value and store it into the replacement.
    IRGenerator irGen(funct_->ParentUnit());
    auto block = beforeInstr->ParentBlock();
    auto addressOp = GetAddressForPath(path, variable, beforeInstr);

    auto loadedOp = irGen.GetTemporary(replacement->GetType());
    auto loadInstr = irGen.GetLoad(addressOp, loadedOp);
    block->InsertInstructionBefore(loadInstr, beforeInstr);
    
    auto replacementRef = irGen.GetVariableRef(replacement);
    auto storeInstr = irGen.GetStore(replacementRef, loadedOp);
    block->InsertInstructionBefore(storeInstr, beforeInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::ReloadAllReplacements(TreeNode* node, 
                                                          Variable* variable,
                                                          Instruction* beforeInstr) {
    DebugValidator::IsNotNull(node);
    DebugValidator::IsNotNull(variable);
    DebugValidator::IsNotNull(beforeInstr);

    // If this is a replaced variable (a leaf in the tree)
    // we generate code that reloads it from the aggregate.
    // Else we process each child that has replacements.
    if(node->Children.Count() == 0) {
        if(node->Replacement) {
            ReloadReplacement(variable, node->Path,
                              node->Replacement, beforeInstr);
        }
    }
    else {
        // Reload the replacements of the children.
        for(int i = 0; i < node->Children.Count(); i++) {
            auto child = node->Children[i];

            if(child->ScalarizedChildren > 0) {
                ReloadAllReplacements(child, variable, beforeInstr);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::MakeReplacementCopy(Variable* destReplacement,
                                                        Variable* sourceReplacement, 
                                                        Instruction* beforeInstr) {
    DebugValidator::IsNotNull(destReplacement);
    DebugValidator::IsNotNull(sourceReplacement);
    DebugValidator::IsNotNull(beforeInstr);

    // Generate a load from the source variable, then store 
    // the loaded value to the destination variable.
    IRGenerator irGen(funct_->ParentUnit());
    auto block = beforeInstr->ParentBlock();

    auto loadedOp = irGen.GetTemporary(sourceReplacement->GetType());
    auto sourceReplacementRef = irGen.GetVariableRef(sourceReplacement);
    auto loadInstr = irGen.GetLoad(sourceReplacementRef, loadedOp);
    block->InsertInstructionBefore(loadInstr, beforeInstr);
                                   
    auto destReplacementRef = irGen.GetVariableRef(destReplacement);
    auto storeInstr = irGen.GetStore(destReplacementRef, loadedOp);
    block->InsertInstructionBefore(storeInstr, beforeInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::WriteReplacementToMemory(Variable* replacement, 
                                                             CopyAccess& access, 
                                                             int toOffset,
                                                             Instruction* beforeInstr) {
    DebugValidator::IsNotNull(replacement);
    DebugValidator::IsLargerOrEqual(toOffset, 0);
    DebugValidator::IsNotNull(beforeInstr);

    // Load the value of the replacement, compute the offset
    // into the other candidate, then store the value.
    IRGenerator irGen(funct_->ParentUnit());
    auto block = beforeInstr->ParentBlock();

    Operand* destAddress = access.DestinationVariable ?
                           irGen.GetVariableRef(access.DestinationVariable) :
                           access.IntrinsicCall->GetArgument(0);
    
    auto loadedOp = irGen.GetTemporary(replacement->GetType());
    auto replacementRef = irGen.GetVariableRef(replacement);
    auto loadInstr = irGen.GetLoad(replacementRef, loadedOp);
    block->InsertInstructionBefore(loadInstr, beforeInstr);

    // Convert to 'int8' pointer in order to compute the offset,
    // then to the replacement type's pointer.
    auto destPtop1 = irGen.GetTemporary(irGen.GetInt8Pointer());
    auto ptopInstr1 = irGen.GetPtop(destAddress, irGen.GetInt8Pointer(), destPtop1);
    block->InsertInstructionBefore(ptopInstr1, beforeInstr);

    // Compute the offset.
    auto destIncremented = irGen.GetTemporary(irGen.GetInt8Pointer());
    auto offsetConst = irGen.GetInt32Const(toOffset);
    auto addrInstr = irGen.GetAddress(destPtop1, offsetConst, destIncremented);
    block->InsertInstructionBefore(addrInstr, beforeInstr);

    // Now store the loaded value.
    auto destType = irGen.GetPointer(replacement->GetType());
    auto destPtop2 = irGen.GetTemporary(destType);
    auto ptopInstr2 = irGen.GetPtop(destIncremented, destType, destPtop2);
    block->InsertInstructionBefore(ptopInstr2, beforeInstr);

    auto storeInstr = irGen.GetStore(destPtop2, loadedOp);
    block->InsertInstructionBefore(storeInstr, beforeInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::ImproveFunction() {
    // We look for 'load' instructions that target an operand
    // resulting from  'phi' or 'quest' instruction. This makes
    // the associated variables to appear as address-taken, 
    // even if they are not (at least in these cases). If we know 
    // that theses operands can't be null,  we can load each of them 
    // and 'phi'/'quest' on the loaded values. For example,
    // t1 = phi {p1, B1}, {p2, B2}   ->   B1: t1 = load p1
    // t2 = load t1                       B2: t2 = load p2
    //                                        t3 = phi {t1, B1}, {t2, B2}
    bool improved = false; 

    funct_->ForEachInstruction([this, &improved](Instruction* instr) -> bool {
        if(auto loadInstr = instr->As<LoadInstr>()) {
            // If the 'load' is volatile don't touch it.
            if(loadInstr->IsVolatile()) {
                return true;
            }

            auto sourceOp = loadInstr->SourceOp();

            if(auto phiInstr = sourceOp->DefiningInstrAs<PhiInstr>()) {
                if(ImproveLoadFromPhi(loadInstr, phiInstr)) {
                    improved = true;
                }
            }
            else if(auto questInstr = sourceOp->DefiningInstrAs<QuestionInstr>()) {
                if(ImproveLoadFromQuestion(loadInstr, questInstr)) {
                    improved = true;
                }
            }
        }
        return true;
    });

    return improved;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::ImproveLoadFromPhi(LoadInstr* loadInstr, 
                                                       PhiInstr* phiInstr) {
    // The 'phi' should be used only by 'load' instructions.
    if(UsedOnlyByLoad(phiInstr->ResultOp()) == false) {
        return false;
    }

    // Make sure that all operands are known not to be null,
    // else we could introduce exceptions that were not
    // in the original program.
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());

    for(int i = 0; i < phiInstr->OperandCount(); i++) {
        auto op = phiInstr->GetOperand(i);

        // None of the operands should be a null constant
        // or undefined.
        if(op->IsNullConstant() || op->IsUndefinedConstant()) {
            return false;
        }

        if(opInfo.IsPointerNotNull(phiInstr->GetOperand(i)) == false) {
            return false;
        }
    }

    // If the incoming operands may be written to 
    // in this block we give up.
    for(int i = 0; i < phiInstr->OperandCount(); i++) {
        if(MayBeWrittenTo(phiInstr->GetOperand(i), 
                          loadInstr->PreviousInstruction())) {
            return false;
        }
    }

    // All is OK, insert 'load' instruction in the parent block
    // of each operand, then a 'phi' that combines the loaded values.
    IRGenerator irGen(funct_->ParentUnit());
    auto resultType = phiInstr->ResultOp()->GetType();
    auto loadedType = resultType->As<PointerType>()->PointeeType();

    auto phiResultOp = irGen.GetTemporary(loadedType);
    auto newPhiInstr = irGen.GetPhi(phiResultOp, phiInstr->OperandCount());
    phiInstr->ParentBlock()->InsertInstructionBefore(newPhiInstr, phiInstr);

    for(int i = 0; i < phiInstr->OperandCount(); i++) {
        auto op = phiInstr->GetOperand(i);
        auto block = phiInstr->GetOperandBlock(i)->Target();

        // Load the incoming operand and add it to the new 'phi'.
        auto loadedOp = irGen.GetTemporary(loadedType);
        auto loadInstr = irGen.GetLoad(op, loadedOp);
        block->InsertInstructionBefore(loadInstr, block->LastInstruction());
        newPhiInstr->AddOperand(loadedOp, phiInstr->GetOperandBlock(i));
    }

    // Replace the 'load' with the new 'phi' result.
    if(loadInstr->HasDestinationOp()) {
        loadInstr->ResultOp()->ReplaceWith(phiResultOp);
        loadInstr->RemoveFromBlock(true);
    }

    // If this is the last use of the original 'phi'
    // delete it now, else it confuses the address-taken analysis.
    AccessImproved(phiInstr);

    if(phiInstr->ResultOp()->HasUsers() == false) {
        phiInstr->RemoveFromBlock(true);
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::ImproveLoadFromQuestion(LoadInstr* loadInstr, 
                                                            QuestionInstr* questInstr) {
    // The 'quest' should be used only by 'load' instructions.
    if(UsedOnlyByLoad(questInstr->ResultOp()) == false) {
        return false;
    }

    // Make sure that both operands are known not to be null,
    // else we could introduce exceptions that were not
    // in the original program.
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());

    if((opInfo.IsPointerNotNull(questInstr->TrueOp()) &&
        opInfo.IsPointerNotNull(questInstr->FalseOp())) == false) {
        return false;
    }

    // If the operands may be written to in this block
    // then we give up.
    if(MayBeWrittenTo(questInstr->TrueOp(), loadInstr->PreviousInstruction()) ||
       MayBeWrittenTo(questInstr->FalseOp(), loadInstr->PreviousInstruction())) {
        return false;
    }

    // Create code that loads both operands, then
    // a 'quest' that selects between the loaded values.
    IRGenerator irGen(funct_->ParentUnit());
    auto block = questInstr->ParentBlock();
    auto resultType = questInstr->ResultOp()->GetType();
    auto loadedType = resultType->As<PointerType>()->PointeeType();

    // Load the 'true' operand.
    auto loadedTrueOp = irGen.GetTemporary(loadedType);
    auto loadInstr1 = irGen.GetLoad(questInstr->TrueOp(), loadedTrueOp);
    block->InsertInstructionBefore(loadInstr1, questInstr);

    // Load the 'false' operand.
    auto loadedFalseOp = irGen.GetTemporary(loadedType);
    auto loadInstr2 = irGen.GetLoad(questInstr->FalseOp(), loadedFalseOp);
    block->InsertInstructionBefore(loadInstr2, questInstr);

    // Create the new 'quest' and replace the 'load' with it.
    auto questResultOp = irGen.GetTemporary(loadedType);
    auto newQuestInstr = irGen.GetQuestion(questInstr->ConditionOp(),
                                           loadedTrueOp, loadedFalseOp,
                                           questResultOp);
    if(loadInstr->HasDestinationOp()) {
        loadInstr->ResultOp()->ReplaceWith(questResultOp);
        loadInstr->RemoveFromBlock(true);
    }

    // If this is the last use of the original 'quest'
    // delete it now, else it confuses the address-taken analysis.
    AccessImproved(questInstr);

    if(questInstr->ResultOp()->HasUsers() == false) {
        questInstr->RemoveFromBlock(true);
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::UsedOnlyByLoad(Temporary* temp) {
    bool valid = true;
    
    temp->ForEachUser([&valid](Instruction* user, int i) -> bool {
        if(user->IsLoad() == false) {
            valid = false;
            return false;
        }
        else return true;
    });

    return valid;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ScalarReplacementOfAggregates::MayBeWrittenTo(Operand* addressOp, 
                                                   Instruction* startInstr) {
    //! TODO: USE ALIAS ANALYSIS!!!
    while(startInstr) {
        if(startInstr->IsCall() || startInstr->IsStore()) {
            return true;
        }

        startInstr = startInstr->PreviousInstruction();
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ScalarReplacementOfAggregates::AccessPath::ToString() const {
    string path = "";

    for(int i = 0; i < Indices.Count(); i++) {
        path += string::Format(L" %d,", Indices[i]);
    }

    return path;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ScalarReplacementOfAggregates::Access::ToString() const {
    string text = Type == Access_Load ? "Load: " : "Store: ";
    text += Path.ToString();
    text += "\n    Block:" + *Source->ParentBlock()->Name();
    return text;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ScalarReplacementOfAggregates::CopyAccess::ToString() const {
    string text = Type == CopyAccess_Copy ? "Copy: " : "Set: ";
    text += "\n    Destination:";
    text += "\n        Variable: " + (DestinationVariable ? 
                                      *DestinationVariable->Name() : "-");
    text += "\n        Path: " + (DestinationVariable ?
                                  DestinationPath.ToString() : "-");
    text += "\n    Source:";
    text += "\n        Variable: " + (SourceVariable ? 
                                      *SourceVariable->Name() : "-");
    text += "\n        Path: " + (SourceVariable ?
                                  SourcePath.ToString() : "-");
    text += "\n    Block: " + *IntrinsicCall->ParentBlock()->Name();
    return text;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ScalarReplacementOfAggregates::CallAccess::ToString() const {
    string text = "Call";
    text += Path.ToString();
    text += "\n    No Write: ";
    text += (IsNoWrite ? "Yes" : "No");
    text += "\n    No Read: ";
    text += (IsNoRead ? "Yes" : "No");
    text += "\n    Block: " + *Call->ParentBlock()->Name();
    return text;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ScalarReplacementOfAggregates::Candidate::ToString() const {
    string text = "** Candidate: " + *Base->Name() + " **";
    text += "\n\nComponents: " + string::Format(L"%d", ComponentCount);
    text += "\nHas Store: ";
    text += (HasStore ? "Yes" : "No");
    text += "\nHas Load: ";
    text += (HasLoad ? "Yes" : "No");
    text += "\nNeeds Tree: ";
    text += (NeedsTree ? "Yes" : "No");

    text += "\n\n-> Accesses:" + string::Format(L"%d", Accesses.Count());

    for(int i = 0; i < Accesses.Count(); i++) {
        text += string::Format(L"\n\n(%d) ", i);
        text += Accesses[i].ToString();
    }

    text += "\n\n-> Copy Accesses:" + string::Format(L"%d", CopyAccesses.Count());

    for(int i = 0; i < CopyAccesses.Count(); i++) {
        text += string::Format(L"\n\n(%d) ", i);
        text += CopyAccesses[i].ToString();
    }

    text += "\n\n-> Call Accesses:" + string::Format(L"%d", CallAccesses.Count());

    for(int i = 0; i < CallAccesses.Count(); i++) {
        text += string::Format(L"\n\n(%d) ", i);
        text += CallAccesses[i].ToString();
    }

    if(Data) {
        text += "\n\n-> Scalarized Components:" + 
                string::Format(L"%d", Data->Replacements.Count());
        typedef Dictionary<AccessPath, Variable*>::TPair TPair;
        Data->Replacements.ForEach([&text](TPair& pair) -> bool {
            text += "\n    " + pair.Key.ToString();
            return true;
        });
    }

    return text;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::Dump() {
    string text;

    for(int i = 0; i < candidates_.Count(); i++) {
        text += "\n" + candidates_[i].ToString();
    }

    ObjectDumper(text, "Scalar replacement").Dump();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::AccessReplaced(Access& access) {
#if 1
    auto function = access.Source->ParentFunction();
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
    Log::Warning("SRA Access in" + functionName + ":" + access.ToString());
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::CopyAccessReplaced(CopyAccess& access) {
#if 1
    auto function = access.IntrinsicCall->ParentFunction();
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
    Log::Warning("SRA Copy Access in" + functionName + ":" + access.ToString());
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::CallAccessReplaced(CallAccess& access) {
#if 1
    auto function = access.Call->ParentFunction();
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
    Log::Warning("SRA Call Access in" + functionName + ":" + access.ToString());
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScalarReplacementOfAggregates::AccessImproved(Instruction* instr) {
#if 1
	auto block = instr->ParentBlock();
	auto function = instr->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string text = IRPrinter(instr).ToString();
	Log::Warning("SRA improvement in " + functionName + ":" + blockName +
				 ": " + text);
#endif
}

} // namespace Optimization