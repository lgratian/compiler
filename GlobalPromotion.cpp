// GlobalPromotion.hpp
// Copyright (c) Lup Gratian
//
// Implements the GlobalPromotion pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "GlobalPromotion.hpp"

namespace Optimization {

void GlobalPromotion::Execute(Unit* unit, CallGraph* callGraph) {
    callGraph_ = callGraph;
    unit_ = unit;

    // Compute the postorder numbers for the functions
    // using the call graph. The numbers are necessary when
    // trying to mark a variable as being constant, for example.
    ComputePostorderNumbers();

    // Process all function that have a definition.
    for(auto function = unit->Functions().First(); 
        function; function = function->Next) {
        if(function->Value->IsDefinition()) {
            ProcessInstructions(function->Value);
        }
    }

#if 1
    Dump();
#endif

    // Optimize using the gathered information.
    PromoteFunctionsToStatic();
    PromoteVariablesToConstants();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ComputePostorderNumbers() {
    local<PostorderVisitor> visitor = new PostorderVisitor(&postorderNumbs_);
    callGraph_->FindRoots(true);
    callGraph_->PostorderTraversalFromRoots(visitor);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::PostorderVisitor::
     Visit(CallNode* node, CallGraph* callGraph) {
    // Assign the postorder number for function definitions.
    auto functionRef = node->GetFunctionReference();

    if(functionRef->IsDefinition()) {
        postorderNumbs_->Add(functionRef, postorderNumb_);
    }

    postorderNumb_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::PostorderVisitor::
     Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) {
    // The same postorder number is assigned to all 
    // function definitions in the group.
    for(int i = 0; i < nodeGroup->NodeCount(); i++) {
        auto node = nodeGroup->GetNode(i);
        
        if(node->IsNodeGroup() == false) {
            auto callNode = static_cast<CallNode*>(node);
            auto functionRef = callNode->GetFunctionReference();

            if(functionRef->IsDefinition()) {
                postorderNumbs_->Add(functionRef, postorderNumb_);
            }
        }
    }

    postorderNumb_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessInstructions(Function* function) {
    // Process each instructions and check if the tracked
    // parameter/operands escape because one of them.
    TrackedReferencesDict trackedRefs;
    auto instructionEnum = function->GetInstructionEnum();

    while(instructionEnum.IsValid()) {
        auto instr = instructionEnum.Next();
        
        if(auto loadInstr = instr->As<LoadInstr>()) {
            ProcessLoad(loadInstr, trackedRefs);
        }
        else if(auto storeInstr = instr->As<StoreInstr>()) {
            ProcessStore(storeInstr, trackedRefs);
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            ProcessCall(callInstr, trackedRefs);
        }
        else if(auto retInstr = instr->As<ReturnInstr>()) {
            ProcessReturn(retInstr, trackedRefs);
        }
        else if(auto ptopInstr = instr->As<PtopInstr>()) {
            TrackResultOperand(ptopInstr->ResultOp(), 
                               ptopInstr->TargetOp(), trackedRefs);
        }
        else if(instr->IsAddressing()) {
            // 'index', 'addr' and 'elem'.
            TrackResultOperand(instr->GetDestinationOp(),
                               instr->GetSourceOp(0), trackedRefs);
        }
        else if(auto cmpInstr = instr->As<CmpInstrBase>()) {
            ProcessCompare(cmpInstr, trackedRefs);
        }
        else if(auto phiInstr = instr->As<PhiInstr>()) {
            ProcessPhi(phiInstr, trackedRefs);
        }
        else if(auto questInstr = instr->As<QuestionInstr>()) {
            ProcessQuestion(questInstr, trackedRefs);
        }
        else {
            // Mark as address-taken any global used
            // as a source operand for all other instruction types.
            for(int i = 0; i < instr->SourceOpCount(); i++) {
                GlobalInfoList globals;

                if(GetGlobalInfo(instr->GetSourceOp(i), trackedRefs, globals)) {
                    for(int j = 0; j < globals.Count(); j++) {
                        AddUser(globals[j], instr);
                        MarkAddressTaken(globals[j]);
                    }
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::TrackResultOperand(Operand* resultOp, Operand* sourceOp,
                                         TrackedReferencesDict& trackedRefs) {
    // Ignore instructions that have no users.
    if(resultOp == nullptr) {
        return;
    }

    // Any global associated with the source operand
    // is associated with the resulting one too.
    GlobalInfoList globals;

    if(GetGlobalInfo(sourceOp, trackedRefs, globals)) {
        if(trackedRefs.ContainsKey(resultOp) == false) {
            // This is the first time the operand is added.
            trackedRefs.Add(resultOp, globals);
        }
        else {
            // The operand is already tracked, combine the sets.
            auto& previousRefs = trackedRefs[resultOp];

            for(int i = 0; i < globals.Count(); i++) {
                auto global = globals[i];

                if(previousRefs.Contains(global)) {
                    previousRefs.Add(global);
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessLoad(LoadInstr* instr, TrackedReferencesDict& trackedRefs) {
    // We're interested only in global variables.
    GlobalInfoList sourceGlobals;

    if(GetGlobalInfo(instr->SourceOp(), trackedRefs, sourceGlobals)) {
        for(int i = 0; i < sourceGlobals.Count(); i++) {
            auto global = sourceGlobals[i];
            global->HasRead = true;
            AddUser(global, instr);
            UpdateReadPostorder(global, instr);

            // If the global is a single-dimension array
            // mark the index of the read element.
            AccessPath path;

            if(FindAccessPath(instr->SourceOp(), path, global->TrackedReference)) {
               if(path.Indices.Count() == 1) {
                    // We use a bitvector to mark the index.
                    global->ReadPositions.SetBit(path.Indices[0]);
               }
            }
            else {
                // Mark the fact that we don't know exactly
                // which elements are read.
                global->HasUnknownPositionRead = true;
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessStore(StoreInstr* instr, TrackedReferencesDict& trackedRefs) {
    // We're interested only in global variables.
    // If the source operand is stored we consider it's address taken.
    GlobalInfoList sourceGlobals;

    if(GetGlobalInfo(instr->SourceOp(), trackedRefs, sourceGlobals)) {
        for(int i = 0; i < sourceGlobals.Count(); i++) {
            AddUser(sourceGlobals[i], instr);
            MarkPassedToUnknown(sourceGlobals[i]);
        }
    }

    // Mark each destination global as being written,
    // and try to determine if it's initialized with constants.
    GlobalInfoList destGlobals;

    if(GetGlobalInfo(instr->DestinationOp(), trackedRefs, destGlobals)) {
        for(int i = 0; i < destGlobals.Count(); i++) {
            auto global = destGlobals[i];
            global->HasWrite = true;
            AddUser(global, instr);
            UpdateWritePostorder(global, instr);
        }

        // Check if the stored value is a constant used to initialize 
        // the global. Sadly we can do this only if we have a single target.
        if(destGlobals.Count() > 1) {
            for(int i = 0; i < destGlobals.Count(); i++) {
                destGlobals[i]->HasNonConstantWrite = true;
                destGlobals[i]->HasUnknownPositionWrite = true;
            }

            return;
        }

        ConstantList constants;
        bool hasOnlyConstants;
        auto destGlobal = destGlobals[0];

        if(FindConstants(instr->SourceOp(), constants, hasOnlyConstants)) {
            AccessPath path;

            if(FindAccessPath(instr->DestinationOp(), path, 
                              destGlobal->TrackedReference)) {
                AddInitializers(destGlobal, path, constants);
                destGlobal->HasNonConstantWrite |= hasOnlyConstants == false;
            }
            else {
                // Mark the fact that a write with variable index targets 
                // the global (it can't be converted to a constant, for example).
                destGlobal->HasUnknownPositionWrite = true;
                destGlobal->HasNonConstantWrite |= hasOnlyConstants == false;
            }
        }
        else {
            destGlobal->HasNonConstantWrite = true;
            destGlobal->HasUnknownPositionWrite = true;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::FindConstants(Operand* op, ConstantList& constants,
                                    bool& hasOnlyConstants) {
    // We consider the obvious case of a constant operand,
    // but also 'phi' and 'quest' instructions with constants.
    if(auto constant = op->As<Constant>()) {
        constants.Add(constant);
        hasOnlyConstants = true;
        return true;
    }
    else if(auto phiInstr = op->DefiningInstrAs<PhiInstr>()) {
        int constantOps = 0;

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            if(auto constant = phiInstr->GetSourceOp(i)->As<Constant>()) {
                constantOps++;
                AddConstant(constant, constants);
            }
        }

        hasOnlyConstants = constantOps == phiInstr->OperandCount();
        return constantOps > 0;
    }
    else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
        bool trueIsConstant = false;
        bool falseIsConstant = false;

        if(auto constant = questInstr->TrueOp()->As<Constant>()) {
            trueIsConstant = true;
            AddConstant(constant, constants);
        }

        if(auto constant = questInstr->FalseOp()->As<Constant>()) {
            falseIsConstant = true;
            AddConstant(constant, constants);
        }

        hasOnlyConstants = trueIsConstant && falseIsConstant;
        return trueIsConstant || falseIsConstant;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::FindAccessPath(Operand* op, AccessPath& path,
                                     Reference* requiredBase) {
    if(op->HasDefiningInstruction() == false) {
        // Reached the base operand, make sure it's the expected one.
        return op == requiredBase;
    }
    
    // Consider 'index', 'addr' and 'elem' instructions only.
    auto definingInstr = op->DefiningInstruction();

    if(definingInstr->IsIndex() || definingInstr->IsAddress()) {
        // Only constant indices are valid.
        if(auto intConst = definingInstr->GetSourceOp(1)->As<IntConstant>()) {
            if(FindAccessPath(definingInstr->GetSourceOp(0), path, requiredBase)) {
                path.Indices.Add(intConst->Value());
                return true;
            }
        }
    }
    else if(definingInstr->IsElement()) {
        if(FindAccessPath(definingInstr->GetSourceOp(0), path, requiredBase)) {
            auto intConst = definingInstr->GetSourceOp(1)->As<IntConstant>();
            path.Indices.Add(intConst->Value());
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::AddInitializers(GlobalInfo* globalInfo, AccessPath& path, 
                                      ConstantList& constants) {
    PathConstantsPair* pathConstants = nullptr;

    // Check if we already have initializers for the access path.
    for(int i = 0; i < globalInfo->Initializers.Count(); i++) {
        if(globalInfo->Initializers[i].Path == path) {
            pathConstants = &globalInfo->Initializers[i];
            break;
        }
    }

    // Create the path/constant list pair if required.
    if(pathConstants == nullptr) {
        globalInfo->Initializers.Add(PathConstantsPair(path));
        pathConstants = &globalInfo->Initializers.PeekLast();
    }

    // Add the constants, making sure no duplicates appear.
    for(int i = 0; i < constants.Count(); i++) {
        auto constant = constants[i];

        if(pathConstants->Constants.Contains(constant) == false) {
            pathConstants->Constants.Add(constant);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessReturn(ReturnInstr* instr, 
                                    TrackedReferencesDict& trackedRefs) {
    if(instr->IsVoid()) {
        return;
    }

    // Any returned variable is marked as address-taken,
    // non-constant and having writes at unknown positions.
    GlobalInfoList returnedGlobals;

    if(GetGlobalInfo(instr->ReturnedOp(), trackedRefs, returnedGlobals)) {
        for(int i = 0; i < returnedGlobals.Count(); i++) {
            AddUser(returnedGlobals[i], instr);
            MarkPassedToUnknown(returnedGlobals[i]);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessCall(CallInstr* instr, 
                                  TrackedReferencesDict& trackedRefs) {
    for(int i = 0; i < instr->ArgumentCount(); i++) {
        // Check only pointer arguments. Globals that were
        // converted to other types are already marked as escaped.
        auto argument = instr->GetArgument(i);

        if(argument->IsPointer() == false) {
            continue;
        }

        // Get the list of globals associated with the argument.
        GlobalInfoList argumentGlobals;

        if(GetGlobalInfo(argument, trackedRefs, argumentGlobals) == false) {
            // Skip this argument, it's not tracked.
            continue; 
        }

        // Using the Call Graph check what happens with
        // the pointer in each of the potentially called functions.
        // If the Unknown node is called we need to presume
        // the global is read/written/external.
        auto callSite = callGraph_->GetCallSite(instr);

        if(callSite->CallsUnknownFunctions()) {
            for(int i = 0; i < argumentGlobals.Count(); i++) {
                AddUser(argumentGlobals[i], instr);
                MarkPassedToUnknown(argumentGlobals[i]);
            }

            continue;
        }
        
        bool isRead;
        bool isWritten;
        bool isEscaped;
        DetermineSideEffects(callSite, instr, argumentGlobals, i,
                             isRead, isWritten, isEscaped);

        // No matter in what way the argument may be modified,
        // it will be marked as address taken, but marked
        // read/write only if the corresponding flags are set.
        // An exception is when the argument may escape,
        // because we don't know what might happen with it.
        for(int j = 0; j < argumentGlobals.Count(); j++) {
            AddUser(argumentGlobals[j], instr);
            MarkArgumentGlobal(argumentGlobals[j], isRead, isWritten, isEscaped);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::DetermineSideEffects(CallSite* callSite, CallInstr* instr, 
                                           GlobalInfoList& argumentGlobals,
                                           int parameterIndex, bool& isRead, 
                                           bool& isWritten, bool& isEscaped) {
    // Presume the pointer argument isn't touched at all.
    // We stop as soon as the escaped flag is set.
    isRead = false;
    isWritten = false;
    isEscaped = false;

    // Process each potentially called function.
    for(int i = 0; i < callSite->CalledFunctionsCount(); i++) {
        // If the called node is a group of nodes
        // we consider their worst unified effect.
        auto calledNode = callSite->GetCalledNode(i);

        if(calledNode->IsNodeGroup()) {
            DetermineGroupSideEffects(static_cast<CallNodeGroup*>(calledNode),
                                      isRead, isWritten, isEscaped);
            if(isEscaped) {
                return; // Escaped implies read and written.
            }
            else continue;
        }

        // If the called function is a definition (has a body)
        // we can use the flags from the parameter.
        // Functions that are only declared are considered
        // to modify/escape the pointer, unless we can prove
        // otherwise using intrinsic/language information.
        auto callNode = static_cast<CallNode*>(calledNode);
        auto function = callNode->GetFunction();

        if(function->IsDefinition()) {
            auto parameterVariable = function->GetParameterVariable(parameterIndex);
            isRead |= parameterVariable->IsRead();
            isWritten |= parameterVariable->IsWrite();
            isEscaped |= parameterVariable->IsEscape();
        }
        else if(DetermineSideEffectsExternal(callSite, instr, argumentGlobals,
                                             parameterIndex, isRead, 
                                             isWritten, isEscaped) == false) {
            // We don't know anything about the external function.
            isRead = isWritten = isEscaped = true;
            return;
        }

        if(isEscaped) {
            return; // Escaped implies read and written.
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::DetermineSideEffectsExternal(CallSite* callSite, CallInstr* instr, 
                                                   GlobalInfoList& argumentGlobals, 
                                                   int parameterIndex, bool& isRead, 
                                                   bool& isWritten, bool& isEscaped) {
    // We handle memory intrinsics and calls to known
    // standard library functions using the language interface.
    if(auto intrinsic = instr->GetIntrinsic()) {
        if(intrinsic->Is<PrefetchIntr>()) {
            // Prefetch is considered to not affect memory.
            return true;
        }
        else if(intrinsic->Is<SetMemoryIntr>()) {
            DetermineSetMemorySideEffects(instr, argumentGlobals, parameterIndex,
                                          isRead, isWritten, isEscaped);
            return true;
        }
        else if(intrinsic->Is<CopyMemoryIntr>()) {
            DetermineCopyMemorySideEffects(instr, argumentGlobals, parameterIndex,
                                           isRead, isWritten, isEscaped);
            return true;
        }
    }
    else if(auto languageInfo = GetLanguageInfo()) {
        // Test if the external function is part of the standard library
        // and if it can read/write/escape any of the arguments.
        auto function = callSite->GetFunction();

        for(int i = 0; argumentGlobals.Count(); i++) {
            auto global = argumentGlobals[i]->TrackedReference;

            if(isRead == false) {
                isRead |= languageInfo->CallMayReadFromAddress(instr, global, this);
            }

            if(isWritten == false) {
                isWritten |= languageInfo->CallMayWriteToAddress(instr, global, this);
            }

            if(isEscaped == false) {
                isEscaped |= languageInfo->CallMayCaptureParameter(function, instr,
                                                                   parameterIndex);
            }
            else break; // Escaped implies read and written.
        }

        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::DetermineSetMemorySideEffects(CallInstr* instr, 
                                                    GlobalInfoList& argumentGlobals, 
                                                    int parameterIndex, bool& isRead, 
                                                    bool& isWritten, bool& isEscaped) {
    // Any global that appears as the destination is marked written.
    for(int i = 0; argumentGlobals.Count(); i++) {
        auto global = argumentGlobals[i]->HasWrite = true;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::DetermineCopyMemorySideEffects(CallInstr* instr, 
                                                     GlobalInfoList& argumentGlobals, 
                                                     int parameterIndex, bool& isRead, 
                                                     bool& isWritten, bool& isEscaped) {
    // Any global that appears as the destination is marked written.
    // Any global that appears as the source is marked read.
    // Only globals in the source list that alias with
    // destination operand are marked as written.
    for(int i = 0; argumentGlobals.Count(); i++) {
        if(parameterIndex == 1) {
            // The source reference might be read from.
            auto global = argumentGlobals[i];
            global->HasRead = true;

            if(MightBeAlias(global->TrackedReference, instr->GetSourceOp(0))) {
                // The source reference might be written to.
                global->HasWrite = true;
            }
        }
        else {
            // The destination reference might be written to.
            argumentGlobals[i]->HasWrite = true;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::DetermineGroupSideEffects(CallNodeGroup* nodeGroup, 
                                                bool& isRead, bool& isWritten, 
                                                bool& isEscaped) {
    // For groups we take a very conservative approach
    // and presume that external functions can modify/escape the argument.
    if(nodeGroup->CallsUnknownFunctions() ||
       nodeGroup->CallsExternalFunctions()) {
        isRead = isWritten = isEscaped = true;
        return;
    }

    for(int i = 0; i < nodeGroup->NodeCount(); i++) {
        // Nested node groups are not tested at all.
        auto childNode = nodeGroup->GetNode(i);

        if(childNode->IsNodeGroup()) {
            isRead = isWritten = isEscaped = true;
            return;
        }

        // Check each pointer parameter of the function
        // and unify their read/write/escape flags.
        auto callNode = static_cast<CallNode*>(childNode);
        auto function = callNode->GetFunction();

        for(int j = 0; j < function->ParameterCount(); j++) {
            auto parameterVariable = function->GetParameterVariable(j);

            if(parameterVariable->IsPointer()) {
                isRead |= parameterVariable->IsRead();
                isWritten |= parameterVariable->IsWrite();
                isEscaped |= parameterVariable->IsEscape();

                if(isEscaped) {
                    return; // Escaped implies read and written.
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessCompare(CmpInstrBase* instr, 
                                     TrackedReferencesDict& trackedRefs) {
    // Any compared global is marked as address-taken.
    GlobalInfoList leftGlobals;
    GlobalInfoList rightGlobals;

    if(GetGlobalInfo(instr->LeftOp(), trackedRefs, leftGlobals)) {
        for(int i = 0; i < leftGlobals.Count(); i++) {
            AddUser(leftGlobals[i], instr);
            leftGlobals[i]->IsAddressTaken = true;
        }
    }

    if(GetGlobalInfo(instr->RightOp(), trackedRefs, rightGlobals)) {
        for(int i = 0; i < rightGlobals.Count(); i++) {
            AddUser(rightGlobals[i], instr);
            rightGlobals[i]->IsAddressTaken = true;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessPhi(PhiInstr* instr, 
                                 TrackedReferencesDict& trackedRefs) {
    // Any incoming global is marked as address-taken
    // and we track it further through the 'phi' result operand.
    for(int i = 0; i < instr->OperandCount(); i++) {
        auto incomingOp = instr->GetOperand(i);
        GlobalInfoList incomingGlobals;

        if(GetGlobalInfo(incomingOp, trackedRefs, incomingGlobals)) {
            for(int j = 0; j < incomingGlobals.Count(); j++) {
                AddUser(incomingGlobals[i], instr);
                incomingGlobals[j]->IsAddressTaken = true;
            }

            TrackResultOperand(instr->ResultOp(), incomingOp, trackedRefs);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessQuestion(QuestionInstr* instr, 
                                      TrackedReferencesDict& trackedRefs) {
    GlobalInfoList trueGlobals;
    GlobalInfoList falseGlobals;

    if(GetGlobalInfo(instr->TrueOp(), trackedRefs, trueGlobals)) {
        for(int i = 0; i < trueGlobals.Count(); i++) {
            AddUser(trueGlobals[i], instr);
            trueGlobals[i]->IsAddressTaken = true;
        }

        TrackResultOperand(instr->ResultOp(), instr->TrueOp(), trackedRefs);
    }

    if(GetGlobalInfo(instr->FalseOp(), trackedRefs, falseGlobals)) {
        for(int i = 0; i < falseGlobals.Count(); i++) {
            AddUser(falseGlobals[i], instr);
            falseGlobals[i]->IsAddressTaken = true;
        }

        TrackResultOperand(instr->ResultOp(), instr->FalseOp(), trackedRefs);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GlobalPromotion::GlobalInfo*
GlobalPromotion::GetGlobalInfo(Reference* reference) {
    // If this is the first time we request the reference info
    // create it now, presuming it has no read/writes.
    if(globalInfo_.ContainsKey(reference) == false) {
        globalInfo_.Add(reference, new GlobalInfo(reference));
    }
    
    return globalInfo_[reference];
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::GetGlobalInfo(Operand* op, TrackedReferencesDict& trackedRefs,
                                    GlobalInfoList& foundInfos) {
    if(auto reference = AsGlobalReference(op)) {
        foundInfos.Add(GetGlobalInfo(reference));
        return true;
    }
    else if(trackedRefs.ContainsKey(op)) {
        foundInfos.AddRange(trackedRefs[op]);
        return foundInfos.Count() > 0;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reference* GlobalPromotion::AsGlobalReference(Operand* op) {
    if(auto reference = op->As<Reference>()) {
        if((reference->IsLocalVariableRef() || 
            reference->IsBlockReference()) == false) {
            return reference;
        }
    }
        
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::MarkAddressTaken(GlobalInfo* globalInfo) {
    globalInfo->HasRead = true;
    globalInfo->HasWrite = true;
    globalInfo->IsAddressTaken = true;
    globalInfo->HasNonConstantWrite = true;
    globalInfo->HasUnknownPositionWrite = true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::MarkPassedToUnknown(GlobalInfo* globalInfo) {
    MarkAddressTaken(globalInfo);
    globalInfo->IsExternal = true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::MarkArgumentGlobal(GlobalInfo* globalInfo, bool isRead, 
                                         bool isWritten, bool isEscaped) {
    // No matter in what way the argument may be modified,
    // it will be marked as address taken, but marked
    // read/write only if the corresponding flags are set.
    // An exception is when the argument may escape,
    // because we don't know what might happen with it.
    if(isEscaped) {
        // Assume the worst (read/written/external/address-taken).
        MarkPassedToUnknown(globalInfo);
    }
    else {
        globalInfo->IsAddressTaken = true;
        globalInfo->HasRead |= isRead;
        globalInfo->HasWrite |= isWritten;

        if(isWritten) {
            // We don't know exactly what values and
            // in which positions they are written.
            globalInfo->HasNonConstantWrite = true;
            globalInfo->HasUnknownPositionWrite = true;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::MightBeAlias(Operand* a, Operand* b) {
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::IsDefinitelyNoAlias(Operand* a, Operand* b) {
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::PromoteFunctionsToStatic() {
    // Many functions, although marked 'static', have a call
    // from the External node because their address might be
    // taken and so escape from their parent unit.
    // If we known this can't happen we remove the call.
    auto& nodes = callGraph_->GetCallNodes();

    for(int i = 0; i < nodes.Count(); i++) {
        auto callNode = static_cast<CallNode*>(nodes[i]);
        PromoteFunctionToStatic(callNode);
    }

    // Now do the same for node groups.
    auto& nodeGroups = callGraph_->GetCallNodeGroups();

    for(int i = 0; i < nodeGroups.Count(); i++) {
        auto nodeGroup = nodeGroups[i];

        for(int j = 0; j < nodeGroup->NodeCount(); j++) {
            auto callNode = static_cast<CallNode*>(nodeGroup->GetNode(j));
            PromoteFunctionToStatic(callNode);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::PromoteFunctionToStatic(CallNode* callNode) {
    auto functionRef = callNode->GetFunctionReference();
    GlobalInfo* global = nullptr;
    globalInfo_.TryGetValue(functionRef, &global);

    // If the function appears to be called by an external
    // function, but we have proven it can't remove the call.
    if(IsStaticFunction(functionRef, global) &&
        callNode->IsCalledByExternalFunctions()) {
        callNode->RemoveCallFromExternal();
        ExternalCallNode::GetExternalNode()->RemoveCalledNode(callNode);
    }

    // Update the address-taken flag, it's useful for alias analysis.
    functionRef->Target()->SetIsAddresTaken(global == nullptr ? false :
                                            global->IsAddressTaken);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::PromoteVariablesToConstants() {
    auto& references = unit_->References();
    auto& types = unit_->Types();

    for(auto variable = unit_->Variables().First();
        variable; variable = variable->Next) {
        auto globalVariable = variable->Value;
        auto type = types.GetPointer(globalVariable->GetType());
        auto variableRef = references.GetGlobalVariableRef(globalVariable, type);
        GlobalInfo* global;

        if(globalInfo_.TryGetValue(variableRef, &global)) {
            PromoteVariableToConstant(globalVariable, global);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::PromoteVariableToConstant(GlobalVariable* globalVariable, 
                                                 GlobalInfo* globalInfo) {
    // If the variable has an initializer, is 'static',
    // has no write and doesn't escape the unit it can be marked 'const'.
    if(globalVariable->HasInitializer() &&
       globalVariable->IsStatic() &&
       (globalInfo->HasWrite == false) &&
       (globalInfo->IsExternal == false)) {
        // Make it a constant, this may allow constant folding.
        globalVariable->SetIsConstant(true);
    }

    // Update the address-taken flag, it's useful for alias analysis.
    globalVariable->SetIsAddresTaken(globalInfo->IsAddressTaken);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::Dump() {
    Dictionary<Reference*, bool> dumpedRefs;

    globalInfo_.ForEachValue([&dumpedRefs](GlobalInfo* info) -> bool {
        if(dumpedRefs.ContainsKey(info->TrackedReference) == false) {
            dumpedRefs.Add(info->TrackedReference, true);

            auto symbol = info->TrackedReference->GetSymbol();
            string title = (symbol->IsFunction() ? "Function " : "Variable ") +
                            *symbol->Name();

            string text;
            text += "External: ";
            text += (info->IsExternal ? "Yes" : "No");
            text += "\nHasRead: ";
            text += (info->HasRead ? "Yes" : "No");
            text += "\nHasWrite: ";
            text += (info->HasWrite ? "Yes" : "No");
            text += "\nIsAddressTaken: ";
            text += (info->IsAddressTaken ? "Yes" : "No");
            text += "\nHasNonConstantWrite: ";
            text += (info->HasNonConstantWrite ? "Yes" : "No");
            text += "\nHasUnknownPositionWrite: ";
            text += (info->HasUnknownPositionWrite ? "Yes" : "No");
            text += "\nHasUnknownPositionRead: ";
            text += (info->HasUnknownPositionRead ? "Yes" : "No");
            text += "\nReadPostorder: " + string::Format(L"%d, ", info->SmallestReadPostorder);
            text += "\nWritePostorder: " + string::Format(L"%d, ", info->LargestWritePostorder);
            text += "\nInitializers: ";
            
            for(int i = 0; i < info->Initializers.Count(); i++) {
                auto init = info->Initializers[i];
                text += "\n    {";

                for(int j = 0; j < init.Path.Indices.Count(); j++) {
                    text += string::Format(L"%d ", init.Path.Indices[j]);
                }

                text += "} : ";

                for(int j = 0; j < init.Constants.Count(); j++) {
                    auto constant = init.Constants[j];

                    if(auto intConst = constant->As<IntConstant>()) {
                        text += string::Format(L"%d, ", intConst->Value());
                    }
                    else if(auto floatConst = constant->As<FloatConstant>()) {
                        text += string::Format(L"%f, ", floatConst->Value());
                    }
                    else if(constant->IsNullConstant()) {
                        text += "nullptr, ";
                    }
                    else text += "undef, "; 
                }
            }

            text += "\nUsers: ";

            for(int i = 0; i < info->Users.Count(); i++) {
                text += "\n    " + *info->Users[i]->GetSymbol()->Name();
            }

            if(info->ReadPositions.SetBitsCount() > 0) {
                text += "\nRead positions: ";

                info->ReadPositions.ForEachSetBit([&text](int index) -> bool {
                    text += string::Format(L"%d, ", index);
                    return true;
                });
            }

            ObjectDumper(text, title).Dump();
        }

        return true;
    });
}

} // namespace Optimization