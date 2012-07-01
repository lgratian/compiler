// CallGraphBuilder.cpp
// Copyright (c) Lup Gratian
//
// Implements the CallGraphBuilder class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "CallGraphBuilder.hpp"

namespace Analysis {

shared<CallGraph> CallGraphBuilder::BuildCallGraph(Unit* unit) {
    // Create the new call graph.
    callGraph_ = new CallGraph();
    auto& functions = unit->Functions();

    // Initialize the constant folder.
    IRGenerator irGen(unit);
    folder_ = ConstantFolder(&irGen, GetTarget());
    
    // Create nodes for all functions that have a definition
    // in this translation unit.
    for(auto funct = unit->Functions().First(); funct; funct = funct->Next) {
        if(funct->Value->IsDefinition()) {
            CreateCallNode(funct->Value->GetReference());
        }
    }

    // If there were calls to unknown targets
    // try to determine where these targets might point.
    // In most cases this completes the call graph.
    if(unknownCalls_.Count() > 0) {
        CollectPotentialTargets(unit);
        ProcessBindings();
        CompleteCallGraph();
    }

    return callGraph_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::CreateCallNode(FunctionReference* functionRef) {
    // Get the node associated with the function,
    // then add calls from the external and unknown 
    // nodes if necessary.
    auto node = callGraph_->GetNode(functionRef);
    bool calledByExternal = false;

    if(CanBeCalledByExternal(functionRef)) {
        node->AddCallFromExternal();
        ExternalCallNode::GetExternalNode()->AddCalledNode(node);
        calledByExternal = true;
    }

    AddCalledFunctions(functionRef->Target(), node);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::AddCalledFunctions(Function* funct, CallNode* callerNode) {
    // Enumerate over all instructions and analyze each found 'call'.
    auto instrEnumerator = funct->GetInstructionEnum();

    while(instrEnumerator.IsValid()) {
        if(auto callInstr = instrEnumerator.Next()->As<CallInstr>()) {
            // Create a new call site.
            auto callSite = callGraph_->GetCallSite(callInstr);

            if(auto functionRef = callInstr->TargetOp()->As<FunctionReference>()) {
                AddCall(callSite, functionRef);
            }
            else {
                // If we call through a function pointer we mark it 
                // as a call to the unknown node.
                HandleCallToUnknown(callInstr, callSite);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::AddCall(CallSite* callSite, FunctionReference* callee) {
    auto calledNode = callGraph_->GetNode(callee);
    callSite->AddCalledNode(calledNode);
    calledNode->AddCallingSite(callSite);

    // If we call a function that is only a declaration
    // we also add a call to the external node. This way it's easy
    // to respond to "does the callee call any external functions?".
    if(callee->IsDeclaration()) {
        callSite->AddCallToExternal();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::HandleCallToUnknown(CallInstr* instr, CallSite* callSite) {
    // First we try to solve the targets by constant folding
    // of loads and by considering 'phi' instructions.
    if(AddMultipleTargets(instr->TargetOp(), callSite)) {
        return;
    }

    // We use an interprocedural analysis to approximate the call targets;
    // try to get some information about the target.
    unknownCalls_.Add(UnknownCall(instr));
    auto& unknownCall = unknownCalls_[unknownCalls_.Count() - 1];

    if(IdentifyTargets(instr->TargetOp(), &unknownCall) == false) {
        // No information could be obtained,
        // add a call to the unknown node.
        callSite->AddCallToUnkown();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::AddMultipleTargets(Operand* targetOp, CallSite* callSite) {
    // Try to constant-fold a load from a global variable.
    // Many applications have global arrays initialized with function pointers.
    if(auto loadInstr = targetOp->DefiningInstrAs<LoadInstr>()) {
        if(auto loadedOp = folder_.FoldLoad(loadInstr)) {
            loadInstr->ResultOp()->ReplaceWith(loadedOp);

            // Make sure the value is a function reference (it might be 'nullptr').
            if(auto functionRef = loadedOp->As<FunctionReference>()) {
                AddCall(callSite, functionRef);
                return true;
            }
        }
    }
    else if(auto phiInstr = targetOp->DefiningInstrAs<PhiInstr>()) {
        // There might be a 'phi' that combines function references.
        if(phiInstr->OperandCount() > 8) {
            return false;
        }

        // Presume all incoming operands are function references.
        bool allFunctionRefs = true;

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            auto incomingOp = phiInstr->GetOperand(i);
            
            if(auto functionRef = incomingOp->As<FunctionReference>()) {
                AddCall(callSite, functionRef);
            }
            else {
                // A call to the unknown must still be added.
                allFunctionRefs = false;
            }
        }

        return allFunctionRefs;
    }
    else if(auto questInstr = targetOp->DefiningInstrAs<QuestionInstr>()) {
        // There might be a 'quest' that combines function references.
        auto trueFunctionRef = questInstr->TrueOp()->As<FunctionReference>();
        auto falseFunctionRef = questInstr->FalseOp()->As<FunctionReference>();

        if(trueFunctionRef) {
            AddCall(callSite, trueFunctionRef);
        }

        if(falseFunctionRef) {
            AddCall(callSite, falseFunctionRef);
        }

        return trueFunctionRef && falseFunctionRef;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::CanBeCalledByExternal(FunctionReference* functionRef) {
    // A function that is marked 'extern' can be called by a function 
    // in another translation unit. Sadly even 'static' functions 
    // might be called if their address is taken.
    return functionRef->Target()->IsExtern() ||
           functionRef->IsAddressTaken();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::IdentifyTargets(Operand* op, UnknownCall* unknownCall, 
                                       UnknownTargetList* targetList, int level) {
    // Stop if we're too deep into the the value graph.
    // It is unlikely that we find anything useful, and this
    // also prevents entering a 'phi' cycle.
    if(level == 0) {
        return false;
    }

    // If 'target' is set the user doesn't want the target
    // to be added to the data structures.
    if(auto functionRef = op->As<FunctionReference>()) {
        // We reached a function reference; this is possible
        // if it is an incoming operand for a 'phi' instruction
        // considered at a previous step. '-1' is an indicator
        // that the target has no associated function list.
        if(targetList == nullptr) {
            unknownCall->Targets.Add(UnknownTarget(functionRef, SINGLE_FUNCTION));
        }

        return true;
    }
    else if(auto variableRef = op->As<VariableReference>()) {
        // We're interested in loads from global variables only.
        if(targetList == nullptr) {
            return AddNewTarget(variableRef, 0, unknownCall);
        }
        else {
            targetList->Add(UnknownTarget(variableRef, 0));
            return true;
        }
    }
    else if(auto parameter = op->As<Parameter>()) {
        // We record the parameter as a pair formed from
        // the reference to the current function and the parameter index.
        auto parameterVar = parameter->GetVariable();
        auto symbolTable = parameter->GetVariable()->ParentTable();
        auto function = static_cast<Function*>(symbolTable->Parent());
        
        // Identify the parameter index.
        int parameterIndex = 0;

        for(int i = 0; i < function->ParameterCount(); i++) {
            if(function->GetParameterVariable(i) == parameterVar) {
                parameterIndex = i;
                break;
            }
        }

        // Create the target.
        UnknownTarget unknownTarget(function->GetReference(), parameterIndex);
        
        if(targetList == nullptr) {
            return AddNewTarget(unknownTarget, unknownCall);
        }
        else {
            targetList->Add(unknownTarget);
            return true;
        }
    }
    else if(auto definingInstr = op->DefiningInstruction()) {
        // Handle 'load', 'phi', 'index' and 'elem'.
        return IdentifyTargets(definingInstr, unknownCall, targetList, level);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::IdentifyTargets(Instruction* instr, UnknownCall* unknownCall, 
                                       UnknownTargetList* targetList, int level) {
    if(auto loadInstr = instr->As<LoadInstr>()) {
        // We might load from a global variable or parameter.
        return IdentifyTargets(loadInstr->SourceOp(), unknownCall, 
                               targetList, level - 1);
    }
    else if(auto phiInstr = instr->As<PhiInstr>()) {
        // The 'phi' might combine multiple targets for which
        // we can determine the functions.
        if(phiInstr->OperandCount() > 8) {
            return false;
        }

        bool hasTarget = false;

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            if(IdentifyTargets(phiInstr->GetOperand(i), unknownCall, 
                               targetList, level - 1)) {
                hasTarget = true;
            }
        }

        return hasTarget;
    }
    else if(auto questInstr = instr->As<QuestionInstr>()) {
        bool resultA = IdentifyTargets(questInstr->TrueOp(), unknownCall, 
                                       targetList, level - 1);
        bool resultB = IdentifyTargets(questInstr->FalseOp(), unknownCall, 
                                       targetList, level - 1);
        return resultA || resultB;
    }

    if(auto indexInstr = instr->As<IndexInstr>()) {
        // Global function pointer arrays are often used
        // especially for programs targeted to multiple operating systems.
        auto variableRef = indexInstr->BaseOp()->As<VariableReference>();
        auto indexConst = indexInstr->IndexOp()->As<IntConstant>();

        if(variableRef && indexConst) {
            if(targetList == nullptr) {
                return AddNewTarget(variableRef, indexConst->Value(), unknownCall);
            }
            else {
                targetList->Add(UnknownTarget(variableRef, indexConst->Value()));
                return true;
            }
        }

        if(variableRef && targetList) {
            // This is an 'index' with a non-constant index.
            targetList->Add(UnknownTarget(variableRef, NON_CONSTANT_INDEX));
            return true;
        }
    }
    else if(auto elemInstr = instr->As<ElementInstr>()) {
        // Global function pointer records are often used
        // especially for programs targeted to multiple operating systems.
        if(auto variableRef = elemInstr->BaseOp()->As<VariableReference>()) {
            __int64 index = elemInstr->GetFieldIndex();

            if(targetList == nullptr) {
                return AddNewTarget(variableRef, index, unknownCall);
            }
            else {
                targetList->Add(UnknownTarget(variableRef, index));
                return true;
            }
        }
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        // We try to track the returned values.
        if(auto functionRef = callInstr->TargetOp()->As<FunctionReference>()) {
            UnknownTarget unknownTarget(functionRef, RETURNED_VALUE);

            if(targetList == nullptr) {
                return AddNewTarget(unknownTarget, unknownCall);
            }
            else {
                targetList->Add(unknownTarget);
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::CollectPotentialTargets(Unit* unit) {
    // Scan all functions once and try to collect 
    // the functions that might be used as call targets.
    // Each function reference is associated with the appropriate target
    // (global variable, parameter, etc.).
    for(auto funct = unit->Functions().First(); funct; funct = funct->Next) {
        if(funct->Value->IsDefinition() == false) {
            continue;
        }

        // Enumerate over the instructions and look at 'store' 'call' and 'ret'.
        auto instrEnumerator = funct->Value->GetInstructionEnum();

        while(instrEnumerator.IsValid()) {
            auto instr = instrEnumerator.Next();

            if(auto storeInstr = instr->As<StoreInstr>()) {
                CollectTargetsFromStore(storeInstr);
            }
            else if(auto callInstr = instr->As<CallInstr>()) {
                CollectTargetsFromCall(callInstr);
            }
            else if(auto returnInstr = instr->As<ReturnInstr>()) {
                CollectTargetsFromReturn(returnInstr);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::CollectTargetsFromStore(StoreInstr* instr) {
    // We have a 'store' instruction that might store into
    // one of the unknown targets (a global variable, for example).
    // Check if the target(s) are among the ones we're interested into.
    UnknownTargetList targetList;
    IdentifyTargets(instr->DestinationOp(), nullptr, &targetList);

    if(targetList.Count() == 0) {
        return;
    }
    
    // Make sure each target is in our data structures
    // (it may not be if it isn't involved in a call).
    AddNewTargets(targetList);

    // Check if any of the targets are array indexed
    // with a non-constant index.
    for(int i = 0; i < targetList.Count(); i++) {
        if(targetList[i].HasNonConstantIndex()) {
            // Mark this fact so that we add a call to the unknown node later.
            unknownTargets_.Add(targetList[i].Target, true);
        }
    }

    // The most frequent case is a store of a function reference.
    // Each found target might call this functions.
    if(auto functionRef = instr->SourceOp()->As<FunctionReference>()) {
        for(int i = 0; i < targetList.Count(); i++) {
            potentialFuncts_[targetList[i]].Add(functionRef);
        }

        return;
    }

    // The store may create a binding between the destination targets
    // and the source ones. Check if the source represents a target.
    UnknownTargetList sourceTargetList;
    IdentifyTargets(instr->SourceOp(), nullptr, &sourceTargetList);

    if(sourceTargetList.Count() > 0) {
        AddNewTargets(sourceTargetList);
        CreateBindings(targetList, sourceTargetList);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::CreateBindings(UnknownTargetList& destList, 
                                      UnknownTargetList& sourceList) {
    // Create bindings between all pairs of targets.
    for(int i = 0; i < sourceList.Count(); i++) {
        for(int j = 0; j < destList.Count(); j++) {
            CreateBinding(sourceList[i], destList[j]);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::CreateBindings(UnknownTarget& destTarget, 
                                      UnknownTargetList& sourceList) {
    // Create bindings between all pairs of targets.
    for(int i = 0; i < sourceList.Count(); i++) {
        CreateBinding(sourceList[i], destTarget);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::CreateBinding(UnknownTarget& sourceTarget,
                                     UnknownTarget& destTarget) {
    if(bindings_.ContainsKey(sourceTarget)) {
        // There already exist a binding for this source target.
        // Add the destination target only if it isn't already in the list.
        auto& binding = bindings_[sourceTarget];

        if(binding.Destinations.Contains(destTarget) == false) {
            binding.Destinations.Add(destTarget);
            return true;
        }

        return false;
    }
    else {
        // This is the first binding with this source target.
        bindings_.Add(sourceTarget, Binding(sourceTarget, destTarget));
        return true;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::CreateCallBinding(UnknownTarget& sourceTarget, __int64 argument,
                                         UnknownTarget& parameterTarget) {
    if(callBindings_.ContainsKey(sourceTarget)) {
        // There already exist a binding for this source target.
        // Add the destination target only if it isn't already in the list.
        auto& binding = callBindings_[sourceTarget];

        if(binding.Arguments.Contains(argument)) {
            binding.Arguments.Add(argument);
        }

        if(binding.Parameters.Contains(parameterTarget) == false) {
            binding.Parameters.Add(parameterTarget);
        }
    }
    else {
        // This is the first binding with this source target.
        CallBinding binding(sourceTarget, argument, parameterTarget);
        callBindings_.Add(sourceTarget, binding);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::CollectTargetsFromCall(CallInstr* instr) {
    // For calls we're interested in the function pointers
    // which are passed as parameters. If the parameter is a function
    // reference we add it to the corresponding candidate list,
    // otherwise we try to create bindings.
    if(instr->HasArguments() == false) {
        return;
    }

    auto callTarget = instr->TargetOp()->As<FunctionReference>();
    UnknownTargetList callTargetList;

    // If the call doesn't target a known function check 
    // if it is one of the unknown targets.
    if(callTarget == nullptr) {
        IdentifyTargets(instr->TargetOp(), nullptr, &callTargetList);

        if(callTargetList.Count() == 0) {
            return;
        }
        
        AddNewTargets(callTargetList);
    }

    // Process the arguments. We're interested only in pointers.
    int pointerArguments = 0;

    for(int i = 0; i < instr->ArgumentCount(); i++) {
        auto argument = instr->GetArgument(i);

        if(argument->IsPointer()) {
            pointerArguments++;

            // The easiest case is when the argument is a function reference
            // and the call target is known. If the call target is not known
            // we create a special kind of binding so that we can propagate
            // the values in the moment we know all/some of the targets.
            if(callTarget == nullptr) {
                // The argument might be one of the unknown targets
                // (global variable, parameter, etc.). If the call target
                // is known we create a binding between each target
                // and the formal parameter, otherwise we create the special
                // call bindings that will be solved when we know the targets.
                UnknownTargetList argumentTargetList;

                if(auto functionRefArgument = argument->As<FunctionReference>()) {
                    argumentTargetList.Add(UnknownTarget(functionRefArgument, 
                                                         SINGLE_FUNCTION));
                }
                else IdentifyTargets(argument, nullptr, &argumentTargetList);

                if(argumentTargetList.Count() > 0) {
                    AddNewTargets(argumentTargetList);
                    AddFunctionToParameter(callTarget, callTargetList, 
                                           argumentTargetList, i);
                }
            }
            else if(auto functionRefArgument = argument->As<FunctionReference>()) {
                // The argument is a known function reference.
                AddFunctionToParameter(callTarget, callTargetList, 
                                       functionRefArgument, i);
            }
            else {
                // The argument is an unknown target,
                // but at least we known the called function.
                UnknownTargetList argumentTargetList;
                IdentifyTargets(argument, nullptr, &argumentTargetList);

                if(argumentTargetList.Count() > 0) {
                    AddNewTargets(argumentTargetList);
                    AddFunctionToParameter(callTarget, callTargetList, 
                                           argumentTargetList, i);
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::AddFunctionToParameter(FunctionReference* callTarget, 
                                              UnknownTargetList& callTargetList,
                                              FunctionReference* functionRefArgument,
                                              __int64 argumentIndex) {
    if(callTarget) {
        // The call target is known, add the function reference to the list
        // of potentially functions associated with the formal parameter.
        UnknownTarget destTarget(callTarget, argumentIndex);
        AddNewTarget(destTarget);

        // Add the function only if it isn't already in the list.
        auto& functions = potentialFuncts_[destTarget];

        if(functions.Contains(functionRefArgument) == false) {
            functions.Add(functionRefArgument);
        }
    }
    else {
        // The call target is not known and it depends on one or more
        // unknown targets. We create a special call binding that will be 
        // solved when all/some targets are known.
        UnknownTarget argument(functionRefArgument, SINGLE_FUNCTION);

        for(int i = 0; i < callTargetList.Count(); i++) {
            CreateCallBinding(callTargetList[i], argumentIndex, argument);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::AddFunctionToParameter(FunctionReference* callTarget, 
                                              UnknownTargetList& callTargetList,
                                              UnknownTargetList& argumentTargetList,
                                              __int64 argumentIndex) {
    if(callTarget) {
        // The call target is known, create a binding between 
        // the argument targets and the formal parameter.
        UnknownTarget destTarget(callTarget, argumentIndex);
        AddNewTarget(destTarget);
        CreateBindings(destTarget, argumentTargetList);
    }
    else {
        // The call target is not known and it depends on one or more
        // unknown targets. We create a special call binding that will be 
        // solved when all/some targets are known.
        for(int i = 0; i < callTargetList.Count(); i++) {
            for(int j = 0; j < argumentTargetList.Count(); j++) {
                CreateCallBinding(callTargetList[i], argumentIndex, 
                                  argumentTargetList[j]);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::CollectTargetsFromReturn(ReturnInstr* instr) {
    // Only returned pointers are considered.
    if(instr->IsVoid() || 
      (instr->ReturnedOp()->IsPointer() == false) ||
       instr->ReturnedOp()->IsUndefinedConstant()) {
        return;
    }

    // Create a target for the returned value.
    auto functionRef = instr->ParentFunction()->GetReference();
    UnknownTarget returnedTarget(functionRef, RETURNED_VALUE);
    
    AddNewTarget(returnedTarget);
    auto& functions = potentialFuncts_[returnedTarget];

    // If a function reference is returned add it to the return target.
    if(auto returnedFunctionRef = instr->ReturnedOp()->As<FunctionReference>()) {
        if(functions.Contains(returnedFunctionRef) == false) {
            functions.Add(returnedFunctionRef);
        }

        // Add the function to the return list.
        AddReturnedFunction(functionRef, returnedFunctionRef);
    }
    else {
        UnknownTargetList returnedTargetList;
        IdentifyTargets(instr->ReturnedOp(), nullptr, &returnedTargetList);

        for(int i = 0; i < returnedTargetList.Count(); i++) {
            auto unknownTarget = returnedTargetList[i];
            CreateBinding(unknownTarget, returnedTarget);

            // If the target is a parameter add it to the list
            // of returned parameters, otherwise mark the fact
            // that the function might return non-parameters.
            if(unknownTarget.IsParameter()) {
                AddReturnedParameter(functionRef, unknownTarget.TargetIndex);
            }
            else MarkDoeNotReturnOnlyParameters(functionRef);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::AddReturnedParameter(FunctionReference* functionRef,
                                            __int64 parameter) {
    if(parameterReturns_.ContainsKey(functionRef)) {
        auto& paramReturn = parameterReturns_[functionRef];

        // Make sure we don't add a parameter twice.
        if(paramReturn.ReturnsOnlyParameters &&
           (paramReturn.ReturnedParameters.Contains(parameter) == false)) {
            paramReturn.ReturnedParameters.Add(parameter);
        }
    }
    else {
        parameterReturns_.Add(functionRef, ParameterReturn(parameter));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::AddReturnedFunction(FunctionReference* functionRef, 
                                           FunctionReference* returnedFunctionRef) {
    if(parameterReturns_.ContainsKey(functionRef)) {
        auto& paramReturn = parameterReturns_[functionRef];

        // Make sure we don't add a function twice.
        if(paramReturn.ReturnsOnlyParameters &&
           (paramReturn.ReturnedFunctions.Contains(returnedFunctionRef) == false)) {
            paramReturn.ReturnedFunctions.Add(returnedFunctionRef);
        }
    }
    else {
        parameterReturns_.Add(functionRef, ParameterReturn(returnedFunctionRef));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::MarkDoeNotReturnOnlyParameters(FunctionReference* functionRef) {
    // Mark the fact that the function returns values
    // that are not unmodified parameters.
    if(parameterReturns_.ContainsKey(functionRef)) {
        auto& paramReturn = parameterReturns_[functionRef];
        paramReturn.ReturnsOnlyParameters = false;
    }
    else {
        parameterReturns_.Add(functionRef, ParameterReturn(false));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::ProcessBindings() {
    // Bindings represent the value flow between unknown targets
    // (involved in assignment or as formal-actual parameters).
    // The values from the source target need to be combined with all
    // values of the destination targets. If any of the destination
    // target is part of a binding, the updated values need to be
    // propagated to its destinations too. This suggests a worklist algorithm.
    if((bindings_.Count() == 0) && 
       (callBindings_.Count() == 0)) {
        return;
    }

    // Create the initial worklist by considering all bindings.
    List<UnknownTarget> worklist;
    Dictionary<UnknownTarget, bool> inWorklist;

    bindings_.ForEachValue([&worklist, &inWorklist](Binding& binding) -> bool {
        worklist.Add(binding.Source);
        inWorklist.Add(binding.Source, true);
        return true;
    });

    callBindings_.ForEachValue([&worklist, &inWorklist](CallBinding& binding) -> bool {
        worklist.Add(binding.Source);
        inWorklist.Add(binding.Source, true);
        return true;
    });
    
    // Iterate until no new values are added to the function sets.
    while(worklist.IsNotEmpty()) {
        auto sourceTarget = worklist.RemoveLast();
        inWorklist.Remove(sourceTarget);

        // Check if we have a binding.
        if(bindings_.ContainsKey(sourceTarget)) {
            auto& binding = bindings_[sourceTarget];
        
            // Propagate the functions from the source
            // to all its destinations. If at least one has changed
            // we add to the worklist all destinations that are bindings too.
            if(UnifyFunctionSets(binding)) {
                for(int i = 0; i < binding.Destinations.Count(); i++) {
                    auto destination = binding.Destinations[i];

                    if(inWorklist.ContainsKey(destination) == false) {
                        // Add to the worklist.
                        worklist.Add(destination);
                        inWorklist.Add(destination, true);
                    }
                }
            }
        }

        // Check if we have a special call binding.
        if(callBindings_.ContainsKey(sourceTarget) &&
           ProcessCallBinding(sourceTarget)) {
            // Add to the worklist the source targets of the added bindings.
            auto& callBinding = callBindings_[sourceTarget];

            for(int i = 0; i < callBinding.Parameters.Count(); i++) {
                if(inWorklist.ContainsKey(callBinding.Parameters[i]) == false) {
                    worklist.Add(callBinding.Parameters[i]);
                    inWorklist.Add(callBinding.Parameters[i], true);
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::UnifyFunctionSets(Binding& binding) {
    // If the target index is -1 this means the source
    // is a function reference, not a list of potential functions.
    if(binding.Source.IsSingleFunction()) {
        auto function = binding.Source.Target->As<FunctionReference>();
        return AddFunctionToDestinationSets(function, binding);
    }

    // Propagate all potential functions to the destination lists.
    bool functionAdded = false;
    auto& sourceFunctions = potentialFuncts_[binding.Source];

    for(int i = 0; i < sourceFunctions.Count(); i++) {
        if(AddFunctionToDestinationSets(sourceFunctions[i], binding)) {
            functionAdded = true;
        }
    }

    return functionAdded;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::ProcessCallBinding(UnknownTarget sourceTarget) {
    // A call binding creates new bindings between source targets
    // and formal parameters when new call targets are found.
    // Once these bindings are created, the values will be propagated
    // to them and more of the call graph is completed.
    auto& callBinding = callBindings_[sourceTarget];
    auto& functions = potentialFuncts_[sourceTarget];
    bool bindingAded = false;
    
    for(int i = 0; i < functions.Count(); i++) {
        for(int k = 0; k < callBinding.Arguments.Count(); k++) {
            // Create the target for the formal parameter.
            UnknownTarget newTarget(functions[i], callBinding.Arguments[k]);
            AddNewTarget(newTarget);

            for(int j = 0; j < callBinding.Parameters.Count(); j++) {
                // Connect the formal parameter to its source.
                if(CreateBinding(callBinding.Parameters[j], newTarget)) {
                    bindingAded = true;
                }
            }
        }
    }

    return bindingAded;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::AddFunctionToDestinationSets(FunctionReference* function, 
                                                    Binding& binding) {
    bool functionAdded = false;

    for(int i = 0; i < binding.Destinations.Count(); i++) {
        auto& destFunctions = potentialFuncts_[binding.Destinations[i]];

        if(destFunctions.Contains(function) == false) {
            destFunctions.Add(function);
            functionAdded = true;
        }
    }

    return functionAdded;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::CompleteCallGraph() {
    // Functions whose target was unknown are handled now,
    // after their potential targets were collected.
    // In the worst case we add a call to the unknown node.
    for(int i = 0; i < unknownCalls_.Count(); i++) {
        auto& unknownCall = unknownCalls_[i];
        auto callSite = callGraph_->GetCallSite(unknownCall.Call);
        
        bool hasAddressTakenTarget = false;
        bool hasAmbiguousTarget = false;
        int functionCount = 0;

        for(int j = 0; j < unknownCall.Targets.Count(); j++) {
            auto unknownTarget = unknownCall.Targets[j];

            // If the target has its address taken sadly
            // we need to add a call to the unknown node.
            if(auto variableRef = unknownTarget.Target->As<VariableReference>()) {
                if(variableRef->IsAddressTaken()) {
                    hasAddressTakenTarget = true;
                }
            }

            // If the target is an array and there was a 'store' that used
            // a non-constant index we need to add a call to the unknown node.
            if(unknownTargets_[unknownTarget.Target]) {
                hasAmbiguousTarget = true;
            }

            // If the value was returned by a call
            // try to refine it taking the context into consideration.
            if(unknownTarget.IsReturnedValue()) {
                auto targetOp = unknownCall.Call->TargetOp();
                auto callInstr = targetOp->DefiningInstrAs<CallInstr>();

                if(callInstr) {
                    if(CompleteUsingContext(unknownTarget, callInstr, callSite)) {
                        functionCount++;
                        continue;
                    }
                }
            }
               
            if(unknownTarget.IsSingleFunction()) {
                // The target is itself a function reference.
                AddCall(callSite, unknownTarget.Target->As<FunctionReference>());
            }
            else {
                // Get the associated function references
                // and add each of them as callees.
                auto& functions = potentialFuncts_[unknownTarget];
                functionCount += functions.Count();
                AddCallsToFoundFunctions(callSite, functions);
            }
        }

        // If any target has its address taken or no function 
        // could be associated with any target or an ambiguous store
        // stored into the target we need to add a call to the unknown node.
        if(hasAddressTakenTarget || hasAmbiguousTarget || (functionCount == 0)) {
            callSite->AddCallToUnkown();
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::CompleteUsingContext(UnknownTarget returnedTarget, 
                                            CallInstr* instr, CallSite* callSite) {
    DebugValidator::IsTrue(returnedTarget.IsReturnedValue());
    DebugValidator::IsNotNull(instr);

    // The target originates from a returned pointer by 'instr'.
    // If the returned pointer depends only on the received parameters
    // (arguments of 'instr'), we might refine the target functions
    // by taking the context (the values of the arguments) into consideration.
    //
    // For example, suppose 'select' selects between the two parameters:
    // FUNCT_PTR select(int condition, FUNCT_PTR a, FUNCT_PTR b) {
    //    if(condition) return a;
    //    else return b;
    // }
    //
    // If we have two pair of calls to 'select' like
    // 'p1 = select(a, b)' and 'p2 = select(x, y)', using the usual algorithm
    // it would appear that both 'p1' and 'p2' could call 'a', 'b', 'x' or 'y'.
    // But taking the context into consideration it's clear that
    // 'p1' calls only 'a' or 'b' and 'p2' only 'x' or 'y'.
    auto functionRef = instr->TargetOp()->As<FunctionReference>();

    if((functionRef == nullptr) ||
       (functionRef != returnedTarget.Target) ||
       (parameterReturns_.ContainsKey(functionRef) == false)) {
        return false;
    }

    // If the returned value doesn't depend only
    // on the parameters (and some optional known functions) we give up.
    auto& paramReturn = parameterReturns_[functionRef];
    
    if(paramReturn.ReturnsOnlyParameters == false) {
        return false;
    }

    // First add any optional known function.
    for(int i = 0; i < paramReturn.ReturnedFunctions.Count(); i++) {
        AddCall(callSite, paramReturn.ReturnedFunctions[i]);
    }

    // Add the functions associated with each argument
    // that passes values to the parameters considered by the return.
    for(int i = 0; i < paramReturn.ReturnedParameters.Count(); i++) {
        __int64 argumentIndex = paramReturn.ReturnedParameters[i];

        /// Ignore invalid indexes.
        if(argumentIndex >= instr->ArgumentCount()) {
            continue;
        }

        // The argument might be either a known function reference
        // or one or more resolved unknown targets.
        auto argument = instr->GetArgument(argumentIndex);

        if(auto functionRef = argument->As<FunctionReference>()) {
            AddCall(callSite, functionRef);
        }
        else {
            UnknownTargetList argumentTargetList;
            IdentifyTargets(argument, nullptr, &argumentTargetList);

            // Add the functions potentially associated with each target.
            for(int j = 0; j < argumentTargetList.Count(); j++) {  
                auto& argumentFunctions = potentialFuncts_[argumentTargetList[j]];
                AddCallsToFoundFunctions(callSite, argumentFunctions);
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraphBuilder::AddCallsToFoundFunctions(CallSite* callSite, 
                                                FunctionList& functions) {
    // Add a call to each associated function.
    for(int i = 0; i < functions.Count(); i++) {
        AddCall(callSite, functions[i]);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::AddNewTargets(UnknownTargetList& targets) {
    for(int i = 0; i < targets.Count(); i++) {
        AddNewTarget(targets[i]);
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::AddNewTarget(UnknownTarget target, UnknownCall* unknownCall) {
    // Make sure we don't add the target twice.
    if(unknownCall) {
        if(unknownCall->Targets.Contains(target) == false) {
            unknownCall->Targets.Add(target);
        }
    }

    if(potentialFuncts_.ContainsKey(target) == false) {
        potentialFuncts_.Add(target, FunctionList());
    }

    // 'false' means that the operand has no ambiguous stores yet.
    // A store targeting this operand is ambiguous if it is
    // a store into an array with an index which is not a constant.
    if(unknownTargets_.ContainsKey(target.Target) == false) {
        unknownTargets_.Add(target.Target, false);
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraphBuilder::AddNewTarget(VariableReference* variableRef, __int64 index, 
                                    UnknownCall* unknownCall) {
    DebugValidator::IsNotNull(variableRef);
    DebugValidator::IsNotNull(unknownCall);

    // We ignore local variables. Global variables with
    // their address taken are not ignored, but we still need
    // to add a call to the unknown node.
    if(variableRef->IsLocalVariableRef()) {
        return false;
    }

    AddNewTarget(UnknownTarget(variableRef, index), unknownCall);
    return true;
}

} // namespace Analysis