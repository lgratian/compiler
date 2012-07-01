// ParameterFlags.hpp
// Copyright (c) Lup Gratian
//
// Implements the ParameterFlags pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ParameterFlags.hpp"

namespace Analysis {

void ParameterFlags::Execute(CallGraph* callGraph) {
    local<EscapedParametersVisitor> visitor = new EscapedParametersVisitor(this);
    callGraph->FindRoots(true);
    callGraph->PostorderTraversalFromRoots(visitor);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::Visit(CallNode* node,
                                                     CallGraph* callGraph) {
    // Ignore the External and Unknown nodes
    // and functions without a body.
    auto function = GetFunctionDefinition(node);
    
    if(function == nullptr) {
        return;
    }

    // Make the list of the parameters that need to be tracked.
    TrackedOperandsList trackedOps;
    MakeTrackedParameterList(function, trackedOps);
    
    if(trackedOps.Count() == 0) {
        // Some parameters might escape because their address is taken.
        return;
    }

    ProcessInstructions(function, trackedOps, callGraph);
#if 1
    Dump(function);
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     ProcessInstructions(Function* function, TrackedOperandsList& trackedOps,
                         CallGraph* callGraph) {
    // Process each instructions and check if the tracked
    // parameter/operands escape because one of them.
    auto instructionEnum = function->GetInstructionEnum();

    while(instructionEnum.IsValid()) {
        auto instr = instructionEnum.Next();

        if(auto storeInstr = instr->As<StoreInstr>()) {
            // If the parameter is stored we mark it as escaped.
            MarkEscapedParameters(storeInstr->SourceOp(), trackedOps);

            // Mark the read/written parameters.
            MarkReadParameters(storeInstr->SourceOp(), trackedOps);
            MarkWrittenParameters(storeInstr->DestinationOp(), trackedOps);
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            ProcessCall(callInstr, trackedOps, callGraph);
        }
        else if(auto retInstr = instr->As<ReturnInstr>()) {
            // If the parameter is stored we mark it as escaped.
            if(retInstr->IsVoid() == false) {
                MarkEscapedParameters(retInstr->ReturnedOp(), trackedOps);
            }
        }
        else if(auto loadInstr = instr->As<LoadInstr>()) {
            // Mark the read parameters.
            MarkReadParameters(loadInstr->SourceOp(), trackedOps);
        }
        else if(instr->IsComparison() == false) {
            if(instr->IsPtop() || instr->IsAddressing()) {
                AddTrackedOperand(instr->GetDestinationOp(),
                                  instr->GetSourceOp(0), trackedOps);
            }
            else if(auto phiInstr = instr->As<PhiInstr>()) {
                for(int i = 0; i < phiInstr->OperandCount(); i++) {
                    AddTrackedOperand(phiInstr->GetDestinationOp(),
                                      phiInstr->GetOperand(i), trackedOps);
                }
            }
            else if(auto questInstr = instr->As<QuestionInstr>()) {
                AddTrackedOperand(questInstr->GetDestinationOp(),
                                  questInstr->TrueOp(), trackedOps);
                AddTrackedOperand(questInstr->GetDestinationOp(),
                                  questInstr->FalseOp(), trackedOps);
            }
            else {
                // Any other instruction that uses the parameters
                // is consider to let them escape.
                for(int i = 0; i < instr->SourceOpCount(); i++) {
                    MarkEscapedParameters(instr->GetSourceOp(i), trackedOps);
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* ParameterFlags::EscapedParametersVisitor::
          GetFunctionDefinition(CallNode* node) {
    // Ignore the External and Unknown nodes.
    if(node->IsExternalNode() || node->IsUnknownNode()) {
        return nullptr;
    }

    // Consider only defined functions.
    auto function = node->GetFunction();

    if(function->IsDefinition() == false) {
        return nullptr;
    }
    else return function;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::Visit(CallNodeGroup* node, 
                                                     CallGraph* callGraph) {
    // Process each node in the group. If any of the parameters
    // escaped we mark all other ones as escaped.
    bool paramsEscaped = false;
    bool paramsRead = false;
    bool paramsWritten = false;

    for(int i = 0; i < node->NodeCount(); i++) {
        auto childNode = node->GetNode(i);

        if(childNode->IsNodeGroup() == false) {
            auto childCallNode = static_cast<CallNode*>(childNode);
            Visit(childCallNode, callGraph);

            HasModifiedParameters(childCallNode, paramsEscaped,
                                  paramsRead, paramsWritten);
            if(paramsEscaped && paramsRead && paramsWritten) {
                break;
            }
        }
        else { 
            paramsEscaped = true;
            paramsRead = true;
            paramsWritten = true;
            break;
        }
    }

    if(paramsEscaped) {
        // Mark all parameters as escaped, it's a very conservative assumption.
        for(int i = 0; i < node->NodeCount(); i++) {
            auto childNode = node->GetNode(i);

            if(childNode->IsNodeGroup() == false) {
                auto childCallNode = static_cast<CallNode*>(childNode);
                MarkAllParameters(childCallNode, paramsEscaped,
                                  paramsRead, paramsWritten);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     HasModifiedParameters(CallNode* node, bool& hasEscaped, 
                           bool& hasRead, bool& hasWwritten) {
    auto function = GetFunctionDefinition(node);
    
    if(function) {
        for(int i = 0; i < function->ParameterCount(); i++) {
            auto parameterVariable = function->GetParameterVariable(i);
            hasEscaped |= parameterVariable->IsEscape();
            hasRead |= parameterVariable->IsRead();
            hasWwritten |= parameterVariable->IsWrite();
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MarkAllParameters(CallNode* node, bool escaped, bool read, bool written) {
    auto function = GetFunctionDefinition(node);
    
    if(function) {
        for(int i = 0; i < function->ParameterCount(); i++) {
            auto parameterVariable = function->GetParameterVariable(i);
            parameterVariable->SetIsNoEscape(escaped == false);
            parameterVariable->SetIsNoRead(read == false);
            parameterVariable->SetIsNoWrite(written == false);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     ProcessCall(CallInstr* instr, TrackedOperandsList& trackedOps, 
                 CallGraph* callGraph) {
    // Calls are more complex than the other instructions.
    // We try to use both language information and information
    // about the escaped parameters of all functions that could be
    // called by this call site.
    for(int i = 0; i < instr->ArgumentCount(); i++) {
        // Check only pointer arguments. Parameters that were
        // converted to other types are already marked as escaped.
        auto argument = instr->GetArgument(i);

        if(argument->IsPointer() == false) {
            continue;
        }

        // Check if this parameter is tracked.
        bool modifiedComputed = false;
        bool isEscaped;
        bool isRead;
        bool isWritten;

        for(int j = 0; j < trackedOps.Count(); j++) {
            if(trackedOps[j].TrackedOperand != argument) {
                continue;
            }

            // Check if the parameter escapes in any
            // of the functions that may be called.
            if(modifiedComputed == false) {
                modifiedComputed = true;
                ParameterIsModifiedInTargets(instr, i, callGraph,
                                                isEscaped, isRead, isWritten);
            }

            if(isEscaped) {
                auto parameterVariable = trackedOps[j].ParameterVariable;
                parameterVariable->SetIsNoEscape(false);
                parameterVariable->SetIsNoRead(false);
                parameterVariable->SetIsNoWrite(false);
                trackedOps.RemoveAt(i); // No reason to track it anymore.
                j--;
            }
            else {
                if(isRead) {
                    trackedOps[j].ParameterVariable->SetIsNoRead(false);
                }

                if(isWritten) {
                    trackedOps[j].ParameterVariable->SetIsNoWrite(false);
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     ParameterIsModifiedInTargets(CallInstr* instr, int parameterIndex,
                                  CallGraph* callGraph, bool& escapes,
                                  bool& read, bool& written) {
    // If the Unknown node is called we sadly need to presume
    // the parameter escapes/is read/is written. 
    // If the External node is called we can use language information.
    auto callSite = callGraph->GetCallSite(instr);

    if(callSite->CallsUnknownFunctions()) {
        escapes = read = written = true;
        return;
    }

    for(int i = 0; i < callSite->CalledFunctionsCount(); i++) {
        auto node = callSite->GetCalledNode(i);

        // For node groups we are conservative and presume it may escape
        // if there is at least one function with an escaping parameter.
        if(node->IsNodeGroup()) {
            auto nodeGroup = static_cast<CallNodeGroup*>(node);
            NodeGroupHasModifiedPointers(nodeGroup, escapes, read, written);
            
            if(escapes && read && written) {
                return;
            }
            else continue;
        }
        
        // We have a single call node. If it represents the External node
        // we skip it because the external function was already processed
        // or will be processed next.
        auto callNode = static_cast<CallNode*>(node);

        if(callNode->IsExternalNode()) {
            continue;
        }

        // If the function has a definition it was already 
        // processed and can use its escape/read/written information.
        auto calledFunction = callNode->GetFunction();

        if(calledFunction->IsDefinition()) {
            auto parameterVar = calledFunction->GetParameterVariable(parameterIndex);

            escapes = parameterVar->IsEscape();
            read = parameterVar->IsRead();
            written = parameterVar->IsWrite();
        }
        else if(auto intrinsic = instr->GetIntrinsic()) {
            // 'setMemory', 'copyMemory' and the prefetch intrinsics
            // don't capture their parameters.
            if(intrinsic->IsMemoryIntrinsic()) {
                escapes = false;
                read = (i == 1);    // Source operand.
                written = (i == 0); // Destination operand.
            }
            else { 
                escapes = read = written = true;
                return;
            }
        }
        else if(auto languageInfo = parent_->GetLanguageInfo()) {
            // Try to use language information for declarations.
            // For some standard library function we know they
            // don't capture any parameters.
            read = written = true;
            escapes = languageInfo->CallMayCaptureParameter(calledFunction,
                                                            instr, parameterIndex);
        }
        else {
            escapes = read = written = true;
            return;
        }

        if(escapes && read && written) {
            return;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     NodeGroupHasModifiedPointers(CallNodeGroup* nodeGroup, bool& escapes, 
                                  bool& read, bool& written) {
    // Check if there is a function in the group
    // with escaping pointer parameters.
    for(int i = 0; i < nodeGroup->NodeCount(); i++) {
        auto node = nodeGroup->GetNode(i);

        // Don't analyze nested node group (they shouldn't really appear anyhow).
        if(node->IsNodeGroup()) {
            escapes = read = written = true;
            return;
        }

        auto callNode = static_cast<CallNode*>(node);
        auto calledFunction = callNode->GetFunction();

        for(int i = 0; i < calledFunction->ParameterCount(); i++) {
            auto parameterVariable = calledFunction->GetParameterVariable(i);

            if(parameterVariable->IsPointer()) {
               escapes = parameterVariable->IsEscape();
               read = parameterVariable->IsEscape();
               written = parameterVariable->IsEscape();
               
               if(escapes && read && written) {
                   return;
               }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MakeTrackedParameterList(Function* function, TrackedOperandsList& trackedOps) {
    // Add to the list all parameters that are pointers.
    // A parameter that has its address taken is marked 
    // as escaped from the start.
    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameterVariable = function->GetParameterVariable(i);

        if(parameterVariable->IsPointer()) {
            if(parameterVariable->IsAddressTaken()) {
                parameterVariable->SetIsNoEscape(false);
                parameterVariable->SetIsNoRead(false);
                parameterVariable->SetIsNoWrite(false);
            }
            else {
                auto parameter = function->GetParameter(i);
                parameterVariable->SetIsNoEscape(true); // Assume it doesn't escape.
                parameterVariable->SetIsNoRead(true);   // Assume it isn't read.
                parameterVariable->SetIsNoWrite(true);  // Assume it isn't written.
                trackedOps.Add(TrackedOperandInfo(parameter, parameter, 
                                                  parameterVariable, i));
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     AddTrackedOperand(Operand* targetOp, Operand* candidateOp, 
                       TrackedOperandsList& trackedOps) {
    // Ignore instructions without users and constant candidates.
    if((targetOp == nullptr) || candidateOp->IsConstant()) {
        return;
    }

    // Scan the tracked operands and for each one that matches
    // 'candidateOp' add tracking information for 'targetOp'
    // having associated the parameter of 'candidateOp'.
    int count = trackedOps.Count();

    for(int i = 0; i < count; i++) {
        auto& trackedOp = trackedOps[i];

        if(trackedOp.TrackedOperand == candidateOp) {
            trackedOps.Add(TrackedOperandInfo(targetOp, trackedOp.TrackedParameter,
                                              trackedOp.ParameterVariable, 
                                              trackedOp.ParameterIndex));
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MarkEscapedParameters(Operand* op, TrackedOperandsList& trackedOps) {
    // Check if the operand is among the tracked ones. 
    // If it is mark the associated parameter as escaped,
    // and sadly we also need to mark it as read and written.
    // A linear search is OK because there are few parameters.
    for(int i = 0; i < trackedOps.Count(); i++) {
        if(trackedOps[i].TrackedOperand == op) {
            auto parameterVariable = trackedOps[i].ParameterVariable;
            parameterVariable->SetIsNoEscape(false);
            parameterVariable->SetIsNoRead(false);
            parameterVariable->SetIsNoWrite(false);
            
            // There is no reason to track it anymore.
            trackedOps.RemoveAt(i); 
            i--;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MarkReadParameters(Operand* op, TrackedOperandsList& trackedOps) {
    // Check if the operand is among the tracked ones. 
    // If it is mark the associated parameter as being read.
    for(int i = 0; i < trackedOps.Count(); i++) {
        if(trackedOps[i].TrackedOperand == op) {
            trackedOps[i].ParameterVariable->SetIsNoRead(false);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MarkWrittenParameters(Operand* op, TrackedOperandsList& trackedOps) {
    // Check if the operand is among the tracked ones. 
    // If it is mark the associated parameter as being written.
    for(int i = 0; i < trackedOps.Count(); i++) {
        if(trackedOps[i].TrackedOperand == op) {
            trackedOps[i].ParameterVariable->SetIsNoWrite(false);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MarkReadWrittenParameters(Operand* op, TrackedOperandsList& trackedOps) {
    // Check if the operand is among the tracked ones. 
    // If it is mark the associated parameter as being both read and written.
    for(int i = 0; i < trackedOps.Count(); i++) {
        if(trackedOps[i].TrackedOperand == op) {
            auto parameterVariable = trackedOps[i].ParameterVariable;
            parameterVariable->SetIsNoRead(false);
            parameterVariable->SetIsNoWrite(false);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::Dump(Function* function) {
    string title = "Modified parameters in " + *function->Name();
    string params;

    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameterVariable = function->GetParameterVariable(i);

        if(parameterVariable->IsPointer()) {
            if(parameterVariable->HasName()) {
                params += *parameterVariable->Name();
            }
            else {
                params += string::Format(L"Parameter %d", i);
            }

            params += (parameterVariable->IsEscape() ? ": escaped, " : ": not escaped, ");
            params += (parameterVariable->IsRead() ? ": read, " : ": not read, ");
            params += (parameterVariable->IsWrite() ? ": written, " : ": not written, ");
            params += "\n";
        }
    }

    ObjectDumper(params, title).Dump();
}

} // namespace Analysis