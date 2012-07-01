// CallGraph.cpp
// Copyright (c) Lup Gratian
//
// Implements the CallGraph class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "CallGraph.hpp"

namespace Analysis {

// Create the nodes that represents the external and unknown calls.
ExternalCallNode* ExternalCallNode::externalNode_ = new ExternalCallNode();
UnknownCallNode* UnknownCallNode::unknownNode_ = new UnknownCallNode();

CallSite* CallSite::externalCallSite_ = new CallSite(ExternalCallNode::GetExternalNode(), -1);
CallSite* CallSite::unknownCallSite_ = new CallSite(UnknownCallNode::GetUnknownNode(), -1);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int CallGraph::CollapseCycles() {
    // We detect cycles in the call graph using Tarjan's algorithm
    // for detecting strongly connected components.
    CallNodeGroupList components;
    CallNodeList nodeStack;
    CallNodeMap inStack;
    int preorderNumber = 1;

    // Collect all SCC's.
    for(int i = 0; i < nodes_.Count(); i++) {
        auto node = static_cast<CallNode*>(nodes_[i]);

        // If the node was not visited yet do it now,
        // it might be part of a SCC.
        if(node->Weight() == 0) {
            DiscoverComponent(node, preorderNumber,
                              nodeStack, inStack, components);
        }
    }

    // If we found SCC's we redirect the call sites that called
    // nodes found in an SCC to call the node that represents the SCC
    // (instead of calling a single node it calls the set of nodes in the SCC).
    for(int i = 0; i < components.Count(); i++) {
        auto component = components[i];
        groupNodes_.Add(component);
        RedirectCallingSites(component);
    }

    return components.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int CallGraph::DiscoverComponent(CallNode* node, int& preorderNumber,
                                 CallNodeList& nodeStack, CallNodeMap& inStack,
                                 CallNodeGroupList& components) {
    // The node "weight" is used to store the preorder number.
    node->SetWeight(preorderNumber);
    int lowLink = preorderNumber;
    preorderNumber++;

    // Add the potential root node on the stack.
    nodeStack.Add(node);
    inStack.Add(node, true);

    // Process each called node from all call sites.
    for(int i = 0; i < node->CallSiteCount(); i++) {
        auto callSite = node->GetCallSite(i);

        for(int j = 0; j < callSite->CalledFunctionsCount(); j++) {
            DebugValidator::IsFalse(callSite->GetCalledNode(j)->IsNodeGroup());
            auto calledNode = static_cast<CallNode*>(callSite->GetCalledNode(j));

            if(calledNode->Weight() == 0) {
                // The successor has not been visited yet.
                int calledLowLink = DiscoverComponent(calledNode, preorderNumber,
                                                      nodeStack, inStack, components);
                lowLink = std::min(lowLink, calledLowLink);
            }
            else if(inStack.ContainsKey(calledNode)) {
                // The successor is in the current SCC.
                lowLink = std::min(lowLink, calledNode->Weight());
            }
        }
    }

    // If the node is a SCC root and the SCC has more than one node
    // we form the SCC and add it to the component list.
    // Note that there is no reason to consider SCC's with a single node.
    if(node->Weight() == lowLink) {
        // Count how many nodes would form the component.
        int componentNodes = 0;

        for(int i = nodeStack.Count() - 1; i >= 0; i--) {
            componentNodes++;
            
            if((nodeStack[i] == node) || (componentNodes > 1)) {
                // We either reached the root node or have more than one node.
                // It's enough for us to known if there are 1 or more nodes.
                break;
            }
        }

        if(componentNodes > 1) {
            // Found a strongly connected component
            // and it contains all nodes found on the stack.
            auto component = CallNodeGroup::GetNodeGroup();
            components.Add(component);

            while(true) {
                auto lastNode = nodeStack.RemoveLast();
                inStack.Remove(lastNode);
                component->AddNode(lastNode);

                if(lastNode == node) {
                    // Step because we reached the root node.
                    break;
                }
            }
        }
        else {
            // We have at least one node on the stack.
            auto lastNode = nodeStack.RemoveLast();
            inStack.Remove(lastNode);
        }
    }

    return lowLink;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraph::RedirectCallingSites(CallNodeGroup* component) {
    // All nodes that are part of the discovered component
    // are no longer called directly, instead all calls
    // are target the component.
    for(int i = 0; i < component->NodeCount(); i++) {
        DebugValidator::IsFalse(component->GetNode(i)->IsNodeGroup());
        auto node = static_cast<CallNode*>(component->GetNode(i));

        // Make all call sites that were calling this node
        // call the component that now contains the node.
        int skipped = 0;

        while((node->CallingSitesCount() - skipped) > 0) {
            auto callSite = node->GetCallingSite(skipped);

            // If the call site is found in a node that is part
            // of the component we don't redirect. We also don't remove
            // calls from the External and Unknown nodes.
            if(callSite->IsExternalCallSite() ||
               callSite->IsUnknownCallSite()  ||
               component->HasNode(callSite->GetParent())) {
                skipped++;
                continue;
            }

            // Redirect all call sites.
            callSite->ReplaceCalledFunction(node, component);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int CallGraph::FindRoots(bool ignoreCallsFromExtern) {
    if(rootNodesComputed_) {
        return rootNodes_.Count();
    }

    // A root node is considered a function that is not
    // called by any other functions.
    rootNodes_.Clear();

    for(int i = 0; i < nodes_.Count(); i++) {
        if(nodes_[i]->IsCalledByFunctions() == false ||
           (ignoreCallsFromExtern && (nodes_[i]->CallingSitesCount() == 1))) {
            rootNodes_.Add(nodes_[i]);
        }
    }

    rootNodesComputed_ = true;
    return rootNodes_.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraph::DepthFirstSearch(CallNodeBase* startNode, CallNodeVisitor* visitor) {
    DebugValidator::IsNotNull(startNode);
    DebugValidator::IsNotNull(visitor);
    
    CallNodeMap visitedNodes;
    DepthFirstSearchImpl(startNode, visitor, visitedNodes);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraph::DepthFirstSearchImpl(CallNodeBase* node, CallNodeVisitor* visitor,
                                     CallNodeMap& visitedNodes) {
    // Visit the node.
    visitedNodes.Add(node, true);
    VisitNode(node, visitor);

    // We don't recourse into node groups.
    if(node->IsNodeGroup()) {
        return;
    }

    // Recourse into each called node by each call site.
    auto callNode = static_cast<CallNode*>(node);

    for(int i = 0; i < callNode->CallSiteCount(); i++) {
        auto callSite = callNode->GetCallSite(i);
            
        for(int j = 0; j < callSite->CalledFunctionsCount(); j++) {
            auto calledNode = callSite->GetCalledNode(j);

            // Don't visit twice.
            if(visitedNodes.ContainsKey(calledNode) == false) {
                DepthFirstSearchImpl(calledNode, visitor, visitedNodes);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraph::DepthFirstSearchFromRoots(CallNodeVisitor* visitor) {
    DebugValidator::IsNotNull(visitor);
    DebugValidator::IsTrue(rootNodesComputed_);

    if(rootNodes_.Count() > 0) {
        CallNodeMap visitedNodes;

        for(int i = 0; i < rootNodes_.Count(); i++) {
            DepthFirstSearchImpl(rootNodes_[i], visitor, visitedNodes);
        }
    }

    return rootNodes_.Count() > 0;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraph::DepthFirstSearchFromUniqueRoot(CallNodeVisitor* visitor) {
    DebugValidator::IsNotNull(visitor);
    DebugValidator::IsTrue(rootNodesComputed_);

    if(rootNodes_.Count() == 1) {
        CallNodeMap visitedNodes;
        DepthFirstSearchImpl(rootNodes_[0], visitor, visitedNodes);
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraph::BreadthFirstSearch(CallNodeBase* startNode, CallNodeVisitor* visitor) {
    DebugValidator::IsNotNull(startNode);
    DebugValidator::IsNotNull(visitor);
    
    CallNodeList startNodes;
    startNodes.Add(startNode);
    BreadthFirstSearchImpl(startNodes, visitor);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraph::BreadthFirstSearchImpl(CallNodeList& startNodes, CallNodeVisitor* visitor) {
    CallNodeMap visitedNodes;
    CallNodeList queue;
    int position = 0;

    // Queue the start nodes.
    for(int i = 0; i < startNodes.Count(); i++) {
        visitedNodes.Add(startNodes[i], true);
        queue.Add(startNodes[i]);
    }

    while(position < queue.Count()) {
        auto node = queue[position];
        position++;

        VisitNode(node, visitor);

        // We don't recourse into node groups.
        if(node->IsNodeGroup()) {
            continue;
        }

        // Enqueue into each called node by each call site.
        auto callNode = static_cast<CallNode*>(node);

        for(int i = 0; i < callNode->CallSiteCount(); i++) {
            auto callSite = callNode->GetCallSite(i);
            
            for(int j = 0; j < callSite->CalledFunctionsCount(); j++) {
                auto calledNode = callSite->GetCalledNode(j);

                // Don't visit twice.
                if(visitedNodes.ContainsKey(calledNode) == false) {
                    queue.Add(calledNode);
                    visitedNodes.Add(calledNode, true);
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraph::BreadthFirstSearchFromRoots(CallNodeVisitor* visitor) {
    DebugValidator::IsNotNull(visitor);
    DebugValidator::IsTrue(rootNodesComputed_);

    CallNodeList startNodes;

    for(int i = 0; i < rootNodes_.Count(); i++) {
        startNodes.Add(rootNodes_[i]);
    }

    BreadthFirstSearchImpl(startNodes, visitor);
    return rootNodes_.Count() > 0;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraph::BreadthFirstSearchFromUniqueRoot(CallNodeVisitor* visitor) {
    DebugValidator::IsNotNull(visitor);
    DebugValidator::IsTrue(rootNodesComputed_);

    if(rootNodes_.Count() == 1) {
        CallNodeList startNodes;
        startNodes.Add(rootNodes_[0]);
        BreadthFirstSearchImpl(startNodes, visitor);
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraph::PostorderTraversal(CallNodeBase* startNode, 
                                   CallNodeVisitor* visitor) {
    DebugValidator::IsNotNull(startNode);
    DebugValidator::IsNotNull(visitor);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraph::PostorderTraversalFromRoots(CallNodeVisitor* visitor) {
    DebugValidator::IsNotNull(visitor);
    DebugValidator::IsTrue(rootNodesComputed_);

    if(rootNodes_.Count() > 0) {
        CallNodeList startNodes;

        for(int i = 0; i < rootNodes_.Count(); i++) {
            startNodes.Add(rootNodes_[i]);
        }

        PostorderTraversalImpl(startNodes, visitor);
    }

    return rootNodes_.Count() > 0;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallGraph::PostorderTraversalFromUniqueRoot(CallNodeVisitor* visitor) {
    DebugValidator::IsNotNull(visitor);
    DebugValidator::IsTrue(rootNodesComputed_);

    if(rootNodes_.Count() == 1) {
        CallNodeList startNodes;
        startNodes.Add(rootNodes_[0]);
        PostorderTraversalImpl(startNodes, visitor);
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallGraph::PostorderTraversalImpl(CallNodeList& startNodes, 
                                       CallNodeVisitor* visitor) {
    CallNodeList stack;
    CallNodeMap visitedNodes;
    CallNodeMap processedNodes;

    for(int i = 0; i < startNodes.Count(); i++) {
        stack.Add(startNodes[i]);
        visitedNodes.Add(startNodes[i], true);
    }

    while(stack.IsNotEmpty()) {
        // Push on the stack all nodes not visited yet.
        // If no node was pushed we process this one.
        auto lastNode = stack.PeekLast();
        int pushed = 0;

        if((lastNode->IsNodeGroup() == false) &&
           (processedNodes.ContainsKey(lastNode) == false)) {
            auto callNode = static_cast<CallNode*>(lastNode);
            
            for(int i = 0; i < callNode->CallSiteCount(); i++) {
                auto callSite = callNode->GetCallSite(i);
            
                for(int j = 0; j < callSite->CalledFunctionsCount(); j++) {
                    auto calledNode = callSite->GetCalledNode(j);

                    // Don't visit twice.
                    if(visitedNodes.ContainsKey(calledNode) == false) {
                        stack.Add(calledNode);
                        visitedNodes.Add(calledNode, true);
                        pushed++;
                    }
                }
            }

            processedNodes.Add(lastNode, true);
        }

        if(pushed == 0) {
            VisitNode(lastNode, visitor);
            stack.RemoveLast();
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallNode::AddCallFromExternal() {
    AddCallingSite(CallSite::GetExternalCallSite());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallNode::AddCallFromUnknown() {
    AddCallingSite(CallSite::GetUnknownCallSite());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int CallNode::CalledFunctionsCount() const {
    int count = 0;

    for(int i = 0; i < callSites_.Count(); i++) {
        count += callSites_[i]->CalledFunctionsCount();
    }

    return count;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallNode::CallsExternalFunctions() const {
    for(int i = 0; i < callSites_.Count(); i++) {
        if(callSites_[i]->CallsExternalFunctions()) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallNode::CallsUnknownFunctions() const {
    for(int i = 0; i < callSites_.Count(); i++) {
        if(callSites_[i]->CallsUnknownFunctions()) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallNode::RemoveCallSite(int callId) {
    for(int i = 0; i < callSites_.Count(); i++) {
        if(callSites_[i]->GetCallId() == callId) {
            callSites_.RemoveAt(i);
            return;
        }
    }

    DebugValidator::Unreachable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallNodeBase::IsCalledByExternalFunctions() const {
    for(int i = 0; i < callingFuncts_.Count(); i++) {
        if(callingFuncts_[i].Caller()->IsExternalCallSite()) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallNodeBase::IsCalledBySite(CallSite* callSite) const {
    DebugValidator::IsNotNull(callSite);

    for(int i = 0; i < callingFuncts_.Count(); i++) {
        if(callingFuncts_[i].Caller() == callSite) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallNodeBase::IsCalledByNode(CallNodeBase* node) const {
    DebugValidator::IsNotNull(node);

    for(int i = 0; i < callingFuncts_.Count(); i++) {
        if(callingFuncts_[i].Caller()->GetParent() == node) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallNodeBase::IsCalledByUnknownFunctions() const {
    for(int i = 0; i < callingFuncts_.Count(); i++) {
        if(callingFuncts_[i].Caller()->IsUnknownCallSite()) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallNodeBase::RemoveCallFromExternal() {
    RemoveCallingSite(CallSite::GetExternalCallSite());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallNodeBase::RemoveCallFromUnknown() {
    RemoveCallingSite(CallSite::GetUnknownCallSite());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallNode::RemoveCallFromSite(CallSite* callSite) {
    DebugValidator::IsNotNull(callSite);
    DebugValidator::IsTrue(IsCalledBySite(callSite));

    for(int i = 0; i < callingFuncts_.Count(); i++) {
        if(callingFuncts_[i].Caller() == callSite) {
            callingFuncts_.RemoveAt(i);
            return;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallNode::RemoveCallsFromNode(CallNodeBase* node) {
    DebugValidator::IsNotNull(node);
    DebugValidator::IsTrue(IsCalledByNode(node));

    for(int i = 0; i < callingFuncts_.Count(); i++) {
        if(callingFuncts_[i].Caller()->GetParent() == node) {
            callingFuncts_.RemoveAt(i);
            i--;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int CallNodeGroup::CalledFunctionsCount() const {
    int count = 0;

    for(int i = 0; i < nodes_.Count(); i++) {
        count += nodes_[i]->CalledFunctionsCount();
    }

    return count;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallNodeGroup::CallsExternalFunctions() const {
    for(int i = 0; i < nodes_.Count(); i++) {
        if(nodes_[i]->CallsExternalFunctions()) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CallNodeGroup::CallsUnknownFunctions() const {
    for(int i = 0; i < nodes_.Count(); i++) {
        if(nodes_[i]->CallsUnknownFunctions()) {
            return true;
        }
    }

    return false;
}

} // namespace Analysis