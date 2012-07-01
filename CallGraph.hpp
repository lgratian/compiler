// CallGraph.hpp
// Copyright (c) Lup Gratian
//
// Defines the CallGraph, CallNode and CallEdge classes.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_CALL_GRAPH_HPP
#define PC_ANALYSIS_CALL_GRAPH_HPP

#include "../IR/Function.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Base/Log.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/List.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

// Forward declarations.
class CallSite;
class CallNodeBase;
class CallGraph;

// Represents an edge in the call graph
// that links the caller to the called function.
class CallEdge {
private:
    CallSite* caller_;     // The call site that does the call.
    CallNodeBase* callee_; // The called function.
    int weight_;           // Can be used by various optimizations.

public:
    CallEdge() : caller_(nullptr), callee_(nullptr), weight_(0) {}

    CallEdge(CallSite* caller, CallNodeBase* callee, int weight = 0) :
            caller_(caller), callee_(callee), weight_(weight) {
        DebugValidator::IsNotNull(caller);
        DebugValidator::IsNotNull(callee);
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Returns the node that does the call.
    CallSite* Caller() {
        return caller_;
    }

    const CallSite* Caller() const {
        return caller_;
    }

    void SetCaller(CallSite* caller) {
        DebugValidator::IsNotNull(caller);
        caller_ = caller;
    }

    // Returns the called node.
    CallNodeBase* Callee() {
        return callee_;
    }

    const CallNodeBase* Callee() const {
        return callee_;
    }

    void SetCallee(CallNodeBase* callee) {
        DebugValidator::IsNotNull(callee);
        callee_ = callee;
    }

    // Returns the weight of the edge.
    int Weight() const {
        return weight_;
    }

    void SetWeight(int value) {
        weight_ = value;
    }

    // Sets the weight to 0.
    void ResetWeight() {
        weight_ = 0;
    }

    bool operator== (const CallEdge& other) const {
        return (caller_ == other.caller_) &&
               (callee_ == other.callee_); // Ignore weight.
    }

    bool operator!= (const CallEdge& other) const {
        return !operator== (other);
    }

    bool operator< (const CallEdge& other) const {
        return weight_ < other.weight_;
    }
};


// Represents the base class for all call nodes.
class CallNodeBase {
protected:
    StaticList<CallEdge, 4> callingFuncts_; // The functions that call this node.
    int weight_;                            // Can be used by various optimizations.    

public:
    virtual ~CallNodeBase() {}

    CallNodeBase() : weight_(0) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Should return 'true' if the node represents a group.
    virtual bool IsNodeGroup() const = 0;

    // Should return 'true' if this node is the external node.
    virtual bool IsExternalNode() const = 0;

    // Should return 'true' if this node is the unknown node.
    virtual bool IsUnknownNode() const = 0;

    // Should return the total count of functions called by all call sites.
    virtual int CalledFunctionsCount() const = 0;

    // Should return 'true' if any call site calls an external function.
    virtual bool CallsExternalFunctions() const = 0;

    // Should return 'true' if any call site calls an unknown function.
    virtual bool CallsUnknownFunctions() const = 0;

    // Returns the number of functions that call this one.
    int CallingSitesCount() const {
        return callingFuncts_.Count();
    }

    // Returns the edge at the specified position that calls this node.
    CallEdge GetCallingEdge(int index) {
        return callingFuncts_[index];
    }

    const CallEdge GetCallingEdge(int index) const {
        return callingFuncts_[index];
    }

    // Returns the node at the specified position that calls this node.
    CallSite* GetCallingSite(int index) {
        return callingFuncts_[index].Caller();
    }

    const CallSite* GetCallingSite(int index) const {
        return callingFuncts_[index].Caller();
    }

    // Performs the specified action on each calling edge.
    // bool Predicate(CallEdge edge)
    template <class Predicate>
    void ForEachCallingEdge(Predicate action) {
        for(int i = 0; i < callingFuncts_.Count(); i++) {
            if(action(callingFuncts_[i]) == false) {
                // The user aborted.
                return;
            }
        }
    }

    // Performs the specified action on each calling node.
    // bool Predicate(CallNode* node)
    template <class Predicate>
    void ForEachCallingNode(Predicate action) {
        for(int i = 0; i < callingFuncts_.Count(); i++) {
            if(action(callingFuncts_[i].Caller()) == false) {
                // The user aborted.
                return;
            }
        }
    }

    // Performs the specified action on each calling function.
    // bool Predicate(FunctionReference* functionRef)
    template <class Predicate>
    void ForEachCallingFunction(Predicate action) {
        for(int i = 0; i < callingFuncts_.Count(); i++) {
            if(action(callingFuncts_[i].Caller()->GetFunction()) == false) {
                // The user aborted.
                return;
            }
        }
    }

    // Returns 'true' if the function is called by the specified call site.
    bool IsCalledBySite(CallSite* callSite) const;

    // Returns 'true' if the function is called by the specified node.
    bool IsCalledByNode(CallNodeBase* node) const;

    // Returns 'true' if the function is called by any external function.
    bool IsCalledByExternalFunctions() const;

    // Returns 'true' if the function is called by any unknown function.
    bool IsCalledByUnknownFunctions() const;

    // Returns 'true' if any call site calls functions in this node.
    bool IsCalledByFunctions() const {
        return callingFuncts_.Count() > 0;
    }

    // Adds a call from the specified call site to the parent node.
    void AddCallingSite(CallSite* callSite) {
        DebugValidator::IsNotNull(callSite);

        if(IsCalledBySite(callSite) == false) {
            callingFuncts_.Add(CallEdge(callSite, this));
        }
    }

    // Removes a call from the specified call site.
    void RemoveCallingSite(CallSite* callSite) {
        DebugValidator::IsNotNull(callSite);

        for(int i = 0; i < callingFuncts_.Count(); i++) {
            if(callingFuncts_[i].Caller() == callSite) {
                callingFuncts_.RemoveAt(i);
                return;
            }
        }

        DebugValidator::Unreachable();
    }

    // Removes the call from the external call site.
    void RemoveCallFromExternal();

    // Removes the call from the unknown call site.
    void RemoveCallFromUnknown();
};


// Represents the call node associated with a single function.
// It can contain multiple call sites.
class CallNode : public CallNodeBase {
protected:
    FunctionReference* functionRef_;        // The associated function.
    StaticList<CallSite*, 4> callSites_;    // The call sites found in the function.
    
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    CallNode();                                 // Should not be created.
    CallNode(const CallNode& other);            // Should not be copied.
    CallNode operator= (const CallNode& other); // Should not be assigned.

    CallNode(FunctionReference* functionRef) : 
            functionRef_(functionRef) {}

public:
    static CallNode* GetNode(FunctionReference* functionRef) {
        return new CallNode(functionRef);
    }

    // Returns the associated function reference.
    FunctionReference* GetFunctionReference() {
        return functionRef_;
    }

    const FunctionReference* GetFunctionReference() const {
        return functionRef_;
    }

    // Returns the associated function.
    Function* GetFunction() {
        if(functionRef_) {
            return functionRef_->Target();
        }
    }

    const Function* GetFunction() const {
        if(functionRef_) {
            return functionRef_->Target();
        }
    }

    // Returns 'true' if this node is the external node.
    virtual bool IsExternalNode() const {
        return false;
    }

    // Returns 'true' if this node is the unknown node.
    virtual bool IsUnknownNode() const {
        return false;
    }

    // Returns the number of call sites in the function.
    int CallSiteCount() const {
        return callSites_.Count();
    }

    // Returns 'true' if the function has any call sites.
    bool HasCallSites() const {
        return callSites_.Count() > 0;
    }

    // Returns the call site found at the specified position.
    CallSite* GetCallSite(int index) {
        return callSites_[index];
    }

    const CallSite* GetCallSite(int index) const {
        return callSites_[index];
    }

    // Adds the specified call site.
    void AddCallSite(CallSite* callSite) {
        DebugValidator::IsNotNull(callSite);
        DebugValidator::IsFalse(callSites_.Contains(callSite));

        callSites_.Add(callSite);
    }

    // Removes the specified call site.
    void RemoveCallSite(CallSite* callSite) {
        DebugValidator::IsNotNull(callSite);
        DebugValidator::IsTrue(callSites_.Contains(callSite));

        callSites_.Remove(callSite);
    }

    // Removes the call site associated with a 'call' having the specified Id.
    void RemoveCallSite(int callId);

    void RemoveCallSite(CallInstr* instr) {
        RemoveCallSite(instr->Id());
    }
    
    // Adds a call from the external node to this call site.
    void AddCallFromExternal();

    // Adds a call from the unknown node to this call site.
    void AddCallFromUnknown();

    // Removes a call from the specified call site.
    void RemoveCallFromSite(CallSite* callSite);

    // Removes any calls originating from the specified node.
    void RemoveCallsFromNode(CallNodeBase* node);

    // Performs the specified action on each call site.
    // bool Predicate(CallSite* callSite)
    template <class Predicate>
    void ForEachCallSite(Predicate action) {
        for(int i = 0; i < callSites_.Count(); i++) {
            if(action(callSites_[i]) == false) {
                return;
            }
        }
    }

    // Returns the weight of the node.
    int Weight() const {
        return weight_;
    }

    void SetWeight(int value) {
        weight_ = value;
    }

    // Sets the weight to 0.
    void ResetWeight() {
        weight_ = 0;
    }

    // Returns the total count of functions called
    // by all call sites of this function.
    virtual int CalledFunctionsCount() const;

    // Returns 'true' if any call site calls an external function.
    virtual bool CallsExternalFunctions() const;

    // Returns 'true' if any call site calls an unknown function.
    virtual bool CallsUnknownFunctions() const;

    virtual bool IsNodeGroup() const {
        return false;
    }
};


// Base class for the nodes that represent external/unknown functions.
class ExternalUnknownCallNode : public CallNode {
protected:
    List<CallNodeBase*> calledNodes_;

    ExternalUnknownCallNode() : CallNode(nullptr) {}

public:
    virtual bool IsExternalNode() const {
        return true;
    }

    // Returns the child node found at the specified position.
    CallNodeBase* GetCalledNode(int index) {
        return calledNodes_[index];
    }

    const CallNodeBase* GetCalledNode(int index) const {
        return calledNodes_[index];
    }

    void AddCalledNode(CallNodeBase* node) {
        DebugValidator::IsNotNull(node);
        calledNodes_.Add(node);
    }

    bool HasCalledNode(CallNodeBase* node) const {
        DebugValidator::IsNotNull(node);
        return calledNodes_.Contains(node);
    }

    void RemoveCalledNode(CallNodeBase* node) {
        DebugValidator::IsNotNull(node);
        DebugValidator::IsTrue(calledNodes_.Contains(node));
        calledNodes_.Remove(node);
    }

    // Performs the specified action on each child node.
    // bool Predicate(CallNodeBase* node)
    template <class Predicate>
    void ForEachCalledNode(Predicate action) {
        for(int i = 0; i < calledNodes_.Count(); i++) {
            if(action(calledNodes_[i]) == false) {
                return;
            }
        }
    }
};


// The node that represents any external function.
class ExternalCallNode : public ExternalUnknownCallNode {
private:
    static ExternalCallNode* externalNode_;

    ExternalCallNode() {}

public:
    static ExternalCallNode* GetExternalNode() {
        return externalNode_;
    }

    virtual bool IsExternalNode() const {
        return true;
    }
};


// The node that represents any unknown functions.
class UnknownCallNode : public ExternalUnknownCallNode {
private:
    static UnknownCallNode* unknownNode_;

    UnknownCallNode() {}

public:
    static UnknownCallNode* GetUnknownNode() {
        return unknownNode_;
    }

    virtual bool IsUnknownNode() const {
        return true;
    }
};


// Represents a group a functions in the call graph.
// Used to collapse mutually-recursive functions into a single node
// where all functions share the same properties/side-effects.
class CallNodeGroup : public CallNodeBase {
private:
    List<CallNodeBase*> nodes_;

public:
    static CallNodeGroup* GetNodeGroup() {
        return new CallNodeGroup();
    }

    virtual bool IsNodeGroup() const {
        return true;
    }

    virtual bool IsExternalNode() const {
        return false;
    }

    virtual bool IsUnknownNode() const {
        return false;
    }

    // Returns the total count of functions called
    // by all call sites of this function.
    virtual int CalledFunctionsCount() const;

    // Returns 'true' if any call site calls an external function.
    virtual bool CallsExternalFunctions() const;

    // Returns 'true' if any call site calls an unknown function.
    virtual bool CallsUnknownFunctions() const;

    // Returns the number of child nodes.
    int NodeCount() const {
        return nodes_.Count();
    }

    // Returns the child node found at the specified position.
    CallNodeBase* GetNode(int index) {
        return nodes_[index];
    }

    const CallNodeBase* GetNode(int index) const {
        return nodes_[index];
    }

    void AddNode(CallNodeBase* node) {
        DebugValidator::IsNotNull(node);
        nodes_.Add(node);
    }

    bool HasNode(CallNodeBase* node) const {
        DebugValidator::IsNotNull(node);
        return nodes_.Contains(node);
    }

    void RemoveNode(CallNodeBase* node) {
        DebugValidator::IsNotNull(node);
        DebugValidator::IsTrue(nodes_.Contains(node));
        nodes_.Remove(node);
    }

    // Performs the specified action on each child node.
    // bool Predicate(CallNodeBase* node)
    template <class Predicate>
    void ForEachNode(Predicate action) {
        for(int i = 0; i < nodes_.Count(); i++) {
            if(action(nodes_[i]) == false) {
                return;
            }
        }
    }
};


// Represents a call site in the application (function/call pair).
// Two special call sites are used in some cases:
// Unknown - when the target of the call is not known (call trough pointer)
// External - when the target is a function that has no definition
class CallSite {
private:
    StaticList<CallEdge, 4> calledFuncts_; // The called functions.
    CallNodeBase* parent_;                 // The function represented by this call site.
    int callId_;                           // The Id of the represented 'call' instruction.

    static CallSite* unknownCallSite_;
    static CallSite* externalCallSite_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    CallSite();                                 // Should not be created.
    CallSite(const CallSite& other);            // Should not be copied.
    CallSite operator= (const CallSite& other); // Should not be assigned.

    CallSite(CallNodeBase* parent, int callId) :
            parent_(parent), callId_(callId) {}

public:
    static CallSite* GetCallSite(int callId, CallNodeBase* parent) {
        DebugValidator::IsNotNull(parent);
        auto callSite = new CallSite(parent, callId);

        if(parent->IsNodeGroup() == false) {
            auto parentNode = static_cast<CallNode*>(parent);
            parentNode->AddCallSite(callSite);
        }

        return callSite;
    }

    static CallSite* GetCallSite(CallInstr* instr, CallNodeBase* parent) {
        DebugValidator::IsNotNull(parent);
        DebugValidator::IsNotNull(instr);
        return GetCallSite(instr->Id(), parent);
    }

    static CallSite* GetUnknownCallSite() {
        return unknownCallSite_;
    }

    static CallSite* GetExternalCallSite() {
        return externalCallSite_;
    }

    // Returns 'true' if the specified object represents the external call site.
    static bool IsExternalCallSite(const CallSite* callSite) {
        DebugValidator::IsNotNull(callSite);
        return callSite == externalCallSite_;
    }

    // Returns 'true' if the specified object represents the unknown call site.
    static bool IsUnknownCallSite(const CallSite* callSite) {
        DebugValidator::IsNotNull(callSite);
        return callSite == unknownCallSite_;
    }

    // Returns 'true' if this call site is the external call site.
    bool IsExternalCallSite() const {
        return IsExternalCallSite(this);
    }

    // Returns 'true' if this call site is the unknown call site.
    bool IsUnknownCallSite() const {
        return IsUnknownCallSite(this);
    }

    // Returns the node that represents the associated function.
    CallNodeBase* GetParent() {
        return parent_;
    }

    const CallNodeBase* GetParent() const {
        return parent_;
    }

    // Returns the associated function reference.
    FunctionReference* GetFunctionReference() {
        if(parent_->IsNodeGroup() == false) {
            auto parentNode = static_cast<CallNode*>(parent_);
            return parentNode->GetFunctionReference();
        }
        else return nullptr;
    }

    const FunctionReference* GetFunctionReference() const {
        if(parent_->IsNodeGroup() == false) {
            auto parentNode = static_cast<CallNode*>(parent_);
            return parentNode->GetFunctionReference();
        }
        else return nullptr;
    }

    // Returns the associated function.
    Function* GetFunction() {
        if(auto functionRef = GetFunctionReference()) {
            return functionRef->Target();
        }
    }

    const Function* GetFunction() const {
        if(auto functionRef = GetFunctionReference()) {
            return functionRef->Target();
        }
    }

    // Returns the Id of the associated 'call' instruction.
    int GetCallId() const {
        return callId_;
    }

    // Returns the associated 'call' instruction.
    CallInstr* GetCallInstruction() {
        if(auto function = GetFunction()) {
            return function->GetCallInstruction(callId_);
        }

        return nullptr;
    }

    const CallInstr* GetCallInstruction() const {
        if(auto function = GetFunction()) {
            return function->GetCallInstruction(callId_);
        }

        return nullptr;
    }

    // Returns the number of functions called by this one.
    int CalledFunctionsCount() const {
        return calledFuncts_.Count();
    }

    // Returns 'true' if the function calls other functions.
    bool CallsFunctions() const {
        return calledFuncts_.Count() > 0;
    }

    // Returns the edge at the specified position that calls this node.
    CallEdge GetCalledEdge(int index) {
        return calledFuncts_[index];
    }

    const CallEdge GetCalledEdge(int index) const {
        return calledFuncts_[index];
    }

    // Returns the node at the specified position called by this call site.
    CallNodeBase* GetCalledNode(int index) {
        return calledFuncts_[index].Callee();
    }

    const CallNodeBase* GetCalledNode(int index) const {
        return calledFuncts_[index].Callee();
    }

    // Returns the function reference at the specified position
    // called by this call site.
    FunctionReference* GetCalledFunctionReference(int index) {
        auto node = GetCalledNode(index);

        if(node->IsNodeGroup() == false) {
            auto callNode = static_cast<CallNode*>(node);
            return callNode->GetFunctionReference();
        }
        else return nullptr;
    }

    // Returns the function at the specified position called by this call site.
    Function* GetCalledFunction(int index) {
        if(auto functionRef = GetCalledFunctionReference(index)) {
            return functionRef->Target();
        }
        else return nullptr;
    }

    // Replaces calls to 'previousNode' with calls to 'newNode'.
    void ReplaceCalledFunction(CallNodeBase* previousNode, CallNodeBase* newNode) {
        for(int i = 0; i < calledFuncts_.Count(); i++) {
            if(calledFuncts_[i].Callee() == previousNode) {
                previousNode->RemoveCallingSite(this);
                newNode->AddCallingSite(this);
                calledFuncts_[i].SetCallee(newNode);
            }
        }
    }

    // Performs the specified action on each calling edge.
    // bool Predicate(CallEdge edge)
    template <class Predicate>
    void ForEachCalledEdge(Predicate action) {
        for(int i = 0; i < calledFuncts_.Count(); i++) {
            if(action(calledFuncts_[i]) == false) {
                // The user aborted.
                return;
            }
        }
    }

    // Performs the specified action on each calling node.
    // bool Predicate(CallNode* node)
    template <class Predicate>
    void ForEachCalledNode(Predicate action) {
        for(int i = 0; i < calledFuncts_.Count(); i++) {
            if(action(calledFuncts_[i].Callee()) == false) {
                // The user aborted.
                return;
            }
        }
    }

    // Performs the specified action on each calling function.
    // bool Predicate(FunctionReference* functionRef)
    template <class Predicate>
    void ForEachCalledFunction(Predicate action) {
        for(int i = 0; i < calledFuncts_.Count(); i++) {
            if(action(calledFuncts_[i].Callee()->GetFunction()) == false) {
                // The user aborted.
                return;
            }
        }
    }

    // Adds a call from this node to the specified one.
    void AddCalledNode(CallNodeBase* node) {
        DebugValidator::IsNotNull(node);

        if(CallsNode(node) == false) {
            calledFuncts_.Add(CallEdge(this, node));
        }
    }

    // Adds a call from this node to the external node.
    void AddCallToExternal() {
        AddCalledNode(ExternalCallNode::GetExternalNode());
    }

    // Adds a call from this node to the unknown node.
    void AddCallToUnkown() {
        AddCalledNode(UnknownCallNode::GetUnknownNode());
    }

    // Removes a call to the specified node.
    void RemoveCalledNode(CallNode* node) {
        DebugValidator::IsNotNull(node);
        DebugValidator::IsTrue(CallsNode(node));

        for(int i = 0; i < calledFuncts_.Count(); i++) {
            if(calledFuncts_[i].Callee() == node) {
                calledFuncts_.RemoveAt(i);
                return;
            }
        }
    }

    void RemoveCalledFunction(FunctionReference* functionRef) {
        DebugValidator::IsNotNull(functionRef);
    
        for(int i = 0; i < calledFuncts_.Count(); i++) {
            if(calledFuncts_[i].Callee()->IsNodeGroup() == false) {
                auto callNode = static_cast<CallNode*>(calledFuncts_[i].Callee());
                
                if(callNode->GetFunctionReference() == functionRef) {
                    calledFuncts_.RemoveAt(i);
                    return;
                }
            }
        }
    }

    void RemoveCalledFunction(Function* function) {
        DebugValidator::IsNotNull(function);
        RemoveCalledFunction(function->GetReference());
    }

    // Returns 'true' if the function calls the specified node.
    bool CallsNode(CallNodeBase* node) const {
        DebugValidator::IsNotNull(node);

        for(int i = 0; i < calledFuncts_.Count(); i++) {
            if(calledFuncts_[i].Callee() == node) {
                return true;
            }
        }

        return false;
    }

    // Returns 'true' if the function calls the specified function reference.
    bool CallsFunction(FunctionReference* functionRef) const {
        DebugValidator::IsNotNull(functionRef);

        for(int i = 0; i < calledFuncts_.Count(); i++) {
            if(calledFuncts_[i].Callee()->IsNodeGroup() == false) {
                auto callNode = static_cast<const CallNode*>(calledFuncts_[i].Callee());
                
                if(callNode->GetFunctionReference() == functionRef) {
                    return true;
                }
            }
        }

        return false;
    }

    // Returns 'true' if the function calls the specified function.
    bool CallsFunction(Function* function) const {
        DebugValidator::IsNotNull(function);
        return CallsFunction(function->GetReference());
    }

    // Returns 'true' if the function calls any external functions.
    bool CallsExternalFunctions() const {
        for(int i = 0; i < calledFuncts_.Count(); i++) {
            if(calledFuncts_[i].Callee()->IsExternalNode()) {
                return true;
            }
        }

        return false;
    }

    // Returns 'true' if the function calls any unknown functions.
    bool CallsUnknownFunctions() const {
        for(int i = 0; i < calledFuncts_.Count(); i++) {
            if(calledFuncts_[i].Callee()->IsUnknownNode()) {
                return true;
            }
        }

        return false;
    }
};


// Represents the base class for an object that "visits" the call nodes
// in a predetermined order (depth-first search, for example).
class CallNodeVisitor {
public:
    virtual ~CallNodeVisitor() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Called when a 'CallNode' is encountered.
    virtual void Visit(CallNode* node, CallGraph* callGraph) {}

    // Called when a 'CallNodeGroup' is encountered.
    virtual void Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) {}
};


// The call graph definition and implementation.
class CallGraph {
private:
    typedef Dictionary<FunctionReference*, CallNode*> FunctionMap;
    typedef Dictionary<int, CallSite*> CallSiteMap;
    typedef Dictionary<CallNodeBase*, bool> CallNodeMap;
    typedef List<CallNodeGroup*> CallNodeGroupList;
    typedef List<CallNodeBase*> CallNodeList;
    typedef List<CallSite*> CallSiteList;

    FunctionMap functToNode_;
    CallSiteMap callToCallSite_;
    CallNodeList nodes_;           // All function nodes.
    CallNodeGroupList groupNodes_; // All group nodes.
    CallSiteList callSites_;       // All call sites.
    CallNodeList rootNodes_;       // Functions not called by any other function.
    bool rootNodesComputed_;
    
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Tries to discover a strongly connected component
    // using Tarjan's algorithm.
    int DiscoverComponent(CallNode* node, int& preorderNumber,
                          CallNodeList& nodeStack, CallNodeMap& inStack,
                          CallNodeGroupList& components);

    // Redirects the call sites that call nodes part of the component
    // to call the component instead.
    void RedirectCallingSites(CallNodeGroup* component);

    // Calls the appropriate visitor method based
    // on the type of the node (single or group).
    void VisitNode(CallNodeBase* node, CallNodeVisitor* visitor) {
        if(node->IsNodeGroup()) {
            visitor->Visit(static_cast<CallNodeGroup*>(node), this);
        }
        else {
            visitor->Visit(static_cast<CallNode*>(node), this);
        }
    }

    // Performa a depth-first search starting with the specified node.
    void DepthFirstSearchImpl(CallNodeBase* node, CallNodeVisitor* visitor,
                              CallNodeMap& visitedNodes);

    // Performa a breadth-first search starting with the specified node.
    void BreadthFirstSearchImpl(CallNodeList& startNodes, CallNodeVisitor* visitor);

    // Performa a postorder traversal starting with the specified node.
    void PostorderTraversalImpl(CallNodeList& startNodes, CallNodeVisitor* visitor);

public:
    CallGraph() : rootNodesComputed_(false) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    CallNodeList& GetCallNodes() {
        return nodes_;
    }

    CallNodeGroupList& GetCallNodeGroups() {
        return groupNodes_;
    }

    // Returns the node associated with the specified function reference.
    CallNode* GetNode(FunctionReference* functionRef) {
        DebugValidator::IsNotNull(functionRef);
        CallNode* node;

        if(functToNode_.TryGetValue(functionRef, &node)) {
            return node;
        }

        node = CallNode::GetNode(functionRef);
        functToNode_.Add(functionRef, node);
        nodes_.Add(node);
        return node;
    }

    // Returns the node associated with the specified function.
    CallNode* GetNode(Function* function) {
        DebugValidator::IsNotNull(function);
        return GetNode(function->GetReference());
    }

    // Returns the node associated with the parent function
    // of the specified instruction.
    CallNode* GetNode(Instruction* instr) {
        DebugValidator::IsNotNull(instr);
        return GetNode(instr->ParentFunction());
    }

    // Returns the call site associated with the specified call Id.
    CallSite* GetCallSite(int callId, FunctionReference* parentRef) {
        CallSite* callSite = nullptr;

        if(callToCallSite_.TryGetValue(callId, &callSite)) {
            return callSite;
        }

        if(parentRef) {
            auto parentNode = GetNode(parentRef);
            callSite = CallSite::GetCallSite(callId, parentNode);
            
            callSites_.Add(callSite);
            callToCallSite_.Add(callId, callSite);
        }

        return callSite;
    }

    // Returns the call site associated with the specified 'call' instruction.
    CallSite* GetCallSite(CallInstr* instr, FunctionReference* parentRef = nullptr) {
        DebugValidator::IsNotNull(instr);

        if(parentRef == nullptr) {
            parentRef = instr->ParentFunction()->GetReference();
        }

        return GetCallSite(instr->Id(), parentRef);
    }

    // Finds the functions that are not called by any other function,
    // optionally ignoring calls from the External node.
    // 'GetRootNode' can be used afterwards to retrieve the found nodes.
    int FindRoots(bool ignoreCallsFromExtern = false);

    int RootNodeCount() const {
        DebugValidator::IsTrue(rootNodesComputed_);
        return rootNodes_.Count();
    }

    // Returns the root node found at the specified position.
    CallNodeBase* GetRootNode(int index) {
        return rootNodes_[index];
    }

    const CallNodeBase* GetRootNode(int index) const {
        return rootNodes_[index];
    }

    // Performs the specified action on each root node.
    // bool Predicate(CallNodeBase* node)
    template <class Predicate>
    void ForEachRootNode(Predicate action) {
        for(int i = 0; i < rootNodes_.Count(); i++) {
            if(action(rootNodes_[i]) == false) {
                return;
            }
        }
    }

    // Collapses cycles formed between mutually-recursive functions
    // by placing them in a node group, eliminating them.
    // Returns the number of found cycles.
    int CollapseCycles();

    // Performs the specified action on each node that
    // might be called by an external function.
    // bool Predicate(CallNodeBase* node)
    template <class Predicate>
    void ForEachExternalNode(Predicate action) {
        for(int i = 0; i < nodes_.Count(); i++) {
            if(nodes_[i]->IsCalledByExternalFunctions()) {
                if(action(nodes_[i]) == false) {
                    return;
                }
            }
        }
    }

    // Performs the specified action on each node that
    // cannot be called by an external function.
    // bool Predicate(CallNodeBase* node)
    template <class Predicate>
    void ForEachInternalNode(Predicate action) {
        for(int i = 0; i < nodes_.Count(); i++) {
            if(nodes_[i]->IsCalledByExternalFunctions() == false) {
                if(action(nodes_[i]) == false) {
                    return;
                }
            }
        }
    }

    // Performs the specified action on each node that
    // does not call any other function (is a leaf in the call graph).
    // bool Predicate(CallNodeBase* node)
    template <class Predicate>
    void ForEachLeafNode(Predicate action) {
        for(int i = 0; i < nodes_.Count(); i++) {
            if(nodes_[i]->CalledFunctionsCount() == 0) {
                if(action(nodes_[i]) == false) {
                    return;
                }
            }
        }
    }

    // Performs a depth-first search of the call graph
    // starting with the specified node.
    void DepthFirstSearch(CallNodeBase* startNode, 
                          CallNodeVisitor* visitor);

    // Performs a depth-first search of the call graph
    // starting with each of the root nodes.
    bool DepthFirstSearchFromRoots(CallNodeVisitor* visitor);

    // Performs a depth-first search of the call graph
    // starting with the unique root node. If such a node
    // doesn't exist no search is performed and 'false' is returned.
    bool DepthFirstSearchFromUniqueRoot(CallNodeVisitor* visitor);

    // Performs a breadth-first search of the call graph
    // starting with the specified node.
    void BreadthFirstSearch(CallNodeBase* startNode, 
                            CallNodeVisitor* visitor);

    // Performs a breadth-first search of the call graph
    // starting with each of the root nodes.
    bool BreadthFirstSearchFromRoots(CallNodeVisitor* visitor);

    // Performs a breadth-first search of the call graph
    // starting with the unique root node. If such a node
    // doesn't exist no search is performed and 'false' is returned.
    bool BreadthFirstSearchFromUniqueRoot(CallNodeVisitor* visitor);

    // Performs a postorder traversal of the call graph
    // starting with the specified node.
    void PostorderTraversal(CallNodeBase* startNode, 
                            CallNodeVisitor* visitor);

    // Performs a postorder traversal of the call graph
    // starting with each of the root nodes.
    bool PostorderTraversalFromRoots(CallNodeVisitor* visitor);

    // Performs a postorder traversal of the call graph
    // starting with the unique root node. If such a node
    // doesn't exist no search is performed and 'false' is returned.
    bool PostorderTraversalFromUniqueRoot(CallNodeVisitor* visitor);
};

} // namespace Analysis
#endif