// CallGraphBuilder.hpp
// Copyright (c) Lup Gratian
//
// Implements a fast call graph builder that doesn't take function pointers 
// into consideration (calls to the unknown node might appear).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_CALL_GRAPH_BUILDER_HPP
#define PC_ANALYSIS_CALL_GRAPH_BUILDER_HPP

#include "CallGraph.hpp"
#include "ConstantFolder.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Base/Log.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace CompilationPass;

namespace Analysis {

class CallGraphBuilder : public Pass {
protected:
    typedef StaticList<FunctionReference*, 2> FunctionList;

    // Represents a call target that is unknown (call through pointer).
    // If the target is a 'FunctionReference' it represents 
    // a parameter indicated by 'TargetIndex'.
    // Otherwise 'TargetIndex' represents the index into the variable
    // represented by 'Target' (array or record index).
    static const __int64 SINGLE_FUNCTION = -1;
    static const __int64 RETURNED_VALUE = -2;
    static const __int64 NON_CONSTANT_INDEX = -3;

    struct UnknownTarget {
        Operand* Target;     // The unknown target.
        __int64 TargetIndex; // The index associated with the target.

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        UnknownTarget() : Target(nullptr), TargetIndex(0) {}

        UnknownTarget(Operand* target, __int64 targetIndex = 0) :
                Target(target), TargetIndex(targetIndex) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        bool IsParameter() const {
            return Target->IsFunctionReference() && 
                   (TargetIndex >= 0);
        }

        bool IsReturnedValue() const {
            return TargetIndex == RETURNED_VALUE;
        }

        bool IsSingleFunction() const {
            return Target->IsFunctionReference() &&
                   (TargetIndex == SINGLE_FUNCTION);
        }

        bool HasNonConstantIndex() const {
            return TargetIndex == NON_CONSTANT_INDEX;
        }

        unsigned GetHashCode() const {
            // Uses the FNV hash algorithm. 
            unsigned hash = 2166136261;
		    hash = (hash * 16777619) ^ (unsigned)Target;

            if(sizeof(Target) == 8) {
			    // For 64 bit pointers;
			    hash = (hash * 16777619) ^ (unsigned)((__int64)Target >> 32);
		    }

            hash = (hash * 16777619) ^ (unsigned)TargetIndex;
            hash = (hash * 16777619) ^ (unsigned)(TargetIndex >> 32);
            return hash;
        }

        bool operator== (const UnknownTarget& other) const {
            return (Target == other.Target) &&
                   (TargetIndex == other.TargetIndex);
        }

        bool operator!= (const UnknownTarget& other) const {
            return !operator== (other);
        }

        bool operator< (const UnknownTarget& other) const {
            return false; // Required by Dictionary.
        }
    };

    typedef List<UnknownTarget> UnknownTargetList;

    // Represents a 'call' whose target is unknown.
    struct UnknownCall {
        CallInstr* Call;
        StaticList<UnknownTarget, 2> Targets;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        UnknownCall() : Call(nullptr) {}

        UnknownCall(CallInstr* call) : Call(call) {}
    };


    // Represents a binding that takes place between two operands.
    // A binding is used to propagate functions that are found only later.
    struct Binding {
        UnknownTarget Source;
        StaticList<UnknownTarget, 2> Destinations;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        Binding() {}
        
        Binding(UnknownTarget source) : Source(source) {}

        Binding(UnknownTarget source, UnknownTarget dest) : Source(source) {
            Destinations.Add(dest);
        }
    };


    // Represents a special binding for calls with an unknown target.
    // When the targets are finally known the function pointer parameters
    // are propagated to them too.
    struct CallBinding {
        UnknownTarget Source;
        StaticList<__int64, 2> Arguments;
        StaticList<UnknownTarget, 2> Parameters;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        CallBinding() {}
        
        CallBinding(UnknownTarget source, __int64 argument, UnknownTarget parameter) : 
                Source(source) {
            Arguments.Add(argument);
            Parameters.Add(parameter);
        }
    };


    // Marks the function that returns a function pointer which
    // is originates from one or more unmodified parameters.
    // Used to do a simple context sensitive analysis.
    struct ParameterReturn {
        bool ReturnsOnlyParameters; // or FunctionReference...
        StaticList<__int64, 2> ReturnedParameters;
        StaticList<FunctionReference*, 2> ReturnedFunctions;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        ParameterReturn(bool returnsOnlyParams) : 
                ReturnsOnlyParameters(returnsOnlyParams) {}

        ParameterReturn(__int64 parameter, bool returnsOnlyParams = true) : 
                ReturnsOnlyParameters(returnsOnlyParams) {
            ReturnedParameters.Add(parameter);
        }

        ParameterReturn(FunctionReference* functionRef, bool returnsOnlyParams = true) : 
                ReturnsOnlyParameters(returnsOnlyParams) {
            ReturnedFunctions.Add(functionRef);
        }
    };

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    shared<CallGraph> callGraph_;
    List<UnknownCall> unknownCalls_;
    Dictionary<Operand*, bool> unknownTargets_;
    Dictionary<UnknownTarget, Binding> bindings_;
    Dictionary<UnknownTarget, CallBinding> callBindings_;
    Dictionary<UnknownTarget, FunctionList> potentialFuncts_;
    Dictionary<FunctionReference*, ParameterReturn> parameterReturns_;
    ConstantFolder folder_;
    
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Adds the specified callee to the call graph
    // associating it with the specified call site.
    void AddCall(CallSite* callSite, FunctionReference* callee);

    // Tries to solve a call through pointer by constant folding
    // the load and handling 'phi' and 'quest' instructions.
    // If this is not possible a call to the Unknown node is added.
    void HandleCallToUnknown(CallInstr* instr, CallSite* callSite);

    // Performs the actions described above.
    bool AddMultipleTargets(Operand* targetOp, CallSite* callSite);

    // Tries to identify the variables/parameters the specified
    // operand originates from (also looks through indexing
    // and 'phi'/'quest' instructions). If 'targetList' is set
    // the found locations are not added to the data structures,
    // but are added to 'targetList' instead.
    bool IdentifyTargets(Operand* op, UnknownCall* unknownCall, 
                         UnknownTargetList* targetList = nullptr, int level = 3);

    // Tries to identify the variables/parameters the specified
    // operand originates by looking through indexing and 'phi'/'quest' instructions.
    bool IdentifyTargets(Instruction* instr, UnknownCall* unknownCall, 
                         UnknownTargetList* targetList = nullptr, int level = 3);

    // Marks the specified parameter as being returned unmodified.
    void AddReturnedParameter(FunctionReference* functionRef, __int64 parameter);

    // Marks the specified known function as being returned unmodified.
    void AddReturnedFunction(FunctionReference* functionRef, 
                             FunctionReference* returnedFunctionRef);

    // Marks the fact the specified function doesn't return
    // only unmodified parameters or known function references.
    void MarkDoeNotReturnOnlyParameters(FunctionReference* functionRef);

    // Scans the entire unit and collects all pointer variables/parameters
    // targeted by 'store', 'call' and 'ret' instructions.
    // The information is added to the data structures.
    void CollectPotentialTargets(Unit* unit);

    // Tries to collect pointer variables targeted by a 'store' instruction.
    void CollectTargetsFromStore(StoreInstr* instr);

    // Tries to collect pointer parameter targeted by a 'call' instruction.
    void CollectTargetsFromCall(CallInstr* instr);

    // Tries to collect pointer return values targeted by a 'ret' instruction.
    void CollectTargetsFromReturn(ReturnInstr* instr);

    // Associates a known function reference with the parameter
    // found at the specified index of one or more call targets.
    void AddFunctionToParameter(FunctionReference* callTarget, 
                                UnknownTargetList& callTargetList,
                                FunctionReference* functionRefArgument, 
                                __int64 argumentIndex);

    // Associates a series of unknown targets with the parameter
    // found at the specified index of one or more call targets.
    // The values of the unknown targets are determined later.
    void AddFunctionToParameter(FunctionReference* callTarget, 
                                UnknownTargetList& callTargetList,
                                UnknownTargetList& argumentTargetList, 
                                __int64 argumentIndex);

    // Binds the unknown source targets to the destination ones.
    // Any value from an source target is propagated to all the destinations.
    void CreateBindings(UnknownTargetList& destList, UnknownTargetList& sourceList);


    // Binds the unknown source targets to a single destination target.
    void CreateBindings(UnknownTarget& destTarget, UnknownTargetList& sourceList);

    // Binds a single unknown source target to a single destination target.
    // Returns 'true' if the binding didn't already exist.
    bool CreateBinding(UnknownTarget& sourceTarget, UnknownTarget& destTarget);

    // Creates a special call binding that instantiates
    // new binding to the specified parameter when new values
    // for the unknown source target are found.
    void CreateCallBinding(UnknownTarget& sourceTarget, __int64 argument,
                           UnknownTarget& parameterTarget);

    // Processes all collected bindings and call bindings,
    // determining the final values of all unknown targets.
    void ProcessBindings();

    // Processes any call binding associated with the specified unknown target.
    // Returns 'true' if any new binding was created.
    bool ProcessCallBinding(UnknownTarget sourceTarget);

    // Propagates all known function references from the source
    // to all bound destinations.
    bool UnifyFunctionSets(Binding& binding);

    // Adds the specified function reference to all binding destinations.
    bool AddFunctionToDestinationSets(FunctionReference* function, Binding& binding);

    // Adds calls to known functions for calls through pointers
    // using the collected information about where the unknown targets point.
    void CompleteCallGraph();

    // Adds calls to known function taking the context into consideration.
    // Look at the implementation for a detailed example and motivation.
    bool CompleteUsingContext(UnknownTarget returnedTarget, CallInstr* instr, 
                              CallSite* callSite);

    //Adds all functions in the list as functions called at the call site.
    void AddCallsToFoundFunctions(CallSite* callSite, FunctionList& functions);

    // Methods for registering new unknown targets.
    bool AddNewTargets(UnknownTargetList& targets);

    bool AddNewTarget(UnknownTarget target, UnknownCall* unknownCall = nullptr);

    bool AddNewTarget(VariableReference* variableRef, __int64 index, 
                      UnknownCall* unknownCall);

    // Returns 'true' if the specified function can be called 
    // by external functions found outside the translation unit.
    bool CanBeCalledByExternal(FunctionReference* functionRef);

    // Creates the call node for the specified function
    // and links it to all the called functions nodes.
    void CreateCallNode(FunctionReference* functionRef);

    void AddCalledFunctions(Function* funct, CallNode* callerNode);

public:
    shared<CallGraph> BuildCallGraph(Unit* unit);
};

} // namespace Analysis
#endif