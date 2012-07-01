// ParameterFlags.hpp
// Copyright (c) Lup Gratian
//
// Replaces calls through a pointer with a call/a series of calls
// to known functions, as determined by the call graph information.
// Doing this replacement allows us to inline more functions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_ESCAPED_PARAMETERS_HPP
#define PC_OPTIMIZATION_ESCAPED_PARAMETERS_HPP

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Analysis/CallGraph.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Analysis {

class ParameterFlags : public Pass {
private:
    // Stores information about a tracked operand
    // and about the parameter associated with it.
    struct TrackedOperandInfo {
        Operand* TrackedOperand;
        Parameter* TrackedParameter;
        Variable* ParameterVariable;
        int ParameterIndex;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        TrackedOperandInfo() {}

        TrackedOperandInfo(Operand* op, Parameter* param,
                           Variable* paramVar, int index) :
                TrackedOperand(op), TrackedParameter(param),
                ParameterVariable(paramVar), ParameterIndex(index) {}
    };


    // 
    class EscapedParametersVisitor : public CallNodeVisitor {
    private:
        typedef List<TrackedOperandInfo> TrackedOperandsList;

        ParameterFlags* parent_;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        Function* GetFunctionDefinition(CallNode* node);

        //
        void MakeTrackedParameterList(Function* function, 
                                      TrackedOperandsList& trackedOps);

        //
        void ProcessInstructions(Function* function, 
                                 TrackedOperandsList& trackedOps,
                                 CallGraph* callGraph);

        //
        void ProcessCall(CallInstr* instr, TrackedOperandsList& trackedOps,
                         CallGraph* callGraph);

        //
        void ParameterIsModifiedInTargets(CallInstr* instr, int parameterIndex,
                                          CallGraph* callGraph, bool& escapes,
                                          bool& read, bool& written);

        //
        void NodeGroupHasModifiedPointers(CallNodeGroup* nodeGroup,
                                          bool& escapes, bool& read, bool& written);

        //
        void MarkEscapedParameters(Operand* op, TrackedOperandsList& trackedOps);

        void MarkReadParameters(Operand* op, TrackedOperandsList& trackedOps);

        void MarkWrittenParameters(Operand* op, TrackedOperandsList& trackedOps);

        void MarkReadWrittenParameters(Operand* op, TrackedOperandsList& trackedOps);

        //
        void AddTrackedOperand(Operand* trackedOp, Operand* candidateOp, 
                               TrackedOperandsList& trackedOps);

        void HasModifiedParameters(CallNode* node, bool& hasEscaped, 
                                   bool& hasRead, bool& hasWwritten);

        void MarkAllParameters(CallNode* node, bool escaped, bool read, bool written);

        void Dump(Function* function);

    public:
        EscapedParametersVisitor(ParameterFlags* parent) : parent_(parent) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        virtual void Visit(CallNode* node, CallGraph* callGraph);

        virtual void Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph);
    };

public:
    void Execute(CallGraph* callGraph);
};

} // namespace Analysis
#endif