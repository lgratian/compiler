// GlobalPromotion.hpp
// Copyright (c) Lup Gratian
//
// TODO:
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_GLOBAL_PROMOTION_HPP
#define PC_OPTIMIZATION_GLOBAL_PROMOTION_HPP

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
#include "../Base/StaticList.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class GlobalPromotion : public Pass, protected AliasResultProvider {
private:
    // Stores the indexes of the addressing instructions
    // that compute the address of an aggregate component.
    // index (index agg, 1), 2  ->  1,2
    struct AccessPath {
        StaticList<int, 2> Indices; // The accessed components.

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        bool operator== (const AccessPath& other) const {
            if(Indices.Count() != other.Indices.Count()) {
                return false;
            }

            for(int i = 0; i < Indices.Count(); i++) {
                if(Indices[i] != other.Indices[i]) {
                    return false;
                }
            }

            return true;
        }

        bool operator< (const AccessPath& other) const {
            // Compare the common indexes.
            int count = std::min(Indices.Count(), other.Indices.Count());

            for(int i = 0; i < count; i++) {
                if(Indices[i] < other.Indices[i]) {
                    return true;
                }
                else if(Indices[i] > other.Indices[i]) {
                    return false;
                }
            }

            return Indices.Count() < other.Indices.Count();
        }
    };


    // Create a pair formed from an access path 
    // and a list of associated constants assigned to the path.
    struct PathConstantsPair {
        AccessPath Path;
        StaticList<Constant*, 2> Constants;

        PathConstantsPair() {}

        PathConstantsPair(AccessPath path) : Path(path) {}
    };


    // Stores information collected about a global symbol.
    struct GlobalInfo {
        Reference* TrackedReference;
        bool IsExternal;
        bool HasRead;
        bool HasWrite;
        bool IsAddressTaken;
        bool HasNonConstantWrite;
        bool HasUnknownPositionWrite;
        bool HasUnknownPositionRead;
        int SmallestReadPostorder;
        int LargestWritePostorder;
        SparseBitVector ReadPositions;
        List<PathConstantsPair> Initializers;
        List<FunctionReference*> Users;

        GlobalInfo();

        GlobalInfo(Reference* trackedRef) : 
                TrackedReference(trackedRef), IsExternal(false), HasRead(false),
                HasWrite(false), IsAddressTaken(false), HasNonConstantWrite(false),
                HasUnknownPositionWrite(false), HasUnknownPositionRead(false),
                SmallestReadPostorder(std::numeric_limits<int>::max()),
                LargestWritePostorder(std::numeric_limits<int>::min()) {}
    };


    typedef Dictionary<FunctionReference*, int> FunctionPostorderDict;

    // 
    class PostorderVisitor : public CallNodeVisitor {
    private:
        int postorderNumb_;
        FunctionPostorderDict* postorderNumbs_;

    public:
        PostorderVisitor(FunctionPostorderDict* postorderNumbs) : 
            postorderNumbs_(postorderNumbs), postorderNumb_(0) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        virtual void Visit(CallNode* node, CallGraph* callGraph);

        virtual void Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph);
    };


    typedef StaticList<GlobalInfo*, 2> GlobalInfoList;
    typedef Dictionary<Reference*, GlobalInfo*> GlobalInfoDict;
    typedef Dictionary<Operand*, GlobalInfoList> TrackedReferencesDict;
    typedef List<Constant*> ConstantList;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - -
    Unit* unit_;
    GlobalInfoDict globalInfo_;
    FunctionPostorderDict postorderNumbs_;
    CallGraph* callGraph_;
    
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - -
    void ComputePostorderNumbers();

    GlobalInfo* GetGlobalInfo(Reference* reference);

    bool GetGlobalInfo(Operand* op, TrackedReferencesDict& trackedRefs,
                       GlobalInfoList& foundInfos);

    Reference* AsGlobalReference(Operand* op);

    void AddUser(GlobalInfo* globalInfo, FunctionReference* functionRef) {
        if(globalInfo->Users.Contains(functionRef) == false) {
            globalInfo->Users.Add(functionRef);
        }
    }

    void AddUser(GlobalInfo* globalInfo, Instruction* instr) {
        AddUser(globalInfo, instr->ParentFunction()->GetReference());
    }

    void UpdateReadPostorder(GlobalInfo* globalInfo, Instruction* instr) {
        globalInfo->SmallestReadPostorder = 
                std::min(globalInfo->SmallestReadPostorder,
                postorderNumbs_[instr->ParentFunction()->GetReference()]);
    }

    void UpdateWritePostorder(GlobalInfo* globalInfo, Instruction* instr) {
        globalInfo->LargestWritePostorder = 
                std::max(globalInfo->LargestWritePostorder,
                         postorderNumbs_[instr->ParentFunction()->GetReference()]);
    }

    void TrackResultOperand(Operand* resultOp, Operand* sourceOp,
                            TrackedReferencesDict& trackedRefs);

    void ProcessInstructions(Function* function);

    void ProcessStore(StoreInstr* instr, TrackedReferencesDict& trackedRefs);

    void ProcessLoad(LoadInstr* instr, TrackedReferencesDict& trackedRefs);

    void ProcessReturn(ReturnInstr* instr, TrackedReferencesDict& trackedRefs);

    void ProcessCall(CallInstr* instr, TrackedReferencesDict& trackedRefs);

    void ProcessCompare(CmpInstrBase* instr, TrackedReferencesDict& trackedRefs);

    void ProcessPhi(PhiInstr* instr, TrackedReferencesDict& trackedRefs);

    void ProcessQuestion(QuestionInstr* instr, TrackedReferencesDict& trackedRefs);

    bool FindConstants(Operand* op, ConstantList& constants,
                       bool& hasOnlyConstants);

    void AddConstant(Constant* constant, ConstantList& constants) {
        if(constants.Contains(constant) == false) {
            constants.Add(constant);
        }
    }

    bool FindAccessPath(Operand* op, AccessPath& path, Reference* requiredBase);

    void AddInitializers(GlobalInfo* globalInfo, AccessPath& path, 
                         ConstantList& constants);

    void DetermineSideEffects(CallSite* callSite, CallInstr* instr, 
                              GlobalInfoList& argumentGlobals, int parameterIndex, 
                              bool& isRead, bool& isWritten, bool& isEscaped);

    bool DetermineSideEffectsExternal(CallSite* callSite, CallInstr* instr, 
                                      GlobalInfoList& argumentGlobals, 
                                      int parameterIndex, bool& isRead, 
                                      bool& isWritten, bool& isEscaped);

    void DetermineSetMemorySideEffects(CallInstr* instr, GlobalInfoList& globals, 
                                       int parameterIndex, bool& isRead, 
                                       bool& isWritten, bool& isEscaped);

    void DetermineCopyMemorySideEffects(CallInstr* instr, GlobalInfoList& globals, 
                                        int parameterIndex, bool& isRead, 
                                        bool& isWritten, bool& isEscaped);

    void DetermineGroupSideEffects(CallNodeGroup* nodeGroup, bool& isRead, 
                                   bool& isWritten, bool& isEscaped);

    void MarkAddressTaken(GlobalInfo* globalInfo);

    void MarkPassedToUnknown(GlobalInfo* globalInfo);

    void MarkArgumentGlobal(GlobalInfo* globalInfo, bool isRead, 
                            bool isWritten, bool isEscaped);

    void PromoteFunctionsToStatic();

    void PromoteFunctionToStatic(CallNode* callNode);

    void PromoteVariablesToConstants();

    void PromoteVariableToConstant(GlobalVariable* globalVariable, 
                                   GlobalInfo* globalInfo);

    bool IsStaticFunction(FunctionReference* functionRef, GlobalInfo* globalInfo) {
        return functionRef->IsStatic() &&
               ((globalInfo == nullptr) || (globalInfo->IsExternal == false));
    }

    void Dump();

    // create initializer
    // remove calls from external in call graph

    // Methods that implement the AliasResultProvider interface.
    virtual bool MightBeAlias(Operand* a, Operand* b);
    virtual bool IsDefinitelyNoAlias(Operand* a, Operand* b);

public:
    void Execute(Unit* unit, CallGraph* callGraph);
};

} // namespace Optimization
#endif


// Tries to promote variables to constants
// var a [3 int32] = {1,2,3} can be promoted to a constant if never written to.
// promote extern to static (both var and function)
// mark var as const

// struct GlobalInfo { hasread, haswrite, addrtaken, initializers list 
//                    (accesspath to work with matrix?) }
// try to find initializers
// if variable and more init - attach all somehow, or as range
// take AddTaken code from VariableAnalysis

// at end of scan try to mark const, add init, etc.

// mark pointer variables as escaped (can help to not mark extern)
// use bitvector associated with each function
// process as postorder