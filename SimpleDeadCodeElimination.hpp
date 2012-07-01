// SimpleDeadCodeElimination.hpp
// Copyright (c) Lup Gratian
//
// A simple, but fast pass that removes from a function all instructions
// that are definitely dead. Removes only instructions that have no users.
// The CFG Cleaner pass should be run after it.
// Also performs flow-insensitive dead-store elimination.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_SIMPLE_DEAD_CODE_ELIMINATION_HPP
#define PC_OPTIMIZATION_SIMPLE_DEAD_CODE_ELIMINATION_HPP

#include "../IR/Instructions.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/Temporary.hpp"
#include "../IR/References.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Log.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class SimpleDeadCodeElimination : public Pass {
private:
    // Helpers that store information about (potentially) dead stores.
    struct LoadStoreInfo {
        Instruction* Target;
        StaticList<__int64, 2> IndexList;
        bool HasVariableIndex;
        bool Valid;
    };

    struct StoreCollection {
        bool HasVariableIndex;
        StaticList<LoadStoreInfo, 64> StoreList;
    };

    struct CopySetInfo {
        Variable* Aggregate;
        int References;
        int CopySetReferences;
        StaticList<CallInstr*, 2> CopySetCalls;

        CopySetInfo() {}

        CopySetInfo(Variable* agg) :
                Aggregate(agg), References(0), CopySetReferences(0) {}

        CopySetInfo(const CopySetInfo& other) :
                Aggregate(other.Aggregate), References(other.References),
                CopySetReferences(other.CopySetReferences),
                CopySetCalls(other.CopySetCalls) {}
    };

    typedef Dictionary<Variable*, CopySetInfo> CopySetDict;

    //! TODO: make this a statistic
    int storesRemoved_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Removes all the instructions from the specified block,
	// but without removing the block from the parent function.
	void CleanUnreachableBlock(Block* block);

    // Tries to remove 'store' instruction that are definitely dead
    // (there is no 'load' of the stored value in the whole function).
    void RemoveDeadStores(Function* function);

    // Determines the 'store' instructions that target the specified variable.
    void CollectStoreInstructions(Variable* variable, StoreCollection& stores, 
                                  Function* function);

    // Determines which of the collected stores are (or may be) live.
    void DetermineLiveStores(Variable* variable, StoreCollection& stores, 
                             Function* function);

    // Removes from the list all 'store' instructions that
    // might be live (necessary). Returns 'true' if there is
    // no reason to continue scanning, because all stores are live.
    bool RemoveLiveStoresFromList(LoadStoreInfo& info, StoreCollection& stores);

    // Collects the index values of the address instructions
    // that lead to the specified variable.
    void AddIndexes(Operand* op, LoadStoreInfo& info, Variable* variable, bool isStore);

    // Returns 'true' if the two locations may be the same.
    bool IsSameLocation(LoadStoreInfo& store, LoadStoreInfo& load);

    // Removes all variables that are definitely unused.
    void RemoveDeadVariables(Function* function);

    // Implements a simple algorithm that removed aggregate copy/set
    // operations that are dead because the aggregate itself
    // is not used anywhere in the function.
    void RemoveDeadCopyOperations(Function* function);

    // Checks if this is a call of the 'copyMemory' or 'setMemory' intrinsic
    // updates the information if it's the case.
    bool AnalyzeDeadCopyCall(CallInstr* callInstr, CopySetDict& copySetInfo);

    Variable* GetBaseVariable(Operand* op);

	void InstructionRemoved(Instruction* instr);

public:
	void Execute(Function* function);
};

} // namespace Optimization
#endif