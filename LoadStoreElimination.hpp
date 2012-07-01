// LocalLoadStoreElimination.hpp
// Copyright (c) Lup Gratian
//
// Implements a very simple, but fast pass that eliminates redundant loads
// and stores from a single basic block at a time.
// As the name implies, this is run only at the basic block level.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_SIMPLE_LOAD_STORE_ELIMINATION_HPP
#define PC_OPTIMIZATION_SIMPLE_LOAD_STORE_ELIMINATION_HPP

#include "../IR/Instructions.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/Temporary.hpp"
#include "../IR/References.hpp"
#include "../Analysis/StdLibRecognizer.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/LanguageInfo.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Log.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class LoadStoreElimination : public Pass, public AliasResultProvider {
private:
    static const int MAX_DEATH_STORE_LEVEL = 3;
    static const int MAX_DEATH_STORE_SUCCESSORS = 2;
    
    Function* funct_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns 'true' if the specified 'call' instruction could have
    // the address operand as an argument or it may return its
    // value into the address operand.
    bool IsAddressUsedByCall(CallInstr* callInstr, Operand* addressOp);

    // Returns 'true' if the specified 'call' instruction may write to memory.
    // This takes into consideration intrinsics and special standard library functions.
    bool CallMayWriteToAddress(CallInstr* callInstr, Operand* addressOp);

    // Returns 'true' if the specified intrinsic may write to memory.
    bool IntrinsicMayWriteToAddress(Intrinsic* intrinsic, CallInstr* callInstr, 
                                    Operand* addressOp);

    // Returns 'true' if the operands are references to the same variables.
    bool AreReferencesEqual(Operand* opA, Operand* opB);

    // Returns 'true' if the operands are references to different memory locations.
    bool AreReferencesDifferent(Operand* opA, Operand* opB);

    // Returns 'true' if the operands are references to different memory
    // locations that represent array/record elements.
    bool AreArrayRecordRefsDifferent(Operand* opA, Operand* opB,
                                     Operand* baseOpA, Operand* baseOpB, 
                                     int level = 8);

    // Returns 'true' if the accessed array elements are definitely different.
    // For example, '&a[i] != &a[i + 1]'.
    bool AreArrayRefsDifferent(AddressInstr* addrInstrA, AddressInstr* addrInstrB, 
                               Operand* baseOpA, Operand* baseOpB,
                               int level);

    // Returns 'true' if the specified operands are pointers (computed by
    // 'addr' instructions) that point to different memory locations.
    bool ArePointersDifferent(Operand* opA, Operand* opB);

    // Returns 'true' if the fields point to different memory locations.
    // This takes into account records that are "unions".
    bool AreFieldsDifferent(ElementInstr* elemInstrA, ElementInstr* elemInstrB,
                            Operand* baseOpA, Operand* baseOpB, int level = 8);

    // Returns the ancestor record.
    ElementInstr* GetAncestor(ElementInstr* elemInstrA, ElementInstr* elemInstrB);

    // Returns 'true' if the operands indicate the same local/global variable,
    // or if it can be proved that the defining instruction compute the same address.
    bool IsSameAddress(Operand* opA, Operand* opB);

    // Scans the basic block backwards trying to find the nearest 'store' or 'load'
    // instruction that writes/reads to/from the specified address. 
    // If no such instruction is found 'nullptr' is returned.
    // 'notFound' is 'true' if a source was not found, and no instruction
    // may interfere with the 'load'.
    Operand* FindNearestSource(Instruction* startInstr, Operand* addressOp,
                               bool& notFound);

    //
    Operand* ExtractFromSetMemory(CallInstr* instr, Operand* addressOp);

    // Returns 'true' if the store may write to the specified address.
    bool MayStoreWriteToAddress(StoreInstr* storeInstr, Operand* addressOp);

    // Returns 'true' if the destination operand is a pointer
    // (multiple levels of 'addr'/'elem'/'index' instructions are allowed).
    bool StoresIntoPointer(Operand* op);

    // Scans the predecessor blocks trying to find the nearest 'store' or 'load'
    // instruction that writes/reads to/from the specified address. 
    Operand* FindNearestSourceGlobal(Instruction* startInstr, Operand* addressOp,
                                     bool checkCurrent, bool& notFound, int level);

    Operand* FindNearestSourceIfThenElse(Block* ifBlock, Block* thenBlock,
                                         Block* elseBlock, Operand* addressOp,
                                         Block* parentBlock, bool& notFound, int level);

    Operand* FindNearestSourceIfThen(Block* ifBlock, Block* thenBlock,
                                     Operand* addressOp, Block* parentBlock, 
                                     bool& notFound, int level);

    // Tries to eliminate a 'load' when it's available in only one of the 
    // branches of an 'if-then-else' by doing PRE.
    Operand* EliminatePartialLoad(Block* ifBlock, Block* availableBlock, 
                                  Block* unavailableBlock, Block* parentBlock, 
                                  Operand* availableOp, Operand* addressOp);

    // Returns 'true' if the specified 'load' instruction can be moved
    // in the block, while maintaining the execution semantics.
    bool IsSafeToSpeculateLoad(LoadInstr* loadInstr, Block* block);

    // "Combines" the specified operand using a 'phi' instruction.
    Operand* CombineSources(Operand* sourceA, Operand* sourceB, 
                            Block* blockA, Block* blockB, Block* parent);

    // Removes the specified instruction, together with all other instructions
    // whose results were used only by this instruction.
    // This does a light form of dead code elimination.
    void Remove(Instruction* instr);

    // Tries to eliminate loads that are obviously redundant.
    void EliminateRedundantLoads(Block* block);

    // Tries to eliminate all redundant loads from the specified function.
    void EliminateRedundantLoads(Function* function);

    // Returns 'true' if the specified 'call' instruction may read from the address.
    // This takes into consideration intrinsics and special standard library functions.
    bool CallMayReadFromAddress(CallInstr* callInstr, Operand* addressOp);

    // Returns 'true' if it is certain that a 'store' to the specified
    // address is dead (will be overwritten before it's reused).
    bool IsStoreDead(Instruction* startInstr, Operand* addressOp, int level = 0);

    bool IsStoreDeadInSuccessors(Block* block, Operand* addressOp, int level);

    // Tries to eliminate dead stores from the specified block.
    void EliminateDeadStores(Block* block);

    // Tries to eliminate dead stores from the specified function.
    void EliminateDeadStores(Function* function);

    // Returns 'true' if the variable (or the array/record instructions that
    // use the variable as a base) has its address definitely not taken.
    bool IsAddressNotTaken(Operand* op, VariableReference** variable = nullptr);

    // Returns the base operand, if this is an 'addr', 'index' or 'elem' instruction.
    Operand* GetBaseOperand(Operand* op);

    bool AreIndexOpsEqual(Operand* opA, Operand* opB);

    Operand* WithoutPointerCasts(Operand* op);

    Operand* ExtractBaseAndOffset(Operand* destOp, __int64& offset);

    __int64 GetSize(const Type* type) {
        return Analysis::TypeInfo::GetSize(type, GetTarget());
    }

    __int64 GetMultipledSize(const Type* type, IntConstant* intConstant) {
        return GetSize(type) * intConstant->Value();
    }

    bool IsSetMemoryOfSize(CallInstr* instr, __int64 setOffset,
                           __int64 loadOffset, __int64 loadSize);

    IntConstant* GetIntConstant(__int64 value, const Type* type) {
        auto& consts = funct_->ParentUnit()->Constants();
        return consts.GetInt(type, value);
    }

    // Methods for logging during debug builds.
    void LogStoreRemoved(StoreInstr* instr);
    void LogLoadRemoved(LoadInstr* instr);

public:
    // Implements the 'AliasResultProvider' interface.
    virtual bool IsDefinitelyNoAlias(Operand* a, Operand* b) {
        return AreReferencesDifferent(a, b);
    }

    void Execute(Function* function);
};

} // namespace Optimization
#endif