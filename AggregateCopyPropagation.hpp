// AggregateCopyPropagation.cpp
// Copyright (c) Lup Gratian
//
// Implements copy propagation for records and arrays.
// Useful especially after inlining, because it makes the inlined code
// use the original record directly, without an unnecessary copy.
// Note that the analysis is not field-sensitive (it means that it doesn't
// track individual components of a record/array; a write to a[1]
// is considered to modify the entire array, for example).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_AGGREGATE_COPY_PROPAGATION_HPP
#define PC_OPTIMIZATION_AGGREGATE_COPY_PROPAGATION_HPP

#include "../Analysis/IntArithmetic.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Analysis/BitVector.hpp"
#include "../Analysis/TypeInfo.hpp"
#include "../Analysis/CFGWalker.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Tag.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/IRPrinter.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/List.hpp"
#include "../Base/String.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/Log.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class AggregateCopyPropagation : public Pass {
private:
    // Stores information about a 'copyMemory' or 'setMemory' 
    // operation which is a candidate for optimization.
    struct CopySetInfo {
        CallInstr* CopySetCall; // The 'copyMemory'/'setMemory' call.
        Operand* Source;        // The original source.
        Operand* Destination;   // The operand where the copy is made.
        int Index;              // The Id of the operation.
        int Replacements;       // The number of places where the source could be used.
        bool IsCopy;            // 'true' for 'copyMemory'.
        bool IsFromConstant;    // 'true' if source is a global constant.

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        CopySetInfo() {}

        CopySetInfo(CallInstr* copySetCall, Operand* dest, Operand* source,
                    bool isCopy, bool isFromConstant, int index) :
                CopySetCall(copySetCall), Destination(dest), Source(source),
                IsCopy(isCopy), IsFromConstant(isFromConstant), 
                Index(index), Replacements(0) {}

        CopySetInfo(const CopySetInfo& other) :
                CopySetCall(other.CopySetCall), Destination(other.Destination),
                Source(other.Source), IsCopy(other.IsCopy), 
                IsFromConstant(other.IsFromConstant), Index(other.Index), 
                Replacements(other.Replacements) {}
    };

    typedef Analysis::TypeInfo TI;
    typedef Dictionary<Operand*, SparseBitVector> TOperandToBitsDict;
    typedef Dictionary<Operand*, int> TOperandToIdDict;
    typedef Dictionary<Operand*, bool> VisitedDict;
    typedef StaticList<CopySetInfo, 32> TCopyInfoList;

    Function* funct_;
    TCopyInfoList infoList_;      // The candidate operations.
    TOperandToBitsDict destToId_; // Maps the copy to the operation.
    Dictionary<Block*, BitVector> copySets_;
    Dictionary<Block*, BitVector> killSets_;
    Dictionary<Block*, BitVector> inSets_;
    Dictionary<Block*, BitVector> outSets_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns the variable that is used by a series of addressing
    // instructions, or 'nullptr' if there is no such variable.
    // 'lastInstr' is set to the last instruction that uses the variable.
    Operand* GetBaseOperand(Operand* op, Instruction** lastInstr = nullptr);

    // Returns 'true' if the instruction might write
    // to the specified operand.
    //! TODO: USE ALIAS ANALYSIS
    bool MightWriteToOperand(Instruction* instr, Operand* op);

    // Adds the copy that might be modified by the specified
    // instruction to the 'kill' set, and removes an available
    // copy that might be modified from the 'copy' set.
    void AddToKillSet(Instruction* instr, BitVector& killSet, BitVector& copySet);

    // Creates an information object from a 'copyMemory'/'setMemory' call.
    // If successful it returns the Id of the information object, else -1.
    int AddInfo(CallInstr* copySetCall, bool isCopy);

    // Tries to extract information from a potential 'copyMemory'/'setMemory' call.
    // If successful it returns the Id of the information object, else -1.
    // 'destOp' is set to the operand that contains the copy.
    int ExtractCopySetInfo(CallInstr* instr, Operand*& destOp);

    // Returns 'true' if the 'copyMemory' has a constant
    // global variable as its source.
    bool HasConstantSource(CallInstr* instr);

    // Strips any 'ptop' instructions from the specified operand.
    Operand* WithoutPointerCasts(Operand* op);

    // Analyzes all call in the block and tries to extract copy
    // information. It does local copy propagation at the same time,
    // and builds the 'copy' and 'kill' sets.
    void CollectCopySetFromBlock(Block* block);

    // Analyzes all blocks in the function in order to find
    // candidate 'copyMemory'/'setMemory' calls.
    void CollectCopySetFromFunction();

    // Initializes the data structures used by the data-flow analysis.
    void InitializeAnalysis();

    // Implements the data-flow analysis algorithm that propagates
    // the available copies through the flow graph.
    void IterateToFixedpoint();

    // Modifies the instructions to use the original operands
    // using information computed by the data-flow analysis 
    // about the available copies at the entry of each block ('in' set).
    void PropagateCopies();

    // Performs local copy propagation in the specified block.
    void LocalCopyPropagation(Block* block, TOperandToIdDict& availableCopies);

    // Replaces operands that represent copies with the original ones.
    void ReplaceWithOriginal(LoadInstr* instr, 
                             TOperandToIdDict& availableCopies,
                             BitVector& killSet);

    bool GetOldestAvailable(Operand* candidateOp,
                            TOperandToIdDict& availableCopies,
                            BitVector& killSet, CopySetInfo& info);

    // Replaces operands that represent copies with the original ones.
    void ReplaceWithOriginal(CallInstr* instr, TOperandToIdDict& availableCopies,
                             BitVector& killSet);

    // Creates an integer constant from information collected
    // from a 'setMemoory' call.
    Operand* CreateConstant(const Type* requiredType, CopySetInfo& info);

    // Tries to remove copy operations that are unnecessary,
    // because the copy is not used anymore in the entire function.
    // This is a flow-insensitive algorithm.
    void RemovedDeadCopySetCalls();

    // Walks the use-def chain for all source operands of the instruction
    // and marks any referenced copy as live. 
    // 'visited' is used to prevent phi-cycles.
    void MarkUsedCopies(Operand* op, List<int>& copyCount, VisitedDict& visited,
                        bool ignoreLocals = false);

    // Methods to set/reset a bit in a BitVector.
    // Make sure that the BitVector is large enough.
    void SetBit(BitVector& vector, int bit);
    void ResetBit(BitVector& vector, int bit);
    
    // Methods for debugging.
    void CopyPropagated(Instruction* instr);

public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif