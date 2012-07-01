// StoreCombining.cpp
// Copyright (c) Lup Gratian
//
// Tries to combine 'store' instructions that affect nearby locations
// by using the 'setMemory' and 'copyMemory' intrinsics.
// Also handles simple loops that initialize memory with a constant.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_STORE_COMBINING_HPP
#define PC_OPTIMIZATION_STORE_COMBINING_HPP

#include "BlockUtils.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Analysis/IntArithmetic.hpp"
#include "../Analysis/TypeInfo.hpp"
#include "../Analysis/OperandInfo.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/Log.hpp"
#include "../Base/List.hpp"
#include "../Base/Dictionary.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class StoreCombining : public Pass {
private:
    // Holds information about a simple loop.
    struct SimpleLoopInfo {
        Block* LoopHeader;
        Block* LoopBody;
        Block* LoopPredecessor;
        Block* LoopSuccessor;
        Operand* MergedValue;    // The iteration variable.
        Operand* IncrementValue; // The iteration variable incremented in the body.
        Operand* StartValue;     // The start value of the iteration variable.
        Operand* EndValue;       // The end value of the iteration variable.
        bool IsInclusive;        // 'true' if the loop end test is <=, 'false' for <
        bool IsUnsigned;         // 'true' if the loop exit test uses an 'ucmp'.
    };

    // Holds information about a store that is a replacement candidate.
    struct StoreInfo {
        StoreInstr* Store;
        Operand* Destination;
        Operand* Source;
        Operand* BaseOperand; // The targeted operand.
        __int64 Offset;       // Offset relative to the base operand.
        __int64 Size;         // The size of the stored value.
        bool HasElement;      // 'true' if at least one 'elem' instruction is involved.
        bool IsSet;           // 'true' if the stored value is an integer constant.
    };

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    typedef List<StoreInfo> StoreList;
    typedef List<int> PartitionList;
    typedef std::pair<__int64, int> OffsetSizePair;
    typedef List<OffsetSizePair> ComponentList;
    typedef Analysis::TypeInfo TI;

    // The maximum number of store partitions a loop can have.
    // If there are too many there is may be no benefit in using intrinsics.
    static const int MAXIMUM_PARTITIONS = 5;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    Function* funct_;
    Dictionary<Block*, Block*> destinationBlocks_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Tries to combine stores found in the specified block.
    // The stores are partitioned based on their target.
    bool OptimizeStores(Block* block);

    // Tries to create a 'setMemory' call that has the same effect
    // as the stores found between 'rangeFirst' and 'rangeLast'.
    // If possible, the call is created in the parent block of the stores
    // and 'true' is returned.
    bool TryCreateSetMemoryForRange(int rangeFirst, int rangeLast, 
                                    StoreList& stores);

    // Returns 'true' if all 'store' instructions in the specified range
    // are constant that have a single (multiplied) byte value.
    bool IsValidStoreRange(int rangeFirst, int rangeLast, 
                           StoreList& stores, Constant*& constant);

    // Returns 'true' if the specified block is part of a simple loop
    // (only a header and a body block). It stores into 'loopInfo'
    // all information that describes the loop that was found.
    bool IsSimpleLoop(Block* loopBody, SimpleLoopInfo& loopInfo);

    // Tries to find the increment variable of the loop
    // and fills in the information into 'loopInfo'
    // (start value, end value, incremented value).
    bool FindInductionVariable(SimpleLoopInfo& loopInfo);

    // Tries to find the value that represents the loop end
    // (in 'for(i = 0; i < n; i++)' the value would be 'n').
    bool FindLoopEndValue(SimpleLoopInfo& loopInfo);

    // Verifies if the specified 'store' is a candidate for replacement.
    // If it is the found information is stored into 'storeInfo'.
    bool IsValidStore(StoreInstr* instr, StoreInfo& storeInfo,
                      SimpleLoopInfo* loopInfo = nullptr);

    // Tries to extract the base operand of a series of addressing
    // instructions. The offset computed is also extracted.
    // If 'nonConstantOp' is available it is set to the first operand
    // that caused the search to stop. If available, 'hasElement'
    // is set to 'true' if at least one 'elem' instruction is involved.
    Operand* ExtractBaseAndOffset(Operand* destOp, __int64& offset,
                                  Operand** nonConstantOp = nullptr,
                                  bool* hasElement = nullptr);

    // Returns 'true' if the operand consists of a single byte
    // that is "multiplied" (for example, 0 is valid, while 0x00FF is not).
    bool HasSameByteValue(Operand* op);

    // Tries to create replacements for the store
    // found in the loop that has the specified block as the body.
    bool OptimizeLoopStores(Block* block);

    // Adds to 'stores' all 'store' instructions that are valid
    // candidates for replacement. If 'loopInfo' is available
    // then it does further checks for loops.
    int CollectStores(Block* block, StoreList &stores,
                      SimpleLoopInfo* loopInfo = nullptr);

    // Returns 'true' if there is no aliasing between the stores
    // to be eliminated and any other instruction in the block.
    bool ValidToEliminateStores(Block* block, StoreList& stores);

    // Returns 'true' if the specified instruction might alias the operand.
    // This considers both reads and writes from/to the operand.
    bool MayAliasOperand(Instruction* instr, Operand* op);
    
    // Sorts the stores based on their base operand 
    // and on their offset, if the base operands are the same.
    void SortStores(StoreList& stores);

    // Tries to replace the stores found in the loop
    // by the appropriate operations ('setMemory'/'copyMemory').
    void CreateMemoryOperationsForLoop(SimpleLoopInfo& loopInfo, 
                                       StoreList& stores);

    // Partitions the stores so that stores that have the same
    // base operand lay in the same partition.
    // The number of elements in each partition is added to 'partitions'.
    void FormStoreParititions(StoreList& stores, PartitionList& partitions);

    // Tries to replace a single store in a loop with a 'setMemory' call.
    bool CreateSetMemoryForStoreLoop(StoreInfo& store, SimpleLoopInfo& loopInfo);

    // Tries to replace a series of stores that involve records
    // with a 'setMemory' call.
    bool CreateSetMemoryForRecordStoreLoop(StoreList& stores, int firstStore,
                                           int storeCount, SimpleLoopInfo& loopInfo);

    // Returns 'true' if the source values for all stores in the partition
    // match the destination exactly (same offset, same record type).
    bool IsValidRecordSetPartition(StoreList& stores, int firstStore, 
                                   int storeCount, const RecordType* recordType,
                                   ComponentList& components, Operand* requiredOp);

    // Creates a 'setMemory' call that sets memory stored in a loop.
    // Does all necessary computations to compute the final size.
    void CreateSetMemoryForLoop(Operand* destOp, IntConstant* intConst, 
                                IntConstant* elementSizeOp, SimpleLoopInfo& loopInfo);

    // Tries to replace a series of stores that involve records
    // on both the destination and source side with a 'setMemory' call.
    bool CreateCopyMemoryForRecordStoreLoop(StoreList& stores, int firstStore,
                                            int storeCount, SimpleLoopInfo& loopInfo);

    // Tries to replace a single store in a loop with a 'copyMemory' call.
    bool CreateCopyMemoryForStoreLoop(StoreInfo& store, SimpleLoopInfo& loopInfo);

    // Creates a 'copyMemory' call that sets memory stored in a loop.
    // Does all necessary computations to compute the final size.
    void CreateCopyMemoryForLoop(Operand* destOp, Operand* sourceOp, 
                                 IntConstant* elementSizeOp, SimpleLoopInfo& loopInfo);

    // Generates code that computes the number of loop iterations,
    // adjusted according to the information in 'loopInfo'.
    Operand* GenerateLoopSize(SimpleLoopInfo& loopInfo, IRGenerator& irGen,
                              Block* block, Instruction*& lastIP);

    // Generates code that adjusts the base pointer
    // to match the start iteration variable of the loop.
    Operand* GenerateAddressAdjustment(Operand* adjustmentOp, 
                                       IntConstant* elementSizeOp,
                                       IRGenerator& irGen, Block* block, 
                                       Instruction*& lastIP);

    // Determines the components that form the specified type
    // and for each one computes its offset and size.
    // A component is considered to be a basic type (integer, float, or pointer).
    int ExtractTypeComponents(const Type* type, ComponentList& components,
                              __int64 startOffset = 0);

    // Returns the the type of the last record type accessed
    // by the chain of addressing instructions that starts with 'op'.
    const RecordType* GetLastRecordType(Operand* op);

    __int64 GetSize(const Type* type) {
        return Analysis::TypeInfo::GetSize(type, GetTarget());
    }

    __int64 GetMultipledSize(const Type* type, IntConstant* intConstant) {
        return GetSize(type) * intConstant->Value();
    }

    // Creates a 'setMemory' or 'copyMemory' call, optionally
    // applying the specified adjustments to the source/destination operands.
    CallInstr* CreateMemoryOperation(Operand* destOp, Operand* sourceOp,
                                     Operand* sizeOp, bool isSetMemory,
                                     Instruction* insertionPoint,
                                     Operand* sourceAdjustmentOp = nullptr,
                                     Operand* destAdjustmentOp = nullptr);

    // Inserts the specified instruction into the block.
    // If 'lastIP' is null the instruction is inserted before the last
    // instruction in the block, otherwise after 'lastIP'.
    void Insert(Instruction* instr, Block* block, Instruction*& lastIP);

    // Returns the block where the intrinsic calls should be created.
    // If necessary, it creates a new block and inserts it into the function.
    Block* GetDestinationBlock(SimpleLoopInfo& loopInfo);

    // Creates a block where the intrinsic calls can be placed.
    // It also creates a block that tests if the loop
    // executes, and if does it jump to the newly created block.
    Block* CreateDestinationBlock(SimpleLoopInfo& loopInfo);

    // Makes sure the CFG and PHI incoming operands are in
    // a consistent state after the a new destination block was created.
    void RedirectSuccessors(SimpleLoopInfo& loopInfo, Block* testBlock,
                            Block* destBlock, IRGenerator& irGen);

    // Returns 'true' if we statically know that the loop
    // executes at least once. Uses value range information.
    bool ExecutesAtLeastOnce(SimpleLoopInfo& loopInfo);

    // If the base operand is a variable it sets the alignment
    // to a value that allows the use o multimedia instructions
    // to set/copy memory.
    void UpdateBaseAlignment(Operand* baseOp, __int64 sizeInBytes);

    // Returns an integer constant having 'int8' type
    // which contains only the first byte of the specified constant.
    IntConstant* TruncateToByte(Constant* constant);

    // Returns 'true' if the specified operands have the same
    // same binary value (this considers 0 and 0.0 to be the same).
    bool HaveSameBinaryValue(Operand* a, Operand* b);

    bool IsZero(Operand* op) {
        return op->IsZeroInt()      ||
               op->IsNullConstant() ||
               (op->IsFloatingConstant() &&
                op->As<FloatConstant>()->Value() == 0.0);
    }

    string GetBlockName(const string& name);

    void StoresCombined(Block* block);

public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif