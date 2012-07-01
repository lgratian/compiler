// StoreCombining.cpp
// Copyright (c) Lup Gratian
//
// Implements the Store Combining pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "StoreCombining.hpp"

namespace Optimization {

void StoreCombining::Execute(Function* function) {
    funct_ = function;

    // First we try to convert entire loops to 'setMemory'/'copyMemory' calls.
    // An example are loops of the following form:
    // for(i = 0; i < n; i++) a[i] = 0 - replaced by 'setMemory'
    function->ForEachBlock([this](Block* block) -> bool {
        OptimizeLoopStores(block);
        return true;
    });

    // Try to find stores to adjacent locations and collapse them
    // into a single 'setMemory' call. For example,
    // a[0] = 0; a[1] = 0; or s.a = 0; s.b = 0
    function->ForEachBlock([this](Block* block) -> bool {
        OptimizeStores(block);
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::OptimizeLoopStores(Block* block) {
    // Check if this block is the body of a small loop.
    SimpleLoopInfo loopInfo;

    if(IsSimpleLoop(block, loopInfo) == false) {
        return false;
    }

    // Scan all instructions in the body and collect all the stores
    // which depend on the iteration variable ('i' in the example above).
    StoreList stores;

    if(CollectStores(block, stores, &loopInfo) == 0) {
        return false; // No stores found.
    }

    // Make sure that we're allowed to eliminate the stores.
    if(ValidToEliminateStores(block, stores) == false) {
        return false;
    }

    CreateMemoryOperationsForLoop(loopInfo, stores);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int StoreCombining::CollectStores(Block* block, StoreList& stores,
                                  SimpleLoopInfo* loopInfo) {
    // Scan all instructions in the body and collect all the stores
    // which depend on the iteration variable.
    for(auto instr = block->FirstInstruction(); instr;
        instr = instr->NextInstruction()) {
        if(auto storeInstr = instr->As<StoreInstr>()) {
            StoreInfo loopStore;

            // If the 'store' is volatile we don't touch it.
            if(storeInstr->IsVolatile()) {
                continue;
            }

            if(IsValidStore(storeInstr, loopStore, loopInfo)) {
                stores.Add(loopStore);
            }
        }
    }

    return stores.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::IsValidStore(StoreInstr* instr, StoreInfo& storeInfo,
                                  SimpleLoopInfo* loopInfo) {
    DebugValidator::IsNotNull(instr);

    // Extract the base operand and offset from the 'store'
    // and make sure the non-constant index is the 'phi' result.
    Operand* baseOp = nullptr;
    Operand* nonConstantOp = nullptr;
    bool hasElement = false;
    __int64 offset = 0;

    baseOp = ExtractBaseAndOffset(instr->DestinationOp(), offset, 
                                  &nonConstantOp, &hasElement);
    // If no valid base operand was found give up immediately.
    if(baseOp == nullptr) {
        return false;
    }
    else if(loopInfo) {
        // Make sure that the store is dependent on the incremented value
        // if we're inside a loop.
        if(nonConstantOp != loopInfo->MergedValue) {
            return false;
        }
    }
    else if(nonConstantOp) {
        // The store is dependent on an unknown operand, give up.
        return false;
    }

    // The store is valid, fill in the information we have.
    storeInfo.Store = instr;
    storeInfo.BaseOperand = baseOp;
    storeInfo.Offset = offset;
    storeInfo.Size = GetSize(instr->SourceOp()->GetType());
    storeInfo.Destination = instr->DestinationOp();
    storeInfo.Source = instr->SourceOp();
    storeInfo.HasElement = hasElement;
    storeInfo.IsSet = storeInfo.Source->IsConstant();
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::ValidToEliminateStores(Block* block, StoreList& stores) {
    // Make sure that we're allowed to eliminate the stores.
    // This is not the case if there may be an alias between a store
    // and an instruction that reads/writes memory in the same loop.
    for(auto instr = block->FirstInstruction(); instr;
        instr = instr->NextInstruction()) {

        // Only 'store', 'load' and 'call' instructions can read/write memory.
        if(instr->IsStore() || instr->IsLoad() || instr->IsCall()) {
            for(int i = 0; i < stores.Count(); i++) {
                // If the instruction is exactly one of the 'store's
                // in the list it must be skipped.
                if(stores[i].Store == instr) continue;

                //! HACK HACK HACK
                continue;

                if(MayAliasOperand(instr, stores[i].BaseOperand)) {
                    return false;
                }
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::MayAliasOperand(Instruction* instr, Operand* op) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(op);
    DebugValidator::IsTrue(instr->IsLoad() || instr->IsStore() || instr->IsCall());

    //!TODO: Implement real alias analysis!!!
    if(instr->IsLoad() || instr->IsCall()) {
        return true;
    }

    if(auto variableRef = op->As<VariableReference>()) {
        if(variableRef->IsLocalVariableRef() == false) {
            return true;
        }

        auto variableRef2 = instr->GetSourceOp(0)->As<VariableReference>();

        if(variableRef2 == nullptr || variableRef2->IsLocalVariableRef() == false) {
            return true;
        }

        return variableRef == variableRef2;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StoreCombining::SortStores(StoreList& stores) {
    // Sort the 'store' instructions so that the ones that have the same
    // base operand are adjacent, in order of the offset.
    // Because we usually have only a few 'store' instructions
    // we use selection sort, it's fast enough.
    for(int i = 0; i < stores.Count(); i++) {
        for(int j = 0; j < stores.Count(); j++) {
            // Use the memory address to order the stores
            // if the base operands are not the same.
            if(stores[i].BaseOperand != stores[j].BaseOperand) {
                if(stores[i].BaseOperand < stores[j].BaseOperand) {
                    std::swap(stores[i], stores[j]);
                }

                continue;
            }

            // Base operands the same, order by offset.
            if(stores[i].Offset < stores[j].Offset) {
                std::swap(stores[i], stores[j]);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StoreCombining::CreateMemoryOperationsForLoop(SimpleLoopInfo& loopInfo, 
                                                   StoreList& stores) {
    // We have a list with the stores in the loop and want to replace them
    // by 'setMemory' or 'copyMemory' calls. First sort the list based
    // on the base operand and offset, then we form partitions, 
    // where a partition contains stores that have the same base operand.
    SortStores(stores);

    PartitionList partitions;
    FormStoreParititions(stores, partitions);

    // If we have many partitions it is probably not more efficient
    // to replace the stores in the loop.
    if(partitions.Count() > MAXIMUM_PARTITIONS) {
        return;
    }

    // Process each partition.
    int position = 0;

    for(int i = 0; i < partitions.Count(); i++) {
        bool hasRecord = stores[position].HasElement;
        bool isSet = stores[position].IsSet;

        if((hasRecord == false) && (partitions[i] == 1)) {
            // The partition has a single store to a non-record.
            if(isSet) {
                CreateSetMemoryForStoreLoop(stores[position], loopInfo);
            }
            else CreateCopyMemoryForStoreLoop(stores[position], loopInfo);
        }
        else if(hasRecord) {
            if(isSet) {
                CreateSetMemoryForRecordStoreLoop(stores, position, 
                                                  partitions[i], loopInfo);
            }
            else {
                CreateCopyMemoryForRecordStoreLoop(stores, position, 
                                                   partitions[i], loopInfo);
            }
        }
        else {
            // We don't process partitions with multiple stores
            // that don't involve a record.
        }

        position += partitions[i];
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StoreCombining::FormStoreParititions(StoreList& stores, 
                                          PartitionList& partitions) {
    Operand* lastBaseOp = nullptr;

    for(int i = 0; i < stores.Count(); i++) {
        if(stores[i].BaseOperand == lastBaseOp) {
            // It belongs to the current partition.
            partitions[partitions.Count() - 1]++;
        }
        else {
            // Begin a new partition.
            partitions.Add(1);
            lastBaseOp = stores[i].BaseOperand;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::CreateSetMemoryForStoreLoop(StoreInfo& store, 
                                                 SimpleLoopInfo& loopInfo) {
    // A loop store becomes a 'setMemory' if the stored value is a constant.
    auto constant = store.Source->As<Constant>();
    if(constant == nullptr) {
        return false;
    }

    // The stored constant must have the lowest byte replicated in all bytes.
    if(HasSameByteValue(constant) == false) {
        return false;
    }

    // Create the constant that should be set.
    auto& consts = funct_->ParentUnit()->Constants();
    auto elementSizeOp = consts.GetInt32(GetSize(constant->GetType()));
    
    CreateSetMemoryForLoop(store.BaseOperand, TruncateToByte(constant), 
                           elementSizeOp, loopInfo);

    // The 'store' can now be deleted.
    store.Store->RemoveFromBlock(true /* free */);
    StoresCombined(loopInfo.LoopBody);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::CreateSetMemoryForRecordStoreLoop(StoreList& stores,
                                                       int firstStore, int storeCount, 
                                                       SimpleLoopInfo& loopInfo) {
    // The destination should involve at least one 'elem' instruction.
    // If it does make sure that each record components has
    // an associated 'store' in the loop.
    auto recordType = GetLastRecordType(stores[firstStore].Destination);
    if(recordType == nullptr) {
        return false;
    }
    
    ComponentList components;
    ExtractTypeComponents(recordType, components);

    if(storeCount != components.Count()) {
        return false;
    }

    // Make sure that the same value is stored by all instances
    // and that it is a constant integer with the same byte multiplied.
    auto constant = stores[firstStore].Source->As<Constant>();
    DebugValidator::IsNotNull(constant);
    
    if(HasSameByteValue(constant) &&
       IsValidRecordSetPartition(stores, firstStore, storeCount,
                                 recordType, components, constant) == false) {
        return false;
    }

    // Create the call to 'setMemory'.
    auto& consts = funct_->ParentUnit()->Constants();
    auto elementSizeOp = consts.GetInt32(GetSize(recordType));

    CreateSetMemoryForLoop(stores[firstStore].BaseOperand, 
                           TruncateToByte(constant), 
                           elementSizeOp, loopInfo);

    // Now delete all 'store' instructions in the partition.
    for(int i = firstStore; i < (firstStore + storeCount); i++) {
        stores[i].Store->RemoveFromBlock(true /* free */);
    }

    StoresCombined(loopInfo.LoopBody);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::IsValidRecordSetPartition(StoreList& stores,
                                               int firstStore, int storeCount,
                                               const RecordType* recordType,
                                               ComponentList& components,
                                               Operand* requiredOp) {
    for(int i = firstStore + 1; i < (firstStore + storeCount); i++) {
        if(HaveSameBinaryValue(stores[i].Source, requiredOp) == false) {
            // The stored value is not the same for all stores, give up.
            return false;
        }

        if(stores[i].Offset != (components[i - firstStore].first)) {
            // The offset doesn't match the one of the current field.
            return false;
        }

        if(GetLastRecordType(stores[i].Destination) != recordType) {
            // Although the offset matches, not the same record is involved.
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StoreCombining::CreateSetMemoryForLoop(Operand* destOp, IntConstant* intConst, 
                                            IntConstant* elementSizeOp,
                                            SimpleLoopInfo& loopInfo) {
    // Compute the number of bytes copied. It is equal to the number
    // of loop iterations ('endValue - startValue') multiplied
    // by the received size operand.
    IRGenerator irGen(funct_->ParentUnit());
    Instruction* lastIP = nullptr;

    auto type = loopInfo.EndValue->GetType();
    auto block = GetDestinationBlock(loopInfo);
    auto sizeOp = GenerateLoopSize(loopInfo, irGen, block, lastIP);

    auto finalSizeOp = irGen.GetInt32Temp();
    auto mulInstr = irGen.GetMul(sizeOp, elementSizeOp, finalSizeOp);
    Insert(mulInstr, block, lastIP);
    
    // The base operand needs to be adjusted with the start value
    // multiplied by the constant size. For example, in
    // 'for(int i = 2; i < n; i++) a[i] = 0;' writing starts with byte 4.
    auto adjustmentOp = GenerateAddressAdjustment(loopInfo.StartValue, elementSizeOp, 
                                                  irGen, block, lastIP);

    // Now create the call to the 'setMemory' intrinsic.
    CreateMemoryOperation(destOp, intConst, finalSizeOp, true /* setMemory */,
                          lastIP, nullptr, adjustmentOp);
    UpdateBaseAlignment(destOp, elementSizeOp->Value());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* StoreCombining::GenerateLoopSize(SimpleLoopInfo& loopInfo, IRGenerator& irGen,
                                          Block* block, Instruction*& lastIP) {
    // Compute the number of bytes copied. It is equal to the number
    // of loop iterations ('endValue - startValue').
    auto type = loopInfo.EndValue->GetType();
    auto sizeOp = irGen.GetTemporary(type);
    auto subInstr = irGen.GetSub(loopInfo.EndValue, loopInfo.StartValue, sizeOp);
    Insert(subInstr, block, lastIP);

    if(loopInfo.IsInclusive) {
        auto addOp = irGen.GetTemporary(type);
        auto addInstr = irGen.GetAdd(sizeOp, irGen.GetInt32Const(1), addOp);
        Insert(addInstr, block, lastIP);
        sizeOp = addOp;
    }

    if(sizeOp->IsInt32() == false) {
        // The size operand needs to be extended.
        auto extendedOp = irGen.GetInt32Temp();

        if(loopInfo.IsUnsigned) {
            auto zextInstr = irGen.GetZext(sizeOp, irGen.GetInt32(), extendedOp);
            Insert(zextInstr, block, lastIP);
        }
        else {
            auto sextInstr = irGen.GetSext(sizeOp, irGen.GetInt32(), extendedOp); 
            Insert(sextInstr, block, lastIP);
        }

        sizeOp = extendedOp;
    }

    return sizeOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* StoreCombining::GenerateAddressAdjustment(Operand* adjustmentOp, 
                                                   IntConstant* elementSizeOp,
                                                   IRGenerator& irGen, Block* block,
                                                   Instruction*& lastIP) {
    // Multiply 'adjustmentOp' by 'elementSizeOp'.
    auto mulOp = irGen.GetInt32Temp();
    auto mulInstr = irGen.GetMul(adjustmentOp, elementSizeOp, mulOp);
    Insert(mulInstr, block, lastIP);
    return mulOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::CreateCopyMemoryForStoreLoop(StoreInfo& store, 
                                                  SimpleLoopInfo& loopInfo) {
    // We have a store that doesn't have a constant as a source operand.
    // Check if the source is a value loaded from an expression that
    // depends on the loop iteration variable. For example,
    // 'for(int i = 0; i < n; i++) b[i] = a[i];' can be replaced with a 'copyMemory'.
    auto loadInstr = store.Source->DefiningInstrAs<LoadInstr>();
    if(loadInstr == nullptr) {
        return nullptr;
    }

    // The loaded value should be used only by this 'store',
    // else we can't eliminate the loop later.
    if(loadInstr->ResultOp()->HasSingleUser() == false) {
        return false;
    }

    Operand* sourceBaseOp = nullptr;
    Operand* nonConstantOp = nullptr;
    bool hasElement = false;
    __int64 offset = 0;

    sourceBaseOp = ExtractBaseAndOffset(loadInstr->SourceOp(), offset, 
                                        &nonConstantOp, &hasElement);

    if(sourceBaseOp && (nonConstantOp == loopInfo.MergedValue)) {
        // Generate the 'copyMemory', because the address of the source
        // varies the same like the one of the destination.
        auto& consts = funct_->ParentUnit()->Constants();
        __int64 elementSize = GetSize(store.Source->GetType());

        CreateCopyMemoryForLoop(store.BaseOperand, sourceBaseOp, 
                                consts.GetInt32(elementSize), loopInfo);

        // Now delete the 'store' instruction.
        store.Store->RemoveFromBlock(true /* free */);
        StoresCombined(loopInfo.LoopBody);
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::CreateCopyMemoryForRecordStoreLoop(StoreList& stores,
                                                        int firstStore, int storeCount, 
                                                        SimpleLoopInfo& loopInfo) {
    // We have a partition with multiple stores that target a record.
    // If all the stored values originate from another record having the same type,
    // and all fields are stored, we can replace the stores with 'copyMemory'.
    auto recordType = GetLastRecordType(stores[firstStore].Destination);
    if(recordType == nullptr) {
        return false;
    }
    
    ComponentList components;
    ExtractTypeComponents(recordType, components);

    if(storeCount != components.Count()) {
        return false;
    }

    Operand* lastSourceBaseOp = nullptr;

    // Validate each 'store' in the partition.
    for(int i = firstStore; i < (firstStore + storeCount); i++) {
        Operand* sourceBaseOp = nullptr;
        Operand* nonConstantOp = nullptr;
        __int64 offset = 0;
        bool hasElement = false;

        // The source must be the result of a 'load' instruction
        // that targets the same field, but from another record.
        auto loadInstr = stores[i].Source->DefiningInstrAs<LoadInstr>();
        if(loadInstr == nullptr) {
            return false;
        }

        sourceBaseOp = ExtractBaseAndOffset(loadInstr->SourceOp(), offset, 
                                            &nonConstantOp, &hasElement);

        if(sourceBaseOp && hasElement &&
           (nonConstantOp == loopInfo.MergedValue) &&
           (offset == components[i - firstStore].first)) {
            // The base operand should be the same as the previous one.
            if(lastSourceBaseOp) {
                if(sourceBaseOp != lastSourceBaseOp) {
                    return false;
                }
            }
            else lastSourceBaseOp = sourceBaseOp;

            // The 'load' instruction should target the same record type.
            if(GetLastRecordType(loadInstr->SourceOp()) != recordType) {
                return false;
            }
        }
        else return false;
    }

    // Create the 'copyMemory' call.
    auto& consts = funct_->ParentUnit()->Constants();
    __int64 elementSize = GetSize(recordType);

    CreateCopyMemoryForLoop(stores[firstStore].BaseOperand, lastSourceBaseOp, 
                            consts.GetInt32(elementSize), loopInfo);

    // Now delete all 'store' instructions in the partition.
    for(int i = firstStore; i < (firstStore + storeCount); i++) {
        stores[i].Store->RemoveFromBlock(true /* free */);
    }

    StoresCombined(loopInfo.LoopBody);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StoreCombining::CreateCopyMemoryForLoop(Operand* destOp, Operand* sourceOp, 
                                             IntConstant* elementSizeOp, 
                                             SimpleLoopInfo& loopInfo) {
    // Compute the number of bytes copied. It is equal to the number
    // of loop iterations ('endValue - startValue') multiplied
    // by the received size operand.
    IRGenerator irGen(funct_->ParentUnit());
    Instruction* lastIP = nullptr;

    auto block = GetDestinationBlock(loopInfo);
    auto sizeOp = GenerateLoopSize(loopInfo, irGen, block, lastIP);

    auto finalSizeOp = irGen.GetInt32Temp();
    auto mulInstr = irGen.GetMul(sizeOp, elementSizeOp, finalSizeOp);
    Insert(mulInstr, block, lastIP);
        
    // The base operand needs to be adjusted with the start value
    // multiplied by the constant size. For example, in
    // 'for(int i = 2; i < n; i++) a[i] = b[i];' writing starts with byte 4.
    auto adjustmentOp = GenerateAddressAdjustment(loopInfo.StartValue, 
                                                  elementSizeOp, irGen, 
                                                  block, lastIP);

    // Now create the call to the 'setMemory' intrinsic.
    CreateMemoryOperation(destOp, sourceOp, finalSizeOp, 
                          false /* isSetMemory */, lastIP, 
                          adjustmentOp, adjustmentOp);
    UpdateBaseAlignment(destOp, elementSizeOp->Value());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::OptimizeStores(Block* block) {
    // Collect the 'store' instructions in the block,
    // the partition them based on the targeted base operand.
    StoreList stores;
    
    if(CollectStores(block, stores) == 0) {
        return false;
    }

    if(ValidToEliminateStores(block, stores) == false) {
        return false;
    }

    // Sort the stores by the base operand and offset.
    // A series of consecutive 'store' instructions in the same partitions
    // is converted to 'setMemory' if it may be faster.
    SortStores(stores);

    PartitionList partitions;
    FormStoreParititions(stores, partitions);

    bool status = false;
    int position = 0;

    // Process each partition.
    for(int i = 0; i < partitions.Count(); i++) {
        int partitionLength = partitions[i];
        int rangeFirst = position;
        int rangeLast;
        int lastOffset;
        int lastSize;

        for(int j = 0; j < partitionLength; j++) {
            if(j == 0) {
                rangeLast = rangeFirst;
            }
            // To continue the current range the 'store'
            // needs to start at 'lastOffset + lastSize'.
            // Otherwise we start a new range.
            else if(stores[position].Offset == (lastOffset + lastSize)) {
                rangeLast = position;
            }
            else {
                status |= TryCreateSetMemoryForRange(rangeFirst, rangeLast, stores);
                rangeFirst = position;
                rangeLast = position;
            }

            lastOffset = stores[position].Offset;
            lastSize = stores[position].Size;
            position++;
        }

        // Process the last range.
        status |= TryCreateSetMemoryForRange(rangeFirst, rangeLast, stores);
    }

    return status;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::TryCreateSetMemoryForRange(int rangeFirst, int rangeLast, 
                                                StoreList& stores) {
    DebugValidator::IsLargerOrEqual(rangeLast, rangeFirst);

    // If the range consists of a single 'store' there is 
    // no performance benefit if we convert it.
    if(rangeFirst == rangeLast) {
        return false;
    }

    // We need to make sure that each 'store' has the same source operand,
    // and the operand is a constant with the same byte multiplied.
    Constant* constant;

    if(IsValidStoreRange(rangeFirst, rangeLast, stores, constant)) {
        auto& firstStore = stores[rangeFirst];
        auto& lastStore = stores[rangeLast];

        __int64 size = lastStore.Offset - firstStore.Offset +
                       lastStore.Size;
        auto& consts = funct_->ParentUnit()->Constants();
        auto sizeOp = consts.GetInt32(size);
        auto adjustmentOp = consts.GetInt32(firstStore.Offset);

        CreateMemoryOperation(firstStore.BaseOperand, TruncateToByte(constant),
                              sizeOp, true, lastStore.Store->NextInstruction(),
                              nullptr, adjustmentOp);

        // Remove all stores in the range.
        StoresCombined(stores[rangeFirst].Store->ParentBlock());

        for(int i = rangeFirst; i <= rangeLast; i++) {
            stores[i].Store->RemoveFromBlock(true /* free */);
        }
        
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::IsValidStoreRange(int rangeFirst, int rangeLast, 
                                       StoreList& stores, Constant*& constant) {
    // We need to make sure that each 'store' has the same source operand,
    // and the operand is a constant with the same byte multiplied.
    constant = stores[rangeFirst].Source->As<Constant>();

    if(constant == nullptr || (HasSameByteValue(constant) == false)) {
        return false;
    }

    for(int i = rangeFirst + 1; i < rangeLast; i++) {
        if(HaveSameBinaryValue(stores[i].Source, constant) == false) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StoreCombining::Insert(Instruction* instr, Block* block, Instruction*& lastIP) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(block);

    if(lastIP == nullptr) {
        if(block->InstructionCount() <= 1) {
            block->InsertInstructionFirst(instr);
        }
        else block->InsertInstructionBefore(instr, block->LastInstruction());
    }
    else block->InsertInstructionAfter(instr, lastIP);

    lastIP = instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::HasSameByteValue(Operand* op) {
    DebugValidator::IsNotNull(op);

    // The operand must be an integer constant, floating 0.0 or nullptr.
    auto intConst = op->As<IntConstant>();

    if(intConst == nullptr) {
        if(op->IsNullConstant()) {
            return true;
        }
        else if(auto floatConst = op->As<FloatConstant>()) {
            return floatConst->Value() == 0.0;
        }
        else return false;
    }

    // If the type of the operand is 'int8' the result is obvious.
    int size = intConst->GetType()->Size();
    if(size == 1) return true;

    // Make sure that the lowest byte is repeated in the others.
    unsigned __int64 lowestByte = intConst->Value() & 0xFF;
    unsigned __int64 value = intConst->Value();
    
    for(int i = 1; i < size; i++) {
        unsigned __int64 mask = 0xFFULL << (i * 8);
        unsigned __int64 masked = (value & mask) >> (i * 8);

        if(masked != lowestByte) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* StoreCombining::ExtractBaseAndOffset(Operand* destOp, __int64& offset,
                                              Operand** nonConstantOp, bool* hasElement) {
    // Compute the offset relative to the base operand.
    // This looks through 'index', 'elem' and 'addr' instructions,
    // and stops as soon as the index operand is not a constant.
    offset = 0;

    while(destOp->HasDefiningInstruction()) {
        if(auto indexInstr = destOp->DefiningInstrAs<IndexInstr>()) {
            if(auto intConst = indexInstr->IndexOp()->As<IntConstant>()) {
                offset += GetMultipledSize(indexInstr->GetElementType(), intConst);
                destOp = indexInstr->BaseOp();
            }
            else if(nonConstantOp) {
                *nonConstantOp = indexInstr->IndexOp();
                return indexInstr->BaseOp();
            }
            else return nullptr;
        }
        else if(auto elemInstr = destOp->DefiningInstrAs<ElementInstr>()) {
            // The index is always a constant.
            auto recordType = elemInstr->GetRecordType();
            offset += recordType->GetFieldOffset(elemInstr->GetFieldIndex());
            destOp = elemInstr->BaseOp();

            if(hasElement) *hasElement = true;
        }
        else if(auto addrInstr = destOp->DefiningInstrAs<AddressInstr>()) {
            if(auto intConst = addrInstr->IndexOp()->As<IntConstant>()) {
                offset += GetMultipledSize(addrInstr->GetPointeeType(), intConst);
                destOp = addrInstr->BaseOp();
            }
            else if(nonConstantOp) {
                *nonConstantOp = addrInstr->IndexOp();
                return addrInstr->BaseOp();
            }
            else return nullptr;
        }
        else return nullptr;
    }

    return destOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CallInstr* StoreCombining::CreateMemoryOperation(Operand* destOp, Operand* sourceOp,
                                                 Operand* sizeOp, bool isSetMemory,
                                                 Instruction* insertionPoint,
                                                 Operand* sourceAdjustmentOp,
                                                 Operand* destAdjustmentOp) {
    // Convert the source and destination to 'int8*', 
    // then apply the adjustments if required. 
    IRGenerator irGen(funct_->ParentUnit());

    auto destPtopOp = irGen.GetTemporary(irGen.GetInt8Pointer());
    auto ptopInstr1 = irGen.GetPtop(destOp, irGen.GetInt8Pointer(), destPtopOp);
    Insert(ptopInstr1, insertionPoint->ParentBlock(), insertionPoint);
    destOp = destPtopOp;

    if(isSetMemory == false) {
        auto sourcePtopOp = irGen.GetTemporary(irGen.GetInt8Pointer());
        auto ptopInstr2 = irGen.GetPtop(sourceOp, irGen.GetInt8Pointer(), sourcePtopOp);
        Insert(ptopInstr2, insertionPoint->ParentBlock(), insertionPoint);
        sourceOp = sourcePtopOp;
    }

    if(sourceAdjustmentOp) {
        auto addrOp = irGen.GetTemporary(irGen.GetInt8Pointer());
        auto addrInstr = irGen.GetAddress(sourceOp, sourceAdjustmentOp, addrOp);
        Insert(addrInstr, insertionPoint->ParentBlock(), insertionPoint);
        sourceOp = addrOp;
    }

    if(destAdjustmentOp) {
        auto addrOp = irGen.GetTemporary(irGen.GetInt8Pointer());
        auto addrInstr = irGen.GetAddress(destOp, destAdjustmentOp, addrOp);
        Insert(addrInstr, insertionPoint->ParentBlock(), insertionPoint);
        destOp = addrOp;
    }

    // Create the call to the intrinsic.
    if(isSetMemory) {
        auto callInstr = irGen.GetSetMemoryCall(destOp, sourceOp, sizeOp);
        Insert(callInstr, insertionPoint->ParentBlock(), insertionPoint);
    }
    else { 
        auto callInstr = irGen.GetCopyMemoryCall(destOp, sourceOp, sizeOp);
        Insert(callInstr, insertionPoint->ParentBlock(), insertionPoint);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int StoreCombining::ExtractTypeComponents(const Type* type, ComponentList& components,
                                          __int64 startOffset) {
    int count = 0;

    // Count how many "components" a record/array type has.
    // We consider a components to be a basic type (integer, float, or pointer).
    // If an array/record has a member that is also an aggregate we recourse.
    if(auto recordType = type->As<RecordType>()) {
        for(int i = 0; i < recordType->FieldCount(); i++) {
            auto fieldType = recordType->GetFieldType(i);
            __int64 offset = startOffset + recordType->GetFieldOffset(i);

            if(fieldType->IsArray() || fieldType->IsRecord()) {
                count += ExtractTypeComponents(fieldType, components, offset);
            }
            else {
                components.Add(OffsetSizePair(offset, GetSize(fieldType)));
                count++;
            }
        }
    }
    else if(auto arrayType = type->As<ArrayType>()) {
        auto elementType = arrayType->ElementType();
        bool shouldRecurse = elementType->IsArray() || elementType->IsRecord();
        auto elementSize = GetSize(arrayType->ElementType());

        for(int i = 0; i < arrayType->Size(); i++) {
            __int64 offset = startOffset + (i * elementSize);

            if(shouldRecurse) {
                count += ExtractTypeComponents(elementType, components, offset);
            }
            else {
                components.Add(OffsetSizePair(offset, elementSize));
                count++;
            }
        }
    }

    return count;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const RecordType* StoreCombining::GetLastRecordType(Operand* op) {
    const RecordType* lastRecord = nullptr;

    // Walk the chain of addressing instructions and remember
    // the record type of the last 'elem' instruction seen.
    while(op) {
        if(auto elemInstr = op->DefiningInstrAs<ElementInstr>()) {
            lastRecord = elemInstr->GetRecordType();
            op = elemInstr->BaseOp();
        }
        else if(op->DefiningInstrIs<IndexInstr>() ||
                op->DefiningInstrIs<AddressInstr>()) {
            op = op->DefiningInstruction()->GetSourceOp(0);
        }
        else break;
    }

    return lastRecord;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::IsSimpleLoop(Block* loopBody, SimpleLoopInfo& loopInfo) {
    DebugValidator::IsNotNull(loopBody);

    // We search for a loop that has a single body block.
    // The loop header should have a back-edge from the body
    // and have a single entry point.
    auto gotoInstr = loopBody->BranchInstruction()->As<GotoInstr>();
    if(gotoInstr == nullptr) {
        return false;
    }

    auto loopHeader = gotoInstr->TargetOp()->Target();
    if(loopHeader->PredecessorCount() != 2) {
        return false;
    }

    auto ifInstr = loopHeader->BranchInstruction()->As<IfInstr>();
    if(ifInstr == nullptr) {
        return false;
    }

    // The 'true' target should be the loop body.
    if(ifInstr->TrueTargetOp()->Target() != loopBody) {
        return false;
    }

    loopInfo.LoopBody = loopBody;
    loopInfo.LoopHeader = loopHeader;
    loopInfo.LoopPredecessor = loopHeader->PredecessorAt(0) == loopBody ?
                               loopHeader->PredecessorAt(1) :
                               loopHeader->PredecessorAt(0);
    loopInfo.LoopSuccessor = ifInstr->FalseTargetOp()->Target();

    // Now try to find the (unique) induction variable.
    // We handle only the case that involves a single 'phi'
    // instruction that merges a start value from the loop predecessor
    // with an one increment coming from the loop body.
    // After that we try to compute the last value of the IV.
    return FindInductionVariable(loopInfo) &&
           FindLoopEndValue(loopInfo);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::FindInductionVariable(SimpleLoopInfo& loopInfo) {
    // The loop header should contain a single 'phi' instruction
    // that merges the start and end values.
    auto phiInstr = loopInfo.LoopHeader->FirstInstruction()->As<PhiInstr>();
    
    if((phiInstr == nullptr) || 
        phiInstr->NextInstruction()->Is<PhiInstr>()) {
        return false;
    }

    // If the 'phi' has 'int64' type we give up, because the intrinsics
    // support at most 'int32' and we risk to have overflow.
    if(phiInstr->ResultOp()->GetType()->IsInt64()) {
        return false;
    }

    // Find the incoming value that represents the increment.
    Operand* incremendedValue = nullptr;
    Operand* startValue = nullptr;

    for(int i = 0; i < 2; i++) {
        if(phiInstr->GetOperandBlock(i)->Target() == loopInfo.LoopBody) {
            incremendedValue = phiInstr->GetOperand(i);
        }
        else startValue = phiInstr->GetOperand(i);
    }

    // Validate the increment operation.
    if(incremendedValue == nullptr) {
        return false;
    }

    if((incremendedValue->DefiningInstrIs<AddInstr>() ||
        incremendedValue->DefiningInstrIs<AddressInstr>()) == false) {
        return false;
    }

    // One of the operands should be the 'phi', the other one the constant 1.
    auto incrementedInstr = incremendedValue->DefiningInstruction();

    if((incrementedInstr->ParentBlock() == loopInfo.LoopBody) &&
       (incrementedInstr->GetSourceOp(0) == phiInstr->ResultOp()) &&
        incrementedInstr->GetSourceOp(1)->IsOneInt()) {
        loopInfo.MergedValue = phiInstr->ResultOp();
        loopInfo.IncrementValue = incremendedValue;
        loopInfo.StartValue = startValue;
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::FindLoopEndValue(SimpleLoopInfo& loopInfo) {
    // The condition operand should be a 'cmp' or 'ucmp' instruction
    // with "less than" order and the induction variable as its left operand.
    auto ifInstr = loopInfo.LoopHeader->BranchInstruction()->As<IfInstr>();
    auto cmpInstr = ifInstr->ConditionOp()->DefiningInstrAs<CmpInstrBase>();
    if(cmpInstr == nullptr) {
        return false;
    }

    if(((cmpInstr->IsCmp() || cmpInstr->IsUcmp()) == false) ||
        ((cmpInstr->IsLessOrEqual() || cmpInstr->IsLess()) == false)) {
        return false;
    }

    if(cmpInstr->LeftOp() == loopInfo.MergedValue) {
        loopInfo.EndValue = cmpInstr->RightOp();
        loopInfo.IsInclusive = cmpInstr->IsLessOrEqual();
        loopInfo.IsUnsigned = cmpInstr->IsUcmp();
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* StoreCombining::GetDestinationBlock(SimpleLoopInfo& loopInfo) {
    // Because we might not be sure that the loop executes,
    // we need to create a block which tests the loop condition,
    // and if true branches to a block where the 'setMemory'/'copyMemory'
    // intrinsics are placed, otherwise to the loop.
    Block* destination;

    // Check if we already created the blocks.
    if(destinationBlocks_.TryGetValue(loopInfo.LoopPredecessor, &destination)) {
        return destination;
    }

    // Check if we know that the loop executes at least one time
    // (meaning the start value is definitely smaller than the end value).
    // In this case we don't need to introduce a test.
    if(ExecutesAtLeastOnce(loopInfo) && false) {
        destination = loopInfo.LoopHeader;
    }
    else {
        // Create the block where we insert the loop test.
        destination = CreateDestinationBlock(loopInfo);
    }

    destinationBlocks_.Add(loopInfo.LoopPredecessor, destination);
    return destination;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* StoreCombining::CreateDestinationBlock(SimpleLoopInfo& loopInfo) {
    IRGenerator irGen(funct_->ParentUnit());
    auto testBlock = irGen.GetBlockSymbol(GetBlockName("#sc_test"));
    funct_->InsertBlockBefore(testBlock, loopInfo.LoopHeader);

    // Create the block where the set/copy code will be generated.
    auto destBlock = irGen.GetBlockSymbol(GetBlockName("#sc_block"));
    funct_->InsertBlockBefore(destBlock, loopInfo.LoopHeader);
    destinationBlocks_.Add(loopInfo.LoopPredecessor, destBlock);

    // Create the comparison, then the 'if' that branches to
    // 'destBlock' or to the original loop.
    auto cmpOp = irGen.GetTemporary(loopInfo.StartValue->GetType());
    OrderType order = loopInfo.IsInclusive ? 
                      Order_LessOrEqual : Order_Less;

    if(loopInfo.IsUnsigned) {
        irGen.GetUcmp(order, loopInfo.StartValue, 
                      loopInfo.EndValue, cmpOp, testBlock);
    }
    else irGen.GetCmp(order, loopInfo.StartValue, 
                      loopInfo.EndValue, cmpOp, testBlock);

    irGen.GetIf(cmpOp, irGen.GetBlockRef(destBlock), 
                irGen.GetBlockRef(loopInfo.LoopHeader), testBlock);

    RedirectSuccessors(loopInfo, testBlock, destBlock, irGen);
    return destBlock;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StoreCombining::RedirectSuccessors(SimpleLoopInfo& loopInfo, Block* testBlock, 
                                        Block* destBlock, IRGenerator& irGen) {
    // The loop predecessor no longer jumps directly to the loop header,
    // it must jump to the test block instead.
    loopInfo.LoopPredecessor->ReplaceSuccessor(loopInfo.LoopHeader, testBlock);

    // The operands that were incoming from the loop predecessor
    // must now be incoming from 'testBlock'.
    BlockUtils::ReplacePhiOperandsBlock(loopInfo.LoopHeader, 
                                        loopInfo.LoopPredecessor, testBlock);

    // The destination block must jump to the loop successor
    // (this avoids executing the loop at all).
    irGen.GetGoto(irGen.GetBlockRef(loopInfo.LoopSuccessor), destBlock);

    // If in the loop successor there are incoming operands from the loop
    // header they must be incoming from the destination block too.
    BlockUtils::InsertSameIncoming(loopInfo.LoopSuccessor, 
                                   loopInfo.LoopHeader, destBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::ExecutesAtLeastOnce(SimpleLoopInfo& loopInfo) {
    // Consider a loop having an unsigned start value equal to 0.
    // A "less than" comparison of 0 with a number 'n' will be false
    // only if 'n' is zero, case properly handled by the intrinsics
    // (they do nothing if they have to copy 0 bytes).
    if(loopInfo.StartValue->IsZeroInt() && loopInfo.IsUnsigned) {
        return true;
    }

    // Try to use operand info. Consider the following example:
    // if(n > 8) {
    //     for(i = 0; i < n; i++) a[i] = 0;
    // }
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());
    bool executes = false;

    if(loopInfo.IsInclusive) {
        executes = opInfo.IsSmallerOrEqual(loopInfo.StartValue, loopInfo.EndValue,
                                           loopInfo.IsUnsigned == false,
                                           loopInfo.LoopHeader) == Result_Yes;
    }
    else executes = executes = opInfo.IsSmaller(loopInfo.StartValue, loopInfo.EndValue,
                                                loopInfo.IsUnsigned == false, 
                                                loopInfo.LoopHeader) == Result_Yes;
    return executes;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StoreCombining::UpdateBaseAlignment(Operand* baseOp, __int64 sizeInBytes) {
    auto variableRef = baseOp->As<VariableReference>();
    if((variableRef == nullptr) ||
       (variableRef->IsLocalVariableRef() == false)) {
       return;
    }

    // If the number of set/copies bytes is at least equal to the
    // required alignment for multimedia instructions we align
    // the variable so that theses instructions can be used
    // (on most platforms that is much faster).
    int alignment = GetTarget()->MultimediaAlignment();

    if(sizeInBytes >= alignment) {
        variableRef->GetVariable()->SetAlignment(alignment);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string StoreCombining::GetBlockName(const string& name) {
    int ct = 0;
    string blockName = name;

    while(funct_->Symbols().Contains(&blockName)) {
        blockName = name + string::Format(L"%d", ++ct);
    }

    return blockName;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IntConstant* StoreCombining::TruncateToByte(Constant* constant) {
    auto& consts = funct_->ParentUnit()->Constants();

    if(auto intConst = constant->As<IntConstant>()) {
        return consts.GetInt8(intConst->Value() & 0xFF);
    }
    else {
        // This is a 0.0 or nullptr constant.
        return consts.GetInt8(0);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StoreCombining::HaveSameBinaryValue(Operand* a, Operand* b) {
    // The obvious case first.
    if(a == b) return true;
          
    // Any other valid case expects a binary zero constant.
    return IsZero(a) && IsZero(b);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StoreCombining::StoresCombined(Block* block) {
#if 1
	auto function = block->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	Log::Warning("Stores combined in " + functionName + ":" + blockName);
    //IRPrinter(function).Dump();
#endif
}

} // namespace Optimization