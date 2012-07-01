// AddressLowering.hpp
// Copyright (c) Lup Gratian
//
// Implements the AddressLowering pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "AddressLowering.hpp"

namespace Optimization {

Operand* AddressLowering::ComputeAddress(Operand* baseOp, Operand* indexOp, 
                                         __int64 size, Instruction* instr) {
    // Generate the following code:
    // ptopOp = ptop baseOp, int8*
    // offsetOp = mul indexOp, size
    // addrOp = addr ptopOp, offsetOp
    auto ptopOp = irGen_.GetTemporary(irGen_.GetInt8Pointer());
    auto ptopInstr = irGen_.GetPtop(baseOp, irGen_.GetInt8Pointer(), 
                                    ptopOp, nullptr, instr);
    Operand* offsetOp;
    Instruction* mulInstr = nullptr;

    // If 'indexOp' is a constant we generate directly the constant offset.
    if(auto intConst = indexOp->As<IntConstant>()) {
        offsetOp = irGen_.GetIntConst(indexOp->GetType(), intConst->Value() * size);
    }
    else {
        offsetOp = irGen_.GetTemporary(indexOp->GetType());
        auto sizeOp = irGen_.GetIntConst(indexOp->GetType(), size);
        mulInstr = irGen_.GetMul(indexOp, sizeOp, offsetOp, nullptr, ptopInstr);
    }

    // Generate the 'addr' now.
    auto addrOp = irGen_.GetTemporary(irGen_.GetInt8Pointer());
    irGen_.GetAddress(ptopOp, offsetOp, addrOp, nullptr, 
                      mulInstr ? mulInstr : ptopInstr);
    return addrOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* AddressLowering::CastToType(Operand* op, const Type* type, Instruction* instr) {
    // Convert only if it's necessary.
    if(op->GetType() != type) {
        auto ptopOp = irGen_.GetTemporary(type);
        irGen_.GetPtop(op, type, ptopOp, nullptr, instr);
        return ptopOp;
    }
    else return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* AddressLowering::LowerAddress(AddressInstr* instr) {
    // Nothing to do if the pointee has 'int8' type.
    if(instr->GetPointeeType()->IsInt8()) {
        return instr->NextInstruction();
    }

    __int64 pointeeSize = Size(instr->GetPointeeType());
    auto addrOp = ComputeAddress(instr->BaseOp(), instr->IndexOp(), pointeeSize, instr);
    auto castOp = CastToType(addrOp, instr->ResultOp()->GetType(), 
                             addrOp->DefiningInstruction());

    //  Replace all uses with the new result operand and delete the original instruction.
    instr->ResultOp()->ReplaceWith(castOp);
    instr->RemoveFromBlock(true /* free */);
    return castOp->DefiningInstruction();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* AddressLowering::LowerIndex(IndexInstr* instr) {
    __int64 elemSize = Size(instr->GetElementType());
    auto addrOp = ComputeAddress(instr->BaseOp(), instr->IndexOp(), elemSize, instr);
    auto castOp = CastToType(addrOp, instr->ResultOp()->GetType(), 
                             addrOp->DefiningInstruction());

    //  Replace all uses with the new result operand and delete the original instruction.
    instr->ResultOp()->ReplaceWith(castOp);
    instr->RemoveFromBlock(true /* free */);
    return castOp->DefiningInstruction();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* AddressLowering::LowerElement(ElementInstr* instr) {
    // The offset is always a constant in this case.
    __int64 fieldOffset = instr->GetField().FieldOffset;
    auto oneConst = irGen_.GetIntConst(irGen_.GetInt32(), 1);
    auto addrOp = ComputeAddress(instr->BaseOp(), oneConst, fieldOffset, instr);
    auto castOp = CastToType(addrOp, instr->ResultOp()->GetType(), 
                             addrOp->DefiningInstruction());

    //  Replace all uses with the new result operand and delete the original instruction.
    instr->ResultOp()->ReplaceWith(castOp);
    instr->RemoveFromBlock(true /* free */);
    return castOp->DefiningInstruction();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void AddressLowering::Execute(Function* function) {
    irGen_ = IRGenerator(function->ParentUnit());

    // Process each instruction in the function.
    for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
        auto instr = block->FirstInstruction();

        while(instr) {
            if(auto indexInstr = instr->As<IndexInstr>()) {
                instr = LowerIndex(indexInstr);
            }
            else if(auto addrInstr = instr->As<AddressInstr>()) {
                instr = LowerAddress(addrInstr);
            }
            else if(auto elemInstr = instr->As<ElementInstr>()) {
                instr = LowerElement(elemInstr);
            }
            else instr = instr->NextInstruction();
        }
    }
}

} // namespace Optimization