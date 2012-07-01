// MemoryPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements peephole optimizations for 'index', 'elem', 'addr'
// 'load' and 'store' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

Operand* Peephole::HandleAddress(AddressInstr* instr) {
    auto baseOp = instr->BaseOp();
    auto indexOp = instr->IndexOp();
    auto block = instr->ParentBlock();

    // In some cases this can be folded to a constant or to an undefined value.
    if(auto result = folder_.FoldAddress(instr)) {
        return result;
    }

	// addr p, 0 -> p
    if(IsZeroInt(indexOp, block)) {
		return LOG(baseOp);
	}
    
    // Test for a base that is also an 'addr' instruction.
    if(auto addrInstr = baseOp->DefiningInstrAs<AddressInstr>()) {
        return HandleAddrAddr(addrInstr, baseOp, indexOp);
    }

    // Test for an 'addr' that involves an integer converted to a pointer.
	// addr (itop 4, int32*), 2 -> itop 12, int32*
    // This happens rarely, but it's trivial to test for.
	IntConstant* intConst;
    IntConstant* indexConst;
	const Type* pointerType;

	if(MatchConversion<ItopInstr>(MatchIC(&intConst, block),
                                  nullptr, &pointerType)(baseOp) &&
	   (indexConst = AsIntConstant(indexOp, block))) {
		return HandleAddrItop(intConst, pointerType, 
                              baseOp, indexConst);
	}

    // Test for a base that is a pointer conversion. In some cases we can
    // compute the address without the need of the cast.
    if(auto ptopInstr = baseOp->DefiningInstrAs<PtopInstr>()) {
        auto targetOp = ptopInstr->TargetOp();

        if(auto addrInstr = targetOp->DefiningInstrAs<AddressInstr>()) {
            return HandleAddrPtopAddr(ptopInstr, addrInstr, 
                                      baseOp, indexOp);
        }
        else if(auto indexInstr = targetOp->DefiningInstrAs<IndexInstr>()) {
            return HandleAddrPtopIndex(ptopInstr, indexInstr, 
                                       baseOp, indexOp);
        }
        else if(auto elemInstr = targetOp->DefiningInstrAs<ElementInstr>()) {
            return HandleAddrPtopElem(ptopInstr, elemInstr, 
                                      baseOp, indexOp);
        }
        else if(auto arrayType = GetPointedArray(targetOp)) {
            return HandleAddrPtopArray(ptopInstr, arrayType, 
                                       baseOp, indexOp);
        }
        else if(auto recordType = GetPointedRecord(targetOp)) {
            return HandleAddrPtopRecord(ptopInstr, recordType, 
                                        baseOp, indexOp);
        }
    }
    else if(auto indexInstr = baseOp->DefiningInstrAs<IndexInstr>()) {
        return HandleAddrIndex(indexInstr, baseOp, indexOp);
    }
    else if(auto elemInstr = baseOp->DefiningInstrAs<ElementInstr>()){
        return HandleAddrElem(elemInstr, baseOp, indexOp);
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddrAddr(AddressInstr* addrInstr, Operand* baseOp, 
                                  Operand* indexOp) {
    // addr (addr p, C1), C2 -> addr p, C1 + C2
    auto block = addrInstr->ParentBlock();
    auto C1 = AsIntConstant(addrInstr->IndexOp(), block);
    auto C2 = AsIntConstant(indexOp, block);

    if(C1 && C2) {
        // We get rid of one of the 'addr'.
        __int64 sum = IA::Add(C1, C2);
        auto sumOp = irGen_.GetIntConst(C1->GetType(), sum);
        auto addrOp = GetTemporary(addrInstr->BaseOp());

        irGen_.GetAddress(addrInstr->BaseOp(), sumOp, addrOp);
        return LOG(addrOp);
    }

    // addr (addr p, a), b -> addr p, a + b
    // In most cases it's better, because we expose an arithmetic operation.
    // We need to make sure that both index operands have the same type.
    auto indexOp2 = addrInstr->IndexOp();

    if(indexOp2->GetType() != indexOp->GetType()) {
        auto intType = indexOp->GetType()->As<IntegerType>();
        auto intType2 = indexOp2->GetType()->As<IntegerType>();

        // Extend the smaller type.
        if(intType->RankAbove(intType2)) {
            auto zextOp = irGen_.GetTemporary(intType);
            irGen_.GetZext(indexOp2, intType, zextOp);
            indexOp2 = zextOp;
        }
        else {
            auto zextOp = irGen_.GetTemporary(intType2);
            irGen_.GetZext(indexOp, intType2, zextOp);
            indexOp = zextOp;
        }
    }

    auto sumOp = GetTemporary(indexOp);
    irGen_.GetAdd(indexOp, indexOp2, sumOp);

    auto addrOp = GetTemporary(addrInstr->BaseOp());
    irGen_.GetAddress(addrInstr->BaseOp(), sumOp, addrOp);
    return LOG(addrOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddrItop(IntConstant* intConst, const Type* pointerType,
                                  Operand* baseOp, IntConstant* indexOp) {
    // Evaluate the 'addr' instruction by adding the base constant with the
	// the index multiplied by the size of the pointee.
	auto pointeeType = pointerType->As<PointerType>()->PointeeType();
	__int64 pointeeSize = TI::GetSize(pointeeType, GetTarget());
	__int64 baseValue = intConst->Value();
	__int64 index = indexOp->Value();

	__int64 result = baseValue + (index * pointeeSize);
	IntConstant* resultOp = IA::Requires64Bit(result) ? irGen_.GetInt64Const(result) :
														irGen_.GetInt32Const(result);
	auto temp = irGen_.GetTemporary(pointerType);
	irGen_.GetItop(resultOp, pointerType, temp);
	return LOG(temp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddrPtopAddr(PtopInstr* ptopInstr, AddressInstr* addrInstr, 
                                      Operand* baseOp, Operand* indexOp) {
    // addr (ptop (addr p, C1), X*), C2
    auto block = addrInstr->ParentBlock();
    auto C1 = AsIntConstant(addrInstr->IndexOp(), block);
    auto C2 = AsIntConstant(indexOp, block);

    if((C1 && C2) == false) {
        // Both index operands need to be constant.
        return nullptr;
    }

    auto addrType = addrInstr->BaseOp()->GetType()->As<PointerType>();
    auto ptopType = ptopInstr->CastType()->As<PointerType>();

    __int64 addrSize = TI::GetSize(addrType->PointeeType(), GetTarget());
    __int64 ptopSize = TI::GetSize(ptopType->PointeeType(), GetTarget());
    __int64 offset = (C1->Value() * addrSize) + (C2->Value() * ptopSize);
            
    // There are two cases, depending on the size relationship between
    // the type of 'p' and 'X'. Note that for both cases the resulting offset
    // must be a multiple of the smallest type.
    if(addrSize <= ptopSize) {
        // CASE 1: size(p) <= size(X)
        // -> ptop (addr p, C3), X*, where 'C3' is the new offset. Example:
        // char -> int,  ((int*)(p + 3) + 2) -> (int*)(p + 11)
        // Total offset = 3 + 2*4 = 11 -> first apply offset, then convert
        if((offset % addrSize) != 0) {
            // The offset can't be expressed using the inner 'addr'.
            // <ABC> -> <DEF>, size(ABC) = 3, size(def) = 5
            // addr (ptop (addr p, 2), <DEF>*), 2 -> offset = 2*3 + 2*5 = 16,
            // 16 % 3 != 0 -> can't transform!
            // but addr (ptop (addr p, 2), <DEF>*), 3, offset = 2*3 + 3*5 = 21,
            // 21 % 3 == 0 -> can be transformed -> ptop (addr p, 21), <DEF>*
            return nullptr;
        }
                
        offset /= addrSize;
        auto constantOp = irGen_.GetIntConst(C1->GetType(), offset);
        auto addrOp = irGen_.GetTemporary(addrType);
        irGen_.GetAddress(addrInstr->BaseOp(), constantOp, addrOp);

        auto ptopOp = irGen_.GetTemporary(ptopType);
        irGen_.GetPtop(addrOp, ptopType, ptopOp);
        return LOG(ptopOp);
    }
    else {
        // CASE 2: size(p) > size(X)
        // -> addr (ptop p, X*), C3, where 'C3' is the new offset. Example:
        // int - char, ((char*)(p + 3) + 2) -> (char*)p + 14
        // Total offset = 3*4 + 2*1 = 14 -> convert, then apply offset
        if((offset % ptopSize) != 0) {
            // Same reason as above.
            return nullptr;
        }
                
        offset /= ptopSize;
        auto ptopOp = irGen_.GetTemporary(ptopType);
        irGen_.GetPtop(addrInstr->BaseOp(), ptopType, ptopOp);

        auto constantOp = irGen_.GetIntConst(C1->GetType(), offset);
        auto addrOp = irGen_.GetTemporary(ptopType);
        irGen_.GetAddress(ptopOp, constantOp, addrOp);
        return LOG(addrOp);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddrPtopIndex(PtopInstr* ptopInstr, IndexInstr* indexInstr, 
                                       Operand* baseOp, Operand* indexOp) {
    // addr (ptop (index p, C1), X*), C2 -> ptop (index p, C3), X*
    // int a[10]; (char*)&a[2] + 4
    // addr (ptop (index a, 2), int8*), 4 -> ptop (index a, 3), int*8 (&a[3])
    auto block = indexInstr->ParentBlock();
    auto C1 = AsIntConstant(indexInstr->IndexOp(), block);
    auto C2 = AsIntConstant(indexOp, block);

    if((C1 && C2) == false) {
        // Both index operands need to be constant.
        return nullptr;
    }

    // Compute the offset relative to the start of the record.
    auto arrayType = GetPointedArray(indexInstr->BaseOp());
    auto castType = ptopInstr->CastType()->As<PointerType>();

    __int64 elemSize = TI::GetSize(arrayType->ElementType(), GetTarget());
    __int64 castSize = TI::GetSize(castType->PointeeType(), GetTarget());
    __int64 offset = (C1->Value() * elemSize) + (C2->Value() * castSize);

    // See if the offset can address an element of the array (it must be
    // a multiple of the element size). Note that we don't care if the
    // new index is larger than the array size.
    __int64 newElemIndex;

    if(ElementAtOffset(arrayType, offset, newElemIndex) == false) {
        // The offset can't be expressed using just an 'index' instruction.
        return nullptr;
    }

    // Create the 'index' instruction with the new element index.
    auto result = CreateAddressOfElement(indexInstr->BaseOp(), newElemIndex, arrayType);
    return LOG(InsertPtopIfRequired(result, baseOp->GetType()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddrPtopArray(PtopInstr* ptopInstr, const ArrayType* arrayType, 
                                       Operand* baseOp, Operand* indexOp) {
    // addr (ptop p []*, X*), b -> index p, b'.
    // In the following examples we presume 'p' has type '[N int32]'.
    auto block = ptopInstr->ParentBlock();
    bool isMul = false;
    bool isShl = false;
    IntConstant* C;
    Operand* V;

    // 'b' can be one of the following:
    // - a constant: b = C
    //   (char*)p + 4 -> (char*)&p[1]
    //   addr (ptop p, int8*), 4 -> ptop (index p, 1)
    if(auto intConst = AsIntConstant(indexOp, block)) {
        C = intConst;
    }
    // - a variable multiplied by a constant: b = V * C
    //   (char*)p + 4*V -> (char*)&p[V]
    //   addr (ptop p, int8*), (mul 4, V) -> ptop (index p, V), int8*
    //
    //   (char*)p + 8*V -> (char*)&p[V * 2] -> (char*)&p[V << 1]
    //   addr (ptop p, int8*), (mul 4, V) -> ptop (index p, (shl V, 1)), int8*
    else if(auto mulInstr = indexOp->DefiningInstrAs<MulInstr>()) {
        // One of the operands must be a constant.
        V = mulInstr->LeftOp();
        C = AsIntConstant(mulInstr->RightOp(), block);
        if(C == nullptr) return nullptr;
        isMul = true;
    }
    // - a variable shifted left by a constant: b = V << C
    //   (char*)p + V<<2 -> (char*)&p[V]
    //   addr (ptop p, int8*), (shl V, 2) -> ptop (index p, V), int8*
    //
    //   (char*)p + V<<3 -> (char*)&p[V << 1]
    //   addr (ptop p, int8*), (shl V, 3) -> ptop (index p, (shl V, 1)), int8*
    else if(auto shlInstr = indexOp->DefiningInstrAs<ShlInstr>()) {
        // One of the operands must be a constant.
        V = shlInstr->LeftOp();
        C = AsIntConstant(shlInstr->RightOp(), block);

        if(C == nullptr)  {
            return nullptr;
        }

        isShl = true;
    }
    else {
        // Any other instruction as the index is not supported.
        return nullptr;
    }

    // Compute the offset induced by the 'addr' as a multiple of the
    // (potential) variable used in the index. The offset must be a multiple
    // of the element type of the array.
    auto castType = ptopInstr->CastType()->As<PointerType>();
    __int64 pointeeSize = TI::GetSize(castType->PointeeType(), GetTarget());
    __int64 elemSize = TI::GetSize(arrayType->ElementType(), GetTarget());
    Operand* newIndexOp;

    if(isMul) {
        if(C->IsZero() || ((C->Value() % elemSize) != 0)) {
            // The scale factor needs to be a multiple of the element size.
            return nullptr;
        }

        __int64 scale = C->Value() / elemSize;
        newIndexOp = V;

        // If the scale is 1 we don't emit a multiplication anymore.
        if(scale > 1) {
            auto mulOp = GetTemporary(C);
            auto scaleConst = irGen_.GetIntConst(C->GetType(), scale);
            irGen_.GetMul(V, scaleConst, mulOp);
            newIndexOp = mulOp;
        }
    }
    else if(isShl) {
        if(C->IsZero() || (IA::IsPowerOfTwo(elemSize) == false)) {
            // The size of the element should be a power of two
            // to be certain that the transformation is valid.
            return nullptr;
        }

        if(C->Value() < IA::Log2(elemSize)) {
            // The shift amount is too small to be sure that
            // the transformation is valid for all values.
            return nullptr;
        }

        __int64 scale = C->Value() - IA::Log2(elemSize);
        newIndexOp = V;

        if(scale > 0) {
            // The index value must be multiplied by a power of two.
            auto shlOp = GetTemporary(C);
            auto scaleConst = irGen_.GetIntConst(C->GetType(), scale);
            irGen_.GetShl(V, scaleConst, shlOp);
            newIndexOp = shlOp;
        }
    }
    else {
        // The index is an integer constant. It must be a multiple
        // of the element size, and can be negative.
        if((C->Value() % elemSize) != 0) {
            return nullptr;
        }

        __int64 index = C->Value() / elemSize;
        newIndexOp = irGen_.GetIntConst(C->GetType(), index);
    }
            
    // Create the 'index' instruction with the new element index.
    auto ptopType = ptopInstr->CastType()->As<PointerType>();
    auto resultOp = CreateAddressOfElement(ptopInstr->TargetOp(), 
                                           newIndexOp, arrayType);
    return LOG(InsertPtopIfRequired(resultOp, baseOp->GetType()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddrPtopRecord(PtopInstr* ptopInstr, 
                                        const RecordType* recordType, 
                                        Operand* baseOp, Operand* indexOp) {
    // addr (ptop p {}*, X*), C1 -> elem p, C2
    // struct ABC { int a,b,c; }; ABC *p; (char*)p + 4, (char*)p + 2
    // addr (ptop p, int8*), 4 -> ptop (elem p, 1), int32 (&p->b)
    // addr (ptop p, int8*), 2 -> not possible, no field starting at offset 2!
    auto C1 = AsIntConstant(indexOp, ptopInstr->ParentBlock());

    if(C1 == nullptr) {
        return nullptr;
    }

    // Compute the offset relative to the start of the record.
    auto castType = ptopInstr->CastType()->As<PointerType>();
    __int64 pointeeSize = TI::GetSize(castType->PointeeType(), GetTarget());
    __int64 offset = C1->Value() * pointeeSize;

    // Try to promote this to a field selection.
    int newFieldIndex = FieldAtOffset(recordType, offset);

    if(newFieldIndex == -1) {
        // The offset doesn't refer to a valid field.
        return nullptr;
    }

    // Create the 'elem' instruction with the new field index.
    // Depending on which field was selected, the 'ptop' may be needed or not.
    auto elemOp = CreateAddressOfField(ptopInstr->TargetOp(), 
                                       newFieldIndex, recordType);
    return LOG(InsertPtopIfRequired(elemOp, baseOp->GetType()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddrIndex(IndexInstr* indexInstr, Operand* baseOp, 
                                   Operand* indexOp) {
    // addr (index p, C1), C2 -> index p, C1 + C2
    // int a[10]; &a[2] + 1
    // addr (index a, 2), 1 -> index a, 3 (&a[3])
    auto block = indexInstr->ParentBlock();
    auto C1 = AsIntConstant(indexInstr->IndexOp(), block);
    auto C2 = AsIntConstant(indexOp, block);

    if((C1 && C2) == false) {
        // Both index operands need to be constant.
        return nullptr;
    }

    // Replace the two instructions by a single 'index'.
    return LOG(CreateAddressOfElement(indexInstr->BaseOp(), 
                                      C1->Value() + C2->Value(), 
                                      indexInstr->GetArrayType()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddrPtopElem(PtopInstr* ptopInstr, ElementInstr* elemInstr, 
                                      Operand* baseOp, Operand* indexOp) {
    // addr (ptop (elem p, C1), X*), C2 -> ptop (elem p, C3), X*
    // struct ABC { int a,b,c } p; (char*)&p.b + 4
    // addr (ptop (elem p, 1), int8*), 4 -> elem p, 2 ((char*)&p.c)
    auto block = ptopInstr->ParentBlock();
    auto C1 = AsIntConstant(elemInstr->IndexOp(), block);
    auto C2 = AsIntConstant(indexOp, block);

    if((C1 && C2) == false) {
        // Both index operands need to be constant.
        return nullptr;
    }

    // Compute the offset relative to the start of the record.
    auto castType = ptopInstr->CastType()->As<PointerType>();
    auto recordType = GetPointedRecord(elemInstr->BaseOp());
    auto field = recordType->Fields()[C1->Value()];

    __int64 pointeeSize = TI::GetSize(castType->PointeeType(), GetTarget());
    __int64 offset = field.FieldOffset + (C2->Value() * pointeeSize);

    // See if there is a field at that offset. If there is, we can use
    // just the 'elem' instruction with an updated field index.
    int newFieldIndex = FieldAtOffset(recordType, offset);

    if(newFieldIndex == -1) {
        return nullptr; // No valid field found.
    }

    // Create the 'elem' instruction with the new field index.
    // Depending on which field was selected, the 'ptop' may be needed or not.
    auto elemOp = CreateAddressOfField(elemInstr->BaseOp(),
                                       newFieldIndex, recordType);
    return LOG(InsertPtopIfRequired(elemOp, baseOp->GetType()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddrElem(ElementInstr* elemInstr, Operand* baseOp,
                                  Operand* indexOp) {
    // addr (elem p, C1), C2 -> elem p, C3, if 'C3' is a valid field index
    // struct ABC { int a,b,c; }; ABC a; &a.b + 1
    // addr (elem p, 1), 1 -> elem p, 2 (&a.c)
    // We compute the offset generated by both 'C1' and 'C2', and if it's valid,
    // we address the field directly. Note that a 'ptop' is needed if the type
    // of the found field doesn't match the original type.
    auto block = elemInstr->ParentBlock();
    auto C1 = AsIntConstant(elemInstr->IndexOp(), block);
    auto C2 = AsIntConstant(indexOp, block);

    if((C1 && C2) == false) {
        // Both index operands need to be constant.
        return nullptr;
    }

    auto recordType = GetPointedRecord(elemInstr->BaseOp());
    auto field = recordType->Fields()[C1->Value()];

    // Compute the offset relative to the start of the record.
    __int64 fieldSize = TI::GetSize(field.FieldType, GetTarget());
    __int64 offset = field.FieldOffset + (C2->Value() * fieldSize);

    // See if there is a field at that offset. If there is, we can use
    // just the 'elem' instruction with an updated field index.
    int newFieldIndex = FieldAtOffset(recordType, offset);

    if(newFieldIndex == -1) {
        // The offset doesn't refer to a valid field.
        return nullptr;
    }

    // Create the 'elem' instruction with the new field index.
    auto elemOp = CreateAddressOfField(elemInstr->BaseOp(), 
                                       newFieldIndex, recordType);

    // If the new result type doesn't match the original one,
    // we need to insert a 'ptop' instruction. Consider the following example:
    // struct ABC { int a; short b, c, d; } p; &p.a + 1
    // addr (elem p, 0), 2 -> elem p, 3 (&p.d)
    // int32*                 int16* --ptop--> int32*
    return LOG(InsertPtopIfRequired(elemOp, baseOp->GetType()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const RecordType* Peephole::GetPointedRecord(Operand* op) {
    DebugValidator::IsTrue(op->GetType()->IsPointer());
    auto pointerType = op->GetType()->As<PointerType>();
    return pointerType->PointeeType()->As<RecordType>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const ArrayType* Peephole::GetPointedArray(Operand* op) {
    DebugValidator::IsTrue(op->GetType()->IsPointer());
    auto pointerType = op->GetType()->As<PointerType>();
    return pointerType->PointeeType()->As<ArrayType>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Peephole::FieldAtOffset(const RecordType* recordType, __int64 offset) {
    // If the offset is negative, the behavior is undefined.
    if(offset < 0) return -1;

    // If this is a forward declaration of a record 
    // we don't know anything about it.
    if(recordType->FieldCount() == 0) return -1;

    // If the offset is larger than the offset of the las
    // t field it can't be valid.
    auto& fields = recordType->Fields();
    
    if(offset > fields[fields.Count() - 1].FieldOffset) {
        return -1;
    }

    // Test all the fields and check for a matching offset.
    for(int i = 0; i < fields.Count(); i++) {
        if(fields[i].FieldOffset == offset) {
            // Found a matching field.
            return i;
        }
    }

    return -1; // No valid field  was found.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::ElementAtOffset(const ArrayType* arrayType, __int64 offset, 
                               __int64& fieldIndex) {
    __int64 elemSize = TI::GetSize(arrayType->ElementType(), GetTarget());

    // The offset must be a multiple of the element size.
    if((offset % elemSize) == 0) {
		fieldIndex = offset / elemSize;
		return true;
    }
    else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateAddressOfField(Operand* baseOp, int fieldIndex, 
                                        const RecordType* recordType) {
    // Create the 'elem' instruction with the specified field index.
    auto fieldType = recordType->Fields()[fieldIndex].FieldType;
    auto fieldTypePtr = irGen_.GetPointer(fieldType);

    auto elemOp = irGen_.GetTemporary(fieldTypePtr);
    auto constantOp = irGen_.GetInt32Const(fieldIndex);
    irGen_.GetElement(baseOp, constantOp, elemOp);
    return elemOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateAddressOfElement(Operand* baseOp, int index, 
                                          const ArrayType* arrayType) {
    // Create the 'index' instruction with the specified element index.
    auto elemTypePtr = irGen_.GetPointer(arrayType->ElementType());
    auto indexOp = irGen_.GetTemporary(elemTypePtr);
    auto constantOp = irGen_.GetInt32Const(index);
    irGen_.GetIndex(baseOp, constantOp, indexOp);
    return indexOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateAddressOfElement(Operand* baseOp, Operand* indexOp, 
                                          const ArrayType* arrayType) {
    // Create the 'index' instruction with the specified element index.
    auto elemTypePtr = irGen_.GetPointer(arrayType->ElementType());
    auto resultOp = irGen_.GetTemporary(elemTypePtr);
    irGen_.GetIndex(baseOp, indexOp, resultOp);
    return resultOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::InsertPtopIfRequired(Operand* op, const Type* originalType) {
    if(op->GetType() != originalType) {
        auto ptopOp = irGen_.GetTemporary(originalType);
        irGen_.GetPtop(op, originalType, ptopOp);
        return ptopOp;
    }
    else return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleIndex(IndexInstr* instr) {
    auto baseOp = instr->BaseOp();
    auto indexOp = instr->IndexOp();

    // index (ptop p[]*, X[]*), a -> index p, a,
    // This can be done only if we cast to an array of the same type, because
    // [N int32] or [M int32] are both equivalent to the 'index' instruction.
    // struct ABC { int a[10]; } b;
    // int (*p)[4] = &b.a; *p[2]
    // index (ptop (elem a, 0), [4 int32]), 2 -> index (elem a, 0), 2
    if(auto ptopInstr = baseOp->DefiningInstrAs<PtopInstr>()) {
        auto indexArrayType = instr->GetArrayType();
        auto ptopType = ptopInstr->CastType()->As<PointerType>();
        auto ptopArrayType = ptopType->As<ArrayType>();

        // The type of the elements must be the same.
        if(indexArrayType->ElementType() != ptopArrayType->ElementType()) {
            return nullptr;
        }

        // Get rid of the pointer conversion.
        return LOG(CreateAddressOfElement(ptopInstr->TargetOp(), 
                                          indexOp, indexArrayType));
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleElement(ElementInstr* instr) {
    auto baseOp = instr->BaseOp();
    auto indexOp = instr->IndexOp();
    
    // elem (ptop (addr p, C1), X{}), C2 -> elem (ptop p, X{}), C3
    // This can be done only if we find an element at the computed offset.
    // struct ABC { int a,b }; char* p; ((ABC*)(p + 4))->a -> (ABC*)p->b
    // elem (ptop (addr p, 4), <ABC>), 1 -> elem (ptop p, <ABC>), 2
    if(auto ptopInstr = baseOp->DefiningInstrAs<PtopInstr>()) {
        auto addrInstr = ptopInstr->TargetOp()->DefiningInstrAs<AddressInstr>();
        if(addrInstr == nullptr) {
            return nullptr;
        }

        // Both index operands need to be constants.
        auto block = instr->ParentBlock();
        auto C1 = AsIntConstant(addrInstr->IndexOp(), block);
        auto C2 = AsIntConstant(indexOp, block);

        if((C1 && C2) == false) {
            return nullptr;
        }

        // Compute the offset induced by the 'addr', together with the field offset.
        auto addrPointeeType = addrInstr->GetPointeeType();
        auto recordType = instr->GetRecordType();

        __int64 pointeeSize = TI::GetSize(addrPointeeType, GetTarget());
        __int64 offset = (C1->Value() * pointeeSize) + 
                         recordType->Fields()[C2->Value()].FieldOffset;
        int fieldIndex = FieldAtOffset(recordType, offset);

        if(fieldIndex != -1) {
            // We found a valid field we can use. It's possible that a 'ptop' is needed.
            // Consider the following example, where a 'ptop' is required:
            // struct ABC { int a; short b; }; char* p; ((ABC*)(p + 4))->a -> (ABC*)p->b
            // The result should have 'int32*' type, but the 'elem' will have 'int16*'.
            auto ptopOp = GetTemporary(baseOp);
            irGen_.GetPtop(addrInstr->BaseOp(), ptopInstr->CastType(), ptopOp);

            auto elemOp = CreateAddressOfField(ptopOp, fieldIndex, recordType);
            return LOG(InsertPtopIfRequired(elemOp, instr->ResultOp()->GetType()));
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleLoad(LoadInstr* instr) {
    // Don't touch volatile operations.
	if(instr->IsVolatile()) {
        return nullptr;
    }
    
    // Try to fold the 'load', it may be from a constant global.
	if(auto result = folder_.FoldLoad(instr)) {
		return LOG(result);
	}
    
    //! TODO: FORWARDING DISABLED!!!
    return nullptr;

	// Try to sink the 'load' so that it's closer to the instructions that use it.
	// This improves register allocation by reducing the live range.
	// We try to sink as far as possible, even across single-successor,
	// single-predecessor blocks. Function calls and stores may interfere,
	// but in some cases we can prove that they don't.
	auto resultOp = instr->ResultOp();
	auto variableRef = instr->SourceOp()->As<VariableReference>();
	Instruction* currentInstr = instr;
	bool blockChanged = false;

	while(currentInstr) {
		if(blockChanged == false) {
			currentInstr = currentInstr->NextInstruction();
		}

		blockChanged = false;

		// Stop now if the current instruction uses the loaded value.
		if(resultOp->HasUser(currentInstr)) {
            break;
        }
		else if(auto storeInstr = currentInstr->As<StoreInstr>()) {
			// If we store to a different variable we can move the 'load' past it.
			if(auto storeVariableRef = storeInstr->DestinationOp()->As<VariableReference>()) {
				if(variableRef && (storeVariableRef != variableRef)) {
                    continue;
                }
			}

			break;
		}
        else if(currentInstr->IsLoad()) {
            // Don't move it past another 'load' instruction.
            break;
        }
		else if(auto callInstr = currentInstr->As<CallInstr>()) {
			// Calls to most intrinsics don't affect memory.
			if(auto intrinsic = callInstr->GetIntrinsic()) {
				if(intrinsic->IsMathIntrinsic()    || 
                   intrinsic->IsBitwiseIntrinsic() ||
				   intrinsic->IsStackIntrinsic()) {
					// It's safe to move the 'load' past these intrinsics.
					continue;
				}
			}

			break;
		}
        else if(auto phiInstr = currentInstr->As<PhiInstr>()) {
            for(int i = 0; i < phiInstr->OperandCount(); i++) {
                if(phiInstr->GetOperand(i) == resultOp) {
                    // The loaded value is used by the 'phi'.
                    break;
                }
            }
        }
		else if(auto gotoInstr = currentInstr->As<GotoInstr>()) {
			// If the target block has only one predecessor (the block where the
			// 'load' is currently located), we can continue searching there.
			auto targetBlock = gotoInstr->TargetOp()->Target();
			
			if(targetBlock->PredecessorCount() == 1) {
				currentInstr = targetBlock->FirstInstruction();
				blockChanged = true;    
                continue;
			}

            break; // Can't continue anymore.
		}
        else if(currentInstr->IsBranching()) {
            // Any other branching instruction forces us to stop.
            break;
        }
	}

	// Move the 'load' if we could find an insertion point.
    if((currentInstr != instr->NextInstruction()) && 
       (currentInstr->IsPhi() == false)) {
		Log::Info("Forwarding of load");
		instr->RemoveFromBlock();
		currentInstr->ParentBlock()->InsertInstructionBefore(instr, currentInstr);
		return nullptr;
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleStore(StoreInstr* instr) {
    if(instr->DestinationOp()->IsUndefinedConstant()) {
        return nullptr;
    }

    // Don't touch volatile operations.
	if(instr->IsVolatile()) {
        return nullptr;
    }

    // store undef, a -> dead
    if(instr->DestinationOp()->IsUndefinedConstant()) {
        return LOG(instr->DestinationOp());
    }
    
	// Try to move the 'store' outside of the block; this is a simplified version
	// of store forwarding, which should help other optimization reuse
	// the stored value, and also reduces code size.

	// We can forward only it if it's the last instruction (before the 'goto'),
	// or no instruction that depends on it or may modify the stored value
	// is found until the end of the block.
	if(SafeToForwardStore(instr) == false) {
        return nullptr;
    }

	// There are two cases now: the successor block has only one predecessor
	// (the block where the 'store' is located now), or more than one.
	// In the latter case we can continue only if there is an equivalent 'store'
	// in all the predecessors.
	// if(a) *p = 3; else *p = 5; -> *p = PHI(3,5)
	auto gotoInstr = instr->ParentBlock()->BranchInstruction()->As<GotoInstr>();
	auto successorBlock = gotoInstr->TargetOp()->Target();

	if(successorBlock->PredecessorCount() == 1) {
		// Remove the 'store' from the current block and make it the first
		// one in the successor block.
		instr->RemoveFromBlock();
		successorBlock->InsertInstructionFirst(instr);
		Log::Error("Forwarding of store");
		return nullptr;
	}
	else {
        // Check if all predecessors contain a valid 'store'
        // to the same destination operand.
        auto firstBlock = instr->ParentBlock();
        auto addressOp = instr->DestinationOp();
        StaticList<StoreInstr*, 4> availStores;
        availStores.Add(instr);

        successorBlock->ForEachPredecessor([&availStores, addressOp, firstBlock, this]
                                           (Block* block, int i) -> bool {
            if(block == firstBlock) {
                return true;
            }

            // Check if we have a 'store'.
            auto availStore = this->FindAvailableStore(block, addressOp);
            
            if(availStore) {
                availStores.Add(availStore);
                return true;
            }
            else return false; // No reason to continue.
        });

        if(availStores.Count() == successorBlock->PredecessorCount()) {
            // Create a 'phi' that "combines" all stored values,
            // then a store that uses the 'phi'. All previous stores
            // are marked for removal.
            auto resultOp = GetTemporary(instr->SourceOp());
            auto phiInstr = PhiInstr::GetPhi(resultOp, successorBlock->PredecessorCount());
            successorBlock->InsertInstructionFirst(phiInstr);

            // The 'store' must be inserted after all 'phi' instructions.
            auto lastInstr = successorBlock->FirstInstruction();

            while(lastInstr->IsPhi()) {
                lastInstr = lastInstr->NextInstruction();
            }

            // Now create the 'store' in the successor.
            auto newStoreInstr = StoreInstr::GetStore(addressOp, resultOp);
            successorBlock->InsertInstructionBefore(newStoreInstr, lastInstr);

            // Used to signal that a 'store' should be killed.
            auto undefConst = irGen_.GetUndefinedConst(resultOp->GetType());

            // Add the stored values to the 'phi'.
            for(int i = 0; i < availStores.Count(); i++) {
                auto storeInstr = availStores[i];
                phiInstr->AddOperand(storeInstr->SourceOp(),
                                     irGen_.GetBlockRef(storeInstr->ParentBlock()));
                storeInstr->SetDestinationOp(undefConst);
                storeInstr->SetSourceOp(undefConst);
            }

#if 1
            Log::Error("Store forwarded in " + *firstBlock->Name());
#endif            
            return LOG(undefConst);
        }
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::SafeToForwardStore(StoreInstr* instr) {
	// This is definitely safe only if the block ends with a 'goto'.
	auto block = instr->ParentBlock();
	if(block->BranchInstruction()->IsGoto() == false) {
        return false;
    }

    // It's safe if it's the last instruction (before the branch).
	if(block->BranchInstruction()->PreviousInstruction() == instr) {
		return true;
	}
	
	// Arithmetic, logical and comparison instructions don't depend and
	// don't affect the stored value. 'store' and 'load' instructions may
	// affect it, but we may prove that they target a different variable.
	auto variableRef = instr->DestinationOp()->As<VariableReference>();
	Instruction* currentInstr = instr;

	do {
		currentInstr = currentInstr->NextInstruction();

        if(currentInstr == nullptr) {
            break;
        }

		if(currentInstr->IsArithmetic() || 
           currentInstr->IsLogical()    ||
		   currentInstr->IsComparison()) {
            continue;
        }
		else if(auto loadInstr = currentInstr->As<LoadInstr>()) {
			// Don't try to move anything past a volatile 'load', even if
			// the 'store' isn't marked as 'volatile'.
			if(loadInstr->IsVolatile()) {
                return false;
            }

			// If we load from a different variable we can move the 'store' past it.
			if(auto loadVariableRef = loadInstr->SourceOp()->As<VariableReference>()) {
				if(variableRef && (loadVariableRef != variableRef)) {
                    continue;
                }
			}

			return false;
		}
		else if(auto storeInstr = currentInstr->As<StoreInstr>()) {
			// Don't try to move anything past a volatile 'store', even if
			// the 'store' isn't marked as 'volatile'.
			if(storeInstr->IsVolatile()) {
                return false;
            }

			// If we store to a different variable we can move the 'store' past it.
			if(auto storeVariableRef = storeInstr->DestinationOp()->As<VariableReference>()) {
				if(variableRef && (storeVariableRef != variableRef)) {
                    continue;
                }
			}

			return false;
		}
		else if(auto callInstr = currentInstr->As<CallInstr>()) {
			// Calls to most intrinsics don't affect memory.
			if(auto intrinsic = callInstr->GetIntrinsic()) {
				if(intrinsic->IsMathIntrinsic() || intrinsic->IsBitwiseIntrinsic() ||
					intrinsic->IsStackIntrinsic()) {
					// It's safe to move the 'store' past these intrinsics.
					continue;
				}
			}

			return false;
		}
		else if(currentInstr->IsBranching() == false) {
			// We need to presume that all other instructions may affect it somehow.
			return false;
		}
	} while(currentInstr && (currentInstr->IsBranching() == false));

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StoreInstr* Peephole::FindAvailableStore(Block* block, Operand* addressOp) {
    // We scan all instructions in the block backwards, until either
    // we find a 'store' that targets 'addressOp', we reach the end of the block,
    // or we find an instruction that may write to 'addressOp'.
    auto instr = block->BranchInstruction();
    auto variableRef = addressOp->As<VariableReference>();

    while(instr && (instr->IsPhi() == false)) {
        instr = instr->PreviousInstruction();

        if(instr == nullptr) {
            break;
        }

        if(auto storeInstr = instr->As<StoreInstr>()) {
            auto destOp = storeInstr->DestinationOp();

            if(destOp == addressOp) {
                return storeInstr;
            }
            else if(auto destVariableRef = destOp->As<VariableReference>()) {
                if(variableRef && (destVariableRef != variableRef)) {
                    continue;
                }
            }
            
            return nullptr; // May write to 'addressOp'.
        }
        else if(auto loadInstr = instr->As<LoadInstr>()) {
            // Don't move a 'store' past a 'load' if they target the same address.
            if(auto sourceVariableRef = loadInstr->SourceOp()->As<VariableReference>()) {
                if(variableRef && (sourceVariableRef != variableRef)) {
                    continue;
                }
            }
            
            return nullptr;
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            // Calls to most intrinsics don't affect memory.
			if(auto intrinsic = callInstr->GetIntrinsic()) {
				if(intrinsic->IsMathIntrinsic() || intrinsic->IsBitwiseIntrinsic() ||
					intrinsic->IsStackIntrinsic()) {
					// It's safe to move the 'store' past these intrinsics.
					continue;
				}
			}

            return nullptr;
        }
    }

    return nullptr;
}

} // namespace Optimization