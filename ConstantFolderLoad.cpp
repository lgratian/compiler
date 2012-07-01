// ConstantFolderLoad.cpp
// Copyright (c) Lup Gratian
//
// Implements the constant folder for 'load' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConstantFolder.hpp"

namespace Analysis {

Operand* ConstantFolder::HandleLoad(Operand* sourceOp) {
	// Handle cases when we know the result is undefined.
	// load undef -> undef
	// load nullptr -> undef
	if(sourceOp->IsUndefinedConstant()) {
		return sourceOp;
	}
	else if(sourceOp->IsNullConstant()) {
		return GetUndefined(sourceOp);
	}

	// Handle first the simple cases when we load directly from a global variable.
	auto loadType = sourceOp->GetType()->As<PointerType>()->PointeeType();

	if(auto variableRef = sourceOp->IsVariableReference()) {
		return LoadFromGlobal(sourceOp, loadType, 0 /* start offset */);
	}

	// Try to load from an 'addr', 'index' or 'elem' instruction that index into
	// a global variable (for example, 'a[i]', 'a.b', 'a.c[i].d', '*(p + 2)', etc.).
	return LoadFromAddress(sourceOp, loadType, 0 /* start offset */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::LoadFromAddress(Operand* op, const Type* loadType, 
                                         __int64 offset) {
	// If the operand has a defining instruction then we try to compute
	// the offset from which we should load. If it's a variable reference
	// we try to load from the offset that was already computed.
	if(op->HasDefiningInstruction() == false) {
		return LoadFromGlobal(op, loadType, offset);
	}

	auto instr = op->DefiningInstruction();

	switch(instr->GetOpcode()) {
		case Instr_Index: {
			// If the index operand is not a constant give up.
			auto indexInstr = instr->As<IndexInstr>();
			auto indexConst = indexInstr->IndexOp()->As<IntConstant>();
			
            if(indexConst == nullptr) {
                return nullptr;
            }

			// The type of the base is 'pointer-to-array', so we need to strip the pointer.
			auto elementType = indexInstr->GetElementType();
			__int64 index = indexConst->Value();
			__int64 elemSize = TypeInfo::GetSize(elementType, target_);

			// The offset is incremented by the index multiplied with the element size.
			__int64 newOffset = offset + (index * elemSize);
			return LoadFromAddress(indexInstr->BaseOp(), loadType, newOffset);
		}
		case Instr_Element: {
			auto elemInstr = instr->As<ElementInstr>();
			__int64 index = elemInstr->GetFieldIndex();

			// The type of the base is 'pointer-to-record', 
            // so we need to strip the pointer.
			auto recordType = elemInstr->GetRecordType();

			// Obtain the offset of the selected field.
			// The new offset is the old one added with the field offset.
			__int64 fieldOffset = recordType->Fields()[index].FieldOffset;
			__int64 newOffset = offset + fieldOffset;
			return LoadFromAddress(elemInstr->BaseOp(), loadType, newOffset);
		}
		case Instr_Address: {
			// If the index operand is not a constant give up.
			auto addrInstr = instr->As<AddressInstr>();
			auto indexConst = addrInstr->IndexOp()->As<IntConstant>();

			if(indexConst == nullptr) {
                return nullptr;
            }

			// The type of the base is 'pointer-to-object',
            // so we need to strip the pointer.
			auto objectType = addrInstr->GetPointeeType();
			__int64 index = indexConst->Value();
			__int64 elemSize = TypeInfo::GetSize(objectType, target_);

			// The offset is incremented by the index multiplied with the object size.
			__int64 newOffset = offset + (index * elemSize);
			return LoadFromAddress(addrInstr->BaseOp(), loadType, newOffset);
		}
		case Instr_Ptop: {
			// This instruction is ignored (the previous recursion step
			// has already taken care about its effects).
			auto ptopInstr = instr->As<PtopInstr>();
			auto targetInstr = ptopInstr->TargetOp()->DefiningInstruction();
			return LoadFromAddress(ptopInstr->TargetOp(), loadType, offset);
		}
		case Instr_Load: {
			// This happens when the variable is a pointer.
			auto loadInstr = instr->As<LoadInstr>();
			return LoadFromAddress(loadInstr->SourceOp(), loadType, offset);
		}
		default: {
			// All other cases don't lead to a constant operand.
			return nullptr;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::LoadFromGlobal(Operand* op, const Type* loadType, 
                                        __int64 offset) {
	// If the operand is not a reference to a global variable then we can't do anything,
	// even if the variable may be a constant.
	auto variableRef = op->As<VariableReference>();
	
    if(variableRef == nullptr) {
        return nullptr;
    }

	auto globalVar = variableRef->GetGlobal();

	if(globalVar == nullptr) {
        return nullptr;
    }

	// It's a global variable, now make sure that it's a constant
	// with an initializer that is not a tentative definition.
	if((globalVar->HasInitializer() == false) ||
	   (globalVar->IsConstant() == false)     ||
	    globalVar->IsTentative()) {
        return nullptr;
    }

	// If the offset is negative or larger than the size of the type we give up 
	// (some bytes may be available, but it's not worth the effort trying to extract them).
	if(IsValidOffset(globalVar, loadType, offset) == false) {
		return nullptr;
	}

	// If this is a simple value load it now; this is the common case.
	// Note that we can't load the value if it originates from something like
	// 'int a = (int)&a;', because it's not a compile-time constant.
	// An exception is when we the converted operand is 0 or a null pointer.
    if(globalVar->HasZeroInitializer()) {
		// The variable is initialized only with zeros, so the offset
		// doesn't matter as long as it's valid (and we checked that above).
		__int64 data = 0;
		return GetOperandHavingData((unsigned char*)&data, 8, loadType);
	}

	Initializer* initializer = globalVar->GetInitializer();
    DebugValidator::IsNotNull(initializer);

	if(initializer->IsInitializerList() == false) {
		return LoadFromInitializer(initializer, loadType, offset);
	}

	// We have an initializer list, try to compute the index 
	// of the initializer from which we should load.
	return LoadFromOffset(initializer, globalVar->GetType(), 
                          loadType, offset);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::IsValidOffset(GlobalVariable* globalVar, const Type* loadType,
								   __int64 offset) {
	__int64 loadSize = TypeInfo::GetSize(loadType, target_);
	__int64 initSize = TypeInfo::GetSize(globalVar->GetType(), target_);
	
    if(offset < 0) {
        return false;
    }

	if((offset + loadSize) > initSize) {
		// Make an exception for a char array initialized by a string.
		auto initializer = globalVar->GetInitializer();

		if((initializer->Conversion() == InitConv_PointerToPointer) &&
		   (initializer->IsInitializerList() == false) &&
		   (initializer->Value()->IsStringConstant())) {
			// Make sure the offset is in the bounds of the string.
			auto stringConst = initializer->Value()->As<StringConstant>();
			auto arrayType = stringConst->GetType()->As<ArrayType>();

			if(offset < arrayType->Size()) {
				return true;
			}
		}
		
		return false;
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::LoadFromOffset(Initializer* initializer, const Type* sourceType,
										const Type* loadType, __int64 offset) {
	DebugValidator::IsNotNull(initializer);
	DebugValidator::IsLargerOrEqual(offset, 0);

	// If the initializer is an initializer list, try to see at which position
	// the initializer indicated by the offset is found.
	if(initializer->IsInitializerList()) {
		auto initList = static_cast<InitializerList*>(initializer);
		
		if(auto arrayType = sourceType->As<ArrayType>()) {
			// If it's an array type we can compute the index directly.
			auto elementType = arrayType->ElementType();
			__int64 elemSize = TypeInfo::GetSize(elementType, target_);
			__int64 childIndex = offset / elemSize;
			DebugValidator::IsSmaller(childIndex, arrayType->Size());

			// We need to test for the case in which the offset lies between
			// two elements in an array. Consider the following case:
			// const int a[] = {1,2,3,4};
			// *((short*)((char*)a + 3)) - data from both 1 and 2 is loaded.
			if((elementType->IsArray() || elementType->IsRecord()) == false) {
				__int64 childOffset = offset % elemSize;
				__int64 loadSize = TypeInfo::GetSize(loadType, target_);

				// Note that this also catches the case when the data to be loaded
				// is larger than the size of the array element.
				if((childOffset + loadSize) > elemSize) {
					return LoadFromMultipleArray(initList, childIndex, childOffset,
												 arrayType, loadType);
				}
			}

			return LoadFromOffset((*initList)[childIndex], arrayType->ElementType(),
								  loadType, offset - (childIndex * elemSize));
		}
		else if(auto recordType = sourceType->As<RecordType>()) {
			// For records we need to iterate all the fields 
			// until we find a suitable one.
			auto& fields = recordType->Fields();

			for(int i = 0; i < fields.Count(); i++) {
				const RecordField& field = fields[i];
				__int64 fieldOffset = field.FieldOffset;
				__int64 fieldSize = TypeInfo::GetSize(field.FieldType, target_);

				if(offset < (fieldOffset + fieldSize)) {
					// If we need to load from two fields we give up; most of the time
					// it can't be performed (there may be padding between fields),
					// it would be complicated and this situation shouldn't actually
					// occur in a standard-conforming application.
					__int64 loadSize = TypeInfo::GetSize(loadType, target_);

					if((offset + loadSize) > (fieldOffset + fieldSize)) {
                        return nullptr;
                    }

					return LoadFromOffset((*initList)[i], field.FieldType, 
										  loadType, offset - fieldOffset);
				}
			}
		}

		// The offset is invalid, give up.
		return nullptr;
	}

	if(initializer->Conversion() == InitConv_PointerToPointer) {
		// Frequently used for strings, like in the following example:
		// var a int8* = ptop("abcd", int8*)
		if(auto stringConst = initializer->Value()->As<StringConstant>()) {
			return LoadFromString(stringConst, loadType, offset);
		}

		// All other cases are not supported.
		return nullptr;
	}

	// We should load the operand from the initializer.
	return LoadFromInitializer(initializer, loadType, offset);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::LoadFromMultipleArray(InitializerList* initList, 
											   __int64 childIndex, __int64 childOffset, 
											   const ArrayType* sourceType,
										       const Type* loadType) {
	DebugValidator::IsNotNull(initList);
	DebugValidator::IsSmaller(childIndex, sourceType->Size());
	DebugValidator::IsFalse(sourceType->ElementType()->IsArray());
	DebugValidator::IsFalse(sourceType->ElementType()->IsRecord());

	// The data should be loaded is either larger than the element size of the array,
	// or it lies between two array elements, like in the following example:
	// [0|1|2|3] [4|5|6|7] [8|9|...]
	//        \   /
	//    to be loaded (example in C: 'int a[] ={1,2,...}; short b = *((short*)a + 1);')
	// The loaded data can't be larger than 8 bytes (int64/double).
	unsigned char data[8];
	std::memset(data, 0, 8);

	int dataPos = 0; // We store from right to left.
	__int64 requestedBytes = TypeInfo::GetSize(loadType, target_);
	__int64 elemSize = TypeInfo::GetSize(sourceType->ElementType(), target_);
	bool firstIteration = true;

	// Fill the data from the array elements. 
	// Note that this works only on little-endian systems (like x86). 
	// For big-endian the order of the bytes should be reversed!
	while(requestedBytes > 0) {
		unsigned char temp[8];
		__int64 bytes;
		int bytePos = firstIteration ? childOffset : 0;
		firstIteration = false;

		if(GetOperandData((*initList)[childIndex]->Value(), temp, bytes) == false) {
			// The operand can't be used, give up.
			return nullptr;
		}
		else childIndex++;

		while((bytePos < bytes) && (requestedBytes > 0)) {
			data[dataPos] = temp[bytePos];
			dataPos++;
			bytePos++;
			requestedBytes--;
		}

	}

	// Returns an operand having the required type and data.
	return GetOperandHavingData(data, 8, loadType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::LoadFromInitializer(Initializer* initializer, const Type* loadType, 
											 __int64 offset) {
	DebugValidator::IsFalse(initializer->IsInitializerList());
	bool isNull = false;
	bool isZero = false;
	
	// We may be loading from string constant.
	if(auto stringConst = initializer->Value()->As<StringConstant>()) {
		return LoadFromString(stringConst, loadType, offset);
	}

	// Exclude initializers that have an incompatible conversion.
	if(initializer->Conversion() == InitConv_PointerToInt) {
		if(initializer->Value()->IsNullConstant()) {
			isZero = true;
		}
		else {
			// Definitely not a constant operand.
			return nullptr;
		}
	}
	else if((initializer->Conversion() == InitConv_PointerToPointer) ||
			(initializer->Conversion() == InitConv_IntToPointer)) {
		if(initializer->Value()->IsNullConstant() || MatchInt(0)(initializer->Value())) {
			isNull = true;
		}
		else {
			// Definitely not a constant operand.
			return nullptr;
		}
	}

	// It's undefined behavior if we try to load from a null pointer.
	if(isNull) {
        return GetNullptr(loadType);
    }
	else if(isZero) {
        return GetZeroInt(loadType);
    }

	// Obtain the value from the initializer. If the offset is not 0, or the
	// types don't match, we must extract the value.
	// Not that we give up if we are requested to extract a value that is larger
	// than the one in the initializer, or if the offset is too large.
	Operand* value = initializer->Value();
	auto valueType = value->GetType();

	if((offset == 0) && (loadType == initializer->Value()->GetType())) {
		return initializer->Value();
	}
	else if((offset > TypeInfo::GetSize(valueType, target_)) ||
		    (TypeInfo::GetSize(loadType, target_) > 
			 TypeInfo::GetSize(valueType, target_))) {
		// This is undefined behavior, because we want to read something that is 
		// in memory located after the operand, and there are no constraints
		// on the way global variables are laid out int memory.
		return GetUndefined(loadType);
	}
	
    return ExtractFromOperand(value, loadType, offset);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::ExtractFromOperand(Operand* op, const Type* loadType, 
											__int64 offset) {
	DebugValidator::IsNotNull(op);
	DebugValidator::IsLargerOrEqual(offset, 0);

	__int64 sourceSize = OperandInfo::Size(op, target_);
	__int64 loadSize = TypeInfo::GetSize(loadType, target_);

	// If the amount of data we need to load is larger than it's available
	// we give up, because the rest of the data is undefined.
	if(loadSize > sourceSize) {
        return nullptr;
    }

	// Get the data from the operand. Only integer, floating and nullptr
	// constants are supported. The maximum size of the data is 8 bytes (int64/double).
	unsigned char data[8];
	__int64* dataPtr = (__int64*)data;

	if(GetOperandData(op, data, sourceSize) == false) {
		// The operand is not supported.
		return nullptr;
	}

	if(loadSize != sourceSize) {
		// We actually need to extract something.
		// For example, if the offset is 2 it means that the first 2 bytes
		// are not needed, so we shift the value to the right. To limit the number
		// to the desired size, we use a bit mask.
		__int64 mask = IA::ValueFromBitCount(loadSize * 8);
		*dataPtr = (*dataPtr & mask) >> (offset * 8);
	}

	// Try to create a constant operand that uses the extracted data.
	return GetOperandHavingData(data, sizeof(data), loadType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::LoadFromString(StringConstant* stringConst, const Type* loadType, 
										__int64 offset) {
	// Test for the common case first, when a single element of the string
	// is requested by the 'load' instruction.
	auto arrayType = stringConst->GetType()->As<ArrayType>();

	if(loadType == arrayType->ElementType()) {
		// Just return the requested character as an integer constant.
		__int64 value = stringConst->Value()[offset];
		return irGen_->GetIntConst(loadType, value);
	}

	// If the types don't match we need to form a value from the available data.
	// The maximum size of the data is 8 bytes (int64, double).
	unsigned char data[8];
    std::memset(data, 0, 8);

	int dataPos = 0; // We store from right to left.
	__int64 loadSize = TypeInfo::GetSize(loadType, target_);

	while(loadSize > 0) {
		data[dataPos] = (unsigned char)stringConst->Value()[offset];
		dataPos++;
		offset++;
		loadSize--;
	}
    
	// Returns an operand having the required type and data.
	return GetOperandHavingData(data, 8, loadType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::GetOperandData(Operand* op, unsigned char* data, __int64& length) {
	if(auto intConst = op->As<IntConstant>()) {
		*((__int64*)data) = intConst->Value();
		length = TypeInfo::GetSize(intConst->GetType(), target_);
		return true;
	}
	else if(auto floatConst = op->As<FloatConstant>()) {
		*((double*)data) = floatConst->Value();
		length = TypeInfo::GetSize(floatConst->GetType(), target_);
		return true;
	}
	else if(auto nullConst = op->As<NullConstant>()) {
		*((__int64*)data) = 0;
		length = TypeInfo::GetSize(nullConst->GetType(), target_);
		return true;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::GetOperandHavingData(unsigned char* data, __int64 length, 
											  const Type* type) {
	DebugValidator::IsLargerOrEqual(length, IntegerType::GetInt64()->Size());
	
	// Integer, floating and null pointer constants are supported.
	if(auto intType = type->As<IntegerType>()) {
		return irGen_->GetIntConst(intType, *((__int64*)data));
	}
	else if(auto floatType = type->As<FloatingType>()) {
		return irGen_->GetFloatingConst(floatType, *((double*)data));
	}
	else if(auto pointerType = type->As<PointerType>()) {
		// Return 'nullptr' if the data is 0.
		if(*((__int64*)data) == 0) {
			return GetNullptr(pointerType);
		}
	}
	
	return nullptr; // Give up for all other cases.
}

} // namespace Analysis