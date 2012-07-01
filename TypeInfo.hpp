// TypeInfo.hpp
// Copyright (c) Lup Gratian
//
// Provides methods for testing properties of types, like size and alignment.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_TYPE_INFO_HPP
#define PC_ANALYSIS_TYPE_INFO_HPP

#include "../IR/Operand.hpp"
#include "../IR/IRTypes.hpp"
#include "../Targets/TargetInfo.hpp"
#include "../Base/DebugValidator.hpp"
using namespace IR;
using namespace Base;
using namespace Target;

namespace Analysis {

class TypeInfo {
public:
	// Returns the size (in bytes) of the specified type.
	static int GetSize(const Type* type, const TargetInfo* target = nullptr) {
		if(auto intType = type->As<IntegerType>()) {
			return intType->Size();
		}
		else if(auto floatType = type->As<FloatingType>()) {
			return floatType->Size();
		}
		else if(auto pointerType = type->As<PointerType>()) {
            DebugValidator::IsNotNull(target);
			return target->GetPointerSize();
		}
		else if(auto arrayType = type->As<ArrayType>()) {
			return arrayType->Size() * GetSize(arrayType->ElementType(), target);
		}
		else if(auto recordType = type->As<RecordType>()) {
			// An empty record has no size.
			if(recordType->FieldCount() == 0) {
				return 0;
			}

			// The size of the record is the maximum offset summed with the
			// field found at that offset. We can't use the sum of the field
			// sizes  because more fields can be found at the same offset (union).
			int max = -1;
			auto& fields = recordType->Fields();

			for(int i = 0; i < fields.Count(); i++) {
				int size = fields[i].FieldOffset + GetSize(fields[i].FieldType, target);

				if(size > max) {
					// Found a field that makes the record larger.
					max = size;
				}
			}

			return max;
		}
		else return 0;
	}

	// Returns the size of the specified type in bits.
	static int GetSizeBits(const Type* type, const TargetInfo* target = nullptr) {
		return GetSize(type, target) * 8;
	}

	// Returns the alignment (in bytes) of the specified type.
	static int GetAlignment(const Type* type, const TargetInfo* target = nullptr) {
		if(auto intType = type->As<IntegerType>()) {
			return target->GetAlignment(intType->GetSubtype());
		}
		else if(auto floatType = type->As<FloatingType>()) {
			return target->GetAlignment(floatType->GetSubtype());
		}
		else if(auto pointerType = type->As<PointerType>()) {
            DebugValidator::IsNotNull(target);
			return target->GetPointerAlignment();
		}
		else if(auto arrayType = type->As<ArrayType>()) {
			// The alignment of an array is the alignment of its element type.
			return GetAlignment(arrayType->ElementType(), target);
		}
		else if(auto recordType = type->As<RecordType>()) {
			// If the record has no members then we use the minimum allowed alignment.
			if(recordType->FieldCount() == 0) {
				return 0;
			}

			// The alignment of a record is the alignment of its largest field.
			int max = -1;
			const Type* maxType;
			auto& fields = recordType->Fields();

			for(int i = 0; i < fields.Count(); i++) {
				int size = GetSize(fields[i].FieldType, target);

				if(size > max) {
					max = size;
					maxType = fields[i].FieldType;
				}
			}

			return GetAlignment(maxType, target);
		}
		
		return 0;
	}

	// Returns the alignment of the specified type in bits.
	static int GetAlignmentBits(const Type* type, const TargetInfo* target = nullptr) {
		return GetAlignment(type, target) * 8;
	}
};

} // namespace Analysis
#endif