// SymbolicAliasAnalysis.cpp
// Copyright (c) Lup Gratian
//
// Implements the SymbolicAliasAnalysis class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "SymbolicAliasAnalysis.hpp"

namespace Analysis {

AliasResult SymbolicAliasAnalysis::ComputeAlias(AliasLocation locationA, 
												AliasLocation locationB) {
	// If both operands are parameters this analysis
	// can't do anything, so give up early.
	if(locationA.Base()->IsParameter() &&
	   locationB.Base()->IsParameter()) {
		return Alias_May;
	}

	// If we don't know the size of the access we give up.
	// For example, 'a[1]' normally does not alias 'a[2]',
	// but if the access is 8 bytes then it does.
	if(CanResultBeValid(locationA, locationB) == false) {
		return Alias_May;
	}

	// Try to determine the alias by comparing the accessed regions. 
	// This works mostly for cases like 'a[1][2]' and 'a[1][3]'.
	auto result = ComputeOffsetAlias(locationA, locationB);

	if(result != Alias_May) {
		return result;
	}

	// Try to determine the alias for accesses on arrays
	// and pointers treated as arrays.
	result = ComputeArrayAlias(locationA, locationB);

	if(result != Alias_May) {
		return result;
	}

    // Try to determine the alias for accesses involving records.
	result = ComputeRecordAlias(locationA, locationB);

	if(result != Alias_May) {
		return result;
	}

    // Try to determine the alias for accesses on arrays
    // using the GCD test and other complex rules.
	result = ComputeDependenceAlias(locationA, locationB);

    if(result != Alias_May) {
		return result;
	}

    // Try to determine the alias for accesses on pointers.
    return ComputePointerAlias(locationA, locationB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasLocation SymbolicAliasAnalysis::ComputeOffset(AliasLocation& location) {
	Operand* base = location.Base();
	__int64 offset = 0;

	// Process until we're done with all addressing instructions.
	while(base->HasDefiningInstruction()) {
		if(auto indexInstr = base->DefiningInstrAs<IndexInstr>()) {
			// Consider only constant indices.
			if(auto intConst = indexInstr->IndexOp()->As<IntConstant>()) {
				__int64 elementSize = TI::GetSize(indexInstr->GetElementType(),
												  Parent()->GetTarget());
				offset += elementSize * intConst->Value();
				base = indexInstr->BaseOp();
			}
			else break;
		}
		else if(auto addrInstr = base->DefiningInstrAs<AddressInstr>()) {
			// Consider only constant indices.
			if(auto intConst = addrInstr->IndexOp()->As<IntConstant>()) {
				__int64 elementSize = TI::GetSize(addrInstr->GetPointeeType(),
												  Parent()->GetTarget());
				offset += elementSize * intConst->Value();
				base = addrInstr->BaseOp();
			}
			else break;
		}
		else if(auto elemInstr = base->DefiningInstrAs<ElementInstr>()) {
			// The index is always a constant.
			auto intConst = elemInstr->IndexOp()->As<IntConstant>();
			auto recordType = elemInstr->GetRecordType();

			offset += recordType->GetFieldOffset(intConst->Value());
			base = elemInstr->BaseOp();
		}
		else if(auto ptopInstr = base->DefiningInstrAs<PtopInstr>()) {
			base = ptopInstr->TargetOp();
		}
		else break;
	}

	return AliasLocation(base, location.Offset() + offset, location.Size());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputeOffsetAlias(AliasLocation& locationA, 
													  AliasLocation& locationB) {
	// We try to prove alias/no alias by looking at the offset
	// where the access takes place. For example, this proves no alias for
	// cases like a[2][3] and a[2][4]/a[3][3].
	// For cases like 'a[i][2]' we can't compute the offset
	// and handle them in another place.
	if((locationA.HasKnownSize() && locationB.HasKnownSize()) == false) {
		return Alias_May;
	}

	auto newLocationA = ComputeOffset(locationA);
	auto newLocationB = ComputeOffset(locationB);

	if(newLocationA.Base() == newLocationB.Base()) {
		// The same operand acts as the base, 
		// if the ranges don't overlap there is no alias.
		if(RangesOverlap(newLocationA, newLocationB)) {
			return Alias_Must;
		}
		else return Alias_None;
	}

	// Even if the base operands are not the same, if we can prove
	// that they don't alias then there is no alias.
	// Note that we do this test only if the base operands changed,
	// else we could enter an infinite loop.
	if((newLocationA.Base() != locationA.Base()) ||
	   (newLocationB.Base() != locationB.Base())) {
		return Parent()->ComputeAliasWithUnknownSize(newLocationA.Base(), 
													 newLocationB.Base());
	}

	return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::CanResultBeValid(AliasLocation& locationA,
											 AliasLocation& locationB) {
	// We have confidence in the result only if we know
	// the size of the accessed regions, or if both operands
	// are non-address-taken variables.
	if(locationA.HasKnownSize() && locationB.HasKnownSize()) {
		return true;
	}

	return IsAddressNotTaken(locationA) &&
		   IsAddressNotTaken(locationB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputeArrayAlias(AliasLocation& locationA,
													 AliasLocation& locationB) {
	auto instrA = locationA.Base()->DefiningInstruction();
	auto instrB = locationB.Base()->DefiningInstruction();

	if((instrA || instrB) == false) {
		return Alias_May;
	}

	// At least one of the instructions should be 'index'/'addr',
	// and if we have both they should be the same kind.
	bool instrAIsAddr = instrA && (instrA->Is<IndexInstr>() || 
								   instrA->Is<AddressInstr>());
	bool instrBIsAddr = instrB && (instrB->Is<IndexInstr>() || 
								   instrB->Is<AddressInstr>());

	if(instrAIsAddr && instrBIsAddr &&
	   (instrA->SameKind(instrB) == false)) {
		return Alias_May;
	}

	auto addressInstrA = instrAIsAddr ? static_cast<AddressInstr*>(instrA) : nullptr;
	auto addressInstrB = instrBIsAddr ? static_cast<AddressInstr*>(instrB) : nullptr;
	AliasResult result;

	if(addressInstrA && addressInstrB) {
		// Check for some simpler cases like 'a[i]' and 'a[i + 1]'.
		result = ComputeBasicArrayAlias(addressInstrA, addressInstrB,
										locationA, locationB);
		if(result != Alias_May) {
			return result;
		}
	}
	else if(addressInstrA || addressInstrB) {
		auto addressInstr = addressInstrA ? addressInstrA : addressInstrB;
		auto other = addressInstrA ? locationB.Base() : locationA.Base();

		// Check for cases like 'a' and 'a[i]'. Sadly in most cases
		// they can't be solved because we don't have enough information.
		result = ComputeSingleArrayAlias(addressInstr, other, locationA, locationB);

		if(result != Alias_May) {
			return result;
		}
	}

	return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputeBasicArrayAlias(AddressInstr* instrA, 
														  AddressInstr* instrB,
														  AliasLocation& locationA,
														  AliasLocation& locationB) {
	// Here we do some simple and fast checks 
	// to prove that two array accesses don't alias.
	auto constIndexA = instrA->IndexOp()->As<IntConstant>();
	auto constIndexB = instrB->IndexOp()->As<IntConstant>();

	if(constIndexA && constIndexB) {
		if(constIndexA->Value() != constIndexB->Value()) {
			// &a[4] != &a[5]            
			// If the base operands are the same, but the indices
			// different there is no alias.
			if(instrA->BaseOp() == instrB->BaseOp()) {
				if(IsNoArrayElementOverlap(constIndexA->Value(), constIndexB->Value(),
										   instrA, instrB, locationA, locationB)) {
					return Alias_None;
			   }
			   else return Alias_May;
			}

			// &a[4] != &b[5]
			// If the base operands are definitely different locations,
			// then the computed addresses are not the same.
			return Parent()->ComputeAliasWithUnknownSize(instrA->BaseOp(), 
														 instrB->BaseOp());
		}
		else {
			// &a[4] == &a[4]
			// If both the base operands and indices are the same there is alias.
			if(instrA->BaseOp() == instrB->BaseOp()) {
				return Alias_Must;
			}

			// &a[4] != &b[4]
			// If the base operands are different there is no alias.
			return Parent()->ComputeAliasWithUnknownSize(instrA->BaseOp(), 
														 instrB->BaseOp());
		}
	}

	if(instrA->IndexOp() == instrB->IndexOp()) {
		// &a[i] == &a[i]
		// If both the base and index operands are the same there is alias.
		if(instrA->BaseOp() == instrB->BaseOp()) {
			return Alias_Must;
		}

		// &a[i] != &b[i]
		// If the base operand don't alias the computed addresses don't alias.
		return Parent()->ComputeAliasWithUnknownSize(instrA->BaseOp(), 
													 instrB->BaseOp());
	}

	return ComputeVariableArrayAlias(instrA, instrB, locationA, locationB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsNoArrayElementOverlap(__int64 constIndexA, 
													__int64 constIndexB,
													AddressInstr* instrA, 
													AddressInstr* instrB,
													AliasLocation& locationA,
													AliasLocation& locationB) {
	DebugValidator::AreNotEqual(constIndexA, constIndexB);

	// If both locations represent non-address taken variables
	// we can consider there is no overlap (overlap in an array
	// can be achieved only if the address is taken somehow,
	// unlike in the case of a C 'union').
	if(IsAddressNotTaken(locationA) && 
	   IsAddressNotTaken(locationB)) {
		return true;
	}
	else {
		DebugValidator::IsTrue(locationA.HasKnownSize());
		DebugValidator::IsTrue(locationB.HasKnownSize());

		// Compute the effect the indexing has on the offset
		// (the size of the accessed region remains the same).
		// We check if after the adjustment there is overlap or not.
		__int64 offsetAdjustmentA = constIndexA * GetElementSize(instrA);
		__int64 offsetAdjustmentB = constIndexB * GetElementSize(instrB);

		auto newLocationA = ComputeOffset(instrA->BaseOp(), locationA.Offset(), 
										  locationA.Size());
		auto newLocationB = ComputeOffset(instrB->BaseOp(), locationB.Offset(), 
										  locationB.Size());

		// If the base operands are not the same
		// we know there is no overlap only if the objects
		// are completely different.
		if(newLocationA.Base() != newLocationB.Base()) {
			return Parent()->ComputeAliasWithUnknownSize(newLocationA.Base(),
														 newLocationB.Base());
		}

		newLocationA.SetOffset(newLocationA.Offset() + offsetAdjustmentA);
		newLocationB.SetOffset(newLocationB.Offset() + offsetAdjustmentB);

		return RangesOverlap(newLocationA, newLocationB) == false;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
__int64 SymbolicAliasAnalysis::GetElementSize(AddressInstr* instr) {
	if(auto indexInstr = instr->As<IndexInstr>()) {
		return TI::GetSize(indexInstr->GetElementType(), Parent()->GetTarget());
	}
	else return TI::GetSize(instr->GetPointeeType(), Parent()->GetTarget());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputeVariableArrayAlias(AddressInstr* instrA, 
															 AddressInstr* instrB,
															 AliasLocation& locationA,
															 AliasLocation& locationB) {
	// We require that the types of the base operands to be the same.
	// If they are not and the index operands are variable we might
	// have equal offsets (alias) without knowing this in situations like
	// int* p1 = &v[0];
	// short* p2 = (short*)&v[0];   
	// p1[i] = 4;                       '&p1[i] == p2[i + 1]' iff 'i == 1'.
	// p2[i + 1] = 5;
	if(instrA->BaseOp()->GetType() != instrB->BaseOp()->GetType()) {
		return Alias_May;
	}

	// Test for cases like 'a[i +/- C1]' and 'a[i +/- C1]'.
	// If the operations are different ('+/-' or '-/+'), we know
	// the resulting addresses cannot be equal.
	// If the operations are the same we require the constants to be different.
	auto indexInstrA = instrA->IndexOp()->DefiningInstruction();
	auto indexInstrB = instrB->IndexOp()->DefiningInstruction();
	
	if(indexInstrA) {
		indexInstrA = indexInstrA->As<ArithmeticInstr>();
	}

	if(indexInstrB) {
		indexInstrB = indexInstrB->As<ArithmeticInstr>();
	}

	if((indexInstrA && ((indexInstrA->IsAdd() || indexInstrA->IsSub()) == false)) ||
	   (indexInstrB && ((indexInstrB->IsAdd() || indexInstrB->IsSub()) == false))) {
		return Alias_May; // Only 'add' and 'sub' supported.
	}

	// Canonicalize 'C + i' to 'i + C', so we have fewer cases to test for.
	CanonicalizeAdd(indexInstrA);
	CanonicalizeAdd(indexInstrB);

	// Make sure that the instructions have a constant operand on the right side.
	// Here we don't know enough to handle cases like 'a[i + j]'.
	if((indexInstrA && (indexInstrA->GetSourceOp(1)->IsConstant() == false)) ||
	   (indexInstrB && (indexInstrB->GetSourceOp(1)->IsConstant() == false))) {
		return Alias_May;
	}

	// If the variables involved are different we stop ('a[i]' and 'a[j]').
	// If the base operands are completely different objects
	// we can use their alias result though.
	if((indexInstrA && indexInstrB) &&
	   (indexInstrA->GetSourceOp(0) != indexInstrB->GetSourceOp(0))) {
		if(Parent()->ComputeAliasWithUnknownSize(instrA->BaseOp(), 
                                                 instrB->BaseOp()) == Alias_None) {
            return Alias_None;
        }
        else return Alias_May;
	}

	if(indexInstrA && indexInstrB) {
		// We have something like 'a[i + 1]' and 'a[i + 2]'.
		return ComputeVariableArrayAlias(instrA, instrB,
										 indexInstrA, indexInstrB,
										 locationA, locationB);
	}
	else if(indexInstrA || indexInstrB) {
		// We have something like 'a[i + 1]' and 'a[i]'.
		auto indexInstr = indexInstrA ? indexInstrA : indexInstrB;
		auto other = indexInstrA ? instrB->IndexOp() : instrA->IndexOp();

		return ComputeVariableArrayAlias(instrA, instrB, 
										 indexInstr, other, 
										 locationA, locationB);
	}

	return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputeVariableArrayAlias(AddressInstr* instrA,
															 AddressInstr* instrB,
															 Instruction* indexInstrA,
															 Instruction* indexInstrB,
															 AliasLocation& locationA,
															 AliasLocation& locationB) {
	bool different = false;
	__int64 adjustmentA;
	__int64 adjustmentB;

	if(indexInstrA->SameKind(indexInstrB) == false) {
		if(indexInstrB->IsSub()) {
			// 'a[i + C1]' and 'a[i - C2].
			// Different if 'i + C1 != i - C2' -> 'C1 != -C2'
			auto C1 = indexInstrA->GetSourceOp(1)->As<IntConstant>();
			auto C2 = indexInstrB->GetSourceOp(1)->As<IntConstant>();
			different = C1->Value() != -C2->Value();

			if(different) {
				adjustmentA = C1->Value();
				adjustmentB = -C2->Value();
			}
		}
		else {
			// 'a[i - C1]' and 'a[i + C2].
			// Different if 'i - C1 != i + C2' -> '-C1 != C2'
			auto C1 = indexInstrA->GetSourceOp(1)->As<IntConstant>();
			auto C2 = indexInstrB->GetSourceOp(1)->As<IntConstant>();
			different = -C1->Value() != C2->Value();

			if(different) {
				adjustmentA = -C1->Value();
				adjustmentB = C2->Value();
			}
		}
	}
	else {
		// The operations is the same. To be different, the constants
		// involved need to be different.
		auto C1 = indexInstrA->GetSourceOp(1)->As<IntConstant>();
		auto C2 = indexInstrB->GetSourceOp(1)->As<IntConstant>();
		different = C1->Value() != C2->Value();

		if(different) {
			adjustmentA = C1->Value();
			adjustmentB = C2->Value();
		}
	}

	if(different) {
		if(IsNoArrayElementOverlap(adjustmentA, adjustmentB,
								   instrA, instrB, locationA, locationB)) {
			return Alias_None;
		}
	}

	return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputeVariableArrayAlias(AddressInstr* instrA,
															 AddressInstr* instrB,
															 Instruction* instr,
															 Operand* other,
															 AliasLocation& locationA,
															 AliasLocation& locationB) {
	// We handle only cases involving a constant, like 'a[i]' and 'a[i + 1]'.
	// If the constant is zero they are practically the same.
	auto intConst = instr->GetSourceOp(1)->As<IntConstant>();
	
	if((intConst == nullptr) || intConst->IsZero() ||
	   (instr->GetSourceOp(0) != other)) {
		return Alias_May;
	}

	// Compute the adjustment, applied only on the index with the constant
	__int64 value = instr->IsAdd() ? intConst->Value() : -intConst->Value();
	__int64 adjustmentA = instrA->IndexOp() == instr->GetDestinationOp() ? value : 0;
	__int64 adjustmentB = instrB->IndexOp() == instr->GetDestinationOp() ? value : 0;

	if(IsNoArrayElementOverlap(adjustmentA, adjustmentB, 
							   instrA, instrB, locationA, locationB)) {
		return Alias_None;
	}
	
	return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputeSingleArrayAlias(AddressInstr* instr, 
														   Operand* other,
														   AliasLocation& locationA,
														   AliasLocation& locationB) {
	// We determine the base operand and compute the offset 
	// of the address instruction. If it's the same as the other operand
	// we know if we have alias.
	auto& addrLocation = locationA.Base() == instr->GetDestinationOp() ?
						 locationA : locationB;
	auto newAddrLocation = ComputeOffset(instr->GetDestinationOp(), 
										 addrLocation.Offset(),
										 addrLocation.Size());

	if(newAddrLocation.Base() == other) {
		auto& otherLocation = locationA.Base() == instr->GetDestinationOp() ?
							  locationB : locationA;
		if(RangesOverlap(newAddrLocation, otherLocation) == false) {
			return Alias_None;
		}
		else return Alias_Must;
	}

	// The only case when the offset couldn't be computed we check
	// is 'a' and 'a[i]', because we might know that 'i' is not zero.
	if(instr->BaseOp() == other) {
		auto unit = instr->ParentBlock()->ParentFunction()->ParentUnit();
		OperandInfo opInfo(unit, Parent()->GetTarget());

		if(opInfo.IsNotZero(instr->IndexOp(), instr->ParentBlock())) {
			return Alias_None;
		}
	}
	
	// If we have 'p1' and 'p2[i]' and we know the parameters
	// point to different objects there can't be no alias.
	auto parameterA = instr->BaseOp()->As<Parameter>();
	auto parameterB = other->As<Parameter>();

	if(parameterA && parameterB &&
	   AreParametersIndependent(parameterA, parameterB)) {
		return Alias_None;
	}

	return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputeRecordAlias(AliasLocation& locationA,
													  AliasLocation& locationB) {
	auto elemInstrA = locationA.Base()->DefiningInstrAs<ElementInstr>();
	auto elemInstrB = locationB.Base()->DefiningInstrAs<ElementInstr>();

	if(elemInstrA && elemInstrB) {
		// Determine the alias between two record accesses
		// like 'a.x' and 'a.y.z'.
		return ComputeRecordAlias(elemInstrA, elemInstrB,
								  locationA, locationB);
	}

	return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputeRecordAlias(ElementInstr* instrA, 
													  ElementInstr* instrB,
													  AliasLocation& locationA,
													  AliasLocation& locationB) {
	if(instrA->GetRecordType() == instrB->GetRecordType()) {
		// Test based on the offset in the record, because we may have 
		// an "union", where all fields start at offset 0.
		auto fieldA = instrA->GetField();
		auto fieldB = instrB->GetField();
		
		if(fieldA.FieldOffset != fieldB.FieldOffset) {
			// If the base operand is the same there is no alias
			// (unless the accessed ranges overlap somehow).
			if(instrA->BaseOp() == instrB->BaseOp()) {
				// We need to take into consideration
				// the original accessed offset.
				auto newLocationA = locationA.WithAdjustedOffset(fieldA.FieldOffset);
				auto newLocationB = locationB.WithAdjustedOffset(fieldB.FieldOffset);

				if(RangesOverlap(newLocationA, newLocationB)) {
					return Alias_Must;
				}
				else return Alias_None;
			}
			else {
				// If the base operands are completely different
				// objects there is no alias.
				return Parent()->ComputeAliasWithUnknownSize(instrA->BaseOp(), 
															 instrB->BaseOp());
			}
		}
		else {
			if(instrA->BaseOp() == instrB->BaseOp()) {
				// If the accessed regions don't overlap there is no alias
				// even though the same field is accessed.
				if(RangesOverlap(locationA, locationB)) {
					return Alias_Must;
				}
				else return Alias_None;
			}

			// If the base operands are different objects there is no alias.
			return Parent()->ComputeAliasWithUnknownSize(instrA->BaseOp(), 
														 instrB->BaseOp());
		}
	}

	// We may have a record that is nested into the other. For example,
	// record ABC {             record DEF {             
	//    int32 offset(0)          int32 offset(0)       var p <DEF>
	//    int32 offset(4)          <ABC> offset(4)
	// }                        }
	// 'elem p, 0' and 'elem (elem p, 1), 0' are independent.
	__int64 offsetDifference;

	if(auto ancestorA = GetAncestor(instrA, instrB, offsetDifference)) {
		AliasLocation ancestorLocation(ancestorA->ResultOp(), 
									   locationA.Offset() + offsetDifference,
									   locationA.Size());
		return ComputeRecordAlias(ancestorA, instrB,
								  ancestorLocation, locationB);
	}
	else if(auto ancestorB = GetAncestor(instrB, instrA, offsetDifference)) {
		AliasLocation ancestorLocation(ancestorB->ResultOp(), 
									   locationB.Offset() + offsetDifference,
									   locationB.Size());
		return ComputeRecordAlias(ancestorB, instrA,
								  ancestorLocation, locationA);
	}

	return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ElementInstr* SymbolicAliasAnalysis::GetAncestor(ElementInstr* elemInstrA, 
												 ElementInstr* elemInstrB,
												 __int64& offsetDifference) {
	offsetDifference = 0;

	// Test if 'elemInstrB' is an ancestor for 'elemInstrA'.
	while(elemInstrA) {
		if(elemInstrA->BaseOp()->GetType() == elemInstrB->BaseOp()->GetType()) {
			return elemInstrA;
		}
		else {
			// Walk up the record tree. 
			offsetDifference += elemInstrA->GetField().FieldOffset;
			elemInstrA = elemInstrA->BaseOp()->DefiningInstrAs<ElementInstr>();
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SymbolicAliasAnalysis::CanonicalizeAdd(Instruction* instr) {
	if(instr == nullptr) {
		return;
	}

	// Move constant to the right.
	if(auto addInstr = instr->As<AddInstr>()) {
		if((addInstr->RightOp()->IsConstant() == false) &&
			addInstr->LeftOp()->IsConstant()) {
			addInstr->SwapOperands();
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SymbolicAliasAnalysis::CanonicalizeMul(Instruction* instr) {
	if(instr == nullptr) {
		return;
	}

	// Move constant to the right.
	if(auto mulInstr = instr->As<MulInstr>()) {
		if((mulInstr->RightOp()->IsConstant() == false) &&
			mulInstr->LeftOp()->IsConstant()) {
			mulInstr->SwapOperands();
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
AliasResult SymbolicAliasAnalysis::ComputeDependenceAlias(AliasLocation locationA,
														  AliasLocation locationB) {
	// Try to determine the alias for two array accesses
	// of the form 'i * Factor + Adjustment'.
    SimpleIndex indexA;
	SimpleIndex indexB;
	
    bool onAddress = locationA.Base()->DefiningInstrIs<AddressInstr>() ||
                     locationB.Base()->DefiningInstrIs<AddressInstr>();
    auto block = GetTestBlock(locationA, locationB);
    bool hasIndexA = ExtractIndex(locationA, indexA);
	bool hasIndexB = ExtractIndex(locationB, indexB);

    // Don't try to analyze if we have pointers incremented
    // inside the loop, we handle them somewhere else.
    if(onAddress && 
       (locationA.Base()->DefiningInstrIs<PhiInstr>() ||
        locationB.Base()->DefiningInstrIs<PhiInstr>())) {
        return Alias_May;
    }

	if(hasIndexA && hasIndexB) {
		if(AreIndicesIndependent(indexA, indexB, locationA, locationB, block)) {
			// If the base operands are the same there is no alias.
			// There is no alias also when the base operands are
            // different non-address-taken objects.
			if((locationA.Base() == locationB.Base()) ||
               (IsAddressNotTaken(locationA) && 
                IsAddressNotTaken(locationB))) {
				return Alias_None;
			}
        }

        // As a last hope, base the response on the result
        // obtained by analyzing the base operands. Example:
        // for(i = 0; i < a; i++)
        //     for(j = 0; j < b; j++)
        //         v[i][j*2 + 2] = 5;
		//         v[i+1][j + 9] = 8;
        // 'j*2 + 2' is equal to 'j + 9' for 'j = 7',
        // but there is no alias because we have different rows.
        return Parent()->ComputeAliasWithUnknownSize(locationA.Base(), 
													 locationB.Base());
	}

	return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* SymbolicAliasAnalysis::GetTestBlock(AliasLocation& locationA,
                                           AliasLocation& locationB) {
    Block* blockA = nullptr;
    Block* blockB = nullptr;

    if(locationA.Base()->DefiningInstrIs<IndexInstr>() ||
       locationA.Base()->DefiningInstrIs<AddressInstr>()) {
        blockA = locationA.Base()->DefiningInstruction()->ParentBlock();
    }

    if(locationB.Base()->DefiningInstrIs<IndexInstr>() ||
       locationB.Base()->DefiningInstrIs<AddressInstr>()) {
        blockB = locationB.Base()->DefiningInstruction()->ParentBlock();
    }

    if(blockA && blockB) {
        // Prefer the block that is in a deeper loop.
        return blockA->LoopDepth() > blockB->LoopDepth() ?
               blockA : blockB;
    }
    else if(blockA) {
        return blockA;
    }
    else return blockB;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IdentifyIV(Operand* op, SimpleIV& ivInfo) {
    op = WithoutPointerCasts(op);

    // Try to obtain the IV from the cache.
    if(RetrieveCachedIV(op, ivInfo)) {
        return true;
    }

    // An induction variable is represented by a 'phi' that combines
	// a start value with an incremented one like in the example:
	// t1 = phi {0, t2}
	// t2 = add t1, 1
	auto phiInstr = op->DefiningInstrAs<PhiInstr>();

	// Make sure the loop has a single entry point.
	if((phiInstr == nullptr) || 
	   (phiInstr->OperandCount() != 2)) {
		return false;
	}

	// We presume all operations that increment the induction
	// variable have been folded into a single one.
	// 'i++; i++;' -> 'i += 2;'
	bool isNegativeStep = false;
	bool isOverflowUndefined = false;
	Operand* startValue;
	Operand* step;

	if(step = IdentifyIVIncrement(phiInstr->GetOperand(1), op, 
							      isNegativeStep, isOverflowUndefined)) {
		// We have 'phi {startValue, incrementedValue}'
		startValue = phiInstr->GetOperand(0);
	}
	else if(step = IdentifyIVIncrement(phiInstr->GetOperand(0), op, 
								       isNegativeStep, isOverflowUndefined)) {
		// We have 'phi {incrementedValue, startValue}'
		startValue = phiInstr->GetOperand(1);
	}

	if(step) {
        // Try to determine the value the IV has when the loop ends.
        // It can be helpful when the GCD test fails.
        OrderType comparisonOrder;
        bool isEndValueInclusive;
        auto endValue = IdentifyEndValue(phiInstr, comparisonOrder,
                                         isEndValueInclusive);
        if(endValue) {
            if(IsInvalidEndValue(comparisonOrder, isNegativeStep)) {
                endValue = nullptr;
            }
        }

		ivInfo.IV = op;
		ivInfo.StartValue = startValue;
        ivInfo.EndValue = endValue;
		ivInfo.Step = step;
        ivInfo.HasInclusiveEndValue = isEndValueInclusive;
        ivInfo.HasNotEqualOrder = comparisonOrder == Order_NotEqual;
		ivInfo.HasNegativeStep = isNegativeStep;
		ivInfo.HasUndefinedOverflow = isOverflowUndefined;

        // Cache the IV.
        AddIVToCache(ivInfo);
		return true;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::RetrieveCachedIV(Operand* op, SimpleIV& ivInfo) {
    int availableIVs = std::min(cachedIVs_, 8);

    for(int i = 0; i < availableIVs; i++) {
        if(ivCache_[i].IV == op) {
            ivInfo = ivCache_[i];
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SymbolicAliasAnalysis::AddIVToCache(SimpleIV& ivInfo) {
    if(ivCache_.Count() < 8) {
        // The cache is not yet full.
        ivCache_.Add(ivInfo);
    }
    else {
        // The oldest IV from the cache must be evicted.
        ivCache_[oldestIV_] = ivInfo;
        oldestIV_ = (oldestIV_ + 1) % 8;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* SymbolicAliasAnalysis::IdentifyIVIncrement(Operand* op, Operand* requiredIV,
											        bool& isNegativeStep,
											        bool& isOverflowUndefined) {
    // When deciding if the step is negative or not we need to be
    // conservative and presume it is negative if it can't be
    // proven to be positive (the step is a parameter, for example).
	if(auto addInstr = op->DefiningInstrAs<AddInstr>()) {
		if(addInstr->LeftOp() == requiredIV) {
            auto stepOp = addInstr->RightOp();
            isNegativeStep = IsNegativeStep(stepOp, addInstr->ParentBlock(), true);
			isOverflowUndefined = addInstr->HasUndefinedOverflow();
			return stepOp;
		}
	}
	else if(auto subInstr = op->DefiningInstrAs<SubInstr>()) {
		if(subInstr->LeftOp() == requiredIV) {
            auto stepOp = subInstr->RightOp();
            isNegativeStep = IsNegativeStep(stepOp, subInstr->ParentBlock(), false);
			isOverflowUndefined = subInstr->HasUndefinedOverflow();
			return stepOp;
		}
	}
    else if(op->DefiningInstrIs<AddressInstr>()) {
        return IdentifyAddrIVIncrement(op, requiredIV, isNegativeStep, 
                                       isOverflowUndefined);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* SymbolicAliasAnalysis::IdentifyAddrIVIncrement(Operand* op, Operand* requiredIV,
											            bool& isNegativeStep,
											            bool& isOverflowUndefined) {
    // We accept an increment formed by a single 'addr' instruction,
    // or by multiple one, with the condition that are indexes are constants:
    // t1 = addr iv, 1
    // t2 = addr t1, 2  ->  step = 3
    IntConstant* lastStep = nullptr;

    while(auto addrInstr = op->DefiningInstrAs<AddressInstr>()) {
        auto parentBlock = addrInstr->ParentBlock();

		if(addrInstr->BaseOp() == requiredIV) {
            // We found the induction variable, compute the final step.
            Operand* finalStep = nullptr;

			if(lastStep) {
                if(auto newStepConst = addrInstr->IndexOp()->As<IntConstant>()) {
                    __int64 value = lastStep->Value() + newStepConst->Value();
                    finalStep = GetIntConstant(value, parentBlock);
                }
                else {
                    // A constant step can't be combined
                    // with a variable one, stop now.
                    break;
                }
            }
            else finalStep = addrInstr->IndexOp();
                
            // Note that pointer arithmetic has always
			// the overflow undefined.
			isNegativeStep = IsNegativeStep(finalStep, parentBlock, true);
			isOverflowUndefined = true;
            return finalStep;
		}
        else if(auto newStepConst = addrInstr->IndexOp()->As<IntConstant>()) {
            op = addrInstr->BaseOp();

            if(lastStep == nullptr) {
                // This is the first step we found.
                lastStep = newStepConst;
            }
            else {
                // Add the current step to the old one.
                __int64 value = lastStep->Value() + newStepConst->Value();
                lastStep = GetIntConstant(value, parentBlock);
            }
        }
        else {
            // If the step is not a constant and it's not
            // applied on the induction variable we stop.
            break;
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsNegativeStep(Operand* stepOp, 
                                           Block* testBlock, bool isAdd) {
    if(isAdd) {
        // iv + (-step) -> negative
        return IsGreaterThanZero(stepOp, testBlock) == false;
    }
    else {
        // iv - (-step) -> positive
        return IsSmallerThanZero(stepOp, testBlock) == false;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* SymbolicAliasAnalysis::IdentifyEndValue(PhiInstr* ivInstr, 
                                                 OrderType& comparisonOrder,
                                                 bool& isEndValueInclusive) {
    // Loop trough the users of the induction variable
    // until we find a comparison instruction (in the same block)
    // that compares the IV with another operand and based on the 
    // result it enters/exits the loop.
    auto ivOp = ivInstr->GetDestinationOp();
    
    for(int i = 0; i < ivOp->UserCount(); i++) {
        auto user = ivOp->GetUser(i);

        if((user->IsCmp() || user->IsUcmp()) &&
           (user->ParentBlock() == ivInstr->ParentBlock())) {
            auto cmpInstr = static_cast<CmpInstrBase*>(user);

            // Consider only <, <=, >, >= and != orders.
            // != should be used only for pointer comparisons.
            if(IsEligibleOrder(cmpInstr, ivOp) &&
               DecidesLoopExecution(cmpInstr, ivInstr->ParentBlock())) {
                comparisonOrder = cmpInstr->Order();
                isEndValueInclusive = cmpInstr->IsLessOrEqual() ||
                                      cmpInstr->IsGreaterOrEqual();

                return cmpInstr->LeftOp() == ivOp ?
                       cmpInstr->RightOp() : cmpInstr->LeftOp();
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::DecidesLoopExecution(CmpInstrBase* cmpInstr, 
                                                 Block* loopHeader) {
    //! TODO: Use Loop Information if available.
    // If we don't have any loop information we handle the case
    // of a small loop with a single body block and try to use
    // annotation created by the frontend.
    if((cmpInstr->HasDestinationOp() == false) ||
       (cmpInstr->ResultOp()->HasSingleUser() == false)) {
        return false;
    }

    // An 'if' should decide the loop execution.
    auto ifInstr = cmpInstr->ResultOp()->GetUser(0)->As<IfInstr>();
    
    if((ifInstr == nullptr) || 
       (ifInstr->TrueTargetOp() == ifInstr->FalseTargetOp())) {
        return false;
    }

    // Check if we have a single-block body loop.
    if(IsSingleBlockLoop(ifInstr->TrueTargetOp()->Target(), loopHeader)) {
        return true;
    }

    // Try to use the loop-depth annotation.
    // If the 'true' target has a greater loop-depth
    // than the other one we know the loop can be exited here.
    auto trueBlock = ifInstr->TrueTargetOp()->Target();
    auto falseBlock = ifInstr->FalseTargetOp()->Target();

    return trueBlock->LoopDepth() > falseBlock->LoopDepth();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsSingleBlockLoop(Block* body, Block* loopHeader) {
    auto gotoInstr = body->BranchInstruction()->As<GotoInstr>();
    return gotoInstr && gotoInstr->TargetOp()->Target() == loopHeader;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsInvalidEndValue(OrderType comparisonOrder, 
                                              bool isNegativeStep) {
    // For 'not equal' the direction doesn't matter.
    if(comparisonOrder == Order_NotEqual) {
        return false;
    }
    // for(i = n - 1; i >= 0; i++) invalid
    // for(i = n - 1; i < 0; i--) invalid
    if(isNegativeStep) {
        if((comparisonOrder != Order_Greater) &&
           (comparisonOrder != Order_GreaterOrEqual)) {
            return true;
        }
    }
    // for(i = 0; i < n; i--) invalid
    // for(i = 0; i > n; i++) invalid
    else if((comparisonOrder != Order_Less) &&
            (comparisonOrder != Order_LessOrEqual)) {
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::ExtractIndex(AliasLocation& location, 
                                         SimpleIndex& indexInfo) {
	// Check if an 'index'/'addr' instruction is of the form
	// 'index * factor + adjustment'. A single factor and adjustment
	// is supported, it should be enough for most practical cases.
	auto definingInstr = location.Base()->DefiningInstruction();

	if((definingInstr == nullptr) ||
	   ((definingInstr->IsIndex() || 
         definingInstr->IsAddress()) == false)) {
		return false;
	}

	auto addrInstr = static_cast<AddressInstr*>(definingInstr);
	bool isNegativeAdjustment = false;
	Operand* index = addrInstr->IndexOp();
	Operand* factor = nullptr;
	Operand* adjustment = nullptr;
	
	// Check if there is an adjustment, either addition or subtraction.
	// We consider the new index the operand that is a multiplication.
	if(auto addInstr = index->DefiningInstrAs<AddInstr>()) {
		bool mulOnLeft = addInstr->LeftOp()->DefiningInstrIs<MulInstr>();

		if(mulOnLeft) {
			// There is a factor.
			adjustment = addInstr->RightOp();
			index = addInstr->LeftOp();
		}
		else {
			// The induction variable should be the index.
			bool phiOnLeft = addInstr->LeftOp()->DefiningInstrIs<PhiInstr>();

			adjustment = phiOnLeft ? addInstr->RightOp() : addInstr->LeftOp();
			index = phiOnLeft ? addInstr->LeftOp() : addInstr->RightOp();
		}
	}
	else if(auto subInstr = index->DefiningInstrAs<SubInstr>()) {
		adjustment = subInstr->RightOp();
		index = subInstr->LeftOp();
		isNegativeAdjustment = true;
	}

	// Check if there is a factor.
	// We consider the factor the operand that is a constant.
	if(auto mulInstr = index->DefiningInstrAs<MulInstr>()) {
		bool constOnRight = mulInstr->RightOp()->IsConstant();
		factor = constOnRight ? mulInstr->RightOp() : mulInstr->LeftOp();
		index = constOnRight ? mulInstr->LeftOp() : mulInstr->RightOp();

        // Check if we have the index written in this form:
        // '(i + C1) * C2', equivalent to 'i * C2 + C1*C2'.
        // We do the conversion because we don't handle the former form.
        if((adjustment == nullptr) && 
           (index->DefiningInstrIs<AddInstr>() ||
            index->DefiningInstrIs<SubInstr>())) {
            index = ExtractFactoredIndex(index, factor, adjustment,
                                         definingInstr->ParentBlock());
        }
	}

	indexInfo.Index = index;
	indexInfo.Factor = factor;
	indexInfo.Adjustment = adjustment;
	indexInfo.HasNegativeAdjustment = isNegativeAdjustment;
    location.SetBase(addrInstr->BaseOp());
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* SymbolicAliasAnalysis::ExtractFactoredIndex(Operand* indexOp, 
                                                     Operand* mulFactor,
                                                     Operand*& adjustment,
                                                     Block* block) {
    // Check if the factor is a constant and the index
    // an addition/subtraction, resulting in a situation like this:
    // '(i + C1) * C2' -> 'i * C2 + C1*C2'
    auto mulConst = mulFactor->As<IntConstant>();

    if(mulConst == nullptr) {
        return indexOp;
    }

    if(auto addInstr = indexOp->DefiningInstrAs<AddInstr>()) {
        // '(i + C1) * C2' -> 'i * C2 + C1*C2'
        CanonicalizeAdd(addInstr);
        auto intConst = addInstr->RightOp()->As<IntConstant>();

        if(intConst) {
            __int64 value = mulConst->Value() * intConst->Value();
            adjustment = GetIntConstant(value, block);
            return addInstr->LeftOp();
        }
    }
    else if(auto subInstr = indexOp->DefiningInstrAs<SubInstr>()) {
        // '(i + C1) * C2' -> 'i * C2 - C1*C2'
        auto intConst = subInstr->RightOp()->As<IntConstant>();

        if(intConst) {
            __int64 value = mulConst->Value() * -intConst->Value();
            adjustment = GetIntConstant(value, block);
            return subInstr->LeftOp();
        }
    }

    return indexOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::AreIndicesIndependent(SimpleIndex& indexA, 
												  SimpleIndex& indexB,
												  AliasLocation& locationA, 
												  AliasLocation& locationB,
												  Block* testBlock) {
	// Test for some simple and obvious cases first.
   auto result = AreIndicesIndependentSimple(indexA, indexB, 
											 locationA, locationB, testBlock);
   if(result == Alias_Must) {
	   // Definitely independent.
	   return true;
   }
   else if(result == Alias_None) {
	   // Definitely not independent.
	   return false;
   }

   // Check if the indexes are relative to the addresses
   // of other array elements like in the following example:
   // p1 = &v[3], p2 = &v[0];
   //
   // for(i = 0; i < n; i++) 
   //     p1[i*2], p2[i*2+2]
   // We would like to fold the offset of 'v[3]' into 'i*2'
   // so that we can prove the indices are independent.
   // -> p1[i*2 + 3], p2[i*2+2], p1 = &v[0], p2 = &v[0]
   __int64 adjustmentA = 0;
   __int64 adjustmentB = 0;
   FoldAddressIntoIndex(locationA, adjustmentA);
   FoldAddressIntoIndex(locationB, adjustmentB);

   // If the locations have an offset try to fold it
   // into the indexes (this can be done only if the offset
   // is a multiple of the array element).
   FoldOffsetIntoIndex(locationA, adjustmentA);
   FoldOffsetIntoIndex(locationB, adjustmentB);

   // If we still have location offset we give up
   // because it's too difficult to analyze this situation.
   if(locationA.HasOffset() || locationB.HasOffset()) {
	   return false;
   }

   // If the indexes are equal we use the GCD test,
   // otherwise we try to answer using certain relations
   // that appear between the different indexes.
   if(indexA.Index != indexB.Index) {
	   return AreIndicesIndependent(indexA, indexB, 
									locationA, locationB,
									adjustmentA, adjustmentB, testBlock);
   }
   else return AreIndicesIndependentSameIndex(indexA, indexB, 
											  locationA, locationB, 1, 1,
											  adjustmentA, adjustmentB, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::AreIndicesIndependentSimple(SimpleIndex& indexA, 
															   SimpleIndex& indexB,
															   AliasLocation& locationA, 
															   AliasLocation& locationB,
															   Block* testBlock) {
	// Here we handle only indexes that have the same
	// operand as their index ('a[i*3]' and 'a[j*5]' are not handled).
	if(indexA.Index != indexB.Index) {
		return Alias_May;
	}

	if(indexA.Factor == indexB.Factor) {
		// Either both factors are equal or both indexes don't have factors.
		if(indexA.Adjustment == indexB.Adjustment) {
			if(indexA.HasNegativeAdjustment != indexB.HasNegativeAdjustment) {
				return Alias_None; // &a[i * F + A] != &a[i *F - A]
			}
			else return Alias_Must; // &a[i * F + A] == &a[i * F + A]
		}
		else if(indexA.HasNoAdjustment() && IsNotZero(indexB.Adjustment)) {
			// &a[i * F] != &a[i * F +- A]
			return Alias_None;
		}
		else if(indexB.HasNoAdjustment() && IsNotZero(indexA.Adjustment)) {
			// &a[i * F +- A] != &a[i * F]
			return Alias_None;
		}
		else if((indexA.Adjustment && indexB.Adjustment) &&
				(indexA.HasNegativeAdjustment == indexB.HasNegativeAdjustment)) {
			// &a[i * F + A1] != &a[i * f + A2]
			// &a[i * F - A1] != &a[i * f - A2] iff A1 != A2
			return AreNotEqual(indexA.Adjustment, indexB.Adjustment) ? 
				   Alias_None : Alias_Must;
		}
	}
	
	// &a[i * F1] != &a[i * F2] iff 'i' is never zero.
	if((locationA.Base() == locationB.Base()) &&
	   (indexA.Index == indexB.Index) &&
	   (indexA.HasNoAdjustment() && indexB.HasNoAdjustment())) {
		// The values of the factors should not be equal.
		if(AreNotEqual(indexA.Factor, indexB.Factor)) {
			SimpleIV iv;

			if(IdentifyIV(indexA.Index, iv)) {
				return IsGreaterThanZero(iv, testBlock) ? 
					   Alias_None : Alias_Must;
			}
	   }
	}

	return Alias_May; // Unknown.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::FoldAddressIntoIndex(AliasLocation& location, 
												 __int64& adjustment) {
	// We check if we have an 'addr' that is relative to the address
	// of an array element. If it is, we try to fold the element offset
	// into the computation of the location index. For example,
	// p1 = &v[3], p1[i*2]  ->  p1 = &v[0], p1[i*2 + 3]
	auto addrInstr = location.Base()->DefiningInstrAs<AddressInstr>();
	
	if(addrInstr == nullptr) {
		return false;
	}

	// Check for an 'addr' or 'index' instruction.
	auto baseOp = WithoutPointerCasts(addrInstr->BaseOp());
	auto definingInstr = baseOp->DefiningInstruction();

	if((definingInstr == nullptr) ||
	   ((definingInstr->IsAddress() || definingInstr->IsIndex()) == false)) {
		return false;
	}

	// The index operand must be a constant that is a multiple
	// of the element type of 'addrInstr'.
	auto baseAddrInstr = static_cast<AddressInstr*>(definingInstr);
	auto indexConst = baseAddrInstr->IndexOp()->As<IntConstant>();

	__int64 elementSize = GetElementSize(addrInstr);
	__int64 baseElementSize = GetElementSize(baseAddrInstr);
	__int64 baseOffset = indexConst->Value() * baseElementSize;
	
	if((baseOffset % elementSize) == 0) {
		// Convert the offset to an adjustment
		// and update the location base.
		adjustment += baseOffset / elementSize;
		location.SetBase(baseAddrInstr->BaseOp());
		return true;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::FoldOffsetIntoIndex(AliasLocation& location, 
												__int64& adjustment) {
	// If the offset is different from zero we try to fold it
	// into the location index. This can be done only if it is
	// a multiple of the accessed element size.
	if(location.Offset() == 0) {
		return false;
	}

	auto definingInstr = location.Base()->DefiningInstruction();
	auto addrInstr = static_cast<AddressInstr*>(definingInstr);
	__int64 elementSize = GetElementSize(addrInstr);

	if((location.Offset() % elementSize) == 0) {
		// The offset is not a multiple of the element size.
		adjustment = location.Offset() / elementSize;
		location.SetOffset(0);
		return true;
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::AreIndicesIndependent(SimpleIndex& indexA, 
												  SimpleIndex& indexB, 
												  AliasLocation& locationA, 
												  AliasLocation& locationB,
												  __int64 adjustmentA, 
												  __int64 adjustmentB, 
												  Block* testBlock) {
	// The index operands are different, which is more difficult to analyze.
	// Still, in some cases it's clear the indices are different. For example:
	// for(int i = 0; i < n; i++)
	//     for(int j = i + 1; j < n; j++)
	//         &a[i] != &a[j], for any i, j
	if((indexA.Factor == indexB.Factor) &&
	   (indexA.Adjustment == indexB.Adjustment) &&
	   (adjustmentA == adjustmentB)) {
		// &a[i * F] != &a[j * F] iff j > i
		SimpleIV ivA;
		SimpleIV ivB;

		if(IdentifyIV(indexA.Index, ivA) && 
		   IdentifyIV(indexB.Index, ivB) &&
           ((ivA.HasNotEqualOrder || ivB.HasNotEqualOrder) == false)) {
			if(IsGreater(ivA, ivB, testBlock)) {
				return true;
			}
			else if(IsGreater(ivB, ivA, testBlock)) {
				return true;
			}
		}

		return false;
	}

    // If one index accesses only odd positions, while the other
    // only even there is no alias. For example,
    // '&a[2 * i] != &a[4 * j + 1]' for any 'i' and 'j'.
    // Make sure the factors are constants and both are even.
    // The adjustments, if they exists, should be constants too.
    if((indexA.Factor->IsConstant() && indexB.Factor->IsConstant()) &&
       ((indexA.HasAdjustment() == false) || indexA.Adjustment->IsConstant() &&
       ((indexB.HasAdjustment() == false) || indexB.Adjustment->IsConstant()))) {
        auto factorConstA = indexA.Factor->As<IntConstant>();
        auto factorConstB = indexB.Factor->As<IntConstant>();

        if((factorConstA->Value() % 2 == 0) &&
           (factorConstB->Value() % 2 == 0)) {
            // The factors are even, now test if the adjustments
            // are one even and the other one odd.
            __int64 finalAdjustmentA = adjustmentA + indexA.HasAdjustment() ?
                                       indexA.Adjustment->As<IntConstant>()->Value() : 0;
            __int64 finalAdjustmentB = adjustmentB + indexB.HasAdjustment() ?
                                       indexB.Adjustment->As<IntConstant>()->Value() : 0;
            if((((finalAdjustmentA % 2) == 0) && ((finalAdjustmentB % 2) != 0)) ||
               (((finalAdjustmentA % 2) != 0) && ((finalAdjustmentB % 2) == 0))) {
                if(locationA.Base() == locationB.Base()) {
                    return true;
                }
            }
        }
    }

	// If we have different factors and/or adjustments
	// we must use the GCD test. This can be done only if
	// we can express one of the indexes in terms of the other
	// one like we can do for the example above:
	// a[j] -> a[i + 1]
	// a[j * 2 + 3] -> a[(i + 1) * 2 + 3] -> a[i * 2 + 5]
	__int64 factorA = 1;
	__int64 factorB = 1;

	if((ReplaceIndexOperand(indexA, indexB, factorB, adjustmentB) == false) &&
	   (ReplaceIndexOperand(indexB, indexA, factorA, adjustmentA) == false)) {
		return false;
	}

	// Now do the GCD test.
	return AreIndicesIndependentSameIndex(indexA, indexB, 
										  locationA, locationB,
										  factorA, factorB, 
										  adjustmentA, adjustmentB, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::ReplaceIndexOperand(SimpleIndex& indexA, 
                                                SimpleIndex& indexB,
												__int64& factorB, 
                                                __int64& adjustmentB) {
	// Try to express the second induction variable
	// in terms of the first one if it is possible.
	// If 'j = i + 1' we can proceed like in the next example:
	// a[j] -> a[i + 1]
	// a[j * 2 + 3] -> a[(i + 1) * 2 + 3] -> a[i * 2 + 5]
	SimpleIV ivA;
	SimpleIV ivB;

	if((IdentifyIV(indexA.Index, ivA) && 
		IdentifyIV(indexB.Index, ivB)) == false) {
		return false;
	}

	// Exit if the steps are not the same or are negative.
	if((ivA.Step != ivB.Step) ||
	   (ivA.HasNegativeStep || ivB.HasNegativeStep)) {
		return false;
	}

	// Check if the second induction variable is expressed based
	// based on the other one using addition or multiplication.
	// 'j = i + C', 'j = i * C', 'j = (i * C1) + C2' or 'j = (i + C1) * C2'
	if(auto addInstr = ivB.StartValue->DefiningInstrAs<AddInstr>()) {
		CanonicalizeAdd(addInstr);
		auto addConst = addInstr->RightOp()->As<IntConstant>();

		if(addConst == nullptr) {
			return false;
		}

		if(addInstr->LeftOp() == ivA.IV) {
			// 'j = i + C' case.
			return ReplaceAddedOperand(indexB, indexA.Index, 
									   addConst, factorB, adjustmentB);
		}
		else if(auto mulInstr = addInstr->LeftOp()->DefiningInstrAs<MulInstr>()) {
			CanonicalizeMul(mulInstr);
			auto mullConst = mulInstr->RightOp()->As<IntConstant>();

			if((mullConst == nullptr) ||
			   (mulInstr->LeftOp() != ivA.IV)) {
				return false;
			}

			// 'j = (i * C1) + C2' case.
			return ReplaceMultipliedAddedOperand(indexB, indexA.Index,
												 addConst, mullConst, 
												 factorB, adjustmentB);
		}
	}
	else if(auto mulInstr = ivB.StartValue->DefiningInstrAs<MulInstr>()) {
		CanonicalizeMul(mulInstr);
		auto mullConst = mulInstr->RightOp()->As<IntConstant>();

		if(mullConst == nullptr) {
			return false;
		}

		if(mulInstr->LeftOp() == ivA.IV) {
			// 'j = i * C' case.
			return ReplaceMultipliedOperand(indexB, indexA.Index, 
											mullConst, factorB, adjustmentB);
		}
		else if(auto addInstr = mulInstr->LeftOp()->DefiningInstrAs<AddInstr>()) {
			CanonicalizeAdd(addInstr);
			auto addConst = addInstr->RightOp()->As<IntConstant>();

			if((addConst == nullptr) ||
			   (addInstr->LeftOp() != ivA.IV)) {
				return false;
			}

			// 'j = (i + C1) * C2' case.
			return ReplaceAddedMultipliedOperand(indexB, indexA.Index,
												 addConst, mullConst, 
												 factorB, adjustmentB);
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::ReplaceAddedOperand(SimpleIndex& index, Operand* otherIndex,
												IntConstant* intConst, __int64& factor,
												__int64& adjustment) {
	// We have an index of the form 'j * Factor + Adjustment',
	// where 'j = i + C'. We rewrite the index as
	// 'i * Factor + (Adjustment + C * Factor)
	if(index.HasNoFactor() || index.Factor->IsOneInt()) {
		adjustment += intConst->Value();
	}
	else if(auto factorConst = index.Factor->As<IntConstant>()) {
		adjustment += intConst->Value() * factorConst->Value();
	}
	else {
		// The factor is not a constant.
		return false;
	}

	index.Index = otherIndex;
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::ReplaceMultipliedOperand(SimpleIndex& index, 
                                                     Operand* otherIndex,
													 IntConstant* intConst, 
                                                     __int64& factor,
													 __int64& adjustment) {
	// We have an index of the form 'j * Factor + Adjustment',
	// where 'j = i * C'. We rewrite the index as
	// 'i * (Factor * C) + Adjustment'
	factor *= intConst->Value();
	index.Index = otherIndex;
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::ReplaceMultipliedAddedOperand(SimpleIndex& index, 
														  Operand* otherIndex,
														  IntConstant* addConst, 
														  IntConstant* mulConst,
														  __int64& factor, 
														  __int64& adjustment) {
	// We have an index of the form 'j * Factor + Adjustment',
	// where 'j = (i * MC) + AC'. We rewrite the index as
	// 'i * (Factor * MC) + (Adjustment + AC * Factor)
	if(index.HasNoFactor() || index.Factor->IsOneInt()) {
		adjustment += addConst->Value();
	}
	else if(auto factorConst = index.Factor->As<IntConstant>()) {
		adjustment += addConst->Value() * factorConst->Value();
	}
	else {
		// The factor is not a constant.
		return false;
	}

	factor *= mulConst->Value();
	index.Index = otherIndex;
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::ReplaceAddedMultipliedOperand(SimpleIndex& index, 
														  Operand* otherIndex,
														  IntConstant* addConst, 
														  IntConstant* mulConst,
														  __int64& factor, 
														  __int64& adjustment) {
	// We have an index of the form 'j * Factor + Adjustment',
	// where 'j = (i + AC) * MC'. We rewrite the index as
	// 'i * (Factor * MC) + (Adjustment + AC * MC * Factor)
	if(index.HasNoFactor() || index.Factor->IsOneInt()) {
		adjustment += addConst->Value() * mulConst->Value();
	}
	else if(auto factorConst = index.Factor->As<IntConstant>()) {
		adjustment += addConst->Value() * mulConst->Value() * factorConst->Value();
	}
	else {
		// The factor is not a constant.
		return false;
	}

	factor *= mulConst->Value();
	index.Index = otherIndex;
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::AreIndicesIndependentSameIndex(SimpleIndex& indexA, 
														   SimpleIndex& indexB, 
														   AliasLocation& locationA, 
														   AliasLocation& locationB,
														   __int64 factorA, 
														   __int64 factorB,
														   __int64 adjustmentA, 
														   __int64 adjustmentB, 
														   Block* testBlock) {
	// If any of the index has a non-constant factor
	// or adjustment we can't use the GCD test.
	if((indexA.HasAdjustment() && (indexA.Adjustment->IsIntConstant() == false)) ||
	   (indexB.HasAdjustment() && (indexB.Adjustment->IsIntConstant() == false)) ||
	   (indexA.HasFactor() && (indexA.Factor->IsIntConstant() == false))         ||
	   (indexB.HasFactor() && (indexB.Factor->IsIntConstant() == false))) {
		return false;
	}

	// Compute the final values for the factors and adjustments.
	// This takes into consideration any adjustment induced
    // by various offsets.
    ComputeIndexValues(indexA, indexB, 
                       factorA, factorB, 
                       adjustmentA, adjustmentB);

	// The GCD test guarantees that there is no dependency
	// between two array indexes if the GCD of the factors
	// doesn't perfectly divides the adjustments difference.
	// 'i * F1 + A1' != 'i * F2 + A2' iff
	// '(A2 - A1) % GCD(F1, F2) != 0'
	__int64 gcd = ComputeGCD(factorA, factorB);
	bool independent = ((adjustmentB - adjustmentA) % gcd) != 0;

	if(independent) {
		// The indices are definitely independent.
		return true;
	}
    else return IntersectionPointIsOutOfRange(indexA.Index,
                                              factorA, factorB,
                                              adjustmentA, adjustmentB, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IntersectionPointIsOutOfRange(Operand* indexOp, 
                                                          __int64 factorA, 
                                                          __int64 factorB,
                                                          __int64 adjustmentA, 
                                                          __int64 adjustmentB,
                                                          Block* testBlock) {
    // The indices may be independent even though the GCD
	// says they are not. This happens when the (only) point 
	// where the indices are equal is smaller than the start values of the
	// index operands. For example, the indices '2 * i' and '12 * i + 10'
	// are equal when 'i = -1', but this cannot happen if 'i' is an
	// induction variable that starts with 0, for example.
	SimpleIV iv;
	IdentifyIV(indexOp, iv);

    if(iv.HasNotEqualOrder) {
        return false;
    }

	// Solve the equation 'i * F1 + A1 = i * F1 + A2'.
	__int64 solution = (adjustmentB - adjustmentA) / (factorA - factorB);

	if(solution < 0)  {
		return IsPositive(iv, testBlock);
	}
	else if(solution == 0) {
		return IsGreaterThanZero(iv, testBlock);
	}
    else if(iv.EndValue) {
        if(iv.HasInclusiveEndValue) {
            // for(...; i <= n; i++)
            // n < solution + 1
            auto solutionConst = GetIntConstant(solution + 1, testBlock);
            return IsSmaller(iv.EndValue, solutionConst, testBlock);                 
        }
        else {
            // for(...; i < n; i++)
            // n < solution
            auto solutionConst = GetIntConstant(solution, testBlock);
            return IsSmaller(iv.EndValue, solutionConst, testBlock);
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SymbolicAliasAnalysis::ComputeIndexValues(SimpleIndex& indexA, 
                                               SimpleIndex& indexB, 
                                               __int64& factorA, 
                                               __int64& factorB,
                                               __int64& adjustmentA, 
                                               __int64& adjustmentB) {
    factorA *= indexA.HasFactor() ? 
			   indexA.Factor->As<IntConstant>()->Value() : 1;
	factorB *= indexB.HasFactor() ? 
			   indexB.Factor->As<IntConstant>()->Value() : 1;

	__int64 initialAdjustmentA = indexA.HasAdjustment() ?
								 indexA.Adjustment->As<IntConstant>()->Value() : 0;
	__int64 initialAdjustmentB = indexB.HasAdjustment() ?
								 indexB.Adjustment->As<IntConstant>()->Value() : 0;
	
    // Don't forget about negative adjustment.
	if(indexA.HasNegativeAdjustment) {
		initialAdjustmentA = -initialAdjustmentA;
	}

	if(indexB.HasNegativeAdjustment) {
		initialAdjustmentB = -initialAdjustmentB;
	}

	adjustmentA += initialAdjustmentA;
	adjustmentB += initialAdjustmentB;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
__int64 SymbolicAliasAnalysis::ComputeGCD(__int64 a, __int64 b) {
	// Compute the GCD of a and b using the classic Euclid algorithm.
	// It is fast enough in practice, so common cases are handled separately.
	while(b != 0) {
		int temp = b;
		b = a % b;
		a = temp;
	}

	return a;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputePointerAlias(AliasLocation& locationA,
                                                       AliasLocation& locationB) {
    SimpleIV ivA;
    SimpleIV ivB;
    
    // Try to fold any 'addr' instruction into the offset.
    FoldAddrIntoLocation(locationA);
    FoldAddrIntoLocation(locationB);
    
    auto testBlock = GetTestBlock(locationA, locationB);
    bool hasIVA = IdentifyIV(locationA.Base(), ivA);    
    bool hasIVB = IdentifyIV(locationB.Base(), ivB);

    if(hasIVA && hasIVB) {
        return ComputePointerAliasWithTwoIVs(locationA, locationB,
                                             ivA, ivB, testBlock);
    }
    else if(hasIVA || hasIVB) {
        return ComputePointerAliasWithSingleIV(locationA, locationB,
                                               ivA, ivB, hasIVA, hasIVB,
                                               testBlock);
    }
                      
    // while(p </!= p_end) ... *p, *p_end, p++ -> no alias

    return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputePointerAliasWithTwoIVs(AliasLocation& locationA,
                                                                 AliasLocation& locationB,
                                                                 SimpleIV ivA, SimpleIV ivB,
                                                                 Block* testBlock) {
    // Handle the case of two pointers that are induction variables.
    // If the steps are equal, in the same direction and the start values
    // don't alias there is no alias even for the accessed IVs.
    auto startLocationA = ComputeOffset(AliasLocation(ivA.StartValue,
                                                      locationA.Offset(), 
                                                      locationA.Size()));
    auto startLocationB = ComputeOffset(AliasLocation(ivB.StartValue,
                                                      locationB.Offset(), 
                                                      locationB.Size()));
    if(ivA.HasNegativeStep == ivB.HasNegativeStep) {
        // Both IVs evolve in the same direction:
        // p1 = p;
        // p2 = p + 1;
        // while(...)
        //      *p1 = ...
        //      *p2 = ...
        //       p1++; p2++;
        if((startLocationA.Base() == startLocationB.Base()) &&
            AreEqual(ivA.Step, ivB.Step, testBlock)) {
            // Make sure there is no overlap because of the size.
            if(RangesOverlap(startLocationA, startLocationB) == false) {
                return Alias_None;
            }
            else return Alias_Must;
        }
    }
    else if(startLocationA.Base() == startLocationB.Base()) {
        // The IVs evolve in different directions:
        // p1 = p - 1;
        // p2 = p + 1;
        // while(...)
        //      *p1 = ...
        //      *p2 = ...
        //       p1--; p2++;
        if(ivA.HasNegativeStep && (ivB.HasNegativeStep == false)) {
            if((startLocationA.Offset() < 0 &&
                startLocationB.Offset() >= 0) ||
               (startLocationA.Offset() == 0 &&
                startLocationB.Offset() > 0)) {
                // Make sure there is no overlap because of the size.
                if(RangesOverlap(startLocationA, startLocationB) == false) {
                    return Alias_None;
                }
            }
        }
        else if((ivA.HasNegativeStep == false) && ivB.HasNegativeStep) {
            if((startLocationB.Offset() < 0 &&
                startLocationA.Offset() >= 0) ||
               (startLocationB.Offset() == 0 &&
                startLocationA.Offset() > 0)) {
                // Make sure there is no overlap because of the size.
                if(RangesOverlap(startLocationA, startLocationB) == false) {
                    return Alias_None;
                }
            }
        }
    }

    return Parent()->ComputeAliasWithUnknownSize(startLocationA.Base(),
                                                 startLocationB.Base());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputePointerAliasWithSingleIV(AliasLocation& locationA,
                                                                   AliasLocation& locationB,
                                                                   SimpleIV ivA, SimpleIV ivB,
                                                                   bool hasIVA, bool hasIVB,
                                                                   Block* testBlock) {
    // Only one of the operands a an induction variable. If it is relative
    // to the other one defined outside the loop we might know there is no alias:
    // p1 = p, p2 = p2 + 1;
    // while(...) *p1 = ..., *p2 = ..., p2++;
    SimpleIV singleIV = hasIVA ? ivA : ivB;
    auto other = hasIVA ? locationB.Base() : locationA.Base();

    auto singleIVOffset = hasIVA ? locationA.Offset() : locationB.Offset();
    auto singleIVSize = hasIVA ? locationA.Size() : locationB.Size();
    
    auto otherOffset = hasIVA ? locationB.Offset() : locationA.Offset();
    auto otherSize = hasIVA ? locationB.Size() : locationA.Size();
    
    // Compute the final offsets.
    auto ivLocation = ComputeOffset(AliasLocation(singleIV.StartValue,
                                                  singleIVOffset, singleIVSize));
    auto otherLocation = ComputeOffset(AliasLocation(other, otherOffset, otherSize));

    // Test for the case when an IV that is a pointer
    // is compared to the end value, like in this example:
    // while(p1 < p2)
    //     *p1 = ...  - can't alias 'p2'
    //     *p2 = ...
    if(singleIV.EndValue && (singleIV.HasInclusiveEndValue == false)) {
        auto result = ComputePointerAliasWithEndValue(ivLocation, otherLocation,
                                                      singleIV, other);
        if(result != Alias_May) {
            return result;
        }
    }

    if(ivLocation.Base() == otherLocation.Base()) {
        if(RangesOverlap(ivLocation, otherLocation) == false) {
            // The initial values don't overlap, but we still
            // need to take into consideration how the moving pointer evolves.
            if(singleIV.HasNegativeStep &&
               (ivLocation.Offset() >= otherLocation.Offset())) {
                // If the step is negative the pointer may decrement
                // until it becomes equal to the other one, like here:
                // p1 = p + 6;
                // p2 = p + 1;
                // while(...) *p1 = ..., *p2 = ..., p1--;
                return Alias_May;
            }
            else if((singleIV.HasNegativeStep == false) &&
                    (ivLocation.Offset() <= otherLocation.Offset())) {
                // If the step is negative the pointer may decrement
                // until it becomes equal to the other one, like here:
                // p1 = p + 1;
                // p2 = p + 4;
                // while(...) *p1 = ..., *p2 = ..., p1++;
                return Alias_May;
            }
            else return Alias_None;
        }
    }
        
    // If the pointers originate from completely different objects
    // it doesn't matter how they evolve.
    return Parent()->ComputeAliasWithUnknownSize(ivLocation.Base(),
                                                 otherLocation.Base());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SymbolicAliasAnalysis::ComputePointerAliasWithEndValue(AliasLocation& ivLocation,
                                                                   AliasLocation& otherLocation,
                                                                   SimpleIV& singleIV, 
                                                                   Operand* other) {
    // while(p1 < p2)
    //     *p1 = ...  - can't alias 'p2'
    //     *p2 = ...

    // If the comparison order is 'not equal' we don't know
    // the pointer relationship, just that they are not equal.
    // while(p1 != p2) ...
    if(singleIV.HasNotEqualOrder) {
        // Be conservative and don't allow offsets.
        // Note that in this case the direction doesn't matter.
        if((ivLocation.Offset() == 0) &&
           (ivLocation.Size() == otherLocation.Size())) {
            return Alias_None;
        }
    }
    else if(singleIV.HasNegativeStep == false) {
        // An increasing IV pointer and a pointer representing 
        // the end value (with or without an offset) are compared.
        // We also take into consideration any offset because
        // there might be cases like the following:
        // while(p1 < p2)
        //     *(p1 + 1) = ... - aliases 'p2' on the last step.
        //     *p2 = ...
        //
        // while(p1 < p2 + 1)
        //     *(p1 + 1) = ... - doesn't alias 'p2' on the last step
        //     *p2 = ...         but 'p1 + 2' would alias though.
        auto endLocation = ComputeOffset(AliasLocation(singleIV.EndValue,
                                                       ivLocation.Offset(),
                                                       ivLocation.Size()));
        if(endLocation.Base() == other) {
            // This handles cases like the following:
            // p1 = p;
            // p2 = p + 100;
            // while(p1 < p2) ...
            if(auto addrInstr = singleIV.EndValue->DefiningInstrAs<AddressInstr>()) {
                // Only for constants we're certain about the result.
                if(addrInstr->IndexOp()->IsIntConstant()) {
                    // The offset of the last accessed location by 'p1'
                    // is with an unit less than the end value 'p2'.
                    __int64 elementSize = GetElementSize(addrInstr);
                    endLocation.SetOffset(endLocation.Offset() - elementSize);

                    // Make sure there is no access past the end location
                    // (this happens for 'p1 + 1' in the example above).
                    if((endLocation.Offset() + endLocation.Size()) <=
                        otherLocation.Offset()) {
                        return Alias_None;
                    }
                }
            }
        }
           
        // Even if the base operands are not the same, the 'less than'
        // relation implies that the pointers are not equal inside
        // the loop, so we can still do a conservative analysis.
        if((ivLocation.Offset() == 0) &&
           (ivLocation.Size() == otherLocation.Size())) {
            return Alias_None;
        }

        // Check for a pointer always located before the IV pointer:
        // p1 = p;
        // p2 = p + 1;
        // p3 = p + 100;
        // while(p2 < p3)
        //     *p2 = ...
        //     *p1 = ... - can't alias 'p2' because always 'p1 < p2'
        if(ivLocation.Base() == other) {
            if(RangesOverlap(ivLocation, otherLocation) == false) {
                return Alias_None;
            }
        }
    }

    return Alias_May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SymbolicAliasAnalysis::FoldAddrIntoLocation(AliasLocation& location) {
    // If the base operand is an 'addr' instruction with a constant index
    // we fold it into the location as an offset.
    // 'addr p, 1' -> offset = sizeof(pointee(p))
    // We try to process a chain of 'addr' because Peephole
    // might not have had the chance to fold them together.
    while(auto addrInstr = location.Base()->DefiningInstrAs<AddressInstr>()) {
        if(auto intConst = addrInstr->IndexOp()->As<IntConstant>()) {
            __int64 value = intConst->Value() * GetElementSize(addrInstr);
            location.SetOffset(location.Offset() + value);
            location.SetBase(addrInstr->BaseOp());
        }
        else break;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsGreaterThanZero(SimpleIV iv, Block* testBlock) {
	// An induction variable is always > 0 if it starts
	// with a value > 0, it is incremented with a positive constant
	// and overflow is undefined.
    if(iv.HasUndefinedOverflow == false) {
        return false;
    }

	if(iv.HasNegativeStep) {
        // for(i = n; i > 0; i--)
        // 'i' can never be zero if the step must be negative,
        // and the end value known and is not zero (for exclusive test) 
        // or is not one (for inclusive test).
        if(iv.EndValue == nullptr) {
            return false;
        }

        if(iv.HasInclusiveEndValue) {
            return IsGreaterThanZero(iv.EndValue, testBlock);
        }
        else return IsGreaterOrEqualToZero(iv.EndValue, testBlock);
	}

	return IsGreaterThanZero(iv.StartValue, testBlock) &&
		   IsGreaterThanZero(iv.Step, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsPositive(SimpleIV iv, Block* testBlock) {
	// An induction variable is always >= 0 if it starts
	// with a value >= 0, it is incremented with a positive constant
	// and overflow is undefined.
	if(iv.HasNegativeStep || (iv.HasUndefinedOverflow == false)) {
		return false;
	}

	return IsGreaterOrEqualToZero(iv.StartValue, testBlock) &&
		   IsGreaterThanZero(iv.Step, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsGreater(SimpleIV ivA, SimpleIV ivB, Block* testBlock) {
	// An induction variable is always greater than another
	// if its start value is greater than the other
	// and the step is at least equal to the other.
	if((ivA.HasNegativeStep || ivB.HasNegativeStep) ||
	   (ivA.HasUndefinedOverflow == false) || 
	   (ivB.HasUndefinedOverflow == false)) {
		return false;
	}

	if(IsGreater(ivA.StartValue, ivB.StartValue, testBlock) == false) {
		// Another possibility for the start value to be greater
		// if it is the other induction variable + an adjustment.
		// for(i = 0; i < n; i++) {
		//    for(j = i + 1; j < n; j++) - 'j' always greater than 'i'
		if(auto addInstr = ivA.StartValue->DefiningInstrAs<AddInstr>()) {
			if((addInstr->LeftOp() != ivB.IV) ||
			   (IsGreaterThanZero(addInstr->RightOp(), testBlock) == false)) {
				return false;
			}
		}
		else {
			// We don't handle other cases.
			return false;
		}
	}

	return IsGreaterOrEqual(ivA.Step, ivB.Step);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsNotZero(Operand* op, Block* testBlock) {
	if(auto intConst = op->As<IntConstant>()) {
		return intConst->Value() != 0;
	}

	// As a last resort ask we ask Operand Info.
	OperandInfo opInfo(nullptr, Parent()->GetTarget());
	return opInfo.IsNotZero(op, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsGreaterThanZero(Operand* op, Block* testBlock) {
	// For constants we can test directly.
	if(auto intConst = op->As<IntConstant>()) {
		return intConst->Value() > 0;
	}

	// As a last resort ask we ask Operand Info.
	OperandInfo opInfo(nullptr, Parent()->GetTarget());

	// If we have something like 'a + 1' and we know
	// that 'a' is >= 0 then the addition is > 0.
	if(auto addInstr = op->DefiningInstrAs<AddInstr>()) {
		if(auto intConst = addInstr->RightOp()->As<IntConstant>()) {
			return (intConst->Value() > 0) &&
					opInfo.IsPositive(addInstr->LeftOp(), testBlock);
		}
	}

	return opInfo.IsGreater(op, GetZeroInt(testBlock), 
							true, testBlock) == Result_Yes;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsSmallerThanZero(Operand* op, Block* testBlock) {
	// For constants we can test directly.
	if(auto intConst = op->As<IntConstant>()) {
		return intConst->Value() < 0;
	}

	// As a last resort ask we ask Operand Info.
	OperandInfo opInfo(nullptr, Parent()->GetTarget());
	return opInfo.IsSmaller(op, GetZeroInt(testBlock), 
							true, testBlock) == Result_Yes;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsGreaterOrEqualToZero(Operand* op, Block* testBlock) {
	// For constants we can test directly.
	if(auto intConst = op->As<IntConstant>()) {
		return intConst->Value() >= 0;
	}

	// Check if the operand is zero, otherwise if it is > 0.
	OperandInfo opInfo(nullptr, Parent()->GetTarget());
	
	if(opInfo.IsZero(op, testBlock)) {
		return true;
	}

	return IsGreaterThanZero(op, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::AreNotEqual(Operand* opA, Operand* opB, Block* testBlock) {
	if(opA == opB) {
		// Obviously not equal.
		return false;
	}

	// For constants we compare the values so that
	// the integer type doesn't matter.
	auto intConstA = opA->As<IntConstant>();
	auto intConstB = opB->As<IntConstant>();

	if(intConstA && intConstB) {
		return intConstA->Value() != intConstB->Value();
	}

	// Check if one of the operands is the other one
	// with a constant added/subtracted. If it is then
	// they can't be equal ('a != a + C' / 'a != a - C').
	if(IsAddedSubtractedOperand(opA, opB) ||
	   IsAddedSubtractedOperand(opB, opA)) {
		return true;
	}

	// As a last resort ask we ask Operand Info.
	OperandInfo opInfo(nullptr, Parent()->GetTarget());
	return opInfo.AreNotEqual(opA, opB, testBlock) == Result_Yes;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsAddedSubtractedOperand(Operand* opA, Operand* opB) {
	// Check if 'opB' is 'opA + C' or 'opA - C'.
	if(auto addInstr = opB->DefiningInstrAs<AddInstr>()) {
		CanonicalizeAdd(addInstr);
		return (addInstr->LeftOp() == opA) &&
				addInstr->RightOp()->IsIntConstant() &&
			   (addInstr->RightOp()->IsZeroInt() == false);
	}
	else if(auto subInstr = opB->DefiningInstrAs<SubInstr>()) {
		return (subInstr->LeftOp() == opA) &&
				subInstr->RightOp()->IsIntConstant() &&
			   (subInstr->RightOp()->IsZeroInt() == false);
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::AreEqual(Operand* opA, Operand* opB, Block* testBlock) {
	if(opA == opB) {
		// Obviously equal.
		return true;
	}

	// For constants we compare the values so that
	// the integer type doesn't matter.
	auto intConstA = opA->As<IntConstant>();
	auto intConstB = opB->As<IntConstant>();

	if(intConstA && intConstB) {
		return intConstA->Value() != intConstB->Value();
	}

	// As a last resort ask we ask Operand Info.
	OperandInfo opInfo(nullptr, Parent()->GetTarget());
	return opInfo.AreEqual(opA, opB, testBlock) == Result_Yes;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsGreater(Operand* opA, Operand* opB, Block* testBlock) {
	if(opA == opB) {
		return false;
	}

	// For constants we compare the values so that
	// the integer type doesn't matter.
	auto intConstA = opA->As<IntConstant>();
	auto intConstB = opB->As<IntConstant>();

	if(intConstA && intConstB) {
		return intConstA->Value() > intConstB->Value();
	}

	// As a last resort ask we ask Operand Info.
	OperandInfo opInfo(nullptr, Parent()->GetTarget());
	return opInfo.IsGreater(opA, opB, true, testBlock) == Result_Yes;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsSmaller(Operand* opA, Operand* opB, Block* testBlock) {
	if(opA == opB) {
		return false;
	}

	// For constants we compare the values so that
	// the integer type doesn't matter.
	auto intConstA = opA->As<IntConstant>();
	auto intConstB = opB->As<IntConstant>();

	if(intConstA && intConstB) {
		return intConstA->Value() < intConstB->Value();
	}

	// As a last resort ask we ask Operand Info.
	OperandInfo opInfo(nullptr, Parent()->GetTarget());
	return opInfo.IsSmaller(opA, opB, true, testBlock) == Result_Yes;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SymbolicAliasAnalysis::IsGreaterOrEqual(Operand* opA, Operand* opB, Block* testBlock) {
	if(opA == opB) {
		return true;
	}

	// For constants we compare the values so that
	// the integer type doesn't matter.
	auto intConstA = opA->As<IntConstant>();
	auto intConstB = opB->As<IntConstant>();

	if(intConstA && intConstB) {
		return intConstA->Value() >= intConstB->Value();
	}

	// As a last resort ask we ask Operand Info.
	OperandInfo opInfo(nullptr, Parent()->GetTarget());
	return opInfo.IsGreaterOrEqual(opA, opB, true, testBlock) == Result_Yes;
}

} // namespace Analysis