// ConstantFolderArithmeticLogical.cpp
// Copyright (c) Lup Gratian
//
// Implements the constant folder for arithmetic and logical instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConstantFolder.hpp"

namespace Analysis {

Operand* ConstantFolder::HandleArithmetic(Opcode opcode, Operand* opA, 
										  Operand* opB, FloatMode mode, Block* block) {
	// If both operands are constant we can evaluate them.
    if(Instruction::IsFloatArithmetic(opcode) == false) {
        auto intConstA = AsIntConstant(opA, block);
        auto intConstB = AsIntConstant(opB, block);
        
        if(intConstA && intConstB) {
		    // Perform the operation; note that we don't care about overflow here.
		    __int64 result;

		    switch(opcode) {
			    case Instr_Add:  { result = IA::Add(intConstA, intConstB);  break; }
			    case Instr_Sub:  { result = IA::Sub(intConstA, intConstB);  break; }
			    case Instr_Mul:  { result = IA::Mul(intConstA, intConstB);  break; }
			    case Instr_Div:  {
				    // Don't fold if we divide by 0.
				    if(intConstB->Value() == 0) {
                        return nullptr;
                    }

				    result = IA::Div(intConstA, intConstB); 
				    break; 
			    }
			    case Instr_Udiv: { 
				    if(intConstB->Value() == 0) {
                        return nullptr;
                    }

				    result = IA::Udiv(intConstA, intConstB); 
				    break; 
			    }
			    case Instr_Mod:  { 
				    if(intConstB->Value() == 0) {
                        return nullptr;
                    }

				    result = IA::Mod(intConstA, intConstB);  
				    break; 
			    }
			    case Instr_Umod: {
				    if(intConstB->Value() == 0) {
                        return nullptr;
                    }

				    result = IA::Umod(intConstA, intConstB); 
				    break;
			    }
		    }

		    return irGen_->GetIntConst(intConstA->GetType(), result);
	    }
        else if(intConstA) {
            // Move the left constant to the right, simplifies some tests.
		    // Note that we can do this only for commutative operators.
		    if(Instruction::IsCommutative(opcode)) {
			    SwapOperands(opA, opB);
		    }
        }
    }
	else if(auto floatConstA = opA->As<FloatConstant>()) {
		if(auto floatConstB = opB->As<FloatConstant>()) {
			// If one of the constants is NaN or Infinity we don't evaluate the constant.
			if(FA::IsNaN(floatConstA) || FA::IsInfinity(floatConstA) ||
			   FA::IsNaN(floatConstB) || FA::IsInfinity(floatConstB)) {
			   return nullptr;
			}
			
			double result;
			switch(opcode) {
				case Instr_Fadd: { result = FA::Fadd(floatConstA, floatConstB); break; }
				case Instr_Fsub: { result = FA::Fsub(floatConstA, floatConstB); break; }
				case Instr_Fmul: { result = FA::Fmul(floatConstA, floatConstB); break; }
				case Instr_Fdiv: { 
					// Don't fold if we divide by 0.
                    if(FA::IsZero(floatConstB)) {
                        return nullptr;
                    }

					result = FA::Fdiv(floatConstA, floatConstB); 
					break;
				}
			}

			return irGen_->GetFloatingConst(floatConstA->GetType(), result);
		}
	}

    // a / a -> 1, if 'a' is not zero.
    // 0 / a -> 0, if 'a' is not zero.
    if((opcode == Instr_Div) || (opcode == Instr_Udiv)) {
        if((opA == opB) && IsNotZero(opA, block)) {
            return GetOneInt(opA);
        }
        else if(IsZero(opA, block) && IsNotZero(opB, block)) {
            return GetZeroInt(opA);
        }
    }

	// Try to fold operations involving undefined operands.
	if(auto result = HandleArithmeticUndef(opcode, opA, opB, mode)) {
		return result;
	}

	// If one of the operands is a constant try to simplify.
	if(auto result = HandleArithmeticOneConst(opcode, opA, opB, mode, block)) {
		return result;
	}

    // Test for a operation on a 'quest' instruction.
    if(auto questInstr = opA->DefiningInstrAs<QuestionInstr>()) {
        if(auto intConst = opB->As<IntConstant>()) {
            if(auto result = HandleOperatorOnQuestion(opcode, intConst, questInstr)) {
                return result;
            }
        }
    }

	// Handle pointer subtraction, like '(char*)&a[4] - (char*)&a[2]'.
	return HandlePointerSub(opcode, opA, opB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandleArithmeticUndef(Opcode opcode, Operand* opA, 
											   Operand* opB, FloatMode mode) {
	// If none of the operands is undefined we have nothing to do.
	if((opA->IsUndefinedConstant() || 
        opB->IsUndefinedConstant()) == false) {
		return false;
	}

	switch(opcode) {
		case Instr_Add:
		case Instr_Fadd: {
			// a + undef -> undef
			return GetUndefined(opA);
		}
		case Instr_Sub:
		case Instr_Fsub: {
			// a - undef -> undef
			return GetUndefined(opA);
		}
		case Instr_Mul:
		case Instr_Fmul: {
			// a * undef -> undef
			return GetUndefined(opA);
		}
		case Instr_Div:
		case Instr_Udiv:
		case Instr_Mod:
		case Instr_Umod: {
			if(opA->IsUndefinedConstant()) {
				// undef / a -> 0, because we can presume 'undef' to be zero.
				return opA;
			}
			else {
				// a / undef -> undef
				return opB;
			}
		}
		case Instr_Fdiv: {
			if(opA->IsUndefinedConstant()) {
				// undef / a -> undef
				return opA;
			}
			break;
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandleArithmeticOneConst(Opcode opcode, Operand* opA, 
												  Operand* opB, FloatMode mode,
                                                  Block* block) {
    auto intConstB = AsIntConstant(opB, block);

    if(intConstB && intConstB->IsZero()) {
		switch(opcode) {
			case Instr_Add: {
				// a + 0 -> a
				return opA;
			}
			case Instr_Mul: {
				// a * 0 -> 0
				return GetZeroInt(opA);
			}
			case Instr_Div:
			case Instr_Udiv:
			case Instr_Mod:
			case Instr_Umod: {
				// We can presume it's undefined behavior dividing by 0.
				return GetUndefined(opA);
			}

		}
	}
	else if(MatchFloat(0.0)(opB)) {
		switch(opcode) {
			case Instr_Fadd: {
				// a + 0 -> a, if a != NaN, Infinity.
				auto floatA = opA->As<FloatConstant>();

				if(FA::IsNaN(floatA) || FA::IsInfinity(floatA)) {
					return nullptr;
				}
				else return opA;
			}
			case Instr_Fmul: {
				// a * 0 -> 0, if a != NaN, Infinity, or if the
				// 'FP_Exact' flag is not set on the operation.
				auto floatA = opA->As<FloatConstant>();

				if((FA::IsNaN(floatA) || FA::IsInfinity(floatA)) &&
				   (mode == FP_Exact)) {
					return nullptr;
				}
				else return GetZeroFloat(opA);
			}
			case Instr_Fdiv: {
				// We can presume it's undefined behavior.
				return GetUndefined(opA);
			}
		}
	}
	
    auto intConstA = AsIntConstant(opA, block);

    if(intConstA && intConstA->IsZero()) {
		// 0 / a -> 0
		switch(opcode) {
			case Instr_Div:
			case Instr_Udiv:
			case Instr_Mod: 
			case Instr_Umod: return GetZeroInt(opA);
		}
	}
	else if(MatchFloat(0.0)(opA)) {
		switch(opcode) {
			case Instr_Fdiv: {
				// 0 / a -> 0
				// Don't fold if 'a' is NaN or Infinity.
				auto floatB = opB->As<FloatConstant>();

				if(FA::IsNaN(floatB) || FA::IsInfinity(floatB)) {
					return nullptr;
				}
				else return GetZeroFloat(opA);
			}
		}
	}
    
    if(intConstA && intConstA->IsOne()) {
        switch(opcode) {
            case Instr_Mod: {
                // a % 1 -> 0
                return GetZeroInt(opA);
            }
        }
    }
    else if(intConstA && intConstA->IsMinusOne()) {
        switch(opcode) {
            case Instr_Mod: {
                // a % -1 -> 0
                return GetZeroInt(opA);
            }
            case Instr_Shr: {
                // -1 >> a -> -1
                return GetMinusOneInt(opA);
            }
        }
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandlePointerSub(Opcode opcode, Operand* opA, Operand* opB) {
	// Test for expressions like '&a[4] - &a[2]', which can be folded now.
	// We can do this safely folding because pointer arithmetic doesn't overflow.
	Operand* baseA = nullptr;
	Operand* baseB = nullptr;
	IntConstant* indexOpA;
	IntConstant* indexOpB;
	bool isAddr = false;
	bool isElem = false;

	// If the instruction is not 'sub' there is nothing to do.
	if(opcode != Instr_Sub) {
        return nullptr;
    }

	// Test for array element address subtraction.
	// Example in C: '&a[4] - &a[2]' or '(char*)&a[4] - (char*)&a[2]'
	// sub (ptoi (index a, INDEX_A), int32), (ptoi (index b, INDEX_B), int32)
	if(MatchConversion<PtoiInstr>(Match<IndexInstr>(
	   MatchAny(&baseA), MatchIC(&indexOpA)))(opA) &&
	   MatchConversion<PtoiInstr>(Match<IndexInstr>(
	   MatchAny(&baseB), MatchIC(&indexOpB)))(opB)) {}

	// sub (ptoi (ptop (index a, INDEX_A, int8*), int32), 
    // and similar for the right side.
	else if(MatchConversion<PtoiInstr>(MatchConversion<PtopInstr>(Match<IndexInstr>(
		    MatchAny(&baseA), MatchIC(&indexOpA))))(opA) &&
		    MatchConversion<PtoiInstr>(MatchConversion<PtopInstr>(Match<IndexInstr>(
		    MatchAny(&baseB), MatchIC(&indexOpB))))(opB)) {}

	// Test for arbitrary address subtraction.
	// Example in C: '(p + 5) - (p + 2)', where 'p' is a pointer.
	// sub (ptoi (addr a, INDEX_A), int32), (ptoi (addr b, INDEX_B), int32)
	else if(MatchConversion<PtoiInstr>(Match<AddressInstr>(
		    MatchAny(&baseA), MatchIC(&indexOpA)))(opA) &&
		    MatchConversion<PtoiInstr>(Match<AddressInstr>(
		    MatchAny(&baseB), MatchIC(&indexOpB)))(opB)) {
		isAddr = true;
	}

	// Test for record element address subtraction.
	// Example in C: (char*)&a.y - (char*)&a.x
	// sub (ptoi (ptop (elem a, INDEX_A, int8*), int32), 
    // and similar for the right side.
	else if(MatchConversion<PtoiInstr>(Match<ElementInstr>(
		    MatchAny(&baseA), MatchIC(&indexOpA)))(opA) &&
		    MatchConversion<PtoiInstr>(Match<ElementInstr>(
		    MatchAny(&baseB), MatchIC(&indexOpB)))(opB)) {
		isElem = true;
	}

	// If we couldn't match anything give up.
	// The base operands need to refer to the same temporary
    // or to the same variable.
	if((baseA && baseB) == false) {
        return nullptr;
    }

	if(baseA != baseB) {
        return nullptr;
    }

	// In case we have variable reference, we want to work
    // with the actual variables.
	const Type* baseType;
	
	if(auto variableRefA = baseA->As<VariableReference>()) {
		baseType = variableRefA->GetVariable()->GetType();
	}
	else baseType = baseA->GetType();

	// Use the index values to obtain the distance between 
    // the array elements or the offset from the record type.
	__int64 indexA = indexOpA->Value();
	__int64 indexB = indexOpB->Value();
	__int64 result;

	if(isElem) {
		// Compute the offset difference of the two fields.
		auto recordType = baseType->As<RecordType>();
		__int64 offsetA = recordType->Fields()[indexA].FieldOffset;
		__int64 offsetB = recordType->Fields()[indexB].FieldOffset;
		result = offsetA - offsetB;
	}
	else if(isAddr) {
		// Compute the difference between two addresses.
		auto pointerType = baseType->As<PointerType>();
		__int64 pointeeSize = TypeInfo::GetSize(pointerType->PointeeType(), target_);
		result = (indexA - indexB) * pointeeSize;
	}
	else {
		// Compute the difference between the two array elements.
		auto arrayType = baseType->As<ArrayType>();
		__int64 elemSize = TypeInfo::GetSize(arrayType->ElementType(), target_);
		result = (indexA - indexB) * elemSize;
	}

	return irGen_->GetIntConst(opA->GetType(), result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandleLogical(Opcode opcode, Operand* opA, Operand* opB,
                                       Block* block) {
	// If both operands are constant we can evaluate them.
    auto intConstA = AsIntConstant(opA, block);
    auto intConstB = AsIntConstant(opB, block);

	if(intConstA && intConstB) {
		const IntegerType* type = intConstA->GetType();
		IRIntegerKind kind = type->GetSubtype();
		__int64 result;

		// Perform the operation; note that we don't care about overflow here.
		switch(opcode) {
			case Instr_And: { result = IA::And(intConstA, intConstB); break; }
			case Instr_Or:  { result = IA::Or(intConstA, intConstB);  break; }
			case Instr_Xor: { result = IA::Xor(intConstA, intConstB); break; }
			case Instr_Shl: {
				// Shifting by more than the bitwidth is undefined.
				if(IA::IsLarger(intConstB->Value(), IA::TypeBits(type), kind)) {
					return GetUndefined(opA);
				}
				else result = IA::Shl(intConstA, intConstB);
				break;
			}
			case Instr_Shr: {
				// Shifting by more than the bitwidth is undefined.
				if(IA::IsLarger(intConstB->Value(), IA::TypeBits(type), kind)) {
					return GetUndefined(opA);
				}
				else result = IA::Shr(intConstA, intConstB);
				break;
			}
			case Instr_Ushr: {
				// Shifting by more than the bitwidth is undefined.
				if(IA::IsLarger(intConstB->Value(), IA::TypeBits(type), kind)) {
					return GetUndefined(opA);
				}
				else result = IA::Ushr(intConstA, intConstB);
				break;
			}
		}

		return irGen_->GetIntConst(type, result);
	}
    else if(intConstA) {
        // Move the left constant to the right, simplifies some tests.
		// Note that we can do this only for commutative operators.
		if(Instruction::IsCommutative(opcode)) {
			SwapOperands(opA, opB);
		}
    }

	// Try to fold operations involving undefined operands.
	if(auto result = HandleLogicalUndef(opcode, opA, opB)) {
		return result;
	}

    // Test for a operation on a 'quest' instruction.
    // For commutative operators it doesn't matter where the 'quest' is.
    if(opB->DefiningInstrIs<QuestionInstr>() &&
       (opcode == Instr_And) || (opcode == Instr_Or) || (opcode == Instr_Xor)) {
        std::swap(opA, opB);
    }

    if(auto questInstr = opA->DefiningInstrAs<QuestionInstr>()) {
        if(auto intConst = opB->As<IntConstant>()) {
            if(auto result = HandleOperatorOnQuestion(opcode, intConst, questInstr)) {
                return result;
            }
        }
    }

	// If one of the operands is a constant try to simplify.
	return HandleLogicalOneConst(opcode, opA, opB, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandleLogicalUndef(Opcode opcode, Operand* opA, Operand* opB) {
	// If none of the operands is undefined we have nothing to do.
	if((opA->IsUndefinedConstant() || 
        opB->IsUndefinedConstant()) == false) {
		return false;
	}

	switch(opcode) {
		case Instr_And: {
			if(opA->IsUndefinedConstant() && opB->IsUndefinedConstant()) {
				// undef & undef -> undef
				return opA;
			}
			else {
				// undef & a -> 0, because we may presume that 'a' is 0.
				return GetZeroInt(opA);
			}
			
		}
		case Instr_Or: {
			if(opA->IsUndefinedConstant() && opB->IsUndefinedConstant()) {
				// undef & undef -> undef
				return opA;
			}
			else {
				// a | undef -> -1, because we may presume that 'a' is -1.
				return GetMinusOneInt(opA);
			}
		}
		case Instr_Xor: {
			// undef ^ undef -> undef, because 'undef' may not be the same.
			// Note that we don't do the above because people write things like
			// 'int a; a = a ^ a;' to set 'a' to 0, and using 'undef' in this case
			// would break the program.
			return GetZeroInt(opA);
		}
		case Instr_Shl: {
			if(opA->IsUndefinedConstant() && opB->IsUndefinedConstant()) {
				// undef << undef -> undef
				return opA;
			}
			else {
				// undef << a -> 0, because we may presume 'undef' is 0.
				// a << undef -> 0, because we may presume 'undef' is very large.
				return GetZeroInt(opA);
			}
		}
		case Instr_Shr:
		case Instr_Ushr: {
			if(opA->IsUndefinedConstant() && opB->IsUndefinedConstant()) {
				// undef << undef -> undef
				return opA;
			}
			else if(opcode == Instr_Shr) {
				// undef >> a -> -1, because we may presume 'undef' is -1.
				// a >> undef -> -1, because we may presume 'undef' is very large.
				return GetMinusOneInt(opA);
			}
			else {
				// undef >> a -> 0, because we may presume 'undef' is 0.
				// a >> undef -> 0, because we may presume 'undef' is very large.
				return GetZeroInt(opA);
			}
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandleLogicalOneConst(Opcode opcode, Operand* opA, 
                                               Operand* opB, Block* block) {
    auto intConstB = AsIntConstant(opB, block);

    if(intConstB && intConstB->IsZero()) {
		switch(opcode) {
			case Instr_And: { 
				// a & 0 -> 0
				return GetZeroInt(opA); 
			}
		}
	}
	
    auto intConstA = AsIntConstant(opA, block);
    
    if(intConstA && intConstA->IsZero()) {
		switch(opcode) {
			case Instr_Shl:
			case Instr_Shr:
			case Instr_Ushr: {
				// 0 << a -> 0
				// 0 >> a -> 0, both signed and unsigned
				return GetZeroInt(opA); 
			}
		}
	}
	else if(intConstB) {
		IRIntegerKind kind = opA->GetType()->As<IntegerType>()->GetSubtype();

		switch(opcode) {
			case Instr_Shl:
			case Instr_Ushr: {
				// a << N -> 0, if N > bitwidth
				// a >> N -> 0, if N > bitwidth, unsigned >>
				if(IA::IsLarger(intConstB->Value(), IA::TypeBits(kind), kind)) {
					return GetZeroInt(opA);
				}
				break;
			}
			case Instr_Shr: {
				// a >> N -> -1, N > bitwidth, signed >>
				if(IA::IsLarger(intConstB->Value(), IA::TypeBits(kind), kind)) {
					return GetMinusOneInt(opA);
				}
				break;
			}
			case Instr_And: {
				if(opA->HasDefiningInstruction()) {
					// Try to fold an instruction 
					if(auto result = HandleAndOnInstr(opA, intConstB)) {
						return result;
					}
				}
				break;
			}
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleAndOnInstr(Operand* opA, IntConstant* intConst) {
	auto ptoiInstr = opA->DefiningInstrAs<PtoiInstr>();
	
    if(ptoiInstr == nullptr) {
        return nullptr;
    }

	// Test for a 'pointer to int' conversion of a global variable
	// that is masked with a constant. This works only if the global
	// variable has an alignment specified. For example,
	// (ptoi &a, int32) & 2 -> 0, because 'int32' has alignment 4.
	if(auto variableRef = ptoiInstr->TargetOp()->As<VariableReference>()) {
		if(variableRef->IsGlobal() && variableRef->GetVariable()->HasAlignment()) {
			__int64 alignment = variableRef->GetVariable()->Alignment();

			if(alignment < 2) {
                return nullptr;
            }

			const IntegerType* constType = intConst->GetType();
			__int64 constMask = IA::GetMinusOneMask(constType);
			__int64 constValue = intConst->Value() & constMask;

			if((constValue & alignment) == constValue) {
				// We can make the simplification.
				return GetZeroInt(constType);
			}
		}
	}

	return nullptr;
}

} // namespace Analysis