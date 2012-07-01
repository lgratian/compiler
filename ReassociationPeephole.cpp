// ReassociationPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements peephole optimizations based on the reassociation of expressions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

Operand* Peephole::HandleAssociative(Opcode opcode, Operand* opA, Operand* opB, 
                                     Block* block, FloatMode mode) {
    // OP is a generic operator (can be +, -, *, /, &, |, ^).
    // For floating point instructions we do this reassociation only if
    // the "fast" mode is activated.
    Operand* a;
    Operand* b;
    Operand* c;

    // (a OP b) OP c -> a OP (b OP c), if it simplifies
    // a OP (b OP c) -> (a OP b) OP c, if it simplifies
    // These rules work even if the operator is not commutative. Examples:
    // (a + 3) + 5 -> a + (3 + 5) -> a + 8
    // 5 | (2 | a) -> (5 | 2) | a -> 7 | a
    if(Instruction::IsAssociative(opcode) &&
       ((Instruction::IsFloatArithmetic(opcode) == false) || 
        (mode == FP_Fast))) {
        // (a OP b) OP c -> a OP (b OP c)
        auto arithInstr1 = opA->DefiningInstrAs<ArithmeticInstr>();

        if(arithInstr1) {
			if(arithInstr1->GetOpcode() != opcode) {
                return nullptr;
            }

			if(arithInstr1->IsFloatArithmetic() && (mode != FP_Fast)) {
				// The second instruction must also be marked as "fast".
				return nullptr;
			}
       
			a = arithInstr1->LeftOp();
			b = arithInstr1->RightOp();
			c = opB;

			if(auto result = SimplifyBinary(opcode, b, c,  block, mode)) {
				// Form the new instruction 'a OP result'.
                return LOG(CreateBinaryInstruction(opcode, a, result, 
                                                   result->GetType(), 
                                                   nullptr, nullptr, mode));
			}
		}
        
        // Test for a logical instruction.
        auto logicalInstr1 = opA->DefiningInstrAs<LogicalInstr>();

        if(logicalInstr1) {
			if(logicalInstr1->GetOpcode() != opcode) {
                return nullptr;
            }

			a = logicalInstr1->LeftOp();
			b = logicalInstr1->RightOp();
			c = opB;

			if(auto result = SimplifyBinary(opcode, b, c, block, mode)) {
				// Form the new instruction 'a OP result'.
				return LOG(CreateBinaryInstruction(opcode, a, result, 
                                                   result->GetType(),
                                                   nullptr, nullptr, mode));
			}
		}

        // a OP (b OP c) -> (a OP b) OP c
        auto arithInstr2 = opB->DefiningInstrAs<ArithmeticInstr>();

        if(arithInstr2) {
			if(arithInstr2->GetOpcode() != opcode) {
                return nullptr;
            }

			if(arithInstr2->IsFloatArithmetic() && (mode != FP_Fast)) {
				// The second instruction must also be marked as "fast".
				return nullptr;
			}

			a = opA;
			b = arithInstr2->LeftOp();
			c = arithInstr2->RightOp();

			if(auto result = SimplifyBinary(opcode, a,b, block, mode)) {
				// Form the new instruction 'result OP c'.
				return LOG(CreateBinaryInstruction(opcode, result, c, 
                                                   result->GetType(), 
                                                   nullptr, nullptr, mode));
			}
		}

        // Test for a logical instruction.
        auto logicalInstr2 = opB->DefiningInstrAs<LogicalInstr>();

        if(logicalInstr2) {
			if(logicalInstr2->GetOpcode() != opcode) {
                return nullptr;
            }

			a = opA;
			b = logicalInstr2->LeftOp();
			c = logicalInstr2->RightOp();

			if(auto result = SimplifyBinary(opcode, a, b, block, mode)) {
				// Form the new instruction 'result OP c'.
				return LOG(CreateBinaryInstruction(opcode, result, c, 
                                                   result->GetType(), 
                                                   nullptr, nullptr, mode));
			}
		}
    }

    // If the instruction is also commutative we can try 
    // some more simplifications.
    if(Instruction::IsAssociative(opcode) && 
       Instruction::IsCommutative(opcode) &&
       ((Instruction::IsFloatArithmetic(opcode) == false) || 
        (mode == FP_Fast))) {
        return HandleAssociativeAndCommutative(opcode, opA, opB, block, mode);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAssociativeAndCommutative(Opcode opcode, Operand* opA, 
                                                   Operand* opB, Block* block,
                                                   FloatMode mode) {
    Operand* a;
    Operand* b;
    Operand* c;

    // OP is a generic operator (can be +, -, *, /, &, |, ^).
    // (a OP b) OP c -> (c OP a) OP b, if 'c OP a' simplifies.
    // (2 * b) * 3 -> (3 * 2) * b -> 6 * b
    auto arithInstr1 = opA->DefiningInstrAs<ArithmeticInstr>();

    if((arithInstr1 == nullptr) ||
       (arithInstr1->GetOpcode() != opcode)) {
        return nullptr;
    }

    if(arithInstr1->IsFloatArithmetic() && (mode != FP_Fast)) {
        // The second instruction must also be marked as "fast".
        return nullptr;
    }

    a = arithInstr1->LeftOp();
    b = arithInstr1->RightOp();
    c = opB;

    if(auto result = SimplifyBinary(opcode, c, a, block, mode)) {
        // For the new instruction 'result OP b'.
        return LOG(CreateBinaryInstruction(opcode, result, b, result->GetType(),
                                           nullptr, nullptr, mode));
    }

    // Test for a logical instruction.
    auto logicalInstr1 = opA->DefiningInstrAs<LogicalInstr>();

    if((logicalInstr1 == nullptr) ||
       (logicalInstr1->GetOpcode() != opcode)) {
        return nullptr;
    }

    a = logicalInstr1->LeftOp();
    b = logicalInstr1->RightOp();
    c = opB;

    if(auto result = SimplifyBinary(opcode, c, a, block, mode)) {
        // For the new instruction 'result OP b'.
        return LOG(CreateBinaryInstruction(opcode, result, b, 
                                           result->GetType(), 
                                           nullptr, nullptr, mode));
    }


    // a OP (b OP C) -> (a OP b) OP c
    // 3 & (5 & c) -> (3 & 5) & c -> 1 & c
    auto arithInstr2 = opB->DefiningInstrAs<ArithmeticInstr>();

    if((arithInstr2 == nullptr) ||
       (arithInstr2->GetOpcode() != opcode)) {
        return nullptr;
    }

    if(arithInstr2->IsFloatArithmetic() && (mode != FP_Fast)) {
        // The second instruction must also be marked as "fast".
        return nullptr;
    }

    a = opA;
    b = arithInstr2->LeftOp();
    c = arithInstr2->RightOp();

    if(auto result = SimplifyBinary(opcode, a, b, block, mode)) {
        // For the new instruction 'result OP c'.
        return LOG(CreateBinaryInstruction(opcode, result, c, 
                                           result->GetType(), 
                                           nullptr, nullptr, mode));
    }

    // Test for a logical instruction.
    auto logicalInstr2 = opB->DefiningInstrAs<LogicalInstr>();

    if((logicalInstr2 == nullptr) ||
       (logicalInstr2->GetOpcode() != opcode)) {
        return nullptr;
    }

    a = opA;
    b = logicalInstr2->LeftOp();
    c = logicalInstr2->RightOp();

    if(auto result = SimplifyBinary(opcode, a, b, block, mode)) {
        // For the new instruction 'result OP c'.
        return LOG(CreateBinaryInstruction(opcode, result, c,
                                           result->GetType(), 
                                           nullptr, nullptr, mode));
    }

    // (a OP C1) OP (b OP C2) -> (a OP b) OP (C1 OP C2)
    // (a * 5) * (b * 4) -> (a * b) * (5 * 4) -> (a * b) * 20
    // Note that we don't do this transformation if 'a OP C1' and 'b OP C2'
    // are used in more than one place, because it would increase register pressure.
    if((opA->HasSingleUser() == false) || 
       (opB->HasSingleUser() == false)) {
        return nullptr;
    }

    Instruction* instrA = opA->DefiningInstrAs<ArithmeticInstr>();
    Instruction* instrB = opB->DefiningInstrAs<ArithmeticInstr>();

    if((instrA && instrB) == false) {
        // Check if we have logical instruction otherwise.
        instrA = opA->DefiningInstrAs<LogicalInstr>();
        instrB = opB->DefiningInstrAs<LogicalInstr>();
        if((instrA && instrB) == false) {
            return nullptr;
        }
    }

    if((instrA->GetOpcode() != opcode) || 
       (instrB->GetOpcode() != opcode)) {
        return nullptr;
    }

    // Both right operands need to be constants.
    auto C1 = instrA->GetSourceOp(1)->As<IntConstant>();
    auto C2 = instrB->GetSourceOp(1)->As<IntConstant>();

    if((C1 && C2) == false) {
        return nullptr;
    }

    // We evaluate the constant directly.
    Operand* constantOp = folder_.FoldBinary(opcode, C1, C2, block, mode);

    auto leftOp = CreateBinaryInstruction(opcode, instrA->GetSourceOp(0), 
                                          instrB->GetSourceOp(0), 
                                          instrA->GetSourceOp(0)->GetType(),
                                          nullptr, nullptr, mode);
    return LOG(CreateBinaryInstruction(opcode, leftOp, constantOp,
                                       leftOp->GetType(),
                                       nullptr, nullptr, mode));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleDistributive(Opcode opcode, Operand* opA, Operand* opB, 
                                      Block* block, FloatMode mode) {
    // Try to simplify expressions of the form '(a OP1 b) OP2 (c OP1 d)'.
    // For example, '(a * b) + (a * c) -> a * (b + c)'.
    // Note that we don't do these transformations for integer division,
    // because too many conditions need to be checked (including overflow).
    Instruction* instrA = opA->DefiningInstrAs<ArithmeticInstr>();
    Instruction* instrB = opB->DefiningInstrAs<ArithmeticInstr>();

    if(instrA && instrB && 
       Instruction::IsFloatArithmetic(opcode)) {
        // If we have a floating-point instruction we can continue
        // only if all the instructions involved are marked "fast".
        if(mode != FP_Fast) {
            return nullptr;
        }

        auto arithInstrA = instrA->As<ArithmeticInstr>();
        auto arithInstrB = instrB->As<ArithmeticInstr>();

        if((arithInstrA->GetFPMode() != FP_Fast) ||
           (arithInstrB->GetFPMode() != FP_Fast)) {
            return nullptr;
        }
    }

    if((instrA && instrB) == false) {
        // Check if we have logical instruction otherwise.
        instrA = opA->DefiningInstrAs<LogicalInstr>();
        instrB = opB->DefiningInstrAs<LogicalInstr>();
    }

    // Special case for floating-point division with constants.
    // Note that we already know that we use "fast" float computation mode.
    // (a / C1) +- (a / C2) -> a * (1 / C1 +- 1 / C2).
    if(((opcode == Instr_Fadd) || (opcode == Instr_Fsub)) &&
       (instrA && instrB) && instrA->SameKind(instrB) &&
       (instrA->GetOpcode() == Instr_Fdiv)) {
        auto C1 = instrA->GetSourceOp(1)->As<FloatConstant>();
        auto C2 = instrB->GetSourceOp(1)->As<FloatConstant>();
        
        if(C1 && C2 && (instrA->GetSourceOp(0) == instrB->GetSourceOp(0))) {
            auto floatKind = C1->GetType()->GetSubtype();
            double R1 = FA::Fdiv(1.0, C1->Value(), floatKind);
            double R2 = FA::Fdiv(1.0, C2->Value(), floatKind);
            
            double R  = opcode == Instr_Fadd ? FA::Fadd(R1, R2, floatKind) :
                                               FA::Fsub(R1, R2, floatKind);

            auto constantOp = irGen_.GetFloatingConst(C1->GetType(), R);
            auto mulOp = irGen_.GetTemporary(C1->GetType());
            irGen_.GetFmul(instrA->GetSourceOp(0), constantOp, mulOp);
            return LOG(mulOp);
        }
    }

    if((instrA && instrB) && 
       (instrA->GetOpcode() == instrB->GetOpcode())) {
        // Don't do it for integer division.
        if(instrA->IsDiv() || instrA->IsUdiv()) {
            return nullptr;
        }

        //  a     b       c     d
        // (a OP1 b) OP2 (a OP1 c) -> a OP1 (b OP2 c), if 'OP1' distributes over 'OP2'
        // (b OP1 a) OP2 (a OP1 c) -> a OP1 (b OP2 c), if 'OP2' is also commutative.
        if(Instruction::IsDistributive(instrA->GetOpcode(), opcode) == false) {
            return nullptr; // No reason to continue.
        }

        Operand* a = instrA->GetSourceOp(0);
        Operand* b = instrA->GetSourceOp(1);
        Operand* c = instrB->GetSourceOp(0);
        Operand* d = instrB->GetSourceOp(1);
        bool valid = false;

        if(a == c) valid = true;
        else if((b == c) && Instruction::IsCommutative(instrA->GetOpcode())) {
            // We have a commutative operator in the subexpression.
            // Invert 'a' and 'b' so we have a single case to handle below.
            auto temp = a;
            a = b;
            b = temp;
            valid = true; 
        }

        if(valid) {
            // a OP1 (b OP2 d)
			if(auto result = SimplifyBinary(opcode, b, d, block, mode)) {
				return LOG(CreateBinaryInstruction(instrA->GetOpcode(), a, result, 
                                                   result->GetType(), nullptr, 
                                                   nullptr, mode));
			}
			else return nullptr;
        }

        //  a     b       c     d
        // (a OP1 b) OP2 (c OP1 b) -> b OP1 (a OP2 c), if 'OP1' distributes over 'OP2'
        // (a OP1 b) OP2 (b OP1 c) -> b OP1 (a OP2 c), if 'OP2' is also commutative.
        if(b == d) valid = true;
        else if((b == c) && Instruction::IsCommutative(instrA->GetOpcode())) {
            // We have a commutative operator in the subexpression.
            // Invert 'c' and 'd' so we have a single case to handle below.
            auto temp = c;
            c = d;
            d = c;
            valid = true; 
        }

        if(valid) {
			if(auto result = SimplifyBinary(opcode, a, c, block, mode)) {
				return LOG(CreateBinaryInstruction(instrA->GetOpcode(), b, result,
                                                   result->GetType(), nullptr, 
                                                   nullptr, mode));
			}
			else return nullptr;            
        }
    }

    // Try to expand an expression of the form 'a OP1 (b OP2 c)', but only if
    // this leads to further simplification, and if the instructions are
    // not used more than once (else we would increase register pressure).
    // a OP1 (b OP2 c) -> (a OP1 b) OP2 (a OP1 c)
    if(instrB && Instruction::IsDistributive(opcode, instrB->GetOpcode())) {
        // Don't do it for integer division.
        if((opcode == Instr_Div) || (opcode == Instr_Udiv)) {
            return nullptr;
        }
        
        // OP1 distributes over OP2, try to simplify.
        Operand* a = opA;
        Operand* b = instrB->GetSourceOp(0);
        Operand* c = instrB->GetSourceOp(1);

        if(auto result1 = SimplifyBinary(opcode, a, b, block, mode)) {
            if(auto result2 = SimplifyBinary(opcode, a, c, block, mode)) {
                // Create the new instruction.
                return LOG(CreateBinaryInstruction(instrB->GetOpcode(), result1,
                                                   result2, result1->GetType(), 
                                                   nullptr, nullptr, mode));
            }
        }
    }

    // (a OP1 b) OP2 c -> (a OP2 c) OP1 (b OP2 c)
    if(instrA && Instruction::IsDistributive(opcode, instrA->GetOpcode())) {
        // Don't do it for integer division.
        if((opcode == Instr_Div) || (opcode == Instr_Udiv)) {
            return nullptr;
        }

        Operand* a = instrA->GetSourceOp(0);
        Operand* b = instrA->GetSourceOp(1);
        Operand* c = opB;

        if(auto result1 = SimplifyBinary(opcode, a, c, block, mode)) {
            if(auto result2 = SimplifyBinary(opcode, b, c, block, mode)) {
                // Create the new instruction.
                return LOG(CreateBinaryInstruction(instrA->GetOpcode(), result1, 
                                                   result2, result1->GetType(), 
                                                   nullptr, nullptr, mode));
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleSubAssociative(Operand* opA, Operand* opB, Block* block) {
	// The following simplification using association are performed only
	// if the whole expression can be simplified.
	Operand* a;
	Operand* b;
	Operand* c;

	// (a + b) - c -> a + (b - c) / b + (a - c), 
    // if 'b - c'/'a - c' can be simplified.
	// For example, if 'b == c' only 'a' remains.
	if(Match<AddInstr>(MatchAny(&a), MatchAny(&b))(opA) && 
       MatchAny(&c)(opB)) {
		// Try to simplify 'b - c'.
		if(auto result1 = SimplifyBinary(Instr_Sub, b, c, block)) {
			if(auto result2 = SimplifyBinary(Instr_Add, a, result1, block)) {
				// The operands can be simplified.
				return LOG(result2);
			}

			if(opA->HasSingleUser()) {
				auto addOp = GetTemporary(result1);
				irGen_.GetAdd(result1, a, addOp);
				return addOp;
			}
			else return nullptr;
		}

		// Try to simplify 'a - c'.
		if(auto result1 = SimplifyBinary(Instr_Sub, a, c, block)) {
			if(auto result2 = SimplifyBinary(Instr_Add, b, result1, block)) {
				// The operands can be simplified.
				return LOG(result2);
			}

			if(opA->HasSingleUser()) {
				auto addOp = GetTemporary(result1);
				irGen_.GetAdd(result1, b, addOp);
				return LOG(addOp);
			}
			else return nullptr;
		}
	}

	// a - (b + c) -> (a - b) - c / (a - c) - b, 
    // if 'a - b'/'a - c' can be simplified.
	if(Match<AddInstr>(MatchAny(&b), MatchAny(&c))(opB) && 
       MatchAny(&a)(opA)) {
		// Try to simplify 'a - b'.
		if(auto result1 = SimplifyBinary(Instr_Sub, a, b, block)) {
			if(auto result2 = SimplifyBinary(Instr_Sub, result1, c, block)) {
				// The operands can be simplified.
				return LOG(result2);
			}

			if(opB->HasSingleUser()) {
				auto subOp = GetTemporary(result1);
				irGen_.GetSub(result1, c, subOp);
				return LOG(subOp);
			}
			else return nullptr;
		}

		// Try to simplify 'a - c'.
		if(auto result1 = SimplifyBinary(Instr_Sub, a, c, block)) {
			if(auto result2 = SimplifyBinary(Instr_Sub, result1, b, block)) {
				// The operands can be simplified.
				return LOG(result2);
			}

			if(opB->HasSingleUser()) {
				auto subOp = GetTemporary(result1);
				irGen_.GetSub(result1, b, subOp);
				return LOG(subOp);
			}
			else return nullptr;
		}
	}

	// c - (a - b) -> (c - a) + b / (b - a) + c, 
    // if 'c - a'/'b - a' can be simplified.
	if(Match<SubInstr>(MatchAny(&a), MatchAny(&b))(opB) &&
       MatchAny(&c)(opA)) {
		// Try to simplify 'c - a'.
		if(auto result1 = SimplifyBinary(Instr_Sub, c, a, block)) {
			if(auto result2 = SimplifyBinary(Instr_Add, result1, b, block)) {
				// The operands can be simplified.
				return LOG(result2);
			}

			if(opB->HasSingleUser()) {
				auto addOp = GetTemporary(result1);
				irGen_.GetAdd(result1, b, addOp);
				return LOG(addOp);
			}
			else return nullptr;
		}

		// Try to simplify 'b - a'.
		if(auto result1 = SimplifyBinary(Instr_Sub, b, a, block)) {
			if(auto result2 = SimplifyBinary(Instr_Add, result1, c, block)) {
				// The operands can be simplified.
				return result2;
			}

			if(opB->HasSingleUser()) {
				auto addOp = GetTemporary(result1);
				irGen_.GetAdd(result1, c, addOp);
				return LOG(addOp);
			}
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleAddDistributive(Operand* opA, Operand* opB, Block* block) {
    // Test for things like '(a * b) + (c * d)', where an operand 
    // from the left side of the 'add' is equal to one from the right side.
	Operand* a;
	Operand* b;
	Operand* c;
	Operand* d;

	// We do these simplifications only if the operands have a single user,
	// otherwise more operations could be produced than when we started.
	if(opA->HasDefiningInstruction() && 
       (opA->HasSingleUser() == false)) {
		return nullptr;
	}
	
	if(opB->HasDefiningInstruction() && 
       (opB->HasDefiningInstruction() == false)) {
		return nullptr;
	}

	if(Match<MulInstr>(MatchAny(&a), MatchAny(&b))(opA) &&
	   Match<MulInstr>(MatchAny(&c), MatchAny(&d))(opB)) {
		Operand* mulOp = nullptr;  // The left operand that is multiplied.
		Operand* addOp1 = nullptr; // The left operand that is added.
		Operand* addOp2 = nullptr; // The right operand that is added.

		// Test for all the possible combinations that could 
        // lead to a simplification.
		if(a == c) {
			// (a * b) + (c * d) -> (a * b) + (a * d) -> a * (b + d)
			mulOp = a;
			addOp1 = b;
			addOp2 = d;
		}
		else if(a == d) {
			// (a * b) + (c * d) -> (a * b) + (c * a) -> a * (b + c)
			mulOp = a;
			addOp1 = b;
			addOp2 = c;
		}
		else if(b == c) {
			// (a * b) + (c * d) -> (a * b) + (b * d) -> b * (a + d)
			mulOp = b;
			addOp1 = a;
			addOp2 = d;
		}
		else if(b == d) {
			// (a * b) + (c * d) -> (a * b) + (c * b) -> b * (a + c);
			mulOp = b;
			addOp1 = a;
			addOp2 = c;
		}

		if(mulOp) {
			// We can do the simplification.
			auto addOp = GetTemporary(addOp1);
			irGen_.GetAdd(addOp1, addOp2, addOp);
			auto temp = GetTemporary(mulOp);
			irGen_.GetMul(mulOp, addOp, temp);
			return LOG(temp);
		}
	}

	// Test for some special cases in which a constant is involved.
	// (a * C) + a -> a * (C + 1) - constant folded
	IntConstant* C;
	bool matched = false;

	if(Match<MulInstr>(MatchAny(&a), MatchIC(&C))(opA)) {
		matched = a == opB; // opA: a * C
	}
	else if(Match<MulInstr>(MatchIC(&C), MatchAny(&a))(opA)) {
		matched = a == opB; // opA: C * a
	}
	else if(Match<MulInstr>(MatchAny(&a), MatchIC(&C))(opB)) {
		matched = a == opA; // opB: a * C
	}
	else if(Match<MulInstr>(MatchIC(&C), MatchAny(&a))(opB)) {
		matched = a == opA; // opB: C * a
	}
	
	// If something matched, return the folded expression.
	if(matched) {
		// Fold 'C + 1'.
		auto oneConst = irGen_.GetIntConst(a->GetType(), 1);
		auto constantOp = folder_.FoldBinary(Instr_Add, C, 
                                             oneConst, block);

		// Multiply it by 'a'.
		auto temp = GetTemporary(a);
		irGen_.GetMul(a, constantOp, temp);
		return LOG(temp);
	}

	// (a * C1) + (a * C2) -> a * (C1 + C2)
	IntConstant* C1;
	IntConstant* C2;
	matched = false;

	if(Match<MulInstr>(MatchAny(&a), MatchIC(&C1))(opA) ||
	   Match<MulInstr>(MatchIC(&C1), MatchAny(&a))(opA)) {
		// opB: 'a * C2' or 'C2 * a'
		if(Match<MulInstr>(MatchAny(&b), MatchIC(&C2))(opB) || 
		   Match<MulInstr>(MatchIC(&C2), MatchAny(&b))(opB)) {
			matched = a == b;
		}
	}

	// If something matched return the folded expression.
	if(matched) {
		auto constantOp = folder_.FoldBinary(Instr_Add, C1, C2, block);
		auto temp = GetTemporary(a);
		irGen_.GetMul(a, constantOp, temp);
		return LOG(temp);
	}

	// (a * b) + a -> a * (b + 1)
	// a + (a * b) -> a * (b + 1). This could simplify further if 'b' is a constant.
	if((Match<MulInstr>(MatchAny(&a), MatchAny(&b))(opA) && (a == opB)) ||
	   (Match<MulInstr>(MatchAny(&a), MatchAny(&b))(opB) && (a == opA))) {
        if(b->IsIntConstant()) {
		    auto incOp = GetTemporary(opA);
		    irGen_.GetAdd(b, GetOneInt(opA), incOp);

		    auto mulOp = GetTemporary(opA);
		    irGen_.GetMul(a, incOp, mulOp);
		    return LOG(mulOp);
        }
	}

    return nullptr;
}

} // namespace Optimization