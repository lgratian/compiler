// ComparisonPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 'quest' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

Operand* Peephole::HandleQuestion(QuestionInstr* instr) {
    // Try to constant-fold it.
    if(auto result = folder_.Fold(instr)) {
        return result;
    }

    // quest c, a, a -> a
    if(instr->TrueOp() == instr->FalseOp()) {
        return LOG(instr->TrueOp());
    }

    // quest (quest c, 4, 5), a, b -> a
    if(auto questInstr = instr->ConditionOp()->DefiningInstrAs<QuestionInstr>()) {
        if(questInstr->TrueOp()->IsIntConstant()  &&
           questInstr->FalseOp()->IsIntConstant() &&
           ((questInstr->TrueOp()->IsZeroInt() || 
             questInstr->FalseOp()->IsZeroInt()) == false)) {
            return LOG(instr->TrueOp());
        }
    }

    if(auto condInstr = instr->ConditionOp()->DefiningInstruction()) {
        auto cmpInstr = condInstr->As<CmpInstrBase>();

        if(cmpInstr && cmpInstr->IsEquality()) {
            // quest (cmp eq a, b), a, b -> b
            // quest (cmp neq a, b), a, b -> a
            if(cmpInstr->LeftOp() == instr->TrueOp() &&
               cmpInstr->RightOp() == instr->FalseOp()) {
                return LOG(cmpInstr->IsEqual() ? instr->FalseOp() : 
                                             instr->TrueOp());
            }

            // quest (cmp eq a, b), b, a -> a
            // quest (cmp neq a, b), b, a -> b
            if(cmpInstr->LeftOp() == instr->FalseOp() &&
               cmpInstr->RightOp() == instr->TrueOp()) {
                return LOG(cmpInstr->IsEqual() ? instr->FalseOp() : 
                                                 instr->TrueOp());
            }
        }
    }

    // Check if the condition operand is a boolean.
    if(instr->ConditionOp()->IsBoolean() ||
       instr->ConditionOp()->DefiningInstrIs<CmpInstrBase>()) {
        if(auto result = HandleQuestionOnBool(instr)) {
            return LOG(result);
        }
    }

    // quest c, (ext a), (ext b) -> ext (quest c, a, b)
    if(auto convInstrA = instr->TrueOp()->DefiningInstrAs<ConversionInstr>()) {
        if(auto convInstrB = instr->FalseOp()->DefiningInstrAs<ConversionInstr>()) {
            // The opcodes should be the same.
            if(convInstrA->SameKind(convInstrB)) {
                auto questOp = GetTemporary(convInstrA->TargetOp());
                irGen_.GetQuestion(instr->ConditionOp(), convInstrA->TargetOp(),
                                   convInstrB->TargetOp(), questOp);

                auto conversionOp = GetTemporary(instr->TrueOp());
                irGen_.GetConversion(convInstrA->GetOpcode(), questOp,
                                     convInstrA->CastType(), conversionOp);
                return LOG(conversionOp);
            }
        }
    }

    // quest c, (load a), (load b) -> load (quest c, a, b)
    if(auto loadInstrA = instr->TrueOp()->DefiningInstrAs<LoadInstr>()) {
        if(auto loadInstrB = instr->FalseOp()->DefiningInstrAs<LoadInstr>()) {
            auto questOp = GetTemporary(loadInstrA->SourceOp());
            irGen_.GetQuestion(instr->ConditionOp(), loadInstrA->SourceOp(),
                               loadInstrB->SourceOp(), questOp);

            auto loadOp = GetTemporary(instr->TrueOp());
            irGen_.GetLoad(questOp, loadOp);
            return loadOp;
        }
    }

    // (a > -1) ? C1 : C2 -> ((a >> 31) & (C2 - C1)) + C1
    // (a <  0) ? C2 : C1 -> ((a >> 31) & (C2 - C1)) + C1
    if(auto cmpInstr = instr->ConditionOp()->DefiningInstrAs<CmpInstr>()) {
        if(auto result = ExpandQuestToLogical(instr, cmpInstr)) {
            return LOG(result);
        }
    }

    // quest (a == C) ? a : b -> (a == C) ? C : b
    // quest (a != C) ? b : a -> (a != C) ? b : C
    if(auto cmpInstr = instr->ConditionOp()->DefiningInstrAs<CmpInstrBase>()) {
        // We do this simplification only if we don't increase the register pressure.
        if(cmpInstr->IsEqual() && cmpInstr->RightOp()->IsConstant() &&
           (instr->TrueOp() == cmpInstr->LeftOp())) {
            instr->SetTrueOp(cmpInstr->RightOp());
            return nullptr;
        }
        else if(cmpInstr->IsNotEqual() && cmpInstr->RightOp()->IsConstant() &&
                (instr->FalseOp() == cmpInstr->LeftOp())) {
            instr->SetFalseOp(cmpInstr->RightOp());
            return nullptr;
        }
    }

    // quest c, (add a, b), (add a, d) -> add a, (quest c, a, d)
    if(auto result = HandleQuestionOnBinary(instr)) {
        return result;
    }

    // if(E) a++ -> a += E
    // if(E) a-- -> a -= E
    if(instr->ConditionOp()->DefiningInstrIs<CmpInstrBase>()) {
        if(auto addInstr = instr->TrueOp()->DefiningInstrAs<AddInstr>()) {
            if((addInstr->RightOp()->IsOneInt() ||
                addInstr->RightOp()->IsMinusOneInt()) &&
                addInstr->LeftOp() == instr->FalseOp()) {
                auto resultOp = GetTemporary(instr->TrueOp());
                auto cmpResultOp = instr->ConditionOp();

                // Make sure that the types match.
                if(cmpResultOp->GetType() != instr->TrueOp()->GetType()) {
                    if(cmpResultOp->HasSingleUser()) {
                        // Change the type directly.
                        cmpResultOp->SetType(instr->TrueOp()->GetType());
                    }
                    else cmpResultOp = CreateIntCastIfRequired(cmpResultOp, 
                                                               instr->TrueOp()->GetType(), 
                                                               true /* isSigned */);
                }

                if(addInstr->RightOp()->IsOneInt()) {
                    irGen_.GetAdd(addInstr->LeftOp(), cmpResultOp, resultOp);
                }
                else irGen_.GetSub(addInstr->LeftOp(), cmpResultOp, resultOp);
                
                return LOG(resultOp);
            }
        }
    }

    // t1 = phi (0, 1, 0, 0, 1,...)
    // t2 = quest t1, a, 0     ->    t2 = mul t1, a
    // t3 = quest t1, 0, a     ->    t3 = sub 1, t1      t4 = mul t3, a
    // Make sure that the condition operand can be only 1 or 0.
    if(instr->FalseOp()->IsInteger() &&
       HasOnlyOneOrZeroResult(instr->ConditionOp(), instr->ParentBlock())) {
        bool invert = false;

        // The false or true operand should be 0.
        if(IsZeroInt(instr->FalseOp(), instr->ParentBlock()) == false) {
            if(IsZeroInt(instr->TrueOp(), instr->ParentBlock())) {
                invert = true;
            }
            else return nullptr;
        }
        
        // Make sure that the types match.
        auto conditionOp = instr->ConditionOp();
        
        if(conditionOp->GetType() != instr->TrueOp()->GetType()) {
            if(conditionOp->HasSingleUser()) {
                // Change the type directly.
                conditionOp->SetType(instr->TrueOp()->GetType());
            }
            else conditionOp = CreateIntCastIfRequired(conditionOp, 
                                                       instr->TrueOp()->GetType(), 
                                                       true /* isSigned */);
        }

        if(invert) {
            // sub 1, conditionOp
            auto subOp = GetTemporary(conditionOp);
            irGen_.GetSub(GetOneInt(conditionOp), conditionOp, subOp);
            conditionOp = subOp;
        }

        auto resultOp = GetTemporary(instr->TrueOp());
        auto valueOp = invert ? instr->FalseOp() : instr->TrueOp();

        irGen_.GetMul(conditionOp, valueOp, resultOp);
        return LOG(resultOp);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::HasOnlyOneOrZeroResult(Operand* op, Block* testBlock, int level) {
    if(op->IsUndefinedConstant() || 
       op->IsOneInt() || op->IsZeroInt()) {
       return true;
    }

    // Stop if the level is too high (the chances of finding the operand
    // is only 0/1 are very low, and we might enter a 'phi' loop).
    if(level > 4) {
        return false;
    }

    if(auto definingInstr = op->DefiningInstruction()) {
        if(definingInstr->IsComparison()) {
            return true;
        }
        else if(auto phiInstr = definingInstr->As<PhiInstr>()) {
            // Make sure that all incoming operands are 0 or 1.
            for(int i = 0; i < phiInstr->OperandCount(); i++) {
                if(HasOnlyOneOrZeroResult(phiInstr->GetOperand(i), 
                                          testBlock, level + 1) == false) {
                    return false;
                }
            }

            return true;
        }
        else if(auto questInstr = definingInstr->As<QuestionInstr>()) {
            return HasOnlyOneOrZeroResult(questInstr->TrueOp(), 
                                          testBlock, level + 1) &&
                   HasOnlyOneOrZeroResult(questInstr->FalseOp(), 
                                          testBlock, level + 1);
        }
        else return false;
    }

    // Check if the range of the operand is [0, 1].
    if(op->IsInteger()) {
        OperandInfo opInfo(irGen_.GetUnit(), GetTarget());
        return opInfo.IsGreaterOrEqual(op, GetZeroInt(op), true, testBlock) &&
               opInfo.IsSmallerOrEqual(op, GetOneInt(op), true, testBlock);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionOnBinary(QuestionInstr* instr) {
    // quest c, (add a, b), (add a, d) -> add a, (quest c, a, d)
    auto trueInstr = instr->TrueOp()->DefiningInstruction();
    auto falseInstr = instr->FalseOp()->DefiningInstruction();

    // We should have the same kind of instructions, which have
    // one operand the same (for commutative instructions it doesn't
    // matter where the operator is placed).
    if(((trueInstr && falseInstr) == false) ||
        (trueInstr->SameKind(falseInstr) == false)) {
        return nullptr;
    }

    // Only arithmetic and logical instructions are handled.
    if(((trueInstr->IsArithmetic() || trueInstr->IsLogical()) == false) ||
       ((falseInstr->IsArithmetic() || falseInstr->IsLogical()) == false)) {
        return nullptr;
    }

    Operand* commonOp = nullptr;
    Operand* otherOp1 = nullptr;
    Operand* otherOp2 = nullptr;

    if(trueInstr->GetSourceOp(0) == falseInstr->GetSourceOp(0)) {
        commonOp = trueInstr->GetSourceOp(0);
        otherOp1 = trueInstr->GetSourceOp(1);
        otherOp2 = falseInstr->GetSourceOp(1);
    }
    else if(trueInstr->GetSourceOp(1) == falseInstr->GetSourceOp(1)) {
        commonOp = trueInstr->GetSourceOp(1);
        otherOp1 = trueInstr->GetSourceOp(0);
        otherOp2 = falseInstr->GetSourceOp(0);
    }
    else if(trueInstr->IsCommutative()) {
        if(trueInstr->GetSourceOp(0) == falseInstr->GetSourceOp(1)) {
            commonOp = trueInstr->GetSourceOp(0);
            otherOp1 = trueInstr->GetSourceOp(1);
            otherOp2 = falseInstr->GetSourceOp(0);
        }
        else if(trueInstr->GetSourceOp(1) == falseInstr->GetSourceOp(0)) {
            commonOp = trueInstr->GetSourceOp(1);
            otherOp1 = trueInstr->GetSourceOp(0);
            otherOp2 = falseInstr->GetSourceOp(1);
        }
    }

    // Give up if we don't have a common operand.
    if(commonOp == nullptr) {
        return nullptr;
    }

    // Create the 'quest' that selects between the other operands.
    auto questOp = GetTemporary(commonOp);
    irGen_.GetQuestion(instr->ConditionOp(), otherOp1, otherOp2, questOp);

    if(trueInstr->IsArithmetic()) {
        auto arithOp = GetTemporary(commonOp);
        irGen_.GetArithmetic(trueInstr->GetOpcode(), commonOp, questOp, arithOp);
        return LOG(arithOp);
    }
    else {
        auto logicalOp = GetTemporary(commonOp);
        irGen_.GetLogical(trueInstr->GetOpcode(), commonOp, questOp, logicalOp);
        return LOG(logicalOp);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::ExpandQuestToLogical(QuestionInstr* instr, CmpInstr* cmpInstr) {
    // (a > -1) ? C1 : C2 -> ((a >> 31) & (C2 - C1)) + C1
    // (a <  0) ? C2 : C1 -> ((a >> 31) & (C2 - C1)) + C1
    // Note that the comparison must be signed, so that we get sign extension
    // when we shift by the size of the type.
    if((cmpInstr->IsGreater() && cmpInstr->RightOp()->IsMinusOneInt()) ||
        (cmpInstr->IsLess() && cmpInstr->RightOp()->IsZeroInt())) {
        IntConstant* C1;
        IntConstant* C2;

        if(cmpInstr->IsGreater()) {
            C1 = instr->TrueOp()->As<IntConstant>();
            C2 = instr->FalseOp()->As<IntConstant>();
        }
        else {
            C2 = instr->TrueOp()->As<IntConstant>();
            C1 = instr->FalseOp()->As<IntConstant>();
        }

        if(C1 && C2) {
            // ((a >> 31) & (C2 - C1)) + C1
            auto a = cmpInstr->LeftOp();
            int bits = C1->GetType()->SizeInBits();
            auto shiftOp = irGen_.GetIntConst(a->GetType(), bits - 1);

            auto shrOp = GetTemporary(a);
            irGen_.GetShr(a, shiftOp, shrOp);

            auto constDiffOp = folder_.FoldBinary(Instr_Sub, C2, C1,
                                                  instr->ParentBlock());
            auto andOp = GetTemporary(a);
            irGen_.GetAnd(shrOp, constDiffOp, andOp);
            
            auto addOp = GetTemporary(a);
            irGen_.GetAdd(andOp, C1, addOp);
            return LOG(addOp);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionOnBool(QuestionInstr* instr) {
    // Check if the operands are 1 and 0. This simplification happens often.
    // quest c, 1, 0 -> zext c, intX
    if(instr->TrueOp()->IsOneInt() && instr->FalseOp()->IsZeroInt()) {
        return CreateIntCastIfRequired(instr->ConditionOp(), 
                                       instr->TrueOp()->GetType(), false);
    }

    // quest c, -1, 0 -> sext c, intX
    if(instr->TrueOp()->IsMinusOneInt() && instr->FalseOp()->IsZeroInt()) {
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetSext(instr->ConditionOp(), resultOp->GetType(), resultOp);
        return LOG(resultOp);
    }

    // quest c, 0, 1 -> zext ~c, intX
    if(instr->TrueOp()->IsZeroInt() && instr->FalseOp()->IsOneInt()) {
        auto conditionOp = GetNegated(instr->ConditionOp());
        return LOG(CreateIntCastIfRequired(conditionOp, instr->TrueOp()->GetType(), false));
    }

    // quest c, 0, -1 -> sext ~c, intX
    if(instr->TrueOp()->IsZeroInt() && instr->FalseOp()->IsMinusOneInt()) {
        auto conditionOp = GetNegated(instr->ConditionOp());
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetSext(conditionOp, resultOp->GetType(), resultOp);
        return LOG(resultOp);
    }

    // If the source operands are also boolean then we can do the following:
    // quest c, 1, a -> or c, a
    if(instr->TrueOp()->IsOneInt() && instr->FalseOp()->IsBoolean()) {
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetOr(instr->ConditionOp(), instr->FalseOp(), resultOp);
        return LOG(resultOp);
    }

    // quest c, a, 1 -> or ~c, a
    if(instr->TrueOp()->IsBoolean() && instr->FalseOp()->IsZeroInt()) {
        auto conditionOp = GetNegated(instr->ConditionOp());
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetOr(conditionOp, instr->TrueOp(), resultOp);
        return LOG(resultOp);
    }

    // quest c, a, 0 -> and c, a
    if(instr->TrueOp()->IsBoolean() && instr->FalseOp()->IsZeroInt()) {
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetAnd(instr->ConditionOp(), instr->TrueOp(), resultOp);
        return LOG(resultOp);
    }

    // quest c, 0, a -> and ~c, a
    if(instr->TrueOp()->IsZeroInt() && instr->FalseOp()->IsBoolean()) {
        auto conditionOp = GetNegated(instr->ConditionOp());
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetAnd(conditionOp, instr->FalseOp(), resultOp);
        return LOG(resultOp);
    }

    // quest a, b, a -> and a, b
    if((instr->ConditionOp() == instr->FalseOp()) && instr->TrueOp()->IsBoolean()) {
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetAnd(instr->ConditionOp(), instr->TrueOp(), resultOp);
        return LOG(resultOp);
    }

    // quest a, a, b -> or a, b
    if((instr->ConditionOp() == instr->TrueOp()) && instr->FalseOp()->IsBoolean()) {
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetOr(instr->ConditionOp(), instr->FalseOp(), resultOp);
        return LOG(resultOp);
    }

    if(instr->ConditionOp()->IsBoolean() || 
       instr->ConditionOp()->DefiningInstrIs<CmpInstrBase>()) {
        // quest c, C + POW2, C -> add C, c * POW2
        // quest c, 7, 6 -> add 6, c
        auto trueConst = AsIntConstant(instr->TrueOp(), instr->ParentBlock());
        auto falseConst = AsIntConstant(instr->FalseOp(), instr->ParentBlock());

        if((trueConst && falseConst) &&
           (trueConst->Value() > falseConst->Value())) {
            __int64 difference = trueConst->Value() - falseConst->Value();

            if(IA::IsPowerOfTwo(difference)) {
                auto diffOp = irGen_.GetIntConst(trueConst->GetType(), difference);
                auto conditionOp = CreateIntCastIfRequired(instr->ConditionOp(), 
                                                           instr->TrueOp()->GetType());

                auto mulOp = GetTemporary(instr->TrueOp());
                irGen_.GetMul(conditionOp, diffOp, mulOp);

                auto resultOp = GetTemporary(instr->TrueOp());
                irGen_.GetAdd(falseConst, mulOp, resultOp);
                return LOG(resultOp);
            }
        }

        // quest c, C, C + POW2 -> sub C + POW2, c * POW2
        // quest c, 6, 7 -> sub 7, c
        if((trueConst && falseConst) &&
           (trueConst->Value() < falseConst->Value())) {
            __int64 difference = falseConst->Value() - trueConst->Value();

            if(IA::IsPowerOfTwo(difference)) {
                auto diffOp = irGen_.GetIntConst(falseConst->GetType(), difference);
                auto conditionOp = CreateIntCastIfRequired(instr->ConditionOp(), 
                                                           instr->FalseOp()->GetType());

                auto mulOp = GetTemporary(instr->FalseOp());
                irGen_.GetMul(conditionOp, diffOp, mulOp);

                auto resultOp = GetTemporary(instr->FalseOp());
                irGen_.GetSub(falseConst, mulOp, resultOp);
                return LOG(resultOp);
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleOperationOnQuest(Opcode opcode, Operand* opA, Operand* opB) {
    // add (quest c, 4, 5), 2 -> quest c, 6, 7
    // add (quest c, 4, 5), (quest c, 1, 2) -> quest c, 5, 7
    // We do this only if the 'true' and 'false' operands are constants,
    // and only if the register pressure is not going to increase.
    if(opB->DefiningInstrIs<QuestionInstr>() &&
       Instruction::IsCommutative(opcode)) {
        // Move the 'quest' to the left side (fewer cases to test for).
        std::swap(opA, opB);
    }

    auto questInstrA = opA->DefiningInstrAs<QuestionInstr>();

    if(questInstrA == nullptr) {
        return nullptr;
    }

    // Don't increase register pressure by creating a new 'quest' instruction.
    if(questInstrA->ResultOp()->HasSingleUser() == false) {
        return nullptr;
    }

    if(auto questInstrB = opB->DefiningInstrAs<QuestionInstr>()) {
        // 'OP quest, quest' case.
        // Don't increase register pressure by creating a new 'quest' instruction.
        if(questInstrB->ResultOp()->HasSingleUser() == false) {
            return nullptr;
        }
        else return LOG(HandleOperationOnQuestAndQuest(opcode, questInstrA, 
                                                       questInstrB));
    }

    // Check if there is at least one operand that is not a constant;
    // in this case we use another method to handle the situation.
    if((questInstrA->TrueOp()->IsConstant() == false ||
        questInstrA->FalseOp()->IsConstant() == false)) {
        return LOG(HandleOperationOnQuestAndOther(opcode, questInstrA, opB));
    }

    // Both operands are constants.
    auto trueConst = questInstrA->TrueOp()->As<Constant>();
    auto falseConst = questInstrA->FalseOp()->As<Constant>();

    if((trueConst  && falseConst) == false) {
        return nullptr;
    }

    if(trueConst->IsIntConstant()) {
        auto intConst = AsIntConstant(opB, nullptr);

        if(intConst == nullptr) {
            return nullptr;
        }

        auto intTrueConst = trueConst->As<IntConstant>();
        auto intFalseConst = falseConst->As<IntConstant>();

        switch(opcode) {
            case Instr_Add: 
            case Instr_Sub:
            case Instr_Mul:
            case Instr_Div:
            case Instr_Udiv:
            case Instr_Mod:
            case Instr_Umod:
            case Instr_And:
            case Instr_Or:
            case Instr_Xor:
            case Instr_Shl:
            case Instr_Shr:
            case Instr_Ushr: {
                auto newTrueConst = folder_.FoldBinary(opcode, intTrueConst, 
                                                       intConst, nullptr);
                auto newFalseConst = folder_.FoldBinary(opcode, intFalseConst, 
                                                        intConst, nullptr);

                // Create the result 'quest' instruction.
                auto resultOp = GetTemporary(newTrueConst);
                irGen_.GetQuestion(questInstrA->ConditionOp(), newTrueConst, 
                                   newFalseConst, resultOp);
                return LOG(resultOp);
            }
        }
    }
    else if(trueConst->IsFloatingConstant()) {
        auto floatConst = opB->As<FloatConstant>();

        if(floatConst == nullptr) {
            return nullptr;
        }

        auto floatTrueConst = trueConst->As<FloatConstant>();
        auto floatFalseConst = falseConst->As<FloatConstant>();

        switch(opcode) {
            case Instr_Fadd:
            case Instr_Fsub:
            case Instr_Fmul:
            case Instr_Fdiv: {
                auto newTrueConst = folder_.FoldBinary(opcode, floatTrueConst, 
                                                       floatConst, nullptr);
                auto newFalseConst = folder_.FoldBinary(opcode, floatFalseConst, 
                                                        floatConst, nullptr);
                auto resultOp = GetTemporary(newTrueConst);
                irGen_.GetQuestion(questInstrA->ConditionOp(), newTrueConst, 
                                   newFalseConst, resultOp);
                return LOG(resultOp);
            }
        }
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleOperationOnQuestAndOther(Opcode opcode, QuestionInstr* instr,
                                                  Operand* op) {
    // Try to simplify cases like the following one:
    // t1 = add a, C1                t1 = add a, C1 + C3
    // t2 = add b, C2          =>    t2 = add b, C2 + C3
    // t3 = quest c, t1, t2          t3 = quest c, t1, t2
    // t4 = add t3, C2               
    auto trueOp = instr->TrueOp();
    auto falseOp = instr->FalseOp();

    // We handle only arithmetic and logical instructions (the common cases).
    if((Instruction::IsArithmetic(opcode) || 
        Instruction::IsLogical(opcode)) == false) {
        return nullptr;
    }

    // We do it only if the operands used in 'quest' have a single user,
    // so that we don't need to clone the instructions and 
    // increase the register pressure.
    if(auto temp = trueOp->As<Temporary>()) {
        if(temp->HasSingleUser() == false) {
            return nullptr;
        }
    }
    
    if(auto temp = falseOp->As<Temporary>()) {
        if(temp->HasSingleUser() == false) {
            return nullptr;
        }
    }

    // Try to simplify both operands.
    auto newTrueOp = SimplifyBinary(opcode, trueOp, op, 
                                    instr->ParentBlock());
    if(newTrueOp == nullptr) return nullptr; {
        return nullptr;
    }

    auto newFalseOp = SimplifyBinary(opcode, falseOp, op,
                                    instr->ParentBlock());
    if(newFalseOp == nullptr) {
        return nullptr;
    }

    // Both operands simplified, create a new 'quest' instruction.
    auto resultOp = GetTemporary(trueOp);
    irGen_.GetQuestion(instr->ConditionOp(), newTrueOp, 
                       newFalseOp, resultOp);
    return LOG(resultOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleOperationOnQuestAndQuest(Opcode opcode, 
                                                  QuestionInstr* instrA,
                                                  QuestionInstr* instrB) {
    // add (quest c, 4, 5), (quest c, 1, 2) -> quest c, 5, 7
    // We do the simplification only if the 'quest' result operands are used
    // only by the min/max intrinsics (prevents creating new instructions).
    if((instrA->ResultOp()->HasSingleUser() == false) ||
       (instrB->ResultOp()->HasSingleUser() == false)) {
        return nullptr;
    }

    // Check if only constants are used.
    auto trueConstA = instrA->TrueOp()->As<Constant>();

    if(trueConstA == nullptr) {
        return nullptr;
    }

    auto falseConstA = instrA->FalseOp()->As<Constant>();

    if(falseConstA == nullptr) {
        return nullptr;
    }

    auto trueConstB = instrB->TrueOp()->As<Constant>();

    if(trueConstB == nullptr) {
        return nullptr;
    }

    auto falseConstB = instrB->FalseOp()->As<Constant>();

    if(falseConstB == nullptr) {
        return nullptr;
    }

    if(trueConstA->IsIntConstant()) {
        auto intTrueConstA = trueConstA->As<IntConstant>();
        auto intFalseConstA = falseConstA->As<IntConstant>();
        auto intTrueConstB = trueConstB->As<IntConstant>();
        auto intFalseConstB = falseConstB->As<IntConstant>();

        switch(opcode) {
            case Instr_Add: 
            case Instr_Sub:
            case Instr_Mul:
            case Instr_Div:
            case Instr_Udiv:
            case Instr_Mod:
            case Instr_Umod:
            case Instr_And:
            case Instr_Or:
            case Instr_Xor:
            case Instr_Shl:
            case Instr_Shr:
            case Instr_Ushr: {
                auto newTrueConst = folder_.FoldBinary(opcode, intTrueConstA, 
                                                       intTrueConstB, nullptr);
                auto newFalseConst = folder_.FoldBinary(opcode, intFalseConstA, 
                                                        intFalseConstB, nullptr);
                auto resultOp = GetTemporary(newTrueConst);
                irGen_.GetQuestion(instrA->ConditionOp(), newTrueConst, 
                                   newFalseConst, resultOp);
                return LOG(resultOp);
            }
        }
    }
    else if(trueConstB->IsFloatingConstant()) {
        auto floatTrueConstA = trueConstA->As<FloatConstant>();
        auto floatFalseConstA = falseConstA->As<FloatConstant>();
        auto floatTrueConstB = trueConstB->As<FloatConstant>();
        auto floatFalseConstB = falseConstB->As<FloatConstant>();

        switch(opcode) {
            case Instr_Fadd:
            case Instr_Fsub:
            case Instr_Fmul:
            case Instr_Fdiv: {
                auto newTrueConst = folder_.FoldBinary(opcode, floatTrueConstA, 
                                                       floatTrueConstB, nullptr);
                auto newFalseConst = folder_.FoldBinary(opcode, floatFalseConstA, 
                                                        floatFalseConstB, nullptr);
                auto resultOp = GetTemporary(newTrueConst);
                irGen_.GetQuestion(instrA->ConditionOp(), newTrueConst, 
                                   newFalseConst, resultOp);
                return LOG(resultOp);
            }
        }
    }

    return nullptr;
}

} // namespace Optimization