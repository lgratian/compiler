// TreeBalancing.cpp
// Copyright (c) Lup Gratian
//
// Implements the TreeBalancing pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "TreeBalancing.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

void TreeBalancing::ComputeOperandRanks(Function* function) {
    // Compute the rank of each instruction result (temporary).
    // Instructions that are deeply nested in a loop will get
    // a higher rank because we use a reverse-postorder traversal.
    CFGInfo<Block, Function> cfgInfo(function->FirstBlock(), false);
    auto& postorderList = cfgInfo.PostorderList();

    // Constants get rank 0, parameters rank 1,
    // and temporaries rank >= 2.
    int rank = 2;

    for(int i = postorderList.Count() - 1; i >= 0; i--) {
        auto block = const_cast<Block*>(postorderList[i]);

        // Assign a rank for each result operand in the block.
        block->ForEachInstruction([rank, this](Instruction* instr) -> bool {
            if(instr->HasDestinationOp()) {
                operandRanks_.Add(instr->GetDestinationOp(), rank);                
            }

            return true;
        });

        rank++;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::ProcessBlock(Block* block) {
    // We walk the instructions in the block from last to first
    // and try to optimize the expressions as we see them.
    // Note that instructions that were involved in other expressions
    // are ignored.
    auto instr = block->LastInstruction();

    while(instr) {
        auto previousInstr = instr->PreviousInstruction();
        
        if(IsEligibleRootInstruction(instr)) {
            OptimizeExpression(instr);
        }

        instr = previousInstr;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::OptimizeExpression(Instruction* instr) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsTrue(IsEligibleRootInstruction(instr));

    // Move from a list-based representation to an expression tree.
    // This allows us to apply easily various transformation on the expression.
    auto root = CreateExpression(instr);
    ExpressionTreePrinter("J:\\expr1.dot").Print(root);

    // If we have a single expression we stop here,
    // there is nothing to optimize.
    if(IsSingleExpression(root)) {
        return;
    }

    // Convert subtraction to addition wherever it's possible.
    // This allows the reassociation of more expressions and might
    // discover pairs that cancel each other like in the following example:
    // 'a + b - a' -> 'a + b + (0 - a)' -> 'b'
    ConvertSubtractionToAddition(root);
    ExpressionTreePrinter("J:\\expr_sub.dot").Print(root);

    // Try to convert as many instructions as possible to multiplications.
    // This might allow better expression reassociation/canceling.
    ConvertToMultiplication(root);
    ExpressionTreePrinter("J:\\expr_sub.dot").Print(root);

    // Liniarize the expression tree.
    Opcode rootOperation = static_cast<OperationExpression*>(root)->Operation;
    ExpressionList expressionRoots;
    
    LiniarizeTree(root, rootOperation, expressionRoots);
    ExpressionTreePrinter("J:\\expr_liniar.dot").Print(expressionRoots);

    // Distribute the factors over the expressions.
    // This will be reverted after expressions are canceled and unified.
    if(rootOperation == Instr_Add) {
        DistributeFactors(expressionRoots);
        ExpressionTreePrinter("J:\\expr_distr.dot").Print(expressionRoots);
    }

    // Remove pairs of expressions that cancel themselves (like 'a + (-a)').
    CancelExpressions(rootOperation, expressionRoots);
    ExpressionTreePrinter("J:\\expr_canceled.dot").Print(expressionRoots);

    if(rootOperation == Instr_Add) {
        // Represent expressions that appear multiple times
        // as 'factor * expr' ('a + a + a' -> '3 * a').
        UnifyIdentical(expressionRoots);
        ExpressionTreePrinter("J:\\expr_unify.dot").Print(expressionRoots);
    }

    // Extract factors common to multiple expressions.
    if((rootOperation == Instr_Add ) || 
       (rootOperation == Instr_Mul)) {
        ExtractFactors(expressionRoots);
        ExpressionTreePrinter("J:\\expr_extracted.dot").Print(expressionRoots);
    }

    //! TODO: Only if high optimization activated
#if 1
    CancelExpressions(rootOperation, expressionRoots);
    ExpressionTreePrinter("J:\\expr_canceled.dot").Print(expressionRoots);

    // If we remained with a single root expression try to 
    // liniarize the expression again.
    if((expressionRoots.Count() == 1) && 
        expressionRoots[0]->IsOperation()) {
        root = expressionRoots[0];
        rootOperation = static_cast<OperationExpression*>(root)->Operation;

        expressionRoots.Clear();
        LiniarizeTree(root, rootOperation, expressionRoots);
        ExpressionTreePrinter("J:\\expr_liniar.dot").Print(expressionRoots);
    }
    
    if(rootOperation == Instr_Add) {
        UnifyIdentical(expressionRoots);
        ExpressionTreePrinter("J:\\expr_unify.dot").Print(expressionRoots);
    }
#endif

    // We (hopefully) optimized the expression tree,
    // now emit it as a series of instructions that replace the old ones.
    auto result = EmitExpressionRoots(expressionRoots, rootOperation, instr);
    instr->GetDestinationOp()->ReplaceWith(result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::CreateExpression(Operand* op) {
    DebugValidator::IsNotNull(op);

    // If the operand is defined by an instruction
    // we might create subexpressions for its operands.
    if(auto definingInstr = op->DefiningInstruction()) {
        return CreateExpression(definingInstr);
    }

    // For any other operand (constant, parameter)
    // create a ValueExpression object.
    if(auto intConst = op->As<IntConstant>()) {
        return GetValueForConstant(intConst);
    }
    else return new ValueExpression(op, op->GetType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::CreateExpression(Instruction* instr) {
    DebugValidator::IsNotNull(instr);

    MarkProcessedInstruction(instr);

    // We create a subexpression for each operand if the instruction
    // could benefit from reassociation/factoring.
    if(IsEligibleInstruction(instr)) {
        DebugValidator::AreEqual(instr->SourceOpCount(), 2);

        auto left = CreateExpression(instr->GetSourceOp(0));
        auto right = CreateExpression(instr->GetSourceOp(1));
        
        // If the expression is commutative we want to have
        // the constant/parameter on the right (canonicalization).
        if((instr->IsCommutative() && (right->IsConstant() == false)) &&
           (left->IsConstant() || left->IsParameter())) {
            return new OperationExpression(instr->GetOpcode(), right, left,
                                           instr->GetDestinationOp()->GetType());
        }
        else return new OperationExpression(instr->GetOpcode(), left, right,
                                            instr->GetDestinationOp()->GetType());
    }

    return new ValueExpression(instr->GetDestinationOp(),
                               instr->GetDestinationOp()->GetType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool TreeBalancing::IsEligibleRootInstruction(Instruction* instr) {
    // Skip the instruction if it has been processed
    // as a part of another expression.
    if(WasInstructionProcessed(instr)) {
        return false;
    }

    // Ignore dead instructions.
    if(instr->HasDestinationOp() == false) {
        return false;
    }

    // The expression tree can begin only with associative
    // instructions like 'add' and 'mul' applied on integers.
    // An exception are 'sub' (which we convert to addition)
    // and 'shl' by a constant (converted to multiplication).
    return (instr->IsAssociative() && (instr->IsFloatArithmetic() == false)) ||
            instr->IsSub() || IsLeftShiftByConstant(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool TreeBalancing::IsLeftShiftByConstant(Instruction* instr) {
    if(auto shiftInstr = instr->As<ShlInstr>()) {
        return shiftInstr->RightOp()->IsIntConstant();
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool TreeBalancing::IsEligibleInstruction(Instruction* instr) {
    // We create subexpressions only for the following instruction types
    // (all other instructions are represented as 'ValueExpression' objects).
    return instr->IsAdd() || instr->IsSub() || instr->IsMul() ||
           instr->IsAnd() || instr->IsOr()  || instr->IsXor() ||
           instr->IsShl() || instr->IsUshr();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::ConvertSubtractionToAddition(Expression* expr) {
    // We recursively check the expression tree for a subtraction operation.
    // If we find one we try to propagate the negation as much as possible
    // in the subexpressions, then change the operation to addition.
    if(expr->IsValue()) {
        return;
    }

    auto operationExpr = static_cast<OperationExpression*>(expr);

    if(operationExpr->IsSubtraction()) {
        // Convert from 'a - b' to 'a + (-b)'.
        auto newRight = NegateExpression(operationExpr->Right);
        operationExpr->Right = newRight;
        operationExpr->Operation = Instr_Add;

        // Try to apply the transformation on the left subexpression.
        ConvertSubtractionToAddition(operationExpr->Left);
    }
    else {
        // Try to apply the transformation to the subexpressions.
        ConvertSubtractionToAddition(operationExpr->Left);
        ConvertSubtractionToAddition(operationExpr->Right);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::NegateExpression(Expression* expr) {
    if(expr->IsValue()) {
        auto valueExpr = static_cast<ValueExpression*>(expr);
        return NegateValue(valueExpr);
    }

    // We want to propagate the negation as deeply as possible,
    // creating more reassociation  opportunities.
    auto operationExpr = static_cast<OperationExpression*>(expr);

    // Check if we need to negate a value already negated.
    // -(0 - a) -> a
    if(operationExpr->IsNegation()) {
        auto value = operationExpr->Right;
        operationExpr->Right = nullptr;
        delete operationExpr;
        return value;
    }

    switch(operationExpr->Operation) {
        case Instr_Add: {
            NegateAddition(operationExpr);
            return expr;
        }
        case Instr_Sub: {
            NegateSubtraction(operationExpr);
            return expr;
        }
        case Instr_Mul: {
            NegateMultiplication(operationExpr);
            return expr;
        }
    }

    // For any other operation we simply subtract it from 0.
    return NegateOperation(operationExpr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::NegateAddition(OperationExpression* expr) {
    // Propagate the subtraction on both sides.
    // '-(a + b)' -> '(-a) + (-b)'
    if(auto newLeft = NegateExpression(expr->Left)) {
        expr->Left = newLeft;
    }

    if(auto newRight = NegateExpression(expr->Right)) {
        expr->Right = newRight;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::NegateSubtraction(OperationExpression* expr) {
    // Propagate the subtraction on the left side
    // and change the operation to addition.
    // '-(a - b)' -> '(-a) + b'
    if(auto newLeft = NegateExpression(expr->Left)) {
        expr->Left = newLeft;
    }

    expr->Operation = Instr_Add;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::NegateMultiplication(OperationExpression* expr) {
    // Propagate the subtraction to the subexpression which is
    // a value, otherwise to the left one. Propagating to the constant 
    // might create more canceling opportunities.
    // '-(a * b)' -> '(-a) * b'
    // '-(a * 5)' -> 'a * (-5)'
    if(expr->Right->IsConstant() || expr->Right->IsParameter()) {
        auto newRight = NegateExpression(expr->Right);
        expr->Right = newRight;
    }
    else {
        auto newLeft= NegateExpression(expr->Left);
        expr->Left = newLeft;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::NegateValue(ValueExpression* expr) {
    // If the expression is an integer constant we create a new constant
    // that represents the negated value ('2' -> '-2').
    if(auto intConst = expr->AsIntConstant()) {
        return GetIntConstantValue(-intConst->Value(), expr->ResultType);
    }

    // Any other value is negated by subtracting it from 0.
    // 'param' -> '0 - param'.
    return new OperationExpression(Instr_Sub, GetZeroValue(expr->ResultType),
                                   expr, expr->ResultType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::NegateOperation(OperationExpression* expr) {
    // Create the expression '0 - expr'.
    return new OperationExpression(Instr_Sub, GetZeroValue(expr->ResultType),
                                   expr, expr->ResultType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::ConvertToMultiplication(Expression* expr) {
    if(expr->IsValue()) {
        return nullptr;
    }

    // Try to apply some simple peephole simplifications.
    // We process the subexpression first, then the main expression
    // (this creates more opportunities because subexpressions might
    // fold to constants, for example).
    auto operationExpr = static_cast<OperationExpression*>(expr);

    if(auto newLeft = ConvertToMultiplication(operationExpr->Left)) {
        DeleteExpression(operationExpr->Left);
        operationExpr->Left = newLeft;
    }

    if(auto newRight = ConvertToMultiplication(operationExpr->Right)) {
        DeleteExpression(operationExpr->Right);
        operationExpr->Right = newRight;
    }

    // Make sure the constant (if it exists) is on the right.
    CanonicalizeOperation(operationExpr);

    switch(operationExpr->Operation) {
        case Instr_Add: {
            // 'a + a' -> '2 * a'
            if(AreExpressionsIdentical(operationExpr->Left,
                                       operationExpr->Right)) {
                auto twoExpr = GetIntConstantValue(2, expr->ResultType);
                auto resultExpr = CreateMultiply(operationExpr->Left, twoExpr);

                DeleteExpression(operationExpr->Right);
                operationExpr->Left = nullptr; // Don't delete Left.
                operationExpr->Right = nullptr;

                return resultExpr;
            }
            break;
        }
        case Instr_Mul: {
            // '(a * C1) * C2' -> 'a * (C1 + C2)'
            if(operationExpr->Left->IsOperation() &&
               operationExpr->Right->IsConstant()) {
                auto otherExpr = static_cast<OperationExpression*>(operationExpr->Left);
                auto C2 = operationExpr->Right->AsIntConstant();

                if((otherExpr->Operation != Instr_Mul) ||
                   (otherExpr->Right->IsConstant() == false)) {
                    return nullptr;
                }

                // Create the new expression.
                auto C1 = otherExpr->Right->AsIntConstant();
                auto sumExpr = GetIntConstantValue(IA::Mul(C1, C2),
                                                   otherExpr->ResultType);

                auto resultExpr = CreateMultiply(otherExpr->Left, sumExpr);
                otherExpr->Left = nullptr; // Don't delete Left.
                return resultExpr;
            }
            break;
        }
        case Instr_Shl: {
            // a << C -> a * 2^C
            if(auto intConst = operationExpr->Right->AsIntConstant()) {
                auto intType = operationExpr->ResultType->As<IntegerType>();

                if((intConst->Value() > 0) &&
                   (intConst->Value() < intType->SizeInBits())) {
                    auto resultType = operationExpr->ResultType->As<IntegerType>();

                    __int64 powValue = IA::Shl(1, intConst->Value(), 
                                               resultType->GetSubtype());
                    auto powExpr = GetIntConstantValue(powValue, resultType);
                    
                    auto resultExpr = CreateMultiply(operationExpr->Left, powExpr);
                    operationExpr->Left = nullptr; // Don't delete Left.
                    return resultExpr;
                }
            }
            break;
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::LiniarizeTree(Expression* expr, Opcode requiredOperation, 
                                  ExpressionList& expressionRoots) {
    DebugValidator::IsNotNull(expr);

    // Make sure we don't liniarize non-associative expressions.
    if(Instruction::IsAssociative(requiredOperation) == false) {
        expressionRoots.Add(expr);
        return;
    }

    // If we have an operation that matches the required one
    // we recursively add its subexpressions to the list.
    if(expr->IsOperation()) {
        auto operationExpr = static_cast<OperationExpression*>(expr);

        if(operationExpr->Operation == requiredOperation) {
            LiniarizeTree(operationExpr->Left, requiredOperation, expressionRoots);
            LiniarizeTree(operationExpr->Right, requiredOperation, expressionRoots);

            // The operation can now be deleted (note that we reset
            // the subexpressions first so they are not deleted).
            operationExpr->Left = nullptr;
            operationExpr->Right = nullptr;
            delete operationExpr;
            return;
        }
    }
    
    // Any other expression is simply added to the list.
    expressionRoots.Add(expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::DistributeFactors(ExpressionList& expressionRoots) {
    // We try to distribute a multiplication factor over the other
    // subexpression, creating more canceling opportunities. For example,
    // '2 * (a + b)' -> 'a + a + b + b'.
    // Each of the generated expression is added to the root list.
    StaticList<Expression*, 4> deletedRoots;
    int count = expressionRoots.Count();

    for(int i = 0; i < count; i++) {
        auto expr = expressionRoots[i];

        if(expr->IsOperation()) {
            auto operationExpr = static_cast<OperationExpression*>(expr);

            // Check if we have 'expr *  C'.
            if((operationExpr->IsMultiplication()) &&
                operationExpr->Right->IsConstant()) {
                // To prevent an explosion of the number of generated
                // expression we limit the number, adjusting the factor.
                __int64 times;
                __int64 value = operationExpr->Right->AsIntConstant()->Value();
                bool negated = value < 0;

                if(negated) {
                    // -15 -> MAX_DISTRIBUTE_FACTOR times
                    times = std::abs(std::max(value, -MAX_DISTRIBUTE_FACTOR));
                }
                else {
                    // 15 - MAX_DISTRIBUTE_FACTOR times
                    times = std::abs(std::min(value, MAX_DISTRIBUTE_FACTOR));
                }

                DistributeFactors(operationExpr->Left, times, 
                                  negated, expressionRoots);

                // If the expression has been "unrolled" completely
                // we must delete it later.
                if(times == std::abs(value)) {
                    deletedRoots.Add(operationExpr);
                }
                else {
                    // The factor is not zero, so it needs some adjustment.
                    // 'factor = 100, times = 8' -> 'factor = 92'
                    // 'factor = -10, times = 8' -> 'factor = -2'
                    __int64 remaining = negated ? 
                                        std::min(0LL, value + MAX_DISTRIBUTE_FACTOR) :
                                        std::max(0LL, value - MAX_DISTRIBUTE_FACTOR);
                    auto remainingExpr = GetIntConstantValue(remaining, expr->ResultType);
                    operationExpr->Right = remainingExpr;
                }
            }
        }
    }

    // Delete all root expressions with factor 0.
    for(int i = 0; i < deletedRoots.Count(); i++) {
        DeleteExpression(deletedRoots[i]);
        expressionRoots.Remove(deletedRoots[i]);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::DistributeFactors(Expression* expr, __int64 times, bool negated,
                                      ExpressionList& expressionRoots) {
    // If the expression over which we need to distribute the factor
    // is 'add' or 'sub' we recourse. For example, '2 * (a + (b + c))'
    // is expanded as 'a + a + b + b + c + c'.
    if(expr->IsOperation()) {
        auto operationExpr = static_cast<OperationExpression*>(expr);

        if(operationExpr->IsAddition()) {
            DistributeFactors(operationExpr->Left, times, negated, expressionRoots);
            DistributeFactors(operationExpr->Right, times, negated, expressionRoots);
            return;
        }
        else if(operationExpr->IsSubtraction()) {
            // Note that the right expression is negated.
            // '2 * (a - b)' -> 'a + a - b - b'
            DistributeFactors(operationExpr->Left, times, negated, expressionRoots);
            DistributeFactors(operationExpr->Right, times, !negated, expressionRoots);
            return;
        }
    }

    // For value expressions and other operations we just create
    // the required number of copies, taking care about the negation.
    for(int i = 0; i < times; i++) {
        auto newExpr = CloneExpression(expr);
            
        if(negated) {
            newExpr = NegateExpression(newExpr);
        }

        expressionRoots.Add(newExpr);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::CancelExpressions(Opcode rootOperation, 
                                      ExpressionList& expressionRoots) {
    if((rootOperation != Instr_Add) &&
       (rootOperation != Instr_Xor)) {
        return;
    }

    // We scan the list of root expressions and check if there are pairs
    // that cancel themselves, like in the following example:
    // 'a + b + c - a + d -c' -> 'b + d'.
    // The algorithm is quadratic, but it shouldn't be a problem 
    // for the small expressions usually found in the code.
    for(int i = 0; i < expressionRoots.Count() - 1; i++) {
        bool removed = false;

        for(int j = i + 1; j < expressionRoots.Count(); j++) {
            auto firstExpr = expressionRoots[i];
            auto secondExpr = expressionRoots[j];

            if(ExpressionsCancelThemselves(rootOperation, firstExpr, secondExpr)) {
                // Delete the expressions, then remove them from the list.
                DeleteExpression(firstExpr);
                DeleteExpression(secondExpr);

                expressionRoots.RemoveAt(j);
                expressionRoots.RemoveAt(i);
                removed = true;
                break;
            }
        }

        if(removed) {
            i--;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool TreeBalancing::ExpressionsCancelThemselves(Opcode operation, 
                                                Expression* a, Expression* b) {
    // We consider three cases:
    if(operation == Instr_Add) {
        // 'a + (-a)' -> 0
        if(a->IsConstant() && b->IsConstant()) {
            return a->AsIntConstant()->Value() == -b->AsIntConstant()->Value();
        }
        else if(a->IsNegation()) {
            auto operationExpr = static_cast<OperationExpression*>(a);
            return AreExpressionsIdentical(operationExpr->Right, b);
        }
        else if(b->IsNegation()) {
            auto operationExpr = static_cast<OperationExpression*>(b);
            return AreExpressionsIdentical(operationExpr->Right, a);
        }

        // 'C*a + (-C)*a' -> 0
        if(a->IsOperation() && b->IsOperation()) {
            auto operationExprA = static_cast<OperationExpression*>(a);
            auto operationExprB = static_cast<OperationExpression*>(b);

            if((operationExprA->IsMultiplication()) &&
               (operationExprB->IsMultiplication()) &&
                operationExprA->Right->IsConstant() &&
                operationExprB->Right->IsConstant()) {
                auto intConstA = operationExprA->Right->AsIntConstant();
                auto intConstB = operationExprB->Right->AsIntConstant();

                return (intConstA->Value() == -intConstB->Value()) &&
                        AreExpressionsIdentical(operationExprA->Left,
                                                operationExprB->Left);
            }
        }
    }
    else if(operation == Instr_Xor) {
        // 'a ^ a' -> 0
        // '(a ^ a) ^ b' -> '0 ^ b' -> 'b'
        return AreExpressionsIdentical(a, b);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::UnifyIdentical(ExpressionList& expressionRoots) {
    // Search for a series of identical expressions and represent them
    // as 'factor * expr'. For example, 'a + b + a + a' -> '3*a + b'.
    for(int i = 0; i < expressionRoots.Count() - 1; i++) {
        auto searchedExpr = expressionRoots[i];
        StaticList<Expression*, 8> identical;

        // Identify the instructions that are identical.
        for(int j = i + 1; j < expressionRoots.Count(); j++) {
            auto otherExpr = expressionRoots[j];

            if(AreExpressionsIdentical(searchedExpr, otherExpr)) {
                identical.Add(otherExpr);
            }
        }

        if(identical.Count() == 0) {
            continue;
        }

        // Found at least one instruction that is identical.
        // Replace the first one with 'factor * expr', where
        // 'factor' is the total number of identical instructions.
        auto resultType = searchedExpr->ResultType;
        auto factorExpr = GetIntConstantValue(identical.Count() + 1, resultType);
        expressionRoots[i] = new OperationExpression(Instr_Mul, factorExpr,
                                                     searchedExpr, resultType);

        // Remove the other ones.
        for(int j = identical.Count() - 1; j >= 0; j--) {
            auto deadExpr = identical[j];
            DeleteExpression(deadExpr);
                
            for(int k = expressionRoots.Count() - 1; k > i; k--) {
                if(expressionRoots[k] == deadExpr) {
                    expressionRoots.RemoveAt(k);
                    break;
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool TreeBalancing::AreExpressionsIdentical(Expression* a, Expression* b,
                                            bool useTree) {
    if(a == b) {
        // Obviously they are equal.
        return true;
    }

    if(a->IsValue() != b->IsValue()) {
        // A value can't be equal to an operations
        // (we ignore the possibility of constant folding here).
        return false;
    }

    if(a->IsValue() && b->IsValue()) {
        // Values expressions are equal if the operands are equal.
        auto valueExprA = static_cast<ValueExpression*>(a);
        auto valueExprB = static_cast<ValueExpression*>(b);
        return valueExprA->Value == valueExprB->Value;
    }

    // Recursively test for operations.
    auto operationExprA = static_cast<OperationExpression*>(a);
    auto operationExprB = static_cast<OperationExpression*>(b);

    if(operationExprA->Operation != operationExprB->Operation) {
        return false;
    }

    // a OP b == a OP b
    if(AreExpressionsIdentical(operationExprA->Left, operationExprB->Left) &&
       AreExpressionsIdentical(operationExprA->Right, operationExprB->Right)) {
        return true;
    }

    // a OP b == b OP a
    if(Instruction::IsCommutative(operationExprA->Operation) &&
       AreExpressionsIdentical(operationExprA->Left, operationExprB->Right, false) &&
       AreExpressionsIdentical(operationExprA->Right, operationExprB->Left, false)) {
        return true;
    }

    // Perform a comparison that considers a chain of commutative operations
    // that use the same values, but not necessary in the same order.
    // (a * b) * c == c * (b * a)
    if(useTree) {
        return IsCommutativeWithSameOperands(operationExprA, operationExprB);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool TreeBalancing::IsCommutativeWithSameOperands(OperationExpression* a, 
                                                  OperationExpression* b) {
    DebugValidator::AreEqual(a->Operation, b->Operation);

    // Check if two expression trees based on the same commutative
    // operations are identical by using exactly the same operands.
    // This will consider 'a * (b * c)' and 'c * (a * b)' to be identical.
    if(Instruction::IsCommutative(a->Operation) == false) {
        return false;
    }

    OccurrenceList foundExprsA;
    OccurrenceList foundExprsB;

    ExtractFactors(a, a->Operation, false, foundExprsA);
    ExtractFactors(b, b->Operation, false, foundExprsB);

    if(foundExprsA.Count() != foundExprsB.Count()) {
        return false;
    }

    // Make sure that each expression from 'a' can be found in 'b'.
    OccurrenceList commonExprs;
    FindCommonExpressions(foundExprsA, foundExprsB, 
                          commonExprs, true /* stopOnMismatch */);

    return commonExprs.Count() == foundExprsA.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::ExtractFactors(ExpressionList& expressionRoots) {
    // Try to extract factors that are common to at least two expressions.
    // For example, 'a*b*c + a*b*d + a*b*e' -> 'a * b * (c + d + e)'.
    // The algorithm processes the expressions until no change is made,
    // being able to catch second-order opportunities.
    bool changed = true;

    while(changed && (expressionRoots.Count() > 1)) {
        changed = false;

        // Determine the factors for each of the expressions.
        Dictionary<Expression*, OccurrenceList> occurrences;
        int withFactors = 0;

        for(int i = 0; i < expressionRoots.Count(); i++) {
            OccurrenceList foundExprs;
            auto expr = expressionRoots[i];

            ExtractFactors(expr, Instr_Mul, false /* onlyValues */, foundExprs);
            occurrences.Add(expr, foundExprs);

            if(foundExprs.Count() > 0) {
                withFactors++;
            }
        }

        // No reason to continue if there aren't at least
        // two expressions with factors.
        if(withFactors < 2) {
            return;
        }

        // Analyze the expression with all other ones that follow.
        // If at least one common operand is found the first expression
        // is rewritten, while the second one removed.
        for(int i = 0; i < expressionRoots.Count(); i++) {
            auto exprA = expressionRoots[i];
            auto candidateExprA = exprA->IsValue() ? 
                                  exprA : GetOperationAs(exprA, Instr_Mul);
            if(candidateExprA == nullptr) {
                continue;
            }

            for(int j = i + 1; j < expressionRoots.Count(); j++) {
                auto exprB = expressionRoots[j];
                auto candidateExprB = exprB->IsValue() ? 
                                      exprB : GetOperationAs(exprB, Instr_Mul);
                if(candidateExprB == nullptr) {
                    continue;
                }

                // Compute the factors and the factors that are common.
                OccurrenceList commonExprs;

                FindCommonExpressions(occurrences[candidateExprA], 
                                      occurrences[candidateExprB], 
                                      commonExprs);

                if(commonExprs.Count() > 0) {
                    // There are common factors, extract them.
                    auto unifiedExpr = UnifyWithFactors(candidateExprA,
                                                        candidateExprB, 
                                                        commonExprs);
                    expressionRoots[i] = unifiedExpr;
                    expressionRoots.Remove(candidateExprB);
                    
                    i = j - 1; // Continue after the second instruction.
                    changed = true;
                    break;
                }
            }
        }

        // Clean up the expression tree, many operations like 'a * 1'
        // have been probably created and might interfere.
        if(changed) {
            Peephole(expressionRoots);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::UnifyWithFactors(Expression* a, Expression* b, 
                                                           OccurrenceList& factors) {
    DebugValidator::IsLarger(factors.Count(), 0);

    // The expressions are unified by removing the common factors,
    // followed by their addition, result multiplied by the factors.
    // a*b + b*(a*c) -> (1*b + b*(1*c)) * a
    auto factorExpr = GenerateFactor(factors);

    // Remember the number of times each factor appeared
    // (this info is destroyed by 'RemoveFactors').
    OccurrenceList factorsBackup;

    for(int i = 0; i < factors.Count(); i++) {
        factorsBackup.Add(factors[i]);
    }

    ExpressionList deadExprs;
    auto newA = RemoveFactors(a, factors, deadExprs);
    auto newB = RemoveFactors(b, factorsBackup, deadExprs);

    if(newA) {
        AddToDeadList(a, factors, deadExprs);
        a = newA;
    }

    if(newB) {
        AddToDeadList(b, factors, deadExprs);
        b = newB;
    }

    auto sumExpr = new OperationExpression(Instr_Add, a, b, a->ResultType);
    auto result = new OperationExpression(Instr_Mul, sumExpr, 
                                          factorExpr, a->ResultType);

    // Remove all expressions that are dead.
    for(int i = 0; i < deadExprs.Count(); i++) {
        DeleteExpression(deadExprs[i]);
    }

    return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::GenerateFactor(OccurrenceList& factors) {
    // Generate the expression tree that represents the factors. For example,
    // for the factors 'a:1, b:2' -> 'a*b*b'.
    auto result = GenerateFactor(factors[0].Value, factors[0].Occurrences);

    for(int i = 1; i < factors.Count(); i++) {
        auto newFactor = GenerateFactor(factors[i].Value, factors[i].Occurrences);
        result = new OperationExpression(Instr_Mul, result, newFactor, 
                                         result->ResultType);
    }

    return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::GenerateFactor(Expression* term, int times) {
    DebugValidator::IsLarger(times, 0);

    if(times == 1) {
        return term;
    }

    // If the factor 'f' was found 'n' times, we generate the expressions
    // 'f*f*f...*f', 'n' times.
    auto result = term;
    
    while(times-- > 1) {
        result = new OperationExpression(Instr_Mul, result, 
                                         term, term->ResultType);
    }

    return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::RemoveFactors(Expression* expr, 
                                                        OccurrenceList& factors,
                                                        ExpressionList& deadExprs) {
    // Remove factors found in 'factors' by replacing them with 1.
    // For example, if 'factor = {a,c}' and the expression is '(a * b) * c'
    // the result would be '(1 * b) * 1' (this will be simplified later.
    if(auto mulExpr = GetOperationAs(expr, Instr_Mul)) {
        // Search the factors on both side of the multiplication.
        if(auto newLeft = RemoveFactors(mulExpr->Left, factors, deadExprs)) {
            AddToDeadList(mulExpr->Left, factors, deadExprs);
            mulExpr->Left = newLeft;
        }

        if(auto newRight = RemoveFactors(mulExpr->Right, factors, deadExprs)) {
            AddToDeadList(mulExpr->Right, factors, deadExprs);
            mulExpr->Right = newRight;
        }

        return nullptr;
    }

    // Check if this expression is any of the factors that must be removed.
    for(int i = 0; i < factors.Count(); i++) {
        if(AreExpressionsIdentical(expr, factors[i].Value)) {
            // Take care not to remove more factors than required.
            if(factors[i].Occurrences > 0) {
                factors[i].Occurrences--;
                return GetIntConstantValue(1, expr->ResultType);
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::AddToDeadList(Expression* expr, OccurrenceList& factors,
                                  ExpressionList& deadExprs) {
    // Take care not to delete an expression that is a factor.
    for(int i = 0; i < factors.Count(); i++) {
        if(factors[i].Value == expr) {
            return;
        }
    }

    deadExprs.Add(expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::OperationExpression* TreeBalancing::GetOperationAs(Expression* expr, 
                                                                  Opcode operation) {
    if(expr->IsValue()) {
        return nullptr;
    }

    auto operationExpr = static_cast<OperationExpression*>(expr);
    return operationExpr->Operation == operation ?
           operationExpr : nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::ExtractFactors(Expression* expr, Opcode requiredOperation,
                                   bool onlyValues, OccurrenceList& foundExprs) {
    // A value is immediately counted.
    if(expr->IsValue()) {
        CountOccurrence(expr, foundExprs);
        return;
    }

    // If the operation is the one we're searching for
    // recourse in its subexpressions.
    auto operationExpr = static_cast<OperationExpression*>(expr);

    if(operationExpr->Operation == requiredOperation) {
        ExtractFactors(operationExpr->Left, requiredOperation, 
                       onlyValues, foundExprs);
        ExtractFactors(operationExpr->Right, requiredOperation, 
                       onlyValues, foundExprs);
    }
    else if(onlyValues == false) {
        CountOccurrence(operationExpr, foundExprs);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::CountOccurrence(Expression* expr, OccurrenceList& foundExprs) {
    for(int i = 0; i < foundExprs.Count(); i++) {
        if(AreExpressionsIdentical(expr, foundExprs[i].Value, false)) {
            foundExprs[i].Occurrences++;
            return;
        }
    }

    foundExprs.Add(ExpressionOccurrencePair(expr, 1));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::FindCommonExpressions(OccurrenceList& exprsA, 
                                          OccurrenceList& exprsB,
                                          OccurrenceList& commonExprs, 
                                          bool stopOnMismatch) {
    // Identify the expressions that are common to both lists.
    // If 'stopOnMismatch' is set the search stops as soon as an expression
    // is not common or the number of occurrences is not the same.
    for(int i = 0; i < exprsA.Count(); i++) {
        auto searchedExpr = exprsA[i].Value;
        bool valid = false;
        int j;

        for(j = 0; j < exprsB.Count(); j++) {
            if(AreExpressionsIdentical(exprsB[j].Value, searchedExpr, false)) {
                if(stopOnMismatch) {
                    // Check if the expression occurs the same number of times.
                    if(exprsA[i].Occurrences != exprsB[i].Occurrences) {
                        return;
                    }
                }

                int occurrence = std::min(exprsA[i].Occurrences, 
                                          exprsB[j].Occurrences);
                commonExprs.Add(ExpressionOccurrencePair(searchedExpr, occurrence));
                break;
            }
        }

        if(stopOnMismatch && (j == exprsB.Count())) {
            return;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::Peephole(ExpressionList& expressionRoots) {
    for(int i = 0; i < expressionRoots.Count(); i++) {
        if(auto result = Peephole(expressionRoots[i])) {
            DeleteExpression(expressionRoots[i]);
            expressionRoots[i] = result;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::Peephole(Expression* expr) {
    if(expr->IsValue()) {
        return nullptr;
    }

    // Try to apply some simple peephole simplifications.
    // We process the subexpression first, then the main expression
    // (this creates more opportunities because subexpressions might
    // fold to constants, for example).
    auto operationExpr = static_cast<OperationExpression*>(expr);

    if(auto newLeft = Peephole(operationExpr->Left)) {
        DeleteExpression(operationExpr->Left);
        operationExpr->Left = newLeft;
    }

    if(auto newRight = Peephole(operationExpr->Right)) {
        DeleteExpression(operationExpr->Right);
        operationExpr->Right = newRight;
    }

    // Make sure the constant (if it exists) is on the right.
    CanonicalizeOperation(operationExpr);

    switch(operationExpr->Operation) {
        case Instr_Mul: {
            if(auto intConst = operationExpr->Right->AsIntConstant()) {
                // 'a * 1' -> a
                if(intConst->IsOneInt()) {
                    auto result = operationExpr->Left;
                    operationExpr->Left = nullptr;
                    return result;
                }

                // 'a * 0' -> 0
                if(intConst->IsZeroInt()) {
                    return GetZeroValue(expr->ResultType);
                }
            }
            break;
        }
        case Instr_Add:
        case Instr_Sub:{
            if(auto intConst = operationExpr->Right->AsIntConstant()) {
                // 'a + 0' -> a
                // 'a - 0' -> a
                if(intConst->IsZeroInt()) {
                    auto result = operationExpr->Left;
                    operationExpr->Left = nullptr;
                    return result;
                }
            }
            break;
        }
        case Instr_And: {
            if(auto intConst = operationExpr->Right->AsIntConstant()) {
                // 'a & 0' -> 0
                // 'a & -1' -> a
                if(intConst->IsZeroInt()) {
                    return GetZeroValue(expr->ResultType);
                }
                else if(intConst->IsMinusOneInt()) {
                    auto result = operationExpr->Left;
                    operationExpr->Left = nullptr;
                    return result;
                }
            }
            break;
        }
        case Instr_Or: {
            if(auto intConst = operationExpr->Right->AsIntConstant()) {
                // 'a | 0' -> a
                // 'a | -1' -> -1
                if(intConst->IsZeroInt()) {
                    auto result = operationExpr->Left;
                    operationExpr->Left = nullptr;
                    return result;
                }
                else if(intConst->IsMinusOneInt()) {
                    return GetIntConstantValue(-1, expr->ResultType);
                }
            }
            break;
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::CanonicalizeOperation(OperationExpression* expr) {
    // Move the constant on the right.
    if(Instruction::IsCommutative(expr->Operation)) {
        if(expr->Left->IsConstant()) {
            std::swap(expr->Left, expr->Right);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* TreeBalancing::EmitExpressionRoots(ExpressionList& roots, Opcode rootOperation, 
                                            Instruction* positionInstr) {
    for(int i = 0; i < roots.Count(); i++) {
        ComputeRank(roots[i]);
    }

    SortByRank(roots);

    // We can now create the instructions. If we're in a loop
    // we emit them from right to left, otherwise we emit them
    // in a balanced form which improves ILP. Balancing inside a loop
    // might reduce the number of loop-invariant expressions.
    if(ShouldEmitBalanced(roots, positionInstr)) {
        return EmitBalancedTree(rootOperation, roots, 
                                0, roots.Count() - 1, positionInstr);
    }
    else return EmitLiniarizedTree(rootOperation, roots, positionInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool TreeBalancing::ShouldEmitBalanced(ExpressionList& roots,
                                       Instruction* positionInstr) {
    // The expressions are emitted balanced only if
    // they are not found inside a loop or all have the same rank.
    // Consider the expression 'a * b * c * d', where 'b', 'c' and 'd'
    // are loop invariant. If the expression is emitted balanced
    // it would look like '(a * b) * (c * d)', making only 'c * d'
    // loop invariant, while emitting it like '((c * d) * b) * a'
    // makes the whole subexpression '(c * d) * b' invariant.
    if(positionInstr->ParentBlock()->IsInLoop() == false) {
        return true;
    }

    // Check if all ranks are equal.
    int lastRank = roots[0]->Rank;

    for(int i = 1; i < roots.Count(); i++) {
        if(roots[i]->Rank != lastRank) {
            return false;
        }
    }
    
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* TreeBalancing::EmitBalancedTree(Opcode rootOperation, ExpressionList& roots, 
                                         int first, int last, Instruction* positionInstr) {
    // If it's a leaf just emit the expression.
    if(first == last) {
        return EmitExpression(roots[first], positionInstr);
    }

    // Emit the expressions on the left and on the right,
    // then combine their results.
    int middle = first + (last - first) / 2;
    auto leftOp = EmitBalancedTree(rootOperation, roots, 
                                   first, middle, positionInstr);
    auto rightOp = EmitBalancedTree(rootOperation, roots,
                                    middle + 1, last, positionInstr);
    return CreateInstruction(rootOperation, leftOp, rightOp, positionInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* TreeBalancing::EmitLiniarizedTree(Opcode rootOperation, ExpressionList& roots, 
                                           Instruction* positionInstr) {
    // The expressions are combined from right to left,
    // increasing the chance that pairs of loop-invariant 
    // expressions are formed.
    Operand* result = EmitExpression(roots[roots.Count() - 1], positionInstr);

    for(int i = roots.Count() - 2; i >= 0; i--) {
        auto newResult = EmitExpression(roots[i], positionInstr);
        result = CreateInstruction(rootOperation, newResult, result, positionInstr);
    }

    return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* TreeBalancing::EmitExpression(Expression* expr, Instruction* positionInstr) {
    // For values we just return the operand they represent.
    if(expr->IsValue()) {
        auto valueExpr = static_cast<ValueExpression*>(expr);
        return valueExpr->Value;
    }

    auto operationExpr = static_cast<OperationExpression*>(expr);

    // First we try to emit the expression taking the ranks
    // of the subexpression into consideration (this might improve
    // loop-invariant code motion, for example).
    if(auto result = EmitRankedExpression(operationExpr, positionInstr)) {
        return result;
    }

    // Create the subexpressions, then create the appropriate
    // instruction that realizes the operation.    
    auto leftOp = EmitExpression(operationExpr->Left, positionInstr);
    auto rightOp = EmitExpression(operationExpr->Right, positionInstr);

    return CreateInstruction(operationExpr->Operation, 
                             leftOp, rightOp, positionInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* TreeBalancing::EmitRankedExpression(OperationExpression* expr, 
                                             Instruction* positionInstr) {
    // Try to reassociate a subexpression based on the ranks.
    // For example, the following expression (with ranks written below)
    // is reassociated to expose the loop-invariant expression 'a * c'.
    // '(a * b) * c' -> 'b * (a * c)'
    //   1   2    1      2    1   1
    if(Instruction::IsAssociative(expr->Operation) == false) {
        return nullptr;
    }

    if(expr->Left->IsOperation(expr->Operation)) {
        auto otherExpr = static_cast<OperationExpression*>(expr->Left);

        if(otherExpr->Left->Rank != otherExpr->Right->Rank) {
            if(otherExpr->Left->Rank <= expr->Right->Rank) {
                // '(a * b) * c' -> 'b * (a * c)'
                auto opA = EmitExpression(otherExpr->Left, positionInstr);
                auto opB = EmitExpression(otherExpr->Right, positionInstr);
                auto opC = EmitExpression(expr->Right, positionInstr);
                
                auto opAC = CreateInstruction(expr->Operation, opA, opC, positionInstr);
                return CreateInstruction(expr->Operation, opB, opAC, positionInstr);
            }
            else if(otherExpr->Right->Rank <= expr->Right->Rank) {
                // '(a * b) * c' -> 'a * (b * c)'
                auto opA = EmitExpression(otherExpr->Left, positionInstr);
                auto opB = EmitExpression(otherExpr->Right, positionInstr);
                auto opC = EmitExpression(expr->Right, positionInstr);
                
                auto opBC = CreateInstruction(expr->Operation, opB, opC, positionInstr);
                return CreateInstruction(expr->Operation, opA, opBC, positionInstr);
            }
        }
    }
    
    // Now try on the right side.
    if(expr->Right->IsOperation(expr->Operation)) {
        auto otherExpr = static_cast<OperationExpression*>(expr->Right);

        if(otherExpr->Left->Rank != otherExpr->Right->Rank) {
            if(otherExpr->Left->Rank <= expr->Left->Rank) {
                // 'a * (b * c)' -> '(a * b) * c'
                auto opA = EmitExpression(expr->Left, positionInstr);
                auto opB = EmitExpression(otherExpr->Left, positionInstr);
                auto opC = EmitExpression(otherExpr->Right, positionInstr);
                
                auto opAB = CreateInstruction(expr->Operation, opA, opB, positionInstr);
                return CreateInstruction(expr->Operation, opAB, opC, positionInstr);
            }
            else if(otherExpr->Right->Rank <= expr->Left->Rank) {
                // 'a * (b * c)' -> '(a * c) * b'
                auto opA = EmitExpression(expr->Left, positionInstr);
                auto opB = EmitExpression(otherExpr->Left, positionInstr);
                auto opC = EmitExpression(otherExpr->Right, positionInstr);
                
                auto opAC = CreateInstruction(expr->Operation, opA, opC, positionInstr);
                return CreateInstruction(expr->Operation, opAC, opB, positionInstr);
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* TreeBalancing::CreateInstruction(Opcode opcode, Operand* leftOp,
                                          Operand* rightOp, Instruction* positionInstr) {
    auto block = positionInstr->ParentBlock();
    auto resultOp = Temporary::GetTemporary(leftOp->GetType());
    Instruction* instr;

    if(Instruction::IsArithmetic(opcode)) {
        instr = ArithmeticInstr::GetArithmetic(opcode, leftOp, rightOp, resultOp);
    }
    else instr = LogicalInstr::GetLogical(opcode, leftOp, rightOp, resultOp);

    block->InsertInstructionBefore(instr, positionInstr);
    return resultOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int TreeBalancing::ComputeRank(Expression* expr) {
    // Constants have rank 0, parameters rank 1.
    // Any other operand has a rank dependent on the CFG depth.
    if(expr->IsValue()) {
        auto valueExpr = static_cast<ValueExpression*>(expr);
        int rank;

        if(expr->IsConstant()) {
            rank = 0;
        }
        else if(expr->IsParameter()) {
            rank = 1;
        }
        else {
            rank = operandRanks_[valueExpr->Value];
        }

        expr->Rank = rank;
        return rank;
    }

    // The rank of an operation is the maximum
    // of the subexpression ranks.
    auto operationExpr = static_cast<OperationExpression*>(expr);
    int leftRank = ComputeRank(operationExpr->Left);
    int rightRank = ComputeRank(operationExpr->Right);

    expr->Rank = std::max(leftRank, rightRank);
    return expr->Rank;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::SortByRank(ExpressionList& roots) {
    for(int i = 0; i < roots.Count(); i++) {
        for(int j = i + 1; j < roots.Count(); j++) {
            if(roots[i]->Rank < roots[j]->Rank) {
                std::swap(roots[i], roots[j]);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::ValueExpression* TreeBalancing::GetValueForConstant(IntConstant* intConst) {
    // Try to obtain the expression from the cache.
    ValueExpression* valueExpr;

    if(constValueExprs_.TryGetValue(intConst, &valueExpr)) {
        return valueExpr;
    }

    // First time the expression is requested, create it.
    valueExpr = new ValueExpression(intConst, intConst->GetType());
    constValueExprs_.Add(intConst, valueExpr);
    return valueExpr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::Expression* TreeBalancing::CloneExpression(Expression* expr) {
    if(expr->IsValue()) {
        auto valueExpr = static_cast<ValueExpression*>(expr);
        
        // Constant expressions don't need to be cloned.
        if(valueExpr->IsConstant()) {
            return valueExpr;
        }
        else return new ValueExpression(valueExpr->Value, valueExpr->ResultType);
    }
    else {
        // The subexpressions must be cloned first.
        auto operationExpr = static_cast<OperationExpression*>(expr);
        return new OperationExpression(operationExpr->Operation,
                                       CloneExpression(operationExpr->Left),
                                       CloneExpression(operationExpr->Right),
                                       operationExpr->ResultType);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TreeBalancing::OperationExpression* TreeBalancing::CreateMultiply(Expression* left,
                                                                  Expression* right) {
    return new OperationExpression(Instr_Mul, left, 
                                   right, left->ResultType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::ExpressionTreePrinter::Print(Expression* expr) {
    if(expr->IsOperation()) {
        PrintOperation(static_cast<OperationExpression*>(expr));
    }
    else PrintValue(static_cast<ValueExpression*>(expr));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::ExpressionTreePrinter::Print(List<Expression*>& exprList) {
    for(int i = 0; i < exprList.Count(); i++) {
        Print(exprList[i]);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::ExpressionTreePrinter::PrintOperation(OperationExpression* expr) {
    NewNode(expr, Instruction::OpcodeString(expr->Operation),
            COLOR_LIGHT_PINK, SHAPE_CIRCLE);
    Print(expr->Left);
    Print(expr->Right);

    Link(expr, expr->Left);
    Link(expr, expr->Right);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TreeBalancing::ExpressionTreePrinter::PrintValue(ValueExpression* expr) {
    if(auto intConst = expr->AsIntConstant()) {
        NewNode(expr, string::Format(L"%d", intConst->Value()),
                COLOR_LIGHT_BLUE, SHAPE_RECT);
        return;
    }
    else if(auto parameter = expr->AsParameter()) {
        NewNode(expr, *parameter->GetSymbol()->Name(),
                COLOR_LIGHT_GREEN, SHAPE_RECT);
        return;
    }
    else if(auto temp = expr->Value->As<Temporary>()) {
        if(auto nameTag = temp->GetTag<NameTag>()) {
            NewNode(expr, nameTag->Name(),
                    COLOR_LIGHT_GREY, SHAPE_RECT);
            return;
        }
    }

    NewNode(expr, "Unknown", COLOR_LIGHT_GREY, SHAPE_RECT);
}

} // namespace Optimization