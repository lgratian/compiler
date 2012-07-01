// TreeBalancing.hpp
// Copyright (c) Lup Gratian
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_TREE_BALANCING_HPP
#define PC_OPTIMIZATION_TREE_BALANCING_HPP

#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Unit.hpp"
#include "../Analysis/CFGWalker.hpp"
#include "../Analysis/DotPrinterBase.hpp"
#include "../Analysis/IntArithmetic.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/Log.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class TreeBalancing : public Pass {
private:
    // Represents an expression that should be optimized.
    // The expressions are represented as a tree of operations
    // (OperationExpression) and values (ValueExpression).
    class Expression {
    public:
        const Type* ResultType;
        int Rank;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        Expression(const Type* type) : ResultType(type) {}

        virtual ~Expression() {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        virtual bool IsOperation() const = 0;

        virtual bool IsValue() const {
            return IsOperation() == false;
        }

        virtual bool IsConstant() const {
            return false;
        }

        virtual bool IsZero() const {
            return false;
        }

        virtual bool IsParameter() const {
            return false;
        }

        virtual Constant* AsConstant() {
            return nullptr;
        }

        virtual IntConstant* AsIntConstant() {
            return nullptr;
        }

        virtual Parameter* AsParameter() {
            return nullptr;
        }

        virtual bool IsNegation() const {
            return false;
        }

        virtual bool IsOperation(Opcode operation) const {
            return false;
        }
    };

    // Represents a binary operation (add, mul, etc.) 
    // applied on the child expressions.
    class OperationExpression : public Expression {
    public:
        Opcode Operation;
        Expression* Left;
        Expression* Right;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        OperationExpression(Opcode operation, Expression* left,
                            Expression* right, const Type* type) :
                Expression(type), Operation(operation), Left(left), Right(right) {}

        ~OperationExpression() {
            if(Left && (Left->IsConstant() == false)) {
                delete Left;
            }

            if(Right && (Right->IsConstant() == false)) {
                delete Right;
            }
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        virtual bool IsOperation() const {
            return true;
        }

        bool IsSubtraction() const {
            return Operation == Instr_Sub;
        }

        bool IsAddition() const {
            return Operation == Instr_Add;
        }

        bool IsMultiplication() const {
            return Operation == Instr_Mul;
        }

        virtual bool IsNegation() const {
            return (Operation == Instr_Sub) && Left->IsZero();
        }

        virtual bool IsOperation(Opcode operation) const {
            return Operation == operation;
        }
    };

    // Represents a value used in an expression.
    class ValueExpression : public Expression {
    public:
        Operand* Value;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        ValueExpression(Operand* value, const Type* type) : 
                Expression(type), Value(value) {}

        virtual bool IsOperation() const {
            return false;
        }

        virtual bool IsConstant() const {
            DebugValidator::IsNotNull(Value);
            return Value->IsIntConstant();
        }

        virtual bool IsZero() const {
            DebugValidator::IsNotNull(Value);
            return Value->IsZeroInt();
        }

        virtual bool IsParameter() const {
            DebugValidator::IsNotNull(Value);
            return Value->IsParameter();
        }

        virtual Constant* AsConstant() {
            return Value->As<Constant>();
        }

        virtual IntConstant* AsIntConstant() {
            return Value->As<IntConstant>();
        }

        virtual Parameter* AsParameter() {
            return Value->As<Parameter>();
        }
    };

    // Helper that prints the expression tree
    // as a DOT file that can be opened with Graphviz.
    class ExpressionTreePrinter : public DotPrinterBase {
    public:
        ExpressionTreePrinter(const string& file) : DotPrinterBase(file) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        void PrintOperation(OperationExpression* expr);

        void PrintValue(ValueExpression* expr);

        void Print(Expression* expr);

        void Print(List<Expression*>& exprList);
    };

    // A pair consisting of an expression and its occurrence count.
    MAKE_PAIR(ExpressionOccurrencePair, Expression*, Value, int, Occurrences);

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    typedef List<Expression*> ExpressionList;
    typedef List<ExpressionOccurrencePair> OccurrenceList;
    typedef List<Operand*> OperandList;
    typedef IntArithmetic IA;

    //! TODO: control
    static const __int64 MAX_DISTRIBUTE_FACTOR = 8;

    Function* funct_;
    Dictionary<Instruction*, bool> processedInstrs_;
    Dictionary<IntConstant*, ValueExpression*> constValueExprs_;
    Dictionary<Operand*, int> operandRanks_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Computes a rank for each of the temporary variables
    // created in the function. The rank represents the depth
    // in the CFG (a temporary in an inner loop has a greater
    // rank than one in the outer loop, for example).
    void ComputeOperandRanks(Function* function);

    // Tries to optimize the expression that begins with the specified
    // instruction using reassociation, factorization, canceling, etc.
    void OptimizeExpression(Instruction* instr);

    // Returns 'true' if the specified expression does not use
    // the result of other operations (i.e both operands are values).
    bool IsSingleExpression(Expression* expr) {
        if(expr->IsValue()) {
            return true;
        }

        auto operationExpr = static_cast<OperationExpression*>(expr);
        return operationExpr->Left->IsValue() &&
               operationExpr->Right->IsValue();
    }

    // Creates an expression that represents the specified operand.
    // For constants and parameters a 'ValueExpression' is created,
    // while for instructions an 'OperationExpression'.
    Expression* CreateExpression(Operand* op);

    // Creates an expression that represents the specified instruction.
    Expression* CreateExpression(Instruction* instr);

    // Returns 'true' if the specified instruction can start
    // an expression tree which can be optimized by this pass.
    // It ignores floating-point instructions, for example.
    bool IsEligibleRootInstruction(Instruction* instr);

    // Returns 'true' if the specified instruction can appear
    // as one of the internal nodes in the expression trees.
    // It includes most arithmetic and logical instructions.
    bool IsEligibleInstruction(Instruction* instr);

    // Returns 'true' if the specified instruction is a 'shl' by a constant.
    bool IsLeftShiftByConstant(Instruction* instr);
    
    // Converts to addition and propagates a subtraction operation.
    // This can create more opportunities for reassociation and canceling.
    // 'a - (b + c)' -> 'a + (-b) + (-c)'
    void ConvertSubtractionToAddition(Expression* expr);
    
    // Tries to convert certain operations to multiplications
    // with a constant factor. This can create new opportunities.
    Expression* ConvertToMultiplication(Expression* expr);

    // Flattens the expression tree by adding all matching operations
    // (including the ones in subexpressions) to 'expressionRoots'.
    void LiniarizeTree(Expression* expr, Opcode requiredOperation, 
                       ExpressionList& expressionRoots);

    // Distributes the constant multiplication factors over 
    // the subexpressions by repeating them.
    // '2 * (a + b)' -> 'a + a + b + b'
    void DistributeFactors(ExpressionList& expressionRoots);

    // Repeats the expression the specified number of times,
    // negated if it's the case.
    void DistributeFactors(Expression* expr, __int64 times, bool negated,
                           ExpressionList& expressionRoots);

    // Removes pairs of expressions that cancel themselves,
    // such as addition with the negated value.
    void CancelExpressions(Opcode rootOperation, 
                           ExpressionList& expressionRoots);

    // Returns 'true' if the specified expressions cancel themselves
    // under the operation. For example, 'a + (-a)' -> 0.
    bool ExpressionsCancelThemselves(Opcode operation, 
                                     Expression* a, Expression* b);

    // Searches for expressions that appear multiple times
    // and keeps a single one multiplied with a constant factor,
    // removing the other ones. For example, 'a + a + a' -> '3 * a'.
    void UnifyIdentical(ExpressionList& expressionRoots);

    // Returns 'true' if the expressions are definitely identical.
    // If 'useTree' is set it does a type of comparison where
    // the order of the subexpressions doesn't matter (might be better).
    bool AreExpressionsIdentical(Expression* a, Expression* b,
                                 bool useTree = true);

    // Returns 'true' if the expressions are identical
    // being commutative and having the same operands.
    // This also considers the subexpressions.
    bool IsCommutativeWithSameOperands(OperationExpression* a, 
                                       OperationExpression* b);

    // Tries to extract common factors for all expressions.
    // 'a * b + a * b * c' -> 'a * b * (1 + c)'
    void ExtractFactors(ExpressionList& expressionRoots);

    // Identifies the factors found in the specified expression,
    // including its subexpression if the operation matches.
    void ExtractFactors(Expression* expr, Opcode requiredOperation,
                        bool onlyValues, OccurrenceList& foundExprs);

    // Removes the factors specified in 'factors' from the expression,
    // including its subexpressions. The removed factors are added
    // to 'dedExprs'.
    Expression* RemoveFactors(Expression* expr, OccurrenceList& factors,
                              ExpressionList& deadExprs);

    // Adds the specified expression to the list of dead expressions. 
    void AddToDeadList(Expression* expr, OccurrenceList& factors,
                       ExpressionList& deadExprs);

    // Creates an expression that has the common factors extracted.
    Expression* UnifyWithFactors(Expression* a, Expression* b, 
                                 OccurrenceList& factors);

    // Generates an expression that represents the factor
    // based on the information found in 'factors'.
    Expression* GenerateFactor(OccurrenceList& factors);

    // Generates an expression that multiplies the term
    // the specified number of times.
    Expression* GenerateFactor(Expression* term, int times);

    // Returns the expression if it matches the specified operation,
    // otherwise 'nullptr'.
    OperationExpression* GetOperationAs(Expression* expr, 
                                        Opcode requiredOperation);

    // Increments the number of times the specified expression was found.
    void CountOccurrence(Expression* expr, OccurrenceList& foundExprs);

    // Finds the expressions that are common to both factor lists
    // and adds them to 'commonExpre'. If 'stopOnMismatch' is set
    // it will stop on the first factor count mismatch or if a factor
    // is not found in both lists.
    void FindCommonExpressions(OccurrenceList& exprsA, OccurrenceList& exprsB,
                               OccurrenceList& commonExprs, 
                               bool stopOnMismatch = false);

    // Performs various peephole optimizations on 
    // all root expressions.
    void Peephole(ExpressionList& expressionRoots);

    // Performs various peephole optimizations on 
    // the specified expression.
    Expression* Peephole(Expression* expr);

    // Makes sure the constant is on the right side.
    void CanonicalizeOperation(OperationExpression* expr);

    // Creates an expression that represents the negated form
    // of the specified one. 'a' -> '0 -a', '5' -> '-5'.
    // If we have an addition or multiplication the negation
    // is propagated to the subexpressions: 'a + b' -> '(0 - a) + (0 - b)'
    Expression* NegateExpression(Expression* expr);

    void NegateAddition(OperationExpression* operationExpr);
    void NegateSubtraction(OperationExpression* operationExpr);
    void NegateMultiplication(OperationExpression* operationExpr);

    Expression* NegateValue(ValueExpression* op);
    Expression* NegateOperation(OperationExpression* expr);

    // Generates the instructions that represents all expressions,
    // combining them using 'rootOperation'. All generated instructions
    // are inserted before 'positionInstr'.
    Operand* EmitExpressionRoots(ExpressionList& roots, Opcode rootOperation, 
                                 Instruction* positionInstr);

    // Generates the instruction/operand that represents
    // the specified expression.
    Operand* EmitExpression(Expression* expr, Instruction* positionInstr);

    // Generates instructions taking into consideration the rank
    // of the subexpressions (might improve loop-invariant code motion).
    Operand* EmitRankedExpression(OperationExpression* expr, 
                                  Instruction* positionInstr);

    // Returns 'true' if the expression tree should be emitted
    // in a balanced form (increases ILP).
    bool ShouldEmitBalanced(ExpressionList& roots, Instruction* positionInstr);

    // Emits the instructions in a balanced form (leaves are emitted first, 
    // then their result, and so on).
    Operand* EmitBalancedTree(Opcode rootOperation, ExpressionList& roots, 
                              int first, int last, Instruction* positionInstr);

    // Emits the instructions from right to left, in a linear form.
    // This might improve loop-invariant code motion.
    Operand* EmitLiniarizedTree(Opcode rootOperation, ExpressionList& roots, 
                                Instruction* positionInstr);

    // Creates the instruction represented by the specified opcode.
    Operand* CreateInstruction(Opcode opcode, Operand* leftOp, 
                               Operand* rightOp, Instruction* positionInstr);

    // Creates a multiplication instruction.
    OperationExpression* CreateMultiply(Expression* left, Expression* right);

    // Computes the rank of the specified expression
    // and also caches it in the 'Rank' member.
    int ComputeRank(Expression* expr);

    // Sorts the expressions based on their rank
    // so that the ones with highest rank are first.
    void SortByRank(ExpressionList& roots);

    // Various methods for generating value expressions.
    IntConstant* GetIntConstant(__int64 value, const Type* type) {
        return funct_->ParentUnit()->Constants().GetInt(type, value);
    }

    ValueExpression* GetIntConstantValue(__int64 value, const Type* type) {
        return GetValueForConstant(GetIntConstant(value, type));
    }

    ValueExpression* GetZeroValue(const Type* type) {
        return GetIntConstantValue(0, type);
    }

    ValueExpression* GetValueForConstant(IntConstant* intConst);

    void MarkProcessedInstruction(Instruction* instr) {
        processedInstrs_.Add(instr, true);
    }
    
    bool WasInstructionProcessed(Instruction* instr) {
        return processedInstrs_.ContainsKey(instr);
    }

    // Creates a clone of the specified expression and its subexpressions.
    Expression* CloneExpression(Expression* expr);

    // Removes the specified expression.
    void DeleteExpression(Expression* expr) {
        if(expr->IsConstant() == false) {
            delete expr;
        }
    }
    
    // Processes all eligible instructions from the block.
    void ProcessBlock(Block* block);

public:
    ~TreeBalancing() {
        constValueExprs_.ForEachValue([](ValueExpression* expr) -> bool {
            delete expr;
            return true;
        });
    }

    void Execute(Function* function) {
        funct_ = function;
        ComputeOperandRanks(function);

        for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
            ProcessBlock(block);
        }
    }
};

} // namespace Optimization
#endif