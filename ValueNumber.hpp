// ValueNumber.hpp
// Copyright (c) Lup Gratian
//
// Defines helpers classes for computing value numbers.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_VALUE_NUMBER_HPP
#define PC_ANALYSIS_VALUE_NUMBER_HPP

#include "TypeInfo.hpp"
#include "IntArithmetic.hpp"
#include "../IR/Operand.hpp"
#include "../IR/IRTypes.hpp"
#include "../IR/Constants.hpp"
#include "../IR/References.hpp"
#include "../IR/Instructions.hpp"
#include "../ir/Intrinsics.hpp"
#include "../Targets/TargetInfo.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
using namespace IR;
using namespace Target;
using namespace Base;

namespace Analysis {

// Represents an expression used during Value Numbering optimizations.
class Expression {
private:    
    Opcode opcode_;               // The opcode of the instruction.
    char other_;                  // Additional data.
    StaticList<unsigned, 4> ops_; // The value numbers of the operands.

public:
    Expression() {}

    Expression(Opcode opcode, char other = 0) :
            opcode_(opcode), other_(other) {}

    Expression(const Expression& other) :
            opcode_(other.opcode_), other_(other.other_), ops_(other.ops_) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    void AddOperand(unsigned op) {
        ops_.Add(op);
    }

    unsigned GetHashCode() const;

    bool operator== (const Expression& other) const;

    bool operator< (const Expression& other) const {
        return false;
    }
};

// Helper that provides value numbers for operands and expressions.
class ValueNumber {
private:
    unsigned nextValNumb_;
    Dictionary<Operand*, unsigned> opValNumbers_;
    Dictionary<Expression, unsigned> expValNumbers_;
    Dictionary<Operand*, unsigned> resultOpValNumbers_;

    unsigned NextNumber() {
        return nextValNumb_++;
    }

public:
    ValueNumber() : nextValNumb_(0) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    unsigned GetValueNumber(Operand* op);

    unsigned GetValueNumber(Expression& expr, Operand* destOp = nullptr);

    Expression CreateExpession(Instruction* instr);

    Expression CreateCallExpression(CallInstr* callInstr);

    Expression CreatePhiExpression(PhiInstr* phiInstr);

    // Returns 'true' if the specified 'call' instruction can be value-numbered.
    static bool IsCallSafe(CallInstr* instr);
};


// Represents a scoped hash table (dictionary) for expressions 
// used during Value Numbering optimizations.
class ScopedExpressionTable {
private:
    Dictionary<unsigned, Operand*> valNumberToOp_;
    ScopedExpressionTable* parent_;
    ValueNumber* valNumber_;
    Block* owner_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    unsigned GetValueNumber(Operand* op) {
        return valNumber_->GetValueNumber(op);
    }

    unsigned GetValueNumber(Expression& expr, Operand* destOp = nullptr) {
        return valNumber_->GetValueNumber(expr, destOp);
    }

    Expression CreateExpession(Instruction* instr) {
        return valNumber_->CreateExpession(instr);
    }

    Operand* GetAvailable(unsigned valNumb);

    void InsertAvailable(unsigned valNumb, Operand* op);

public:
    ScopedExpressionTable() {}

    ScopedExpressionTable(ValueNumber* valNumber, Block* owner = nullptr,
                          ScopedExpressionTable* parent = nullptr) :
            valNumber_(valNumber), owner_(owner), parent_(parent) {}

    ScopedExpressionTable(const ScopedExpressionTable& other) :
            parent_(other.parent_), valNumber_(other.valNumber_),
            owner_(other.owner_) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Searches for an available expression that has the value number 
    // of the specified instruction. If no available expression is found,
    // one is created now and inserted into the table.
    Operand* GetOrInsert(Instruction* instr);

    // Searches for an available expression that has the value number 
    // of the specified instruction. Returns 'nullptr' if a suitable expression
    // is not found in this table or any of its ancestors.
    Operand* Get(Instruction* instr);
    
    // Inserts the specified instruction into the table,
    // and associates the specified operand with its value number.
    void Insert(Instruction* instr, Operand* valueOp);

    // Returns the value number for the specified instruction.
    unsigned GetValueNumber(Instruction* instr);

    // Returns the parent of this scoped table.
    ScopedExpressionTable* Parent() {
        return parent_;
    }

    // Returns the block that is associated with this table.
    Block* Owner() const {
        return owner_;
    }

    ScopedExpressionTable& operator= (const ScopedExpressionTable& other) {
        if(&other != this) {
            parent_ = other.parent_;
            valNumber_ = other.valNumber_;
            owner_ = other.owner_;
        }

        return *this;
    }
};

} // namespace Analysis
#endif