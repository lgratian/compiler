// ValueNumber.cpp
// Copyright (c) Lup Gratian
//
// Implements the classes to support value numbering.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ValueNumber.hpp"

namespace Analysis {

unsigned Expression::GetHashCode() const {
    unsigned hash = 2166136261;
    hash = (hash * 16777619) ^ opcode_;
    hash = (hash * 16777619) ^ other_;

    for(int i = 0; i < ops_.Count(); i++) {
        hash = (hash * 16777619) ^ ops_[i];
    }

    return hash;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Expression::operator== (const Expression& other) const {
    if(other.opcode_ != opcode_) return false;
    if(other.other_ != other_) return false;
    if(other.ops_.Count() != ops_.Count()) return false;

    for(int i = 0; i < ops_.Count(); i++) {
        if(other.ops_[i] != ops_[i]) return false;
    }

    return true;
}

// ######################################################################################
// ValueNumber
// ######################################################################################
bool ValueNumber::IsCallSafe(CallInstr* instr) {
    // Value number math, bitwise and bounds-check intrinsics.
    if(auto intrinsic = instr->GetIntrinsic()) {
        return intrinsic->IsMathIntrinsic() || intrinsic->IsBitwiseIntrinsic() ||
               intrinsic->IsBoundsCheckIntrinsic();
    }

    // Otherwise the target of the call must be a 'pure' function.
    if(auto function = instr->GetCalledFunction()) {
        return function->IsNoWrite() && function->IsNoState();
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unsigned ValueNumber::GetValueNumber(Operand* op) {
    unsigned valNumb;
    
    // Check if the operand is defined by an instruction;
    // in this case we use the value number of the expression.
    // An exception is for 'load', 'phi' and most 'call' instructions.
    if(auto definingInstr = op->DefiningInstruction()) {
        if(resultOpValNumbers_.TryGetValue(op, &valNumb)) {
            return valNumb;
        }

        if((definingInstr->IsLoad() || definingInstr->IsPhi() || 
            definingInstr->IsCall()) == false) {
            return GetValueNumber(CreateExpession(definingInstr));
        }
        else if(auto callInstr = definingInstr->As<CallInstr>()) {
            if(IsCallSafe(callInstr)) {
                return GetValueNumber(CreateExpession(definingInstr));
            }
        }
    }

    // Check if the operand has already a value number.
    if(opValNumbers_.TryGetValue(op, &valNumb)) {
        return valNumb;
    }
    
    valNumb = NextNumber();
    opValNumbers_.Add(op, valNumb);
    return valNumb;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unsigned ValueNumber::GetValueNumber(Expression& expr, Operand* destOp) {
    // Search for an existing value number in the current scope,
    // and if not found, in the parent table.
    unsigned valNumb;

    if(expValNumbers_.TryGetValue(expr, &valNumb)) {
        return valNumb;
    }

    // Create a value number.
    valNumb = NextNumber();
    expValNumbers_.Add(expr, valNumb);
    if(destOp) resultOpValNumbers_.Add(destOp, valNumb);
    return valNumb;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Expression ValueNumber::CreateExpession(Instruction* instr) {
    if(instr->IsArithmetic() || instr->IsLogical()) {
        unsigned valNumbs[2];
        valNumbs[0] = GetValueNumber(instr->GetSourceOp(0));
        valNumbs[1] = GetValueNumber(instr->GetSourceOp(1));

        // For commutative instructions we sort the value numbers,
        // so that, for example, 'a * 3' and '3 * a' is considered the same.
        if(instr->IsCommutative()) {
            if(valNumbs[0] > valNumbs[1]) {
                std::swap(valNumbs[0], valNumbs[1]);
            }
        }

        Expression expr(instr->GetOpcode());
        expr.AddOperand(valNumbs[0]);
        expr.AddOperand(valNumbs[1]);
        return expr;
    }
    else if(auto convInstr = instr->As<ConversionInstr>()) {
        // We include the type to which we convert.
        Expression expr(convInstr->GetOpcode());
        expr.AddOperand(GetValueNumber(convInstr->TargetOp()));
        expr.AddOperand((unsigned)convInstr->CastType());
        return expr;
    }
    else if(auto cmpInstr = instr->As<CmpInstrBase>()) {
        Expression expr(cmpInstr->GetOpcode(), cmpInstr->Order());
        expr.AddOperand(GetValueNumber(cmpInstr->LeftOp()));
        expr.AddOperand(GetValueNumber(cmpInstr->RightOp()));
        return expr;
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        return CreateCallExpression(callInstr);
    }
    else if(auto phiInstr = instr->As<PhiInstr>()) {
        return CreatePhiExpression(phiInstr);
    }

    // For all other instructions we use the value numbers of the source operands.
    Expression expr(instr->GetOpcode());

    for(int i = 0; i < instr->SourceOpCount(); i++) {
        expr.AddOperand(GetValueNumber(instr->GetSourceOp(i)));
    }

    return expr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Expression ValueNumber::CreateCallExpression(CallInstr* callInstr) {
    Expression expr(callInstr->GetOpcode(), 1);
    expr.AddOperand(GetValueNumber(callInstr->TargetOp()));
    auto arguments = callInstr->Arguments();
    bool done = false;
        
    // For the 'min' and 'max' intrinsics we order the operands based
    // on the value number. This allows us to consider 'min(a,b)' and 'min(b,a)'
    // to be the same, exposing more simplification opportunities.
    if(auto intrinsic = callInstr->GetIntrinsic()) {
        // We really shouldn't touch these!
        DebugValidator::IsFalse(intrinsic->IsMemoryIntrinsic());
        DebugValidator::IsFalse(intrinsic->IsStackIntrinsic());

        if(auto mathIntrinsic = intrinsic->As<MathIntrinsic>()) {
            if(mathIntrinsic->IsMin() || mathIntrinsic->IsMax()) {
                unsigned valNumbs[2];
                valNumbs[0] = GetValueNumber((*arguments)[0]);
                valNumbs[1] = GetValueNumber((*arguments)[1]);

                if(valNumbs[0] > valNumbs[1]) {
                    std::swap(valNumbs[0], valNumbs[1]);
                }

                expr.AddOperand(valNumbs[0]);
                expr.AddOperand(valNumbs[1]);
                done = true;
            }
        }
    }

    if(arguments && (done == false)) {
        // Add the value numbers of the arguments.
        for(int i = 0; i < arguments->Count(); i++) {
            expr.AddOperand(GetValueNumber((*arguments)[i]));
        }
    }

    return expr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Expression ValueNumber::CreatePhiExpression(PhiInstr* phiInstr) {
    // Sort the incoming values based on their block. This allows
    // 'phi {a, B1}, {b, B2}' and 'phi {b, B2}, {a, B1}' to be considered the same.
    Expression expr(phiInstr->GetOpcode());
    StaticList<int, 16> valNumbs;
    StaticList<Block*, 16> blocks;
    int count = phiInstr->OperandCount();
        
    for(int i = 0; i < count; i++) {
        valNumbs.Add(GetValueNumber(phiInstr->GetOperand(i)));
        blocks.Add(phiInstr->GetOperandBlock(i)->Target());
    }

    // We use a simple selection sort, because 'phi' usually has few operands.
    for(int i = 0; i < count; i++) {
        for(int j = i + 1; j < count; j++) {
            if(blocks[i] > blocks[j]) {
                std::swap(valNumbs[i], valNumbs[j]);
                std::swap(blocks[i], blocks[j]);
            }
        }
    }

    for(int i = 0; i < count; i++) {
        expr.AddOperand(valNumbs[i]);
    }

    return expr;
}

// ######################################################################################
// ScopedExpressionTable
// ######################################################################################
Operand* ScopedExpressionTable::GetAvailable(unsigned valNumb) {
    Operand* op;

    if(valNumberToOp_.TryGetValue(valNumb, &op)) {
        return op;
    }
    else if(parent_) return parent_->GetAvailable(valNumb);
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScopedExpressionTable::InsertAvailable(unsigned valNumb, Operand* op) {
    valNumberToOp_.Add(valNumb, op);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ScopedExpressionTable::GetOrInsert(Instruction* instr) {
    DebugValidator::IsNotNull(instr);
    Expression expr = valNumber_->CreateExpession(instr);
    unsigned valNumb = GetValueNumber(expr, instr->GetDestinationOp());

    // Try to get an available instruction with this value number.
    if(auto availInstr = GetAvailable(valNumb)) {
        return availInstr;
    }
    
    // Make this instruction available for further queries.
    InsertAvailable(valNumb, instr->GetDestinationOp());
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ScopedExpressionTable::Get(Instruction* instr) {
    Expression expr = valNumber_->CreateExpession(instr);
    unsigned valNumb = GetValueNumber(expr, instr->GetDestinationOp());

    if(auto availInstr = GetAvailable(valNumb)) {   
        return availInstr;
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unsigned ScopedExpressionTable::GetValueNumber(Instruction* instr) {
    Expression expr = valNumber_->CreateExpession(instr);
    return GetValueNumber(expr, instr->GetDestinationOp());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ScopedExpressionTable::Insert(Instruction* instr, Operand* valueOp) {
    Expression expr = valNumber_->CreateExpession(instr);
    unsigned valNumb  = GetValueNumber(expr);
    InsertAvailable(valNumb, valueOp);
}

} // namespace Analysis