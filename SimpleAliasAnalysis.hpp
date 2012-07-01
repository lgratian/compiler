// SimpleAliasAnalysis.hpp
// Copyright (c) Lup Gratian
//
// Implements a simple rule-based alias analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SIMPLE_ALIAS_ANALYSIS_HPP
#define PC_ANALYSIS_SIMPLE_ALIAS_ANALYSIS_HPP

#include "AliasInfo.hpp"
#include "AliasAnalyzer.hpp"
#include "CFamilyTag.hpp"
#include "../Base/DebugValidator.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Intrinsics.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class SimpleAliasAnalysis : public AliasAnalyzer {
private:
    // Handles pairs consisting of a variable and
    // an operand of other type.
    AliasResult ComputeSingleVariableAlias(VariableReference* variableRef, 
                                           Operand* other);
    
    // Handles pairs consisting of two variables.
    AliasResult ComputeVariableAlias(AliasLocation& locationA, 
                                     AliasLocation& locationB);

    // Handles pairs consisting of two parameters
    // or a parameter and an operand of other type.
    AliasResult ComputeParameterAlias(AliasLocation& locationA, 
                                      AliasLocation& locationB);

    // Handles operands originated from aggregate variables.
    AliasResult ComputeAggregateAlias(AliasLocation& locationA, 
                                      AliasLocation& locationB);

    AliasResult ComputeAllocAlias(AliasLocation& locationA, 
                                  AliasLocation& locationB);

public:
    SimpleAliasAnalysis(AliasInfo* parent) : AliasAnalyzer(parent) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual AliasResult ComputeAlias(AliasLocation locationA, 
                                     AliasLocation locationB);

    virtual MemoryResult ComputeCallAlias(CallInstr* instr, 
                                          AliasLocation location);

    virtual bool HandlesCalls() const {
        return true;
    }

    virtual string Name() {
        return "Simple Alias Analysis";
    }
};

} // namespace Analysis
#endif