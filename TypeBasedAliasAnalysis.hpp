// TypeBasedAliasAnalysis.hpp
// Copyright (c) Lup Gratian
//
// Implements Type Based alias analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_TYPE_BASED_ALIAS_ANALYSIS_HPP
#define PC_ANALYSIS_TYPE_BASED_ALIAS_ANALYSIS_HPP

#include "AliasInfo.hpp"
#include "AliasAnalyzer.hpp"
#include "TypeClassTag.hpp"
#include "../Base/DebugValidator.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Intrinsics.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class TypeBasedAliasAnalysis : public AliasAnalyzer {
private:
    // Returns 'true' if the type represented by 'tagA'
    // is the base class of 'tagB', meaning that there might be alias 
    // because the pointers might point to the same locations.
    bool IsABaseClassForB(TypeClassTag* tagA, TypeClassTag* tagB);

    TypeClassTag* GetTag(Operand* op);

public:
    TypeBasedAliasAnalysis(AliasInfo* parent) : AliasAnalyzer(parent) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual AliasResult ComputeAlias(AliasLocation locationA, 
                                     AliasLocation locationB);

    virtual string Name() {
        return "Type Based Alias Analysis";
    }
};

} // namespace Analysis 
#endif