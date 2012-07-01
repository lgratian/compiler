// LanguageInfo.cpp
// Copyright (c) Lup Gratian
//
// Base class that provides information about various
// IR aspects that are language-dependent.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_LANGUAGE_INFO_HPP
#define PC_ANALYSIS_LANGUAGE_INFO_HPP

#include "../IR/Instructions.hpp"
#include "../IR/Operand.hpp"
using namespace IR;

namespace Analysis {

// Provides alias information for the language.
class AliasResultProvider {
public:
    virtual ~AliasResultProvider() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Should return 'true' if there may be alias between the locations.
    virtual bool MightBeAlias(Operand* a, Operand* b) {
        return true;
    }

    // Should return 'true' if there is no alias between the locations.    
    virtual bool IsDefinitelyNoAlias(Operand* a, Operand* b) {
        return false;
    }
};

class LanguageInfo /* TODO : public Analysis */ {
public:
    virtual ~LanguageInfo() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns 'true' if the called function has any side-effect
    // (writes to global memory or trough a pointer parameter, 
    // calls a function with side-effects, etc.).
    virtual bool CallMayHaveSideEffects(CallInstr* instr) const {
        return true;
    }

    // Returns 'true' if the called function captures 
    // any of it's pointer parameters (a parameter is captured
    // if its value is written to non-local memory or returned).
    virtual bool CallMayCaptureParameters(CallInstr* instr) const {
        return true;
    }

    // Returns 'true' if the specified parameter is captured
    // by the called function. If the parameter is returned by the function
    // 'paramReturned' is set to 'true'.
    virtual bool CallMayCaptureParameter(CallInstr* instr, int paramIndex,
                                         bool* paramReturned = nullptr) const {
        return true;
    }

    virtual bool CallMayCaptureParameter(Function* function, CallInstr* instr,
                                         int paramIndex, bool* paramReturned = nullptr) const {
        return true;
    }

    // Returns 'true' if the called function may read
    // from the specified address.
    virtual bool CallMayReadFromAddress(CallInstr* instr, Operand* addressOp, 
                                        AliasResultProvider* aliasResult = nullptr) const {
        return true;
    }

    // Returns 'true' if the called function may write
    // to the specified address.
    virtual bool CallMayWriteToAddress(CallInstr* instr, Operand* addressOp, 
                                       AliasResultProvider* aliasResult = nullptr) const {
        return true;
    }
};

} // namespace Analysis
#endif