// CFamilyLanguageInfo.cpp
// Copyright (c) Lup Gratian
//
// Provides information about C-like languages.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_C_FAMILY_LANGUAGE_INFO_HPP
#define PC_ANALYSIS_C_FAMILY_LANGUAGE_INFO_HPP

#include "LanguageInfo.hpp"
#include "StdLibRecognizer.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace Analysis {

class CFamilyLanguageInfo : public LanguageInfo {
private:
    bool IsStdlibCall(CallInstr* instr, StdlibType& type,
                      StdlibCategory& category) const;

    bool IsStdlibCall(Function* function, StdlibType& type,
                      StdlibCategory& category) const;

    bool IsErrnoUsed() const {
        //! TODO: Use user config setting or scan entire app.
        return false;
    }

public:
    virtual bool CallMayHaveSideEffects(CallInstr* instr) const;

    virtual bool CallMayCaptureParameters(CallInstr* instr) const;

    virtual bool CallMayCaptureParameter(CallInstr* instr, int paramIndex,
                                         bool* paramReturned = nullptr) {
        if(auto function = instr->GetCalledFunction()) {
            return CallMayCaptureParameter(function, instr,
                                           paramIndex, paramReturned);
        }

        return true;
    }

    virtual bool CallMayCaptureParameter(Function* function, CallInstr* instr, 
                                         int paramIndex, bool* paramReturned = nullptr) const;

    virtual bool CallMayReadFromAddress(CallInstr* instr, Operand* addressOp, 
                                        AliasResultProvider* aliasResult = nullptr) const;

    virtual bool CallMayWriteToAddress(CallInstr* instr, Operand* addressOp, 
                                       AliasResultProvider* aliasResult = nullptr) const;
};

} // namespace Analysis
#endif