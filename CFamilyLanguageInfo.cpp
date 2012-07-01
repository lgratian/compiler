// CFamilyLanguageInfo.cpp
// Copyright (c) Lup Gratian
//
// Implements the CFamilyLanguageInfo analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "CFamilyLanguageInfo.hpp"

namespace Analysis {

bool CFamilyLanguageInfo::IsStdlibCall(CallInstr* instr, StdlibType& type,
                                       StdlibCategory& category) const {
    if(auto function = instr->GetCalledFunction()) {
        type = StdlibRecognizer::Recognize(function, &category);
        return type != Stdlib_None;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::IsStdlibCall(Function* function, StdlibType& type,
                                       StdlibCategory& category) const {
    type = StdlibRecognizer::Recognize(function, &category);
    return type != Stdlib_None;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayHaveSideEffects(CallInstr* instr) const {
    DebugValidator::IsNotNull(instr);
    
    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(instr, type, category)) {
        // Math functions don't have side effects only if
        // 'errno' is not used by the application.
        if(((category == StdLibCategory_Math) && (IsErrnoUsed() == false)) ||
           (type == Stdlib_memcmp)  ||
           (type == Stdlib_strlen)  ||
           (type == Stdlib_strcmp)  ||
           (type == Stdlib_strncmp) ||
           (type == Stdlib_strchr)  ||
           (type == Stdlib_isdigit) ||
           (type == Stdlib_isascii)) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayCaptureParameters(CallInstr* instr) const {
    DebugValidator::IsNotNull(instr);

    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(instr, type, category)) {
        // The only standard library functions that capture
        // a parameter are the ones that return it.
        switch(type) {
            case Stdlib_memcpy:
            case Stdlib_memmove:
            case Stdlib_memset:
            case Stdlib_strcpy:
            case Stdlib_strncpy:
            case Stdlib_strncat:
            case Stdlib_strchr:
            case Stdlib_strstr:
            case Stdlib_strpbrk: {
                return instr->HasDestinationOp() &&
                       instr->GetDestinationOp()->HasUsers();
            }
            default: return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayCaptureParameter(Function* function, CallInstr* instr,
                                                  int paramIndex, bool* paramReturned) const {
    DebugValidator::IsNotNull(function);

    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(function, type, category)) {
        // The only standard library functions that capture
        // a parameter are the ones that return it, and this
        // can only be the first (destination) parameter.
        switch(type) {
            case Stdlib_memcpy:
            case Stdlib_memmove:
            case Stdlib_memset:
            case Stdlib_strcpy:
            case Stdlib_strncpy:
            case Stdlib_strncat:
            case Stdlib_strchr:
            case Stdlib_strstr:
            case Stdlib_strpbrk: {
                if((paramIndex == 0) &&
                   instr->HasDestinationOp() &&
                   instr->GetDestinationOp()->HasUsers()) {
                    if(paramReturned) {
                        *paramReturned = true;
                    }

                    return true;
                }
            }
            default: return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayReadFromAddress(CallInstr* instr, Operand* addressOp,
                                                 AliasResultProvider* aliasResult) const {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(addressOp);

    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(instr, type, category)) {
        // All functions in the 'math' category don't read from user memory.
        if(category == StdLibCategory_Math) {
            return false;
        }

        // Try to take a decision based on the type 
        // of the standard library function.
        switch(type) {
            case Stdlib_memcpy:
            case Stdlib_memmove:
            case Stdlib_strcpy:
            case Stdlib_strncpy: {
                // We may prove that these memory-copying functions read
                // from a different variable than the one referenced by the address.
                if(aliasResult) {
                    auto source = instr->GetArgument(1);

                    if(aliasResult->IsDefinitelyNoAlias(addressOp, source)) {
                        return false;
                    }
                }

                return true;
            }
            case Stdlib_strcat:
            case Stdlib_strncat:
            case Stdlib_memcmp:
            case Stdlib_strcmp:
            case Stdlib_strncmp:
            case Stdlib_strstr: {
                // As above, but in this case we need to consider both arguments.
                if(aliasResult) {
                    auto first = instr->GetArgument(0);
                    auto second = instr->GetArgument(0);

                    if(aliasResult->IsDefinitelyNoAlias(addressOp, first) &&
                       aliasResult->IsDefinitelyNoAlias(addressOp, second)) {
                        return false;
                    }
                }
                
                return true;
            }
            case Stdlib_strlen:
            case Stdlib_strchr: {
                // As above, but only the first argument needs to be considered.
                if(aliasResult) {
                    auto source = instr->GetArgument(0);
                    
                    if(aliasResult->IsDefinitelyNoAlias(addressOp, source)) {
                        return false;
                    }
                }
                
                return true;
            }
            case Stdlib_isdigit:
            case Stdlib_isascii: {
                // These functions never read to user memory.
                return false;
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayWriteToAddress(CallInstr* instr, Operand* addressOp,
                                                AliasResultProvider* aliasResult) const {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(addressOp);

    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(instr, type, category)) {
        // All functions in the 'math' category don't write to user memory.
        if(category == StdLibCategory_Math) {
            return false;
        }

        // Try to take a decision based on the type 
        // of the standard library function.
        switch(type) {
            case Stdlib_memcpy:
            case Stdlib_memset:
            case Stdlib_memmove:
            case Stdlib_strcpy:
            case Stdlib_strncpy:
            case Stdlib_strcat:
            case Stdlib_strncat: {
                // We may prove that these memory-related functions
                // write to a different variable than the one from which we load.
                if(aliasResult) {
                    auto dest = instr->GetArgument(0);
                    
                    if(aliasResult->IsDefinitelyNoAlias(addressOp, dest)) {
                        return false;
                    }
                }

                return true;
            }
            case Stdlib_memcmp:
            case Stdlib_strlen:
            case Stdlib_strcmp:
            case Stdlib_strncmp:
            case Stdlib_strchr:
            case Stdlib_strstr:
            case Stdlib_isdigit:
            case Stdlib_isascii: {
                // These functions never write to user memory.
                return false;
            }
        }
    }

    return true;
}

} // namespace Analysis