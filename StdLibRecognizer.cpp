// StdlibRecognizer.cpp
// Copyright (c) Lup Gratian
//
// Implements the StdlibRecognizer class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "StdlibRecognizer.hpp"

namespace Analysis {

// Used to mark that the function table has been initialized.
bool StdlibRecognizer::initialized_ = false;
StdlibRecognizer::TDescriptorDict StdlibRecognizer::stdLibFuncts_=
	StdlibRecognizer::TDescriptorDict(64);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StdlibRecognizer::Initialize() {
	if(initialized_) return;
	#define STDLIB_FUNCT(TYPE, NAME, PARAMS, CATEGORY) \
		stdLibFuncts_.Add(NAME, new StdLibDescriptor(TYPE,  NAME, PARAMS, CATEGORY))

	STDLIB_FUNCT(Stdlib_memcpy,  "memcpy",  3, StdLibCategory_Memory);
	STDLIB_FUNCT(Stdlib_memset,  "memset",  3, StdLibCategory_Memory);
	STDLIB_FUNCT(Stdlib_memmove, "memmove", 3, StdLibCategory_Memory);
	STDLIB_FUNCT(Stdlib_memcmp,  "memcmp",  3, StdLibCategory_Memory);
	STDLIB_FUNCT(Stdlib_malloc,  "malloc",  1, StdLibCategory_Memory);
    STDLIB_FUNCT(Stdlib_free,    "free",    1, StdLibCategory_Memory);

	STDLIB_FUNCT(Stdlib_strlen,  "strlen",  1, StdLibCategory_String);
	STDLIB_FUNCT(Stdlib_strcpy,  "strcpy",  2, StdLibCategory_String);
	STDLIB_FUNCT(Stdlib_strncpy, "strncpy", 3, StdLibCategory_String);
	STDLIB_FUNCT(Stdlib_strcmp,  "strcmp",  2, StdLibCategory_String);
	STDLIB_FUNCT(Stdlib_strncmp, "strncmp", 3, StdLibCategory_String);
	STDLIB_FUNCT(Stdlib_strcat,  "strcat",  2, StdLibCategory_String);
	STDLIB_FUNCT(Stdlib_strncat, "strncat", 3, StdLibCategory_String);
	STDLIB_FUNCT(Stdlib_strchr,  "strchr",  2, StdLibCategory_String);
	STDLIB_FUNCT(Stdlib_strstr,  "strstr",  2, StdLibCategory_String);
	STDLIB_FUNCT(Stdlib_strpbrk, "strpbrk", 2, StdLibCategory_String);
	STDLIB_FUNCT(Stdlib_strspn,  "strspn",  2, StdLibCategory_String);
	STDLIB_FUNCT(Stdlib_strcspn, "strcspn", 2, StdLibCategory_String);

	STDLIB_FUNCT(Stdlib_abs,    "abs",     1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_fabs,   "fabs",    1, StdLibCategory_Math);
    STDLIB_FUNCT(Stdlib_labs,   "labs",    1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_pow,    "pow",     2, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_powf,   "powf",    2, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_sqrt,   "sqrt",    1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_sqrtf,  "sqrtf",   1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_exp,    "exp",     1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_expf,   "expf",    1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_floor,  "floor",   1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_floorf, "floorf",  1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_ceil,   "ceil",    1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_ceilf,  "ceilf",   1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_log,    "log",     1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_logf,   "logf",    1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_log10,  "log10",   1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_log10f, "log10f",  1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_fmod,   "fmod",    2, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_fmodf,  "fmodf",   2, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_sin,    "sin",     1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_sinf,   "sinf",    1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_asin,   "asin",    1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_asinf,  "asinf",   1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_cos,    "cos",     1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_cosf,   "cosf",    1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_acos,   "acos",    1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_acosf,  "acosf",   1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_tan,    "tan",     1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_tanf,   "tanf",    1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_atan,   "atan",    1, StdLibCategory_Math);
    STDLIB_FUNCT(Stdlib_atan2,  "atan2",   1, StdLibCategory_Math);
	STDLIB_FUNCT(Stdlib_atanf,  "atanf",   1, StdLibCategory_Math);

	STDLIB_FUNCT(Stdlib_isdigit, "isdigit", 1, StdLibCategory_Other);
	STDLIB_FUNCT(Stdlib_isascii, "isascii", 1, StdLibCategory_Other);
    STDLIB_FUNCT(Stdlib_exit,    "exit",    1, StdLibCategory_Other);
    STDLIB_FUNCT(Stdlib_abort,   "abort",   0, StdLibCategory_Other);
    STDLIB_FUNCT(Stdlib_setjmp,  "setjmp",  1, StdLibCategory_Other);
    STDLIB_FUNCT(Stdlib_longjmp, "longjmp", 2, StdLibCategory_Other);
    STDLIB_FUNCT(Stdlib_alloca,  "alloca",  1, StdLibCategory_Other);

	#undef STDLIB_FUNCT
	initialized_ = true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StdLibDescriptor* StdlibRecognizer::GetDescriptor(const Function* function) {
	StdLibDescriptor* descriptor;

	if(stdLibFuncts_.TryGetValue(*function->Name(), &descriptor)) {
		if(function->ParameterCount() == descriptor->Parameters) {
			return descriptor;
		}
	}
	
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StdlibType StdlibRecognizer::Recognize(const Function* function, 
                                       StdlibCategory* category) {
	// If the function is unnamed give up.
	if(function->HasName() == false) {
		return Stdlib_None;
	}

	// The function must be marked as originating from the standard library.
	// This prevents situations where the user wrote a function that looks
	// exactly like one from the standard library, but does something different.
	if(function->IsFromStdlib() == false) {
		return Stdlib_None;
	}

	// Try to get a descriptor for the specified function. If one is returned, 
	// a function with this name and the same number of parameters was found.
	Initialize();
	StdLibDescriptor* descriptor = GetDescriptor(function);
	if(descriptor == nullptr) return Stdlib_None;

	// Now test the types of the parameters, because we must be sure the function
	// matches the one from the standard library.
	auto& parameters = function->Parameters();

	switch(descriptor->Type) {
		case Stdlib_fabs:  
		case Stdlib_pow:   
		case Stdlib_sqrt:  
		case Stdlib_exp:   
		case Stdlib_floor: 
		case Stdlib_ceil:  
		case Stdlib_log:   
		case Stdlib_log10: 
		case Stdlib_fmod:  
		case Stdlib_sin:   
		case Stdlib_asin:  
		case Stdlib_cos:   
		case Stdlib_acos:  
		case Stdlib_tan:   
		case Stdlib_atan:
        case Stdlib_atan2: {
			// The types of the parameters must be 'double'.
			for(int i = 0; i < parameters.Count(); i++) {
				if(parameters[i]->GetType()->IsDouble() == false) {
					return Stdlib_None;
				}
			}
			
			if(function->ReturnType()->IsDouble() == false) {
				return Stdlib_None;
			}
			break;
		}
		case Stdlib_powf:  
		case Stdlib_sqrtf: 
		case Stdlib_expf:  
		case Stdlib_floorf:
		case Stdlib_ceilf: 
		case Stdlib_logf:  
		case Stdlib_log10f:
		case Stdlib_fmodf: 
		case Stdlib_sinf:  
		case Stdlib_asinf: 
		case Stdlib_cosf:  
		case Stdlib_acosf: 
		case Stdlib_tanf:  
		case Stdlib_atanf: {
			// The type of the parameters must be 'float'.
			for(int i = 0; i < parameters.Count(); i++) {
				if(parameters[i]->GetType()->IsFloat() == false) {
					return Stdlib_None;
				}
			}

			if(function->ReturnType()->IsFloat() == false) {
				return Stdlib_None;
			}
			break;
		}
        case Stdlib_abs: {
            if(parameters[0]->GetType()->IsInt32() == false) {
				return Stdlib_None;
			}

			if(function->ReturnType()->IsInt32() == false) {
				return Stdlib_None;
			}
			break;
        }
        case Stdlib_labs: {
            if(parameters[0]->GetType()->IsInt64() == false) {
				return Stdlib_None;
			}

			if(function->ReturnType()->IsInt64() == false) {
				return Stdlib_None;
			}
			break;
        }
		case Stdlib_memcpy:
		case Stdlib_memmove:
		case Stdlib_memcmp: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt64()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt64()) &&
				(parameters[2]->GetType()->IsInt64() || 
                 parameters[2]->GetType()->IsInt32())) == false) {
				return Stdlib_None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt64()) == false) {
				return Stdlib_None;
			}
			break;
		}
		case Stdlib_memset: {
			if((parameters[0]->GetType()->IsPointer() &&
				parameters[0]->GetType()->As<PointerType>()
                                        ->PointeeType()->IsInt64()) &&
				parameters[1]->GetType()->IsInt32() && 
			   (parameters[2]->GetType()->IsInt64() ||
                parameters[2]->GetType()->IsInt32()) == false) {
				return Stdlib_None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt64()) == false) {
				return Stdlib_None;
			}
			break;
		}
		case Stdlib_malloc: {
			if((parameters[0]->GetType()->IsInt32() ||
                parameters[0]->GetType()->IsInt64()) == false) {
				return Stdlib_None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt64()) == false) {
				return Stdlib_None;
			}
			break;
		}
        case Stdlib_free: {
			if((parameters[0]->IsPointer() &&
				parameters[0]->GetType()->As<PointerType>()
                                        ->PointeeType()->IsInt64()) == false) {
				return Stdlib_None;
			}

            if(function->ReturnType()->IsVoid() == false) {
				return Stdlib_None;
			}
			break;
		}
		case Stdlib_isascii:
		case Stdlib_isdigit: {
			if(parameters[0]->GetType()->IsInt32() == false) {
				return Stdlib_None;
			}

			if(function->ReturnType()->IsInt32() == false) {
				return Stdlib_None;
			}
			break;
		}
		case Stdlib_strcpy: 
		case Stdlib_strstr:
		case Stdlib_strpbrk:
		case Stdlib_strcat: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()    
                                         ->PointeeType()->IsInt8()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8())) == false) {
				return Stdlib_None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt8()) == false) {
				return Stdlib_None;
			}
			break;
		}
		case Stdlib_strlen: {
			if((parameters[0]->GetType()->IsPointer() &&
				parameters[0]->GetType()->As<PointerType>()
                                        ->PointeeType()->IsInt8()) == false) {
				return Stdlib_None;
			}

			if(function->ReturnType()->IsInt32() == false) {
				return Stdlib_None;
			}
			break;
		}
        case Stdlib_strncpy: {
            if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8())) == false) {
				return Stdlib_None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt8()) == false) {
				return Stdlib_None;
			}
			break;
		}
		case Stdlib_strcmp: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8())) == false) {
				return Stdlib_None;
			}

			if(function->ReturnType()->IsInt32() == false) {
				return Stdlib_None;
			}
			break;
		}
		case Stdlib_strcspn:
		case Stdlib_strncmp:
		case Stdlib_strncat: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				(parameters[2]->GetType()->IsInt64() || 
                 parameters[2]->GetType()->IsInt32())) == false) {
				return Stdlib_None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt8()) == false) {
				return Stdlib_None;
			}
			break;
		}
		case Stdlib_strchr: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				 parameters[1]->GetType()->IsInt32()) == false) {
				return Stdlib_None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt8()) == false) {
				return Stdlib_None;
			}
			break;
		}
		case Stdlib_strspn: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8())) == false) {
				return Stdlib_None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt64()) == false) {
				return Stdlib_None;
			}
			break;
		}
        case Stdlib_abort: {
            if(function->ReturnType()->IsVoid() == false) {
                return Stdlib_None;
            }
            break;
        }
        case Stdlib_exit: {
            if(parameters[0]->GetType()->IsInt32() == false) {
				return Stdlib_None;
			}

            if(function->ReturnType()->IsVoid() == false) {
                return Stdlib_None;
            }
            break;
        }
        case Stdlib_setjmp: {
            if(parameters[0]->GetType()->IsPointer() == false) {
				return Stdlib_None;
			}

            if(function->ReturnType()->IsInt32() == false) {
                return Stdlib_None;
            }
            break;
        }
        case Stdlib_longjmp: {
            if((parameters[0]->GetType()->IsPointer() == false) ||
               (parameters[1]->GetType()->IsInt32() == false)) {
				return Stdlib_None;
			}

            if(function->ReturnType()->IsVoid() == false) {
                return Stdlib_None;
            }
            break;
        }
        case Stdlib_alloca: {
            if(parameters[0]->GetType()->IsInt32() == false) {
				return Stdlib_None;
			}
            break;
        }
	}

	if(category) {
		// The user requests the category of the function.
		*category = descriptor->Category;
	}

	return descriptor->Type;
}

} // namespace Analysis