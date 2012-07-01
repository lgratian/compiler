// StdlibRecognizer.hpp
// Copyright (c) Lup Gratian
//
// Defines a helper that recognizes functions from the C standard library.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_STD_LIB_RECOGNIZER_HPP
#define PC_ANALYSIS_STD_LIB_RECOGNIZER_HPP

#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/IRTypes.hpp"
#include "../Base/Dictionary.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

// Represents the category to which a standard function belongs.
enum StdlibCategory {
	StdLibCategory_None, // This is not a standard library function.
	StdLibCategory_Memory,
	StdLibCategory_String,
	StdLibCategory_Math,
	StdLibCategory_Other
};


// Represents the functions that are recognized.
enum StdlibType {
	Stdlib_None, // This is not a standard library function.

	// Memory
	Stdlib_memcpy,
	Stdlib_memset,
	Stdlib_memmove,
	Stdlib_memcmp,
	Stdlib_malloc,
    Stdlib_free,

	// String
	Stdlib_strlen,
	Stdlib_strcpy,
	Stdlib_strncpy,
	Stdlib_strcmp,
	Stdlib_strncmp,
	Stdlib_strcat,
	Stdlib_strncat,
	Stdlib_strchr,
	Stdlib_strstr,
	Stdlib_strpbrk,
	Stdlib_strspn,
	Stdlib_strcspn,

	//! TODO: Add support for wide characters string functions.
    //! TODO: Recognize setjmp / longjmp

	// Math
	Stdlib_abs, 
	Stdlib_fabs, 
    Stdlib_labs,
	Stdlib_pow,  
	Stdlib_sqrt, 
	Stdlib_exp,  
	Stdlib_floor,
	Stdlib_ceil, 
	Stdlib_log,  
	Stdlib_log10,
	Stdlib_fmod, 
	Stdlib_sin,  
	Stdlib_asin, 
	Stdlib_cos,  
	Stdlib_acos, 
	Stdlib_tan,  
	Stdlib_atan, 
    Stdlib_atan2,

	// Math (float)
	Stdlib_powf,  
	Stdlib_sqrtf, 
	Stdlib_expf,  
	Stdlib_floorf,
	Stdlib_ceilf, 
	Stdlib_logf,  
	Stdlib_log10f,
	Stdlib_fmodf, 
	Stdlib_sinf,  
	Stdlib_asinf, 
	Stdlib_cosf,  
	Stdlib_acosf, 
	Stdlib_tanf,  
	Stdlib_atanf, 

	// Other
	Stdlib_isdigit,
	Stdlib_isascii,
    Stdlib_exit,
    Stdlib_abort,
    Stdlib_alloca,
    Stdlib_setjmp,
    Stdlib_longjmp
};


// Describes a function from the standard library.
struct StdLibDescriptor {
	StdlibType Type;         // The type associated with the function.
	const string Name;       // The name, as written in the source file.
	int Parameters;          // The number of expected parameters.
	StdlibCategory Category; // The category to which the function belongs.

	StdLibDescriptor(StdlibType type, const string& name, 
					 int parameters, StdlibCategory category) :
			Type(type), Name(name), Parameters(parameters), Category(category) {}

	StdLibDescriptor(const StdLibDescriptor& other) :
			Type(other.Type), Name(other.Name), Parameters(other.Parameters),
			Category(other.Category) {}
};


class StdlibRecognizer {
private:
	typedef Dictionary<const string, StdLibDescriptor*> TDescriptorDict; 

	static bool initialized_;
	static TDescriptorDict stdLibFuncts_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Initializes the table with the supported functions.
	static void Initialize();

	// Returns a descriptor for the specified function if it seems to be
	// part of the standard library.
	static StdLibDescriptor* GetDescriptor(const Function* function);

public:
	// Returns the type of the specified function if it's part of the standard library,
	// or 'Stdlib_None' if it's not. If 'category' is provided, the category in which
	// the the standard function belongs is stored into it.
	static StdlibType Recognize(const Function* function, 
                                StdlibCategory* category = nullptr);

	// Returns the type of the function called by the specified instruction.
	// Behaves exactly like the 'Recognize' in rest.
	static StdlibType Recognize(const CallInstr* instr, 
                                StdlibCategory* category = nullptr) {
		if(auto function = instr->TargetOp()->As<FunctionReference>()) {
			return Recognize(function->Target(), category);
		}

		return Stdlib_None;
	}

	// Returns 'true' if the specified function is part of the standard library.
	static bool IsFromStdLib(const Function* function) {
		return Recognize(function) != Stdlib_None;
	}
};

} // namespace Analysis
#endif