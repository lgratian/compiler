// FloatArithmetic.hpp
// Copyright (c) Lup Gratian
//
// Implements helper functions that evaluate floating-point numbers according
// to the rules defined by the IEEE 754 standard.
// Supports single and double-precision numbers (float, double).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_FLOAT_ARITHMETIC_HPP
#define PC_ANALYSIS_FLOAT_ARITHMETIC_HPP

#include "IntArithmetic.hpp"
#include "../IR/Constants.hpp"
#include "../IR/IRTypes.hpp"
#include "../Abstraction/Platform.hpp"
#include <cmath>
using namespace IR;

namespace Analysis {

class FloatArithmetic {
public:
	// Returns the specified value limited to the range of the specified floating type.
	static __int64 LimitToType(__int64 value, IRFloatingKind kind) {
		if(kind == IR_Float) return (float)value;
		else return value;
	}

	static __int64 LimitToType(FloatConstant* value, const FloatingType* type) {
		return LimitToType(value->Value(), type->GetSubtype());
	}

	static __int64 LimitToType(FloatConstant* value) {
		return LimitToType(value->Value(), value->GetType()->GetSubtype());
	}

	// Methods for performing arithmetic operations.
	static double Fadd(double a, double b, IRFloatingKind kind) {
		double result = kind == IR_Float ? (double)((float)a + float(b)) : a + b;
		return result;
	}

	static double Fadd(FloatConstant* a, FloatConstant* b) {
		return Fadd(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

    static double Fadd(FloatConstant* a, double b) {
		return Fadd(a->Value(), b, a->GetType()->GetSubtype());
	}

	static double Fsub(double a, double b, IRFloatingKind kind) {
		double result = kind == IR_Float ? (double)((float)a - float(b)) : a - b;
		return result;
	}

	static double Fsub(FloatConstant* a, FloatConstant* b) {
		return Fsub(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}
    
    static double Fsub(FloatConstant* a, double b) {
		return Fsub(a->Value(), b, a->GetType()->GetSubtype());
	}

	static double Fmul(double a, double b, IRFloatingKind kind) {
		double result = kind == IR_Float ? (double)((float)a * float(b)) : a * b;
		return result;
	}

	static double Fmul(FloatConstant* a, FloatConstant* b) {
		return Fmul(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}
       
    static double Fmul(FloatConstant* a, double b) {
		return Fmul(a->Value(), b, a->GetType()->GetSubtype());
	}
    
	static double Fdiv(double a, double b, IRFloatingKind kind) {
		if(b == 0.0) return 0.0;
		double result = kind == IR_Float ? (double)((float)a / float(b)) : a / b;
		return result;
	}

	static double Fdiv(FloatConstant* a, FloatConstant* b) {
		return Fdiv(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}
        
    static double Fdiv(FloatConstant* a, double b) {
		return Fdiv(a->Value(), b, a->GetType()->GetSubtype());
	}          
     
	// Methods for performing conversions.
	static double Fext(double value, IRFloatingKind fromKind, IRFloatingKind toKind) {
		// Nothing needs to be done in this case.
		return value;
	}

	static double Fext(FloatConstant* value, const FloatingType* toKind) {
		return Fext(value->Value(), value->GetType()->GetSubtype(), toKind->GetSubtype());
	}

	static double Ftrunc(double value, IRFloatingKind fromKind, IRFloatingKind toKind) {
		if(toKind == IR_Float) return (double)((float)value);
		else return value;
	}

	static double Ftrunc(FloatConstant* value, const FloatingType* toKind) {
		return Ftrunc(value->Value(), value->GetType()->GetSubtype(), toKind->GetSubtype());
	}

	static __int64 Ftoi(double value, IRFloatingKind fromKind, IRIntegerKind toKind) {
		// Round to single value if necessary.
		__int64 result = fromKind == IR_Float ? (float)value : value;
		return IntArithmetic::LimitToType(result, toKind);
	}

	static __int64 Ftoi(FloatConstant* a, const IntegerType* toKind) {
		return Ftoi(a->Value(), a->GetType()->GetSubtype(), toKind->GetSubtype());
	}

	static __int64 Ftoui(double value, IRFloatingKind fromKind, IRIntegerKind toKind) {
		// Round to single value if necessary.
		if(fromKind == IR_Float) value = (double)((float)value);

		unsigned __int64 result = 0;
		double temp = value;

		// Limit the value to the unsigned integer range.
		if(value >= 9.2233720368547758E+18) {
			temp = value - 9.2233720368547758E+18;

			if(temp < 9.2233720368547758E+18) {
				result = 9223372036854775808L;
			}
		}
			
		return IntArithmetic::LimitToType(result + (__int64)temp, toKind);
	}

	static __int64 Ftoui(FloatConstant* a, const IntegerType* toKind) {
		return Ftoui(a->Value(), a->GetType()->GetSubtype(), toKind->GetSubtype());
	}

	// Methods for testing the relationship between two numbers.
    // Note that this tests for exact equality. 
    // For a test that uses approximation use 'AreClose' instead.
	static bool AreEqual(double a, double b, IRFloatingKind kind) {
		if(kind == IR_Float) return (float)a == (float)b;
		else return a == b;
	}

	static bool AreNotEqual(double a, double b, IRFloatingKind kind) {
		if(kind == IR_Float) return (float)a != (float)b;
		else return a != b;
	}

	static bool IsSmaller(double a, double b, IRFloatingKind kind) {
		if(kind == IR_Float) return (float)a < (float)b;
		else return a < b;
	}

	static bool IsSmallerOrEqual(double a, double b, IRFloatingKind kind) {
		if(kind == IR_Float) return (float)a <= (float)b;
		else return a <= b;
	}

	static bool IsLarger(double a, double b, IRFloatingKind kind) {
		if(kind == IR_Float) return (float)a > (float)b;
		else return a >= b;
	}

	static bool IsLargerOrEqual(double a, double b, IRFloatingKind kind) {
		if(kind == IR_Float) return (float)a >= (float)b;
		else return a >= b;
	}


	// Returns 'true' if the value is not a valid number (NaN).
	static bool IsNaN(double value, IRFloatingKind kind) {
		return Abstraction::FloatInfo::IsNaN(value);
	}

	static bool IsNaN(FloatConstant* value) {
		return IsNaN(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the value is positive (including equal to 0.0).
	static bool IsPositive(double value, IRFloatingKind kind) {
		return value >= 0.0;
	}

	static bool IsPositive(FloatConstant* value) {
		return IsPositive(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the value is negative.
	static bool IsNegative(double value, IRFloatingKind kind) {
		return value < 0.0;
	}

	static bool IsNegative(FloatConstant* value) {
		return IsNegative(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the value is 'positive infinity'.
	static bool IsPositiveInfinity(double value, IRFloatingKind kind) {
		return Abstraction::FloatInfo::IsPositiveInfinity(value);
	}

	static bool IsPositiveInfinity(FloatConstant* value) {
		return IsPositiveInfinity(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the value is 'negative infinity'.
	static bool IsNegativeInfinity(double value, IRFloatingKind kind) {
		return Abstraction::FloatInfo::IsNegativeInfinity(value);
	}

	static bool IsNegativeInfinity(FloatConstant* value) {
		return IsNegativeInfinity(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the value is represents infinity (positive or negative).
	static bool IsInfinity(double value, IRFloatingKind kind) {
		return IsPositiveInfinity(value, kind) || IsNegativeInfinity(value, kind);
	}

	static bool IsInfinity(FloatConstant* value) {
		return IsInfinity(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the value is 'positive zero'.
	static bool IsPositiveZero(double value, IRFloatingKind kind) {
		return Abstraction::FloatInfo::IsPositiveZero(value);
	}

	static bool IsPositiveZero(FloatConstant* value) {
		return IsPositiveZero(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the value is 'negative zero'.
	static bool IsNegativeZero(double value, IRFloatingKind kind) {
		return Abstraction::FloatInfo::IsNegativeZero(value);
	}

	static bool IsNegativeZero(FloatConstant* value) {
		return IsNegativeZero(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the value is 0 (positive or negative).
	static bool IsZero(double value, IRFloatingKind kind) {
		return IsPositiveZero(value, kind) || IsNegativeZero(value, kind);
	}

	static bool IsZero(FloatConstant* value) {
		return IsZero(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the value is 'positive denormalized'.
	static bool IsPositiveDenormalized(double value, IRFloatingKind kind) {
		return Abstraction::FloatInfo::IsPositiveDenormalized(value);
	}

	static bool IsPositiveDenormalized(FloatConstant* value) {
		return IsPositiveDenormalized(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the value is 'negative denormalized'.
	static bool IsNegativeDenormalized(double value, IRFloatingKind kind) {
		return Abstraction::FloatInfo::IsNegativeDenormalized(value);
	}

	static bool IsNegativeDenormalized(FloatConstant* value) {
		return IsNegativeDenormalized(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the value is 'positive denormalized' (positive or negative).
	static bool IsDenormalized(double value, IRFloatingKind kind) {
		return IsPositiveDenormalized(value, kind) || IsNegativeDenormalized(value, kind);
	}

	static bool IsDenormalized(FloatConstant* value) {
		return IsDenormalized(value->Value(), value->GetType()->GetSubtype());
	}

	// The size, in bits, of the fraction part for the specified floating type,
	// as defined by the IEEE 754 standard.
	static int FractionSizeInBits(IRFloatingKind kind) {
		switch(kind) {
			case IR_Float:  return 23; // single precision
			case IR_Double: return 52; // double precision
			default: DebugValidator::Unreachable();
		}

		return 0;
	}

	static int FractionSizeInBits(const FloatingType* type) {
		return FractionSizeInBits(type->GetSubtype());
	}

	// The size, in bits, of the exponent part for the specified floating type,
	// as defined by the IEEE 754 standard.
	static int ExponentSizeInBits(IRFloatingKind kind) {
		switch(kind) {
			case IR_Float: return 8;	// single precision
			case IR_Double: return 11;	// double precision
			default: DebugValidator::Unreachable();
		}

		return 0;
	}

	static int ExponentSizeInBits(const FloatingType* type) {
		return ExponentSizeInBits(type->GetSubtype());
	}

    // Returns 'true' if the specified values are close by a very small error margin.
    static bool AreClose(double a, double b, IRFloatingKind kind) {
        if(kind == IR_Float) return std::fabs((float)a - float(b)) < 0.000001;
        else return std::fabs(a - b) < 0.000000001;
    }

    static bool AreClose(FloatConstant* a, FloatConstant* b) {
        return AreClose(a->Value(), b->Value(), a->GetType()->GetSubtype());
    }

    static bool AreClose(FloatConstant* a, double b) {
        return AreClose(a->Value(), b, a->GetType()->GetSubtype());
    }
};

} // namespace Analysis
#endif