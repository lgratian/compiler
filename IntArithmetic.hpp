// IntArithmetic.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_INT_ARITHMETIC_HPP
#define PC_ANALYSIS_INT_ARITHMETIC_HPP

#include "../IR/Constants.hpp"
#include "../IR/IRTypes.hpp"
#include "../Abstraction/Platform.hpp"
using namespace IR;

namespace Analysis {

class IntArithmetic {
public:
    // Sign-extends the specified value.
	static __int64 SignExtend(__int64 value, IRIntegerKind fromKind) {
		int fromBits = TypeBits(fromKind);
		int toBits = 64;
		return (value << (toBits - fromBits)) >> (toBits - fromBits);
	}

	// Returns a bitmask that represents the -1 number (all bits set).
	static __int64 GetMinusOneMask(IRIntegerKind kind) {
		switch(kind) {
			case IR_Int8:  return (__int64)0xFF;
			case IR_Int16: return (__int64)0xFFFF;
			case IR_Int32: return (__int64)0xFFFFFFFF;
			case IR_Int64: return (__int64)0xFFFFFFFFFFFFFFFF;
		}

		DebugValidator::Unreachable();
		return 0;
	}

	// Returns the minimum value that can be represented using the specified integer kind.
	static __int64 GetMin(IRIntegerKind kind, bool isSigned) {
		if(isSigned) return -GetSignBitMask(kind);
		else return 0;
	}

	static __int64 GetMin(const IntegerType* type, bool isSigned) {
		return GetMin(type->GetSubtype(), isSigned);
	}

	// Returns the maximum value that can be represented using the specified integer kind.
	static __int64 GetMax(IRIntegerKind kind, bool isSigned) {
		__int64 result = GetMinusOneMask(kind);
		
		if(isSigned) {
			// The rightmost bit is used for the sign.
			result = (__int64)(((unsigned __int64)result) >> 1);
		}

        return result;
	}

	static __int64 GetMax(const IntegerType* type, bool isSigned) {
		return GetMax(type->GetSubtype(), isSigned);
	}

	// Returns the specified value limited to the range of the specified integer type.
	static __int64 LimitToType(__int64 value, IRIntegerKind kind, 
                               bool isSigned = true) {
        if(isSigned) {
		    switch(kind) {
			    case IR_Int8:  { value = (value << (64 -  8)) >> (64 - 8);  break; }
			    case IR_Int16: { value = (value << (64 - 16)) >> (64 - 16); break; }
			    case IR_Int32: { value = (value << (64 - 32)) >> (64 - 32); break; }
		    }
        }
        else {
            value &= GetMinusOneMask(kind);
        }

		return value;
	}

	static __int64 LimitToType(IntConstant* value, const IntegerType* type,
                               bool isSigned = true) {
		return LimitToType(value->Value(), type->GetSubtype(), isSigned);
	}

    static __int64 LimitToType(__int64 value, const IntegerType* type,
                               bool isSigned = true) {
		return LimitToType(value, type->GetSubtype(), isSigned);
	}

    static __int64 LimitToType(IntConstant* value, bool isSigned = true) {
		return LimitToType(value->Value(), value->GetType()->GetSubtype(), isSigned);
	}

	static __int64 GetMinusOneMask(const IntegerType* type) {
		return GetMinusOneMask(type->GetSubtype());
	}

	// Returns the number of bits in the specified integer type.
	static int TypeBits(IRIntegerKind kind) {
		switch(kind) {
			case IR_Int8:  return 8;
			case IR_Int16: return 16;
			case IR_Int32: return 32;
			case IR_Int64: return 64;
		}

		DebugValidator::Unreachable();
		return 0;
	}

	static int TypeBits(const IntegerType* type) {
		return TypeBits(type->GetSubtype());
	}

	// Returns the number of bits set to one in the specified number.
	static int OneBits(__int64 value, IRIntegerKind kind) {
		value = LimitToType(value, kind, false /* no sign extension */);
		value = value - ((value >> 1) & 0x5555555555555555ULL);
		value = (value & 0x3333333333333333ULL) + ((value >> 2) & 0x3333333333333333ULL);
		return (int)((((value + (value >> 4)) & 0xF0F0F0F0F0F0F0FULL) * 
					    0x101010101010101ULL) >> 56);
	}

	static int OneBits(IntConstant* value) {
		// A very fast method that counts in parallel.
		// http://en.wikipedia.org/wiki/Hamming_weight#Efficient_implementation
		return OneBits(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns the number of bits set to zero in the specified number.
	static int ZeroBits(__int64 value, IRIntegerKind kind) {
        int typeBits = TypeBits(kind);
		return typeBits - std::min(OneBits(value, kind), typeBits);
	}

	static int ZeroBits(IntConstant* value) {
        return ZeroBits(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns the number of bits set to zero in the right part of the specified number.
    // For example, 0xFF00 has 8 zero bits.    
	static int RightmostZeroBits(__int64 value, IRIntegerKind kind) {
        value = LimitToType(value, kind, false /* no sign extension */);

        if (value & 0x1) {
            // Special case for odd value.
            return 0;
        }
        else {
            int count = 1; // First bit is definitely zero.

            if ((value & 0xffffffff) == 0) {  
                value >>= 32;  
                count += 32;
            }

            if ((value & 0xffff) == 0) {  
                value >>= 16;  
                count += 16;
            }

            if ((value & 0xff) == 0) {  
                value >>= 8;  
                count += 8;
            }

            if ((value & 0xf) == 0) {  
                value >>= 4;
                count += 4;
            }

            if ((value & 0x3) == 0) {  
                value >>= 2;
                count += 2;
            }

            count -= value & 0x1;
            return count;
        }	
	}

	static int RightmostZeroBits(IntConstant* value) {
		return RightmostZeroBits(value->Value(), value->GetType()->GetSubtype());
	}

    // Returns the number of bits set to zero in the right part of the specified number.
    // For example, 0x00FF has 16 one bits.    
    static int RightmostOneBits(__int64 value, IRIntegerKind kind) {
        return RightmostZeroBits(~value, kind);
    }

    static int RightmostOneBits(IntConstant* value) {
        return RightmostOneBits(value->Value(), value->GetType()->GetSubtype());
    }

	// Returns the number of bits set to zero in the left part of the specified number.
	// For example, 0x0000FF has 16 zero bits.
	static int LeftmostZeroBits(__int64 value, IRIntegerKind kind) {
		// Taken from "Hacker's Delight".
        value = LimitToType(value, kind, false /* no sign extension */);
        value = value | (value >> 1);
        value = value | (value >> 2);
        value = value | (value >> 4);
        value = value | (value >> 8);
        value = value | (value >> 16);
        value = value | (value >> 32);

        return OneBits(~value, kind);
	}

	static int LeftmostZeroBits(IntConstant* value) {
		return LeftmostZeroBits(value->Value(), value->GetType()->GetSubtype());
	}

    // Returns the number of bits set to zero in the right part of the specified number.
    // For example, 0xF000 has 8 one bits.    
    static int LeftmostOneBits(__int64 value, IRIntegerKind kind) {
        return LeftmostZeroBits(~value, kind);
    }

    static int LeftmostOneBits(IntConstant* value) {
        return LeftmostOneBits(value->Value(), value->GetType()->GetSubtype());
    }

	// Returns the index of the leftmost bit set to 1.
	// Returns -1 if not bit is set.
	static int LeftmostSetBit(__int64 value, IRIntegerKind kind) {
		value = LimitToType(value, kind, false /* no sign extension */);
		return Abstraction::Integer::LeftmostSetBit(value);
	}

	static int LeftmostSetBit(IntConstant* value) {
		return LeftmostSetBit(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns the index of the rightmost bit set to 1.
	// Returns -1 if not bit is set.
	static int RightmostSetBit(__int64 value, IRIntegerKind kind) {
		value = LimitToType(value, kind, false /* no sign extension */);
		return Abstraction::Integer::RightmostSetBit(value);
	}

	static int RightmostSetBit(IntConstant* value) {
		return RightmostSetBit(value->Value(), value->GetType()->GetSubtype());
	}


	// Returns 'true' if the specified number is a power of two.
	static bool IsPowerOfTwo(IntConstant* value) {
		__int64 temp = value->Value() & GetMinusOneMask(value->GetType()->GetSubtype());
		return (temp & (temp - 1)) == 0;
	}

    static bool IsPowerOfTwo(__int64 value) {
        return (value & (value - 1)) == 0;
    }

	// Returns the next power of two closest to the specified number.
	static __int64 NextPowerOfTwo(IntConstant* value) {
		// Taken from "Hacker's Delight".
		IRIntegerKind kind = value->GetType()->GetSubtype();
		__int64 temp = value->Value() & GetMinusOneMask(kind);
		__int64 shiftAmount = RightmostZeroBits(temp - 1, kind) - 1;
		__int64 result = (__int64)((unsigned __int64)GetSignBitMask(kind)) >> shiftAmount;
		
		// Limit the value in the interval of the int type.
		if(result > GetMinusOneMask(kind)) {
			result = GetMinusOneMask(kind);
		}

		return result;
	}

	// Returns the previous power of two closest to the specified number.
	static __int64 PreviousPowerOfTwo(__int64 value, IRIntegerKind kind) {
		// Taken from "Hacker's Delight".
		value = LimitToType(value, kind, false /* no sign extension */);
		int shiftAmount = RightmostZeroBits(value, kind);
		__int64 result = (__int64)((unsigned __int64)GetSignBitMask(kind)) >> shiftAmount;
		return result;
	}

	// Returns the base-two logarithm of the specified value.
	static __int64 Log2(__int64 value, IRIntegerKind kind) {
		value = LimitToType(value, kind, false /* no sign extension */);

		// log2 is equal to the index of the rightmost set bit.
        if(value > 0) return RightmostSetBit(value, kind);
        else return 0;
	}

    static __int64 Log2(__int64 value) {
        if(value > 0) return RightmostZeroBits(value, IR_Int64);
        else return 0;
	}

	static __int64 Log2(IntConstant* value) {
		return Log2(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns a mask that can be used to test the sign of a number of the specified type.
	static __int64 GetSignBitMask(IRIntegerKind kind) {
		switch(kind) {
			case IR_Int8:  return 0x80LL;
			case IR_Int16: return 0x8000LL;
			case IR_Int32: return 0x80000000LL;
			case IR_Int64: return 0x8000000000000000LL;
		}

		DebugValidator::Unreachable();
		return 0;
	}

	static __int64 GetSignBitMask(const IntegerType* type) {
		return GetSignBitMask(type->GetSubtype());
	}

	// Returns the value of the sign bit. 1 for set, 0 for unset.
	static int GetSignBit(__int64 value, IRIntegerKind kind) {
		return (((value & GetMinusOneMask(kind)) & GetSignBitMask(kind)) != 0) ? 1 : 0;
	}

	static int GetSignBit(IntConstant* value) {
		return GetSignBit(value->Value(), value->GetType()->GetSubtype());
	}

	// Returns 'true' if the specified value requires a 64 location to be stored.
	static bool Requires64Bit(__int64 value) {
		return value > 0xFFFFFFFF;
	}

	// Returns a value having the specified number of bits set to 1.
	static __int64 ValueFromBitCount(int bits) {
		return (bits == 64) ? 0xFFFFFFFFFFFFFFFF : (1LL << bits) - 1;
	}

	static bool IsPositive(IntConstant* value) {
		return value->Value() >= 0;
	}

	static bool IsNegative(IntConstant* value) {
		return value->Value() < 0;
	}

	// Methods for performing arithmetic operations.
	static __int64 Add(__int64 a, __int64 b, IRIntegerKind kind) {
		__int64 result = a + b;
		return LimitToType(result, kind);
	}

	static __int64 Add(IntConstant* a, IntConstant* b) {
		return Add(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static __int64 Sub(__int64 a, __int64 b, IRIntegerKind kind) {
		__int64 result = a - b;
		return LimitToType(result, kind);
	}

	static __int64 Sub(IntConstant* a, IntConstant* b) {
		return Sub(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static __int64 Mul(__int64 a, __int64 b, IRIntegerKind kind) {
		__int64 result = a * b;
		return LimitToType(result, kind);
	}

	static __int64 Mul(IntConstant* a, IntConstant* b) {
		return Mul(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static __int64 Div(__int64 a, __int64 b, IRIntegerKind kind) {
		if(b == 0) return 0;
		__int64 result = a / b;
		return LimitToType(result, kind);
	}

	static __int64 Div(IntConstant* a, IntConstant* b) {
		return Div(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static __int64 Udiv(__int64 a, __int64 b, IRIntegerKind kind) {
		if(b == 0) return 0;
		__int64 result = (__int64)((unsigned __int64)a / (unsigned __int64)b);
		return result & GetMinusOneMask(kind);
	}

	static __int64 Udiv(IntConstant* a, IntConstant* b) {
		return Udiv(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static __int64 Mod(__int64 a, __int64 b, IRIntegerKind kind) {
		if(b == 0) return 0;
		__int64 result = a % b;
		return LimitToType(result, kind);
	}

	static __int64 Mod(IntConstant* a, IntConstant* b) {
		return Mod(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static __int64 Umod(__int64 a, __int64 b, IRIntegerKind kind) {
		if(b == 0) return 0;
		__int64 result = (__int64)((unsigned __int64)a % (unsigned __int64)b);
		return result & GetMinusOneMask(kind);
	}

	static __int64 Umod(IntConstant* a, IntConstant* b) {
		return Umod(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	// Methods for performing bitwise (logical) operations.
	static __int64 And(__int64 a, __int64 b, IRIntegerKind kind) {
		__int64 result = a & b;
		return LimitToType(result, kind);
	}

	static __int64 And(IntConstant* a, IntConstant* b) {
		return And(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static __int64 Or(__int64 a, __int64 b, IRIntegerKind kind) {
		__int64 result = a | b;
		return LimitToType(result, kind);
	}

	static __int64 Or(IntConstant* a, IntConstant* b) {
		return Or(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static __int64 Xor(__int64 a, __int64 b, IRIntegerKind kind) {
		__int64 result = a ^ b;
		return LimitToType(result, kind);
	}

	static __int64 Xor(IntConstant* a, IntConstant* b) {
		return Xor(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static __int64 Not(__int64 value, IRIntegerKind kind) {
		__int64 result = value ^ -1;
		return LimitToType(result, kind);
	}

	static __int64 Not(IntConstant* value) {
		return Not(value->Value(), value->GetType()->GetSubtype());
	}

	static __int64 Shl(__int64 a, __int64 amount, IRIntegerKind kind) {
		if(amount >= TypeBits(kind)) return 0;

		__int64 result = a << amount;
		return LimitToType(result, kind);
	}

	static __int64 Shl(IntConstant* a, IntConstant* b) {
		return Shl(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static __int64 Shr(__int64 a, __int64 amount, IRIntegerKind kind) {
		if(amount >= TypeBits(kind)) return 0;

		__int64 result = a >> amount;
		return LimitToType(result, kind);
	}

	static __int64 Shr(IntConstant* a, IntConstant* b) {
		return Shr(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static __int64 Ushr(__int64 a, __int64 amount, IRIntegerKind kind) {
		if(amount >= TypeBits(kind)) return 0;

		__int64 result = (__int64)((unsigned __int64)a >> (unsigned __int64)amount);
		return result & GetMinusOneMask(kind);
	}

	static __int64 Ushr(IntConstant* a, IntConstant* b) {
		return Ushr(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	// Methods for performing conversions.
	static __int64 Sext(__int64 value, IRIntegerKind fromKind, IRIntegerKind toKind) {
		// The type of the integer to which we extend doesn't matter.
		value = LimitToType(value, fromKind);
		return SignExtend(value, fromKind);
	}
	
	static __int64 Sext(IntConstant* value, const IntegerType* toKind) {
		return Sext(value->Value(), value->GetType()->GetSubtype(), toKind->GetSubtype());
	}

	static __int64 Zext(__int64 value, IRIntegerKind fromKind, IRIntegerKind toKind) {
		// Nothing to be done here.
		return value & GetMinusOneMask(fromKind);
	}

	static __int64 Zext(IntConstant* value, const IntegerType* toKind) {
		return Zext(value->Value(), value->GetType()->GetSubtype(), toKind->GetSubtype());
	}

	static __int64 Trunc(__int64 value, IRIntegerKind fromKind, IRIntegerKind toKind) {
		return value & GetMinusOneMask(toKind);
	}

	static __int64 Trunc(IntConstant* value, const IntegerType* toKind) {
		return Trunc(value->Value(), value->GetType()->GetSubtype(), toKind->GetSubtype());
	}

	static double Itof(__int64 value, IRIntegerKind fromKind, IRFloatingKind toKind) {
		value = LimitToType(value, fromKind);
		if(toKind == IR_Double) return value;
		else return (double)((float)value);
	}

	static double Itof(IntConstant* value, const FloatingType* toKind) {
		return Itof(value->Value(), value->GetType()->GetSubtype(), toKind->GetSubtype());
	}

	static double Uitof(__int64 value, IRIntegerKind fromKind, IRFloatingKind toKind) {
		value = LimitToType(value, fromKind, false /* no sign extension */);
		double result = (double)(value & 0x7FFFFFFFFFFFFFFFLL);

		if((value & 9223372036854775808LL) != 0LL) {
			result += 9.2233720368547758E+18;
		}

		if(toKind == IR_Float) return (double)((float)result);
		else return result;
	}

	static double Uitof(IntConstant* value, const FloatingType* toKind) {
		return Uitof(value->Value(), value->GetType()->GetSubtype(), 
					 toKind->GetSubtype());
	}

	// Methods for testing the relationship between two numbers.
	static bool AreEqual(__int64 a, __int64 b, IRIntegerKind kind) {
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		return a == b;
	}

	static bool AreEqual(IntConstant* a, IntConstant* b) {
		return AreEqual(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static bool AreNotEqual(__int64 a, __int64 b, IRIntegerKind kind) {
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		return a != b;
	}

	static bool AreNotEqual(IntConstant* a, IntConstant* b) {
		return AreNotEqual(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static bool IsSmaller(__int64 a, __int64 b, IRIntegerKind kind) {
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		return a < b;
	}

	static bool IsSmaller(IntConstant* a, IntConstant* b) {
		return IsSmaller(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static bool IsSmallerUnsigned(__int64 a, __int64 b, IRIntegerKind kind) {
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		return (unsigned __int64)a < (unsigned __int64)b;
	}

	static bool IsSmallerUnsigned(IntConstant* a, IntConstant* b) {
		return IsSmallerUnsigned(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static bool IsSmallerOrEqual(__int64 a, __int64 b, IRIntegerKind kind) {
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		return a <= b;
	}

	static bool IsSmallerOrEqual(IntConstant* a, IntConstant* b) {
		return IsSmallerOrEqual(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static bool IsSmallerOrEqualUnsigned(__int64 a, __int64 b, IRIntegerKind kind) {
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		return (unsigned __int64)a <= (unsigned __int64)b;
	}

	static bool IsSmallerOrEqualUnsigned(IntConstant* a, IntConstant* b) {
		return IsSmallerOrEqualUnsigned(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static bool IsLarger(__int64 a, __int64 b, IRIntegerKind kind) {
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		return a > b;
	}

	static bool IsLarger(IntConstant* a, IntConstant* b) {
		return IsLarger(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static bool IsLargerUnsigned(__int64 a, __int64 b, IRIntegerKind kind) {
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		return (unsigned __int64)a > (unsigned __int64)b;
	}

	static bool IsLargerUnsigned(IntConstant* a, IntConstant* b) {
		return IsLargerUnsigned(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static bool IsLargerOrEqual(__int64 a, __int64 b, IRIntegerKind kind) {
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		return a >= b;
	}

	static bool IsLargerOrEqual(IntConstant* a, IntConstant* b) {
		return IsLargerOrEqual(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static bool IsLargerOrEqualUnsigned(__int64 a, __int64 b, IRIntegerKind kind) {
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		return (unsigned __int64)a >= (unsigned __int64)b;
	}

	static bool IsLargerOrEqualUnsigned(IntConstant* a, IntConstant* b) {
		return IsLargerOrEqualUnsigned(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	static bool IsBitSet(int bit, __int64 value, IRIntegerKind kind) {
		value = LimitToType(value, kind);
		return ((value & (1ULL << bit)) != 0);
	}

	static bool IsBitSet(int bit, IntConstant* value) {
		return IsBitSet(bit, value->Value(), value->GetType()->GetSubtype());
	}

	static bool IsMultipleOf(__int64 a, __int64 b, IRIntegerKind kind) {
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		return ((unsigned __int64)a % (unsigned __int64)b == 0);
	}

	static bool IsMultipleOf(IntConstant* a, IntConstant* b) {
		return IsMultipleOf(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	// Returns 'true' if adding the specified numbers would result in overflow.
	static bool AddOverflows(__int64 a, __int64 b, IRIntegerKind kind) {
		// Addition overflow occurs if the operands have the same sign,
		// and the sign of the result is different.
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		int signA = GetSignBit(a, kind);
		int signB = GetSignBit(b, kind);

		if(signA == signB) {
			return (GetSignBit(a + b, kind) != signA);
		}
		else return false;
	}

    static bool AddOverflows(__int64 a, __int64 b, const Type* type) {
        DebugValidator::IsNotNull(type);
        DebugValidator::IsTrue(type->IsInteger());
        return AddOverflows(a, b, type->As<IntegerType>()->GetSubtype());
    }

	static bool AddOverflows(IntConstant* a, IntConstant* b) {
		return AddOverflows(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

	// Returns 'true' if subtracting the specified numbers would result in overflow.
	static bool SubOverflows(__int64 a, __int64 b, IRIntegerKind kind) {
		// Subtraction overflow occurs if the operand have different signs,
		// and the sign of the result is different from the one of the first operand.
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		int signA = GetSignBit(a, kind);
		int signB = GetSignBit(b, kind);

		if(signA != signB) {
			return (GetSignBit(a - b, kind) != signA);
		}
		else return false;
	}

	static bool SubOverflows(IntConstant* a, IntConstant* b) {
		return SubOverflows(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

    static bool SubOverflows(__int64 a, __int64 b, const Type* type) {
        DebugValidator::IsNotNull(type);
        DebugValidator::IsTrue(type->IsInteger());
        return SubOverflows(a, b, type->As<IntegerType>()->GetSubtype());
    }

	// Returns 'true' if multiplying the specified numbers would result in overflow.
	// If 'isSigned' is 'true' then the numbers are considered to be signed.
	static bool MulOverflows(__int64 a, __int64 b, IRIntegerKind kind,
							 bool isSigned = false) {
		// Multiply overflows if by dividing the result by one of the operands
		// we don't obtain the other one. For signed numbers we need to check for
		// 0x8000 / 0xFFFF = 0x8000 (example for 16-bit).

        if(a == 0) return false;

		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		__int64 result = (a * b) & GetMinusOneMask(kind);
		bool overflows = (result / a) != b;

		if(overflows) return true;
		else if(isSigned) {
			__int64 mask = GetSignBitMask(kind);
			return (result == mask) && (a == mask) && (b == GetMinusOneMask(kind));
		}
		else return false;
	}

	static bool MulOverflows(IntConstant* a, IntConstant* b) {
		return MulOverflows(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

    static bool MulOverflows(__int64 a, __int64 b, const Type* type) {
        DebugValidator::IsNotNull(type);
        DebugValidator::IsTrue(type->IsInteger());
        return MulOverflows(a, b, type->As<IntegerType>()->GetSubtype());
    }

	// Returns 'true' if dividing the specified signed numbers would result in overflow.
	static bool DivOverflows(__int64 a, __int64 b, IRIntegerKind kind) {
		// Signed division overflows if the divisor is 0, or the dividend
		// is the sign bit (for example, 0x8000 for int16) and the dividend is -1;
		a = LimitToType(a, kind);
		b = LimitToType(b, kind);
		__int64 result = (a * b) & GetMinusOneMask(kind);
		return (b == 0) || ((a == GetSignBitMask(kind)) && (b == -1));
	}

	static bool DivOverflows(IntConstant* a, IntConstant* b) {
		return DivOverflows(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

    static bool DivOverflows(__int64 a, __int64 b, const Type* type) {
        DebugValidator::IsNotNull(type);
        DebugValidator::IsTrue(type->IsInteger());
        return DivOverflows(a, b, type->As<IntegerType>()->GetSubtype());
    }

	// Returns 'true' if adding the specified unsigned numbers would result in overflow.
	static bool UdivOverflows(__int64 a, __int64 b, IRIntegerKind kind) {
		// Unsigned division overflows only if the divisor is 0.
		return b == 0;
	}

	static bool UdivOverflows(IntConstant* a, IntConstant* b) {
		return UdivOverflows(a->Value(), b->Value(), a->GetType()->GetSubtype());
	}

    static bool UdivOverflows(__int64 a, __int64 b, const Type* type) {
        DebugValidator::IsNotNull(type);
        DebugValidator::IsTrue(type->IsInteger());
        return UdivOverflows(a, b, type->As<IntegerType>()->GetSubtype());
    }
};

} // namespace Analysis
#endif