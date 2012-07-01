// BitVectorBase.hpp
// Copyright (c) Lup Gratian
//
// Implements methods that are shared by all bit vector implementations.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_BIT_VECTOR_BASE_HPP
#define PC_ANALYSIS_BIT_VECTOR_BASE_HPP

#ifdef _MSC_VER
	#include <intrin.h>
	#pragma intrinsic(_BitScanForward64)
#endif

class BitVectorBase {
protected:
	// Returns the number of bits set in the specified value.
	int BitsSet(__int64 value) const {
		// A very fast method that counts in parallel.
		// http://en.wikipedia.org/wiki/Hamming_weight#Efficient_implementation
		value = value - ((value >> 1) & 0x5555555555555555ULL);
		value = (value & 0x3333333333333333ULL) + ((value >> 2) & 0x3333333333333333ULL);
		return (int)((((value + (value >> 4)) & 0xF0F0F0F0F0F0F0FULL) * 
					    0x101010101010101ULL) >> 56);
	}

	// Returns the index of the first set bit, or -1 if no bit is set.
	int FirstSetBit(__int64 value) const {
		#ifdef _MSC_VER
			// On VC++ we can use the the '_BitScanForward64' intrinsic.
			unsigned long index;
			if(_BitScanForward64(&index, value)) {
				return (int)index;
			}
			else return -1;
		#else
			// Compute the index in a naive way.
			int index = 0;

			while(value) {
				if(value & 1) return index;
				index++;
				value >>= 1;
			}

			return -1; // No bit is set.
		#endif
	}

	// Sets to 1 the bit found at the specified position.
	inline __int64 SetBitImpl(__int64 value, int position) const {
		return value | (1ULL << position);
	}

	// Sets to 0 the bit found at the specified position.
	inline __int64 ResetBitImpl(__int64 value, int position) const {
		return value & ~(1ULL << position);
	}

	// Inverts the bit found at the specified position.
	inline __int64 InvertBitImpl(__int64 value, int position) const {
		return value ^ (1ULL << position);
	}

	// Returns 'true' if the bit found at the specified position is set.
	inline bool IsSetImpl(__int64 value, int position) const {
		return (bool)(value & (1ULL << position));
	}
};

#endif