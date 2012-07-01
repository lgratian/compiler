// BitVector.hpp
// Copyright (c) Lup Gratian
//
// Implements a dynamic vector of bits, together with methods to manipulate those bits
// (set, reset, invert, and, or, etc.).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_BIT_VECTOR_HPP
#define PC_ANALYSIS_BIT_VECTOR_HPP

#include "BitVectorBase.hpp"
#include "../Base/List.hpp"
using namespace Base;

namespace Analysis {

class BitVector : public BitVectorBase {
private:
	// The number of bits stored in an unit.
	static const int BITS_PER_UNIT = sizeof(__int64) * 8;

	List<__int64> data_;
	int bitCount_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the number of the unit in which the specified bitfield is stored.
	inline int UnitPosition(int bitPos) const {
		return bitPos / BITS_PER_UNIT;
	}

	// Return the position of the bit in its unit.
	inline int BitPosition(int bitPos) const {
		return bitPos % BITS_PER_UNIT;
	}

	// Returns the number of units required to store the specified number of bits.
	inline int RequiredUnits(int bits) const {
		return (bits / BITS_PER_UNIT) +
			   ((bits % BITS_PER_UNIT) > 0 ? 1 : 0);
	}

public:
	BitVector(int count, bool state = false) : 
			data_(RequiredUnits(std::min(1, count))), bitCount_(count) {
		int units = UnitPosition(count);
		int remaining = BitPosition(count);
		__int64 mask = state ? -1 : 0;

		// Make sure we have space for all bits.
		if(remaining > 0) units++;

		// Set the first bits to the required state.
		for(int i = 0; i < units; i++) {
			data_.Add(mask);
		}
	}

	BitVector(const BitVector& other) :
			data_(other.data_), bitCount_(other.bitCount_) {}

	BitVector(BitVector&& other) : 
			data_(std::move(other.data_)), bitCount_(other.bitCount_) {}

	virtual ~BitVector() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Sets to 1 the bit found at the specified position.
	void SetBit(int position) {
		DebugValidator::IsSmaller(position, bitCount_);

		int listPos = UnitPosition(position);
		int bitPos = BitPosition(position);

		data_[listPos]= SetBitImpl(data_[listPos], bitPos);
	}

	// Sets to 0 the bit found at the specified position.
	void ResetBit(int position) {
		DebugValidator::IsSmaller(position, bitCount_);

		int listPos = UnitPosition(position);
		int bitPos = BitPosition(position);

		data_[listPos] = ResetBitImpl(data_[listPos], bitPos);
	}

	// Inverts the bit found at the specified position.
	void InvertBit(int position) {
		DebugValidator::IsSmaller(position, bitCount_);

		int listPos = UnitPosition(position);
		int bitPos = BitPosition(position);

		data_[listPos] = InvertBitImpl(data_[listPos], bitPos);
	}

	// Returns 'true' if the the bit found at the specified position is set.
	bool IsSet(int position) const {
		DebugValidator::IsSmaller(position, bitCount_);

		int listPos = UnitPosition(position);
		int bitPos = BitPosition(position);

		return IsSetImpl(data_[listPos], bitPos);
	}

	// Returns the number of bits in this object.
	int BitCount() const {
		return bitCount_;
	}

	// Returns the total number of set bits.
	int SetBitsCount() const {
		int count = 0;

		for(int i = 0; i < data_.Count(); i++) {
			count += BitsSet(data_[i]);
		}

		return count;
	}

	// Returns 'true' if all the bits in the vector are set.
	bool AllBitsSet() const {
		int lastUnit = UnitPosition(bitCount_);
		for(int i = 0; i < lastUnit; i++) {
			if(data_[i] != -1) {
                return false;
            }
		}

		// Now check the bits from the last unit.
		int start = lastUnit * BITS_PER_UNIT;
		while(start < bitCount_) {
			if(IsSet(start++) == false) {
                return false;
            }
		}

		return true;
	}

	// Returns 'true' if none of the bits in the vector are set.
	bool NoBitsSet() const {
		for(int i = 0; i < data_.Count(); i++) {
			if(data_[i] != 0) {
                return false;
            }
		}

		return true;
	}

	// Returns 'true' if there are bits that are set in the vector.
	bool HasBitsSet() const {
		return SetBitsCount() > 0;
	}

	// Performs a bitwise AND between this object and the specified object.
	void And(const BitVector& other) {
		Resize(other.bitCount_);

		for(int i = 0; i < data_.Count(); i++) {
			data_[i] = data_[i] & other.data_[i];
		}
	}

	// Performs a bitwise OR between this object and the specified object.
	void Or(const BitVector& other) {
		Resize(other.bitCount_);

		for(int i = 0; i < data_.Count(); i++) {
			data_[i] = data_[i] | other.data_[i];
		}
	}

	// Performs a bitwise XOR between this object and the specified object.
	void Xor(const BitVector& other) {
		Resize(other.bitCount_);

		for(int i = 0; i < data_.Count(); i++) {
			data_[i] = data_[i] ^ other.data_[i];
		}
	}

	// Performs a bitwise difference between this object
    // and the specified object.
	void Difference(const BitVector& other) {
		Resize(other.bitCount_);

		for(int i = 0; i < data_.Count(); i++) {
			data_[i] = data_[i] & ~other.data_[i];
		}
	}

	// Inverts each bit in the vector (set -> unset, unset -> set).
	void InvertAll() {
		int lastUnit = UnitPosition(bitCount_);
		for(int i = 0; i < lastUnit; i++) {
			data_[i] = ~data_[i];
		}

		// Now reverse the bits from the last unit.
		int start = lastUnit * BITS_PER_UNIT;
		while(start < bitCount_) {
			InvertBit(start++);
		}
	}

	// Sets all bits in the vector to 0.
	void Clear() {
		std::memset(data_.GetInternal(), 0, data_.Count() * sizeof(__int64));
	}

	// Resizes the vector so that the specified number of bits fit.
	void Resize(int newBitCount) {
		if(newBitCount <= bitCount_) {
            return;
        }

		int units = RequiredUnits(bitCount_);
		int newUnits = RequiredUnits(newBitCount);

		if(newUnits > units) {
			// We need to add some more units.
			while(units < newUnits) {
				data_.Add(0);
				units++;
			}
		}
		else {
			// Some units need to be removed.
			int removed = units - newUnits;
			if(removed > 0) {
				data_.RemoveRange(units - removed, removed);
			}

			// Check if in the last unit some of the bits need to be unset.
			int bitPos = BitPosition(newBitCount);
			__int64 mask = ((__int64)1 << bitPos) - 1;
			data_[newUnits - 1] = data_[newUnits - 1] & mask;
		}

		bitCount_ = newBitCount;
	}

	// Returns the index of the first set bit, or -1 if no bit is set.
	// If 'startIndex' is specified any set bit before it is ignored.
	int FirstSetBit(int startIndex = 0) const {
		if(startIndex >= bitCount_) {
            return -1;
        }

		int start = UnitPosition(startIndex);
		int index = start * BITS_PER_UNIT;
		int startPos = BitPosition(startIndex);

		for(int i = start; i < data_.Count(); i++) {
			// Compute a mask used to restrict the search space.
			// Used to ignore the bits that come before 'startIndex'.
			__int64 mask = i > start ? -1 : ~(((__int64)1 << startPos) - 1);
			int result = BitVectorBase::FirstSetBit(data_[i] & mask);
			
			if(result != -1) {
				int firstIndex = index + result;
				
				// Check if the found index is valid.
				if(firstIndex < bitCount_) {
                    return firstIndex;
                }
				else return -1; // We're not in the vector anymore.
			}

			index += BITS_PER_UNIT;
		}

		return -1; // No bit is set.
	}

	// Calls the specified predicate for each of the bits.
	// bool Predicate(int index, bool state)
	template <class Predicate>
	void ForEach(Predicate action) const {
		int bits = data_.Count() * BITS_PER_UNIT;

		for(int i = 0; i < bits; i++) {
			if(action(i, IsSet(i)) == false) {
				return; // User canceled the operation.
			}
		}
	}

	// Calls the specified predicate for each of the set bits.
	// bool Predicate(int index)
	template <class Predicate>
	void ForEachSet(Predicate action) const {
		int bits = data_.Count() * BITS_PER_UNIT;
		int index = -1;

		while(index < bits) {
			index = FirstSetBit(index + 1);

			if(index != -1) {
				if(action(index) == false) {
                    return; // User canceled the operation.
                }
			}
			else return; // No other bits set.
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the state of the specified bit.
	bool operator[] (int position) const {
		return IsSet(position);
	}

	bool operator== (const BitVector& other) const {
		if(other.bitCount_ != bitCount_) {
			return false;
		}

		int lastUnit = UnitPosition(bitCount_);
		for(int i = 0; i < lastUnit; i++) {
			if(data_[i] != other.data_[i]) {
				return false;
			}
		}

		// Now test the bits from the last unit.
		int start = lastUnit * BITS_PER_UNIT;

		while(start < bitCount_) {
			if(IsSet(start) != other.IsSet(start)) {
				return false;
			}

			start++;
		}

		return true;
	}

	bool operator!= (const BitVector& other) const {
		return operator== (other) == false;
	}

	BitVector& operator= (const BitVector& other) {
		if(&other != this) {
			data_.Clear();
			data_.AddRange(other.data_);
			bitCount_ = other.bitCount_;
		}

		return *this;
	}

	// Methods that allow a BitVector to be manipulated using the bitwise operators.
	BitVector& operator&= (const BitVector& other) {
		And(other);
		return *this;
	}

	BitVector& operator|= (const BitVector& other) {
		Or(other);
		return *this;
	}

	BitVector& operator^= (const BitVector& other) {
		Xor(other);
		return *this;
	}

	BitVector& operator-= (const BitVector& other) {
		Difference(other);
		return *this;
	}

	BitVector& operator~ () {
		InvertAll();
		return *this;
	}
};

} // namespace Analysis
#endif