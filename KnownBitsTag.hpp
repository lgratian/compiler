// KnownBitsTag.hpp
// Copyright (c) Lup Gratian
//
// Defines a tag that can be used to store information
// about the bits of an operand that are known to be
// definitely set to one or zero.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_KNOW_BITS_TAG_HPP
#define PC_ANALYSIS_KNOW_BITS_TAG_HPP

#include "../IR/Operand.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Tag.hpp"
#include "../Base/DefaultComparer.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class KnownBitsTag : public Tag {
private:
    unsigned __int64 zeroBits_;
    unsigned __int64 oneBits_;
    unsigned char hasOneBits_  : 1;
    unsigned char hasZeroBits_ : 1;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    KnownBitsTag();                                // Should not be created.
	KnownBitsTag(const KnownBitsTag&);             // Should not be copied.
	KnownBitsTag& operator= (const KnownBitsTag&); // Should not be assigned.
    
    KnownBitsTag(unsigned __int64 zeroBits, 
                 unsigned __int64 oneBits) :
            zeroBits_(zeroBits), oneBits_(oneBits),
            hasOneBits_(0), hasZeroBits_(0) {}

public:
    static const int Id = 0xde16f23;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const {
        return Id;
    }

    static KnownBitsTag* GetKnownBits(unsigned __int64 zeroBits = 0,
                                      unsigned __int64 oneBits = 0) {
        return new KnownBitsTag(zeroBits, oneBits);
    }

    // Returns a mask that represents the bits that are definitely zero.
    // If a bit is set it means that its value is zero.
    unsigned __int64 ZeroBits() const {
        return zeroBits_;
    }

    void SetZeroBits(unsigned __int64 value) {
        zeroBits_ = value;
        hasZeroBits_ = 1;
    }

    bool HasZeroBitInfo() const {
        return hasZeroBits_;
    }

    // Returns a mask that represents the bits that are definitely one.
    // If a bit is set it means that its value is one.
    unsigned __int64 OneBits() const {
        return oneBits_;
    }

    void SetOneBits(unsigned __int64 value) {
        oneBits_ = value;
        hasOneBits_ = 1;
    }

    bool HasOneBitInfo() const {
        return hasOneBits_;
    }

    // Returns 'true' if the specified bit is definitely zero.
    // Note that a 'false' result doesn't mean that the bit is one,
    // it means that the bit is unknown.
    bool IsZeroBit(int index) {
        DebugValidator::IsLargerOrEqual(index, 0);
        DebugValidator::IsSmallerOrEqual(index, 64);

        return (zeroBits_ & (1ull << index)) != 0;
    }

    // Returns 'true' if the specified bit is definitely one.
    // Note that a 'false' result doesn't mean that the bit is zero,
    // it means that the bit is unknown.
    bool IsOneBit(int index) {
        DebugValidator::IsLargerOrEqual(index, 0);
        DebugValidator::IsSmallerOrEqual(index, 64);

        return (oneBits_ & (1ull << index)) != 0;
    }

    unsigned GetHashCode() const {
        HashCalculator hashCalc;
        return hashCalc.GetHashCode(&zeroBits_, sizeof(zeroBits_)) ^
               hashCalc.GetHashCode(&oneBits_, sizeof(oneBits_));
    }

    bool operator== (const KnownBitsTag& other) const {
        return (zeroBits_ == other.zeroBits_) &&
               (oneBits_ == other.oneBits_);
    }

    bool operator!= (const KnownBitsTag& other) const {
        return operator==(other) == false;
    }

    bool operator< (const KnownBitsTag& other) const {
        return (zeroBits_ < other.zeroBits_) ||
               (oneBits_ < other.oneBits_);
    }
};

} // namespace Analysis

namespace IR {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct TagPromoter<Analysis::KnownBitsTag> {
		static bool Is(const Tag* tag) {
			return tag->GetId() == Analysis::KnownBitsTag::Id;
		}

		static Analysis::KnownBitsTag* As(Tag* tag) {
			return Is(tag) ? static_cast<Analysis::KnownBitsTag*>(tag) : nullptr;
		}
	};
} // namespace Detail
} // namespace IR
#endif