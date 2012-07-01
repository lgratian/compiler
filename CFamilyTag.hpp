// CFamilyTag.hpp
// Copyright (c) Lup Gratian
//
// Defines a tag that can be used to associate with objects
// information specific to the C-family languages.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_C_FAMILY_TAG_HPP
#define PC_ANALYSIS_C_FAMILY_TAG_HPP

#include "../IR/Tag.hpp"
#include "../IR/Tagged.hpp"
#include "../Base/DebugValidator.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class CFamilyTag : public FunctionTag {
private:
    enum Flags {
        Flag_None               = 0,
        Flag_IsAllocLike        = 1 << 0,
        Flag_IsCheckedAllocLike = 1 << 1,
        Flag_HasSetjmp          = 1 << 2,
        Flag_HasLongjmp         = 1 << 3
    };

    Flags flags_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    CFamilyTag();                              // Should not be created.
	CFamilyTag(const CFamilyTag&);             // Should not be copied.
	CFamilyTag& operator= (const CFamilyTag&); // Should not be assigned.

    CFamilyTag(Flags flags) : flags_(flags) {}

    bool HasFlag(Flags testFlag) const {
        return ((int)flags_ & (int)testFlag) != 0;
    }

    void SetFlag(Flags flag, bool state) {
        if(state) flags_ = (Flags)((int)flags_ | (int)flag);
        else flags_ = (Flags)((int)flags_ & (~(int)flag));
    }

public:
    static const int Id = 0xbcf434be;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const {
        return Id;
    }

    static CFamilyTag* GetCFamily() {
        return new CFamilyTag(Flag_None);
    }

    static CFamilyTag* GetIsAllocLike() {
        return new CFamilyTag(Flag_IsAllocLike);
    }

    static CFamilyTag* GetIsCheckedAllocLike() {
        return new CFamilyTag(Flag_IsCheckedAllocLike);
    }

    static CFamilyTag* GetHasSetjmp() {
        return new CFamilyTag(Flag_HasSetjmp);
    }

    static CFamilyTag* GetHasLongjmp() {
        return new CFamilyTag(Flag_HasLongjmp);
    }

    bool IsAllocLike() const {
        return HasFlag(Flag_IsAllocLike) ||
               HasFlag(Flag_IsCheckedAllocLike);
    }

    bool SetIsAllocLike(bool state) {
        SetFlag(Flag_IsAllocLike, state);
    }

    bool IsCheckedAllocLike() const {
        return HasFlag(Flag_IsCheckedAllocLike);
    }

    bool SetIsCheckedAllocLike(bool state) {
        SetFlag(Flag_IsCheckedAllocLike, state);
    }

    bool HasSetjmp() const {
        return HasFlag(Flag_HasSetjmp);
    }

    bool SetHasSetjmp(bool state) {
        SetFlag(Flag_HasSetjmp, state);
    }

    bool HasLongjmp() const {
        return HasFlag(Flag_HasLongjmp);
    }

    bool SetHasLongjmp(bool state) {
        SetFlag(Flag_HasLongjmp, state);
    }

    unsigned GetHashCode() const {
        return flags_;
    }

    bool operator== (const CFamilyTag& other) const {
        return flags_ == other.flags_;
    }

    bool operator!= (const CFamilyTag& other) const {
        return operator==(other) == false;
    }
};

} // namespace Analysis

namespace IR {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct TagPromoter<Analysis::CFamilyTag> {
		static bool Is(const Tag* tag) {
			return tag->GetId() == Analysis::CFamilyTag::Id;
		}

		static Analysis::CFamilyTag* As(Tag* tag) {
			return Is(tag) ? static_cast<Analysis::CFamilyTag*>(tag) : nullptr;
		}
	};
} // namespace Detail
} // namespace IR
#endif