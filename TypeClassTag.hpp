// TypeClassTag.hpp
// Copyright (c) Lup Gratian
//
// Defines a tag that can be used to partition pointer operands
// into type classes based on the original language type.
// This is useful for Type Based Alias Analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_TYPE_CLASS_TAG_HPP
#define PC_ANALYSIS_TYPE_CLASS_TAG_HPP

#include "../IR/Tag.hpp"
#include "../Base/DefaultComparer.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class TypeClassTag : public Tag {
private:
    TypeClassTag* parent_; // The parent tag.
    bool isUniversal_;     // 'true' if the type can alias any other type.

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    TypeClassTag();                                // Should not be created.
	TypeClassTag(const TypeClassTag&);             // Should not be copied.
	TypeClassTag& operator= (const TypeClassTag&); // Should not be assigned.

    TypeClassTag(TypeClassTag* parent = nullptr, bool isUniversal = false) :
            parent_(parent), isUniversal_(isUniversal) {}

public:
    static const int Id = 0xceb8ae74;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const {
        return Id;
    }

    virtual void Free() {
        // The tags are freed by the tag table.
    }

    // Creates a new type class tag associated with the specified type,
    // optionally being linked to a type class parent.
    static TypeClassTag* GetTypeClass(TypeClassTag* parent = nullptr) {
        return new TypeClassTag(parent);
    }

    // Creates a new type class tag that represents a type
    // that can alias any other type in the language type system.
    // For C-like languages this would be 'char' and 'unsigned char'.
    static TypeClassTag* GetUniversalClass() {
        return new TypeClassTag(nullptr, true /* isUniversal */);
    }

    TypeClassTag* Parent() {
        return parent_;
    }

    TypeClassTag* Parent() const {
        return parent_;
    }

    bool HasParent() const {
        return parent_ != nullptr;
    }

    bool IsUniversalType() const {
        return isUniversal_;
    }

    unsigned GetHashCode() const {
        HashCalculator hashCalc;
        return hashCalc.GetHashCode(&parent_, sizeof(parent_)) ^
               hashCalc.GetHashCode(&isUniversal_, sizeof(isUniversal_));
    }

    bool operator== (const TypeClassTag& other) const {
        return (parent_ == other.parent_) &&
               (isUniversal_ == other.isUniversal_);
    }

    bool operator!= (const TypeClassTag& other) const {
        return operator==(other) == false;
    }

    bool operator< (const TypeClassTag& other) const {
        return false; // Required by Dictionary.
    }
};

} // namespace Analysis

namespace IR {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct TagPromoter<Analysis::TypeClassTag> {
		static bool Is(const Tag* tag) {
			return tag->GetId() == Analysis::TypeClassTag::Id;
		}

		static Analysis::TypeClassTag* As(Tag* tag) {
			return Is(tag) ? static_cast<Analysis::TypeClassTag*>(tag) : nullptr;
		}
	};
} // namespace Detail
} // namespace IR
#endif