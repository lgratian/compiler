// TypeBasedAliasAnalysis.cpp
// Copyright (c) Lup Gratian
//
// Implements the TypeBasedAliasAnalysis class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "TypeBasedAliasAnalysis.hpp"

namespace Analysis {

bool TypeBasedAliasAnalysis::IsABaseClassForB(TypeClassTag* tagA, 
                                              TypeClassTag* tagB) {
    // We need to check if 'tagB' is a subclass of 'tagA'.
    // We walk the chain of parent until we either find 'tagA'
    // (we have a subclass), or don't have any more parents (no subclass).
    while(tagB->HasParent()) {
        tagB = tagB->Parent();
        
        if(tagB == tagA) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TypeClassTag* TypeBasedAliasAnalysis::GetTag(Operand* op) {
    if(auto temp = op->As<Temporary>()) {
        return temp->GetTag<TypeClassTag>();
    }
    else if(auto param = op->As<Parameter>()) {
        // If we have a parameter look at the associated variable.
        return param->GetVariable()->GetTag<TypeClassTag>();
    }
    else if(auto reference = op->As<Reference>()) {
        return reference->GetTag<TypeClassTag>();
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult TypeBasedAliasAnalysis::ComputeAlias(AliasLocation locationA, 
                                                 AliasLocation locationB) {
    // Check if both operands have an associated type class tag.
    // If both of them have we can try to use them.
    auto tagA = GetTag(locationA.Base());
    auto tagB = GetTag(locationB.Base());

    if((tagA && tagB) == false) {
        return Alias_May;
    }

    // If one of the tags represents the universal type we give up.
    // The universal type is the language type able to alias
    // any other type (for C this is 'char' and 'unsigned char').
    if(tagA->IsUniversalType() || tagB->IsUniversalType()) {
        return Alias_May;
    }

    // If we have an object-oriented language the tags
    // might have a parent-child relationship, meaning that
    // one of them is a subclass of the other and the objects
    // might be located at the same address.
    if(IsABaseClassForB(tagA, tagB) ||
       IsABaseClassForB(tagB, tagA)) {
        return Alias_May;
    }

    // If the tags are different there is no alias
    // because each language type is associated with an unique tag.
    if(tagA != tagB) {
        return Alias_None;
    }

    return Alias_May;
}

} // namespace Analysis