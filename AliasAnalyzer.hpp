// AliasAnalyzer.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class from which all alias analysis implementations must derive.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_ALIAS_ANALYZER_HPP
#define PC_ANALYSIS_ALIAS_ANALYZER_HPP

#include "CFamilyTag.hpp"
#include "TypeInfo.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/StackIntrinsics.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

// Represents the result of an alias query.
// Must - the pointers definitely point to the same memory location
// May  - we're not sure if the pointers point to the same memory location or not
// None - the pointers definitely don't point to the same memory location (no alias)
enum AliasResult {
    Alias_Must,
    Alias_May,
    Alias_None
};


enum AddressTakenType {
    AddressTaken_Unknown,
    AddressTaken_Yes,
    AddressTaken_No,
};

// Represents an abstract location used by the alias analysis component
// to represent the locations in the application.
class AliasLocation {
private:
    static const __int64 UNKNOWN_SIZE = -1;

	Operand* base_;  // The base operand.
    __int64 offset_; // The offset (in bytes) relative to the base operand.
	__int64 size_;   // The size of the accessed region.
    AddressTakenType addrTaken_;

public:
	AliasLocation() : base_(nullptr), offset_(0), size_(0), 
                      addrTaken_(AddressTaken_Unknown) {}

	AliasLocation(Operand* base, __int64 offset, 
                  __int64 size = UNKNOWN_SIZE) : 
            base_(base), offset_(offset), size_(size),
            addrTaken_(AddressTaken_Unknown) {}

	AliasLocation(const AliasLocation& other) :
			base_(other.base_), size_(other.size_), 
            offset_(other.offset_), addrTaken_(other.addrTaken_) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    static __int64 GetUnknown() {
        return UNKNOWN_SIZE;
    }

	// Returns the operands that acts as the base for this location.
	Operand* Base() {
		return base_;
	}

	const Operand* Base() const {
		return base_;
	}

	void SetBase(Operand* value) {
		base_ = value;
	}

    // Returns the offset (in bytes) of the location
    // relative to the base operand.
    __int64 Offset() const {
        return offset_;
    }

    void SetOffset(__int64 value) {
        offset_ = value;
    }

    // Returns 'true' if the offset is zero.
    bool HasNoOffset() const {
        return offset_ == 0;
    }

    // Returns 'true' if the offset is different from zero.
    bool HasOffset() const {
        return offset_ != 0;
    }

	// Returns the size of the location.
	__int64 Size() const {
		return size_;
	}

	void SetSize(__int64 value) {
		size_ = value;
	}

    // Returns 'true' if the location size is unknown.
    bool HasUnknownSize() const {
        return size_ == UNKNOWN_SIZE;
    }

    // Returns 'true' if the location size is known.
    bool HasKnownSize() const {
        return size_ != UNKNOWN_SIZE;
    }

    AddressTakenType AddressTaken() const {
        return addrTaken_;
    }

    void SetAddressTaken(AddressTakenType value) {
        addrTaken_ = value;
    }

    bool HasAddressTaken() {
        return addrTaken_ == AddressTaken_Yes;
    }

    bool HasAddressNotTaken() {
        return addrTaken_ == AddressTaken_No;
    }

    AliasLocation WithAdjustedOffset(__int64 adjustment) {
        return AliasLocation(Base(), Offset() + adjustment, Size());
    }

	unsigned GetHashCode() const {
		// Uses the FNV hash algorithm. 
		// Taken from 'http://www.eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx'
		unsigned hash = 2166136261;
		hash = (hash * 16777619) ^ (unsigned)base_;

		if(sizeof(Operand*) == 8) {
			// For 64 bit pointers;
			hash = (hash * 16777619) ^ (unsigned)((__int64)base_ >> 32);
		}

        hash = (hash * 16777619) ^ (unsigned)offset_;
        hash = (hash * 16777619) ^ (unsigned)(offset_ >> 32);

		hash = (hash * 16777619) ^ (unsigned)size_;
        hash = (hash * 16777619) ^ (unsigned)(size_ >> 32);
		return hash;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	AliasLocation& operator= (const AliasLocation& other) {
		if(&other == this) return *this;

		base_ = other.base_;
        offset_ = other.offset_;
		size_ = other.size_;
		return *this;
	}

	bool operator== (const AliasLocation& other) const {
		return (base_ == other.base_) && 
               (offset_ == other.offset_) &&
               (size_ == other.size_);
	}

	bool operator< (const AliasLocation& other) const {
		return false; // Required by Dictionary.
	}
};

// Represents the type of the memory accessed by a 'call' instruction.
// Note that the types can be combined.
enum MemoryType {
    Memory_None           = 0, // Does not access memory at all
    Memory_Global         = 1, // Any global variable
    Memory_GlobalConstant = 2, // Only constant global variables
    Memory_Parameters     = 4, // Any pointer function parameter
    Memory_Unknown = Memory_Global | Memory_GlobalConstant |
                     Memory_Parameters | Memory_Parameters
};


// Represents the operation performed by a 'call' instruction
// on a memory location.
enum MemoryOperation {
    Memory_Invalid = 0,
    Memory_Read    = 1, // "Ref" behavior
    Memory_Write   = 2, // "Mod" behavior
    Memory_ReadWrite = Memory_Read | Memory_Write // Both "Ref" and "Mod" behavior
};


// Represents the type of memory locations accessed by a called function,
// including the operation performed (read, write, or both).
class MemoryResult {
private:
    MemoryType type_;
    char operations_[4];

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    int GetOperationIndex(MemoryType type) const {
        switch(type) {
            case Memory_Global :        return 0;
            case Memory_GlobalConstant: return 1;
            case Memory_Parameters:     return 2;
            default: return 3;
        }
    }

    void SetOperation(MemoryType type, MemoryOperation operation) {
        int index = GetOperationIndex(type);
        operations_[index] = operation;
    }

    MemoryOperation GetOperation(MemoryType type) const {
        int index = GetOperationIndex(type);
        return (MemoryOperation)operations_[index];
    }

public:
    MemoryResult(MemoryType type = Memory_Unknown,
                 MemoryOperation operation = Memory_ReadWrite) {
        type_ = type;
        *((int*)operations_) = 0;

        if(((int)type & ((int)type - 1)) == 0) {
            // A single type of operation is set.
            SetOperation(type, operation);
        }
        else {
            // Multiple types are set, each gets the same behavior.
            for(int i = 0; i < 4; i++) {
                if((int)type & (1 << i)) {
                    SetOperation((MemoryType)(1 << i), operation);
                }
            }
        }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Methods for obtaining various memory type/operation combinations.
    static MemoryResult GetNone() {
        return MemoryResult(Memory_None, Memory_Invalid);
    }

    static MemoryResult GetUnknown() {
        return MemoryResult(Memory_Unknown, Memory_ReadWrite);
    }

    static MemoryResult GetGlobalRead() {
        return MemoryResult(Memory_Global, Memory_Read);
    }

    static MemoryResult GetGlobalWrite() {
        return MemoryResult(Memory_Global, Memory_Write);
    }

    static MemoryResult GetGlobalReadWrite() {
        return MemoryResult(Memory_Global, Memory_ReadWrite);
    }

    static MemoryResult GetGlobalConstantRead() {
        return MemoryResult(Memory_GlobalConstant, Memory_Read);
    }

    static MemoryResult GetParametersRead() {
        return MemoryResult(Memory_Parameters, Memory_Read);
    }

    static MemoryResult GetParametersWrite() {
        return MemoryResult(Memory_Parameters, Memory_Write);
    }

    static MemoryResult GetParametersReadWrite() {
        return MemoryResult(Memory_Parameters, Memory_ReadWrite);
    }

    // Methods for testing the type of the accessed memory.
    bool AccessesMemory() const {
        return type_ != Memory_None;
    }

    bool DoesNotAccessMemory() const {
        return type_ == Memory_None;
    }

    bool AccessesUknownMemory() const {
        return type_ & Memory_Unknown;
    }

    bool AccessesGlobalMemory() const {
        return type_ & Memory_Global;
    }

    bool AccessesGlobalConstantMemory() const {
        return type_ & Memory_GlobalConstant;
    }

    bool AccessesParametersMemory() const {
        return type_ & Memory_Parameters;
    }

    // Returns the memory access for the specified memory type.
    MemoryOperation Operation(MemoryType type) const {
        return GetOperation(type);
    }

    MemoryOperation OperationForGlobal() const {
        return GetOperation(Memory_Global);
    }

    MemoryOperation OperationForGlobalConstant() const {
        return GetOperation(Memory_GlobalConstant);
    }

    MemoryOperation OperationForParameters() const {
        return GetOperation(Memory_Parameters);
    }

    MemoryOperation OperationForUnknown() const {
        return GetOperation(Memory_Unknown);
    }

    unsigned GetHashCode() const {
		// Uses the FNV hash algorithm. 
		// Taken from 'http://www.eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx'
		unsigned hash = 2166136261;
		hash = (hash * 16777619) ^ (unsigned)type_;
        hash = (hash * 16777619) ^ *((unsigned*)operations_);
		return hash;
	}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	MemoryResult& operator= (const MemoryResult& other) {
		if(&other == this) return *this;

        type_ = other.type_;
        *((int*)operations_) = *((int*)other.operations_);
		return *this;
	}

	bool operator== (const MemoryResult& other) const {
        return (type_ == other.type_) &&
               (*((int*)operations_) == *((int*)other.operations_));
	}

	bool operator< (const MemoryResult& other) const {
		return false; // Required by Dictionary.
	}
};


// Forward declarations.
class AliasInfo;


// Base class for all alias analysis algorithms.
class AliasAnalyzer {
private:
    typedef List<CallInstr*> AllocCallList;

    AliasAnalyzer(const AliasAnalyzer& other); // Should not be assigned.
    AliasAnalyzer& operator= (const AliasAnalyzer& other); // Should not be copied.

    AliasInfo* parent_; // The alias manager.
    bool enabled_;

protected:
    typedef Analysis::TypeInfo TI;

    // Returns 'true' if the ranges associated with the specified locations
    // intersect or are contained one inside the other.
    // A range is the interval [offset, offset + size).
    bool RangesOverlap(AliasLocation& locationA, 
                       AliasLocation& locationB);

    // Returns 'true' if the called function is an allocation function
    // (for C this would be 'malloc', for example).
    bool IsAllocCall(CallInstr* instr);

    // Returns 'true' if the operand originates from dynamically
    // allocated memory (this walks the chain of addressing instructions
    // until it finds a call to an allocation function).
    // If 'allocList' is specified the found calls are added to the list.
    bool OriginatesFromAlloc(Operand* op, AllocCallList* allocList = nullptr, 
                             int level = 0);

    // Returns 'true' if none of the allocation calls
    // from 'listA' are found in 'listB'.
    bool AreAllocCallsIndependent(AllocCallList* listA, 
                                  AllocCallList* listB);

    // Returns the operand that acts as the base
    // of a series of addressing instructions.
    Operand* GetBaseOperand(Operand* op);

    // Returns 'true' if the base of a series of addressing instructions
    // is a variable. The variable is assigned to 'variableRef' of specified.
    bool BaseIsVariable(Operand* op, VariableReference** variableRef = nullptr);

    // Returns 'true' if the operand originates from a variable
    // whose address is not taken. If specified, 'veriableRef'
    // will point to the non-address-taken variable.
    bool IsAddressNotTaken(Operand* op, VariableReference** variableRef = nullptr);

    bool IsAddressNotTaken(AliasLocation location, 
                           VariableReference** variableRef = nullptr);

    // Returns 'true' if the record is the equivalent of a C 'union'.
    bool IsUnion(const RecordType* recordType);

    // Returns 'true' if the operands is a pointer that did not
    // originate from local/global aggregate variable.
    bool IsNonAggregatePointer(Operand* op);

    // Returns 'true' if we're certain that the two parameters
    // can't point to the same memory locations. One such case
    // is when both are marked 'restrict' by the programmer.
    bool AreParametersIndependent(Parameter* parameterA, Parameter* parameterB);

    // Returns the operand with all pointer casts removed.
    Operand* WithoutPointerCasts(Operand* op);

public:
    AliasAnalyzer() : parent_(nullptr), enabled_(true) {}

    AliasAnalyzer(AliasInfo* parent) : parent_(parent) {
        DebugValidator::IsNotNull(parent);
    }

    virtual ~AliasAnalyzer() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Analyzes the specified memory locations 
    // and determines their alias relation.
    virtual AliasResult ComputeAlias(AliasLocation locationA, 
                                     AliasLocation locationB) {
        return Alias_May;
    }

    // Returns 'true' if the specified address operands 
    // are in a 'Must' alias relation.
    virtual bool HasMustAlias(AliasLocation locationA, 
                              AliasLocation locationB) {
        return ComputeAlias(locationA, locationB) == Alias_Must;
    }

    // Returns 'true' if the specified address operands 
    // are in a 'May' alias relation.
    virtual bool HasMayAlias(AliasLocation locationA, 
                             AliasLocation locationB) {
        return ComputeAlias(locationA, locationB) == Alias_May;
    }

    // Returns 'true' if the specified address operands
    // are in a 'Must' or 'May' alias relation.
    virtual bool HasAlias(AliasLocation locationA, 
                          AliasLocation locationB) {
        auto result = ComputeAlias(locationA, locationB);
        return (result == Alias_May) || (result == Alias_Must);
    }

    // Returns 'true' if the specified address operands
    // are not aliases of the same memory location.
    virtual bool HasNoAlias(AliasLocation locationA, 
                            AliasLocation locationB) {
        return ComputeAlias(locationA, locationB) == Alias_None;
    }

    virtual MemoryResult ComputeCallAlias(CallInstr* instr, 
                                          AliasLocation location) {
        // If the called function has no definition (in the current translation unit)
        // we need to presume that it may modify any memory location.
        if(instr->GetCalledFunction() == nullptr) {
            return MemoryResult::GetUnknown();
        }

        return MemoryResult::GetUnknown();
    }

    // Returns 'true' if the alias algorithm handles 'call' instructions.
    virtual bool HandlesCalls() const {
        return false;
    }

    // Returns the associated AliasInfo parent.
    AliasInfo* Parent() {
        return parent_;
    }

    void SetParent(AliasInfo* parent) {
        DebugValidator::IsNotNull(parent);
        parent_ = parent;
    }

    bool IsEnabled() const {
        return enabled_;
    }

    void SetIsEnabled(bool value) {
        enabled_ = value;
    }

    // Returns the name of the alias algorithm.
    virtual string Name() = 0;
};

} // namespace Analysis
#endif