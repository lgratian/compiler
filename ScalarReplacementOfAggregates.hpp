// ScalarReplacementOfAggregates.hpp
// Copyright (c) Lup Gratian
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_SCALAR_REPLACEMENT_OF_AGGREGATES_HPP
#define PC_OPTIMIZATION_SCALAR_REPLACEMENT_OF_AGGREGATES_HPP

//! PRINT ACTIONS TO LOG!!!

#include "../Analysis/IntArithmetic.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Analysis/VariableAnalysis.hpp"
#include "../Analysis/ProfileInfo.hpp"
#include "../Analysis/TypeInfo.hpp"
#include "../Analysis/OperandInfo.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Tag.hpp"
#include "../IR/Unit.hpp"
#include "../IR/IRGenerator.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/IRPrinter.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DefaultComparer.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/List.hpp"
#include "../Base/String.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/Log.hpp"

using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class ScalarReplacementOfAggregates : public Pass {
private:
    // Represents the type of a normal access.
    enum AccessType {
        Access_Store,
        Access_Load
    };

    // Represents the type of a copy/set access.
    enum CopyAccessType {
        CopyAccess_Copy,
        CopyAccess_Set
    };


    // Stores the indexes of the addressing instructions
    // that compute the address of an aggregate component.
    // index (index agg, 1), 2  ->  1,2
    struct AccessPath {
        StaticList<int, 3> Indices; // The accessed components.

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        unsigned GetHashCode() const {
            // Uses the FNV hash algorithm. 
            unsigned hash = 2166136261;

            for(int i = 0; i < Indices.Count(); i++) {
                hash = (hash * 16777619) ^ (unsigned)Indices[i];
            }

            return hash;
        }

        bool operator== (const AccessPath& other) const {
            if(Indices.Count() != other.Indices.Count()) {
                return false;
            }

            for(int i = 0; i < Indices.Count(); i++) {
                if(Indices[i] != other.Indices[i]) {
                    return false;
                }
            }

            return true;
        }

        bool operator< (const AccessPath& other) const {
            // Compare the common indexes.
            int count = std::min(Indices.Count(), other.Indices.Count());

            for(int i = 0; i < count; i++) {
                if(Indices[i] < other.Indices[i]) {
                    return true;
                }
                else if(Indices[i] > other.Indices[i]) {
                    return false;
                }
            }

            return Indices.Count() < other.Indices.Count();
        }

        string ToString() const;
    };


    // Represents a load/store access to an aggregate.
    struct Access {
        AccessType Type;     // The type of the access.
        Instruction* Source; // The instruction that accesses the aggregate.
        AccessPath Path;     // The sequence of access indexes.

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        Access() {}

        Access(Instruction* source) : Source(source) {}

        Access(const Access& other) : 
                Type(other.Type), Source(other.Source), 
                Path(other.Path) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        bool operator== (const Access& other) const {
            if(Type != other.Type) return false;
            return Path == other.Path;
        }

        bool operator!= (const Access& other) const {
            return  !operator== (other);
        }

        bool operator< (const Access& other) const {
            return Path < other.Path;
        }

        string ToString() const;
    };


    // Represents a copy/set access to an aggregate.
    // Copy can be to/from an aggregate.
    struct CopyAccess {
        CopyAccessType Type;
        CallInstr* IntrinsicCall;      // The associated 'call'.
        AccessPath DestinationPath;    // The access path for the destination.
        AccessPath SourcePath;         // The access path for the source.
        Variable* DestinationVariable; // The destination variable (if any).
        Variable* SourceVariable;      // The source variable (if any).
        int Size;                      // The amount of bytes copied/set.

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        CopyAccess() {}

        CopyAccess(CallInstr* call) : IntrinsicCall(call) {}

        CopyAccess(const CopyAccess& other) :
                Type(other.Type), IntrinsicCall(other.IntrinsicCall), 
                DestinationPath(other.DestinationPath),
                SourcePath(other.SourcePath),
                DestinationVariable(other.DestinationVariable),
                SourceVariable(other.SourceVariable) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        string ToString() const;
    };


    // Represents a call access to an aggregate (the address of the
    // aggregate, or one it's components, is an argument).
    struct CallAccess {
        CallInstr* Call; // The 'call' instruction.
        AccessPath Path; // The access path of the argument.
        bool IsNoWrite;  // 'true' if the callee doesn't write into the aggregate.
        bool IsNoRead;   // 'true' if the callee doesn't read from the aggregate.

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        CallAccess() {}

        CallAccess(CallInstr* call, AccessPath& path) :
                Call(call), Path(path), 
                IsNoWrite(false), IsNoRead(false) {}

        CallAccess(const CallAccess& other) :
                Call(other.Call), Path(other.Path) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        string ToString() const;
    };


    // Used to construct a tree representation of the aggregate
    // that stores information about each component 
    // (children, offset, size, if it's scalarized, etc.).
    struct TreeNode {
        AccessPath Path;           // The access path to the component.
        List<TreeNode*> Children;  // The child components.
        const Type* ComponentType; // The type of the aggregate component.
        Variable* Replacement;     // The variable used to replace the component.
        int Offset;                // The offset, in bytes.
        int Size;                  // The size, in bytes.
        int ScalarizedChildren;    // The number of scalarized children.
        bool Processed;            // Used by algorithms.
        
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        TreeNode() {}

        TreeNode(const Type* type) : 
                ComponentType(type), Replacement(nullptr),
                ScalarizedChildren(0) {}

        ~TreeNode() {
            for(int i = 0; i < Children.Count(); i++) {
                delete Children[i];
            }
        }
    };


    // Stores information associated with an aggregate
    // that has at least one component that will be scalarized.
    struct CandidateData {
        Dictionary<AccessPath, Variable*> Replacements;
        TreeNode* TreeRoot;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        CandidateData() : TreeRoot(nullptr) {}

        ~CandidateData() {
            if(TreeRoot) delete TreeRoot;
        }
    };


    // Stores information about an aggregate that is a scalarization
    // candidate. Has list to store each kind of accesses.
    struct Candidate {
        Variable* Base;                         // The candidate aggregate.
        StaticList<Access, 16> Accesses;        // The accesses to the aggregate.
        StaticList<CopyAccess, 2> CopyAccesses; // The copy operations.
        StaticList<CallAccess, 2> CallAccesses;
        CandidateData* Data; // Used during scalarization.
        int ComponentCount;  // The number of components that compose the aggregate.
        bool HasStore;       // 'true' if there is at least one write access.
        bool HasLoad;        // 'true' if there is at least one read access.
        bool NeedsTree;      // 'true' if a component tree needs to be created
                             // (the candidate is involved into a copy/set/call access).

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        Candidate() : Data(nullptr) {}

        ~Candidate() {
            if(Data) delete Data;
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        string ToString() const;
    };


    // Used when we want do determine which accesses are hot
    // for a candidate with a large number of components.
    struct ComponentHottness {
        AccessPath Path;   // The access path to the component.
        int Hottness;      // The "hotness' of the component.
        bool HasOnlyStore; // 'true' if only store accesses target the component.

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        ComponentHottness() {}

        ComponentHottness(AccessPath path, int hotness = 0, bool isStore = false) : 
                Path(path), Hottness(hotness), 
                HasOnlyStore(isStore) {}

        ComponentHottness(const ComponentHottness& other) :
                Path(other.Path), Hottness(other.Hottness) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        bool operator== (const ComponentHottness& other) const {
            return (Hottness == other.Hottness) &&
                   (Path == other.Path);
        }

        bool operator!= (const ComponentHottness& other) const {
            return  !operator== (other);
        }

        bool operator< (const ComponentHottness& other) const {
            // Sort in the order Store, Load, Other.
            return Hottness < other.Hottness;
        }
    };


    typedef Dictionary<AccessPath, bool> PathDict;
    typedef List<TreeNode*> NodeList;
    typedef Analysis::TypeInfo TI;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    //! TODO: REPLACE THESE WITH PASS CONTROLS!
	// The maximum number of components a record should have
	// to be considered for complete scalarization.
    static const int RECORD_REPLACEMENT_LIMIT = 32;
	
	// The maximum number of components an array should have
	// to be considered for complete scalarization.
    static const int ARRAY_REPLACEMENT_LIMIT = 16;
	
	// The hotness of a load access. Used when selecting
	// the components for partial scalarization.
    static const int LOAD_ACCESS_HOTTNESS = 24;
	
	// The hotness of a store. Used when selecting
	// the components for partial scalarization.
    static const int STORE_ACCESS_HOTTNESS = 10;
	
	// How much the hotness of an access should be boosted
	// if it's found inside a loop. This increases the likelihood
	// that a component used in a loop is scalarized.
    static const int LOOP_ACCESS_HOTTNESS_BOOST = 5;
	
	// How many components can be replaced for a candidate,
	// relative to the number of available registers.
    static const int REPLACEMENT_LIMIT_FACTOR = 2;
	
	// A penalization that should applied for an access
	// if the candidate aggregate is used in a 'call'. 
	// We penalize the access because the scalarized components 
	// need to be written back before the call.
	static const int CALL_PENALIZATION = 8;
	
	// The minimum hotness that a component should have
	// in order to be scalarized.
    static const int MINIMUM_REPLACEMENT_HOTTNESS = 20;
	
	// The number of candidates after which to increase
	// the minimum hotness (this tries to prevent register pressure).
    static const int CANDIDATE_NUMBER_LIMIT = 3;
	
	// How much an access should be penalized if there is
	// a large number of candidates. This number is multiplied
	// by the number of candidates.
    static const int CANDIDATE_NUMBER_PENALIZATION = 10;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Function* funct_;
    SparseBitVector eligibleCandidates_;
    List<Candidate> candidates_;
    Dictionary<Variable*, int> varToCandidateIndex_;
    int replacementsCreated_; // For debugging.

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns the number of variables that are needed in case
    // the aggregate associated with the type is fully scalarized.
    int ComputeRequiredVariables(const Type* type);

    // Sets a bit in 'eligibleCandidates_' for all aggregates
	// that are eligible for scalarization.
    int MarkCandidates();

    // Returns 'true' if an aggregate is eligible for scalarization.
	// It is not if its address is taken or if it's the equivalent
	// of a C union.
    bool IsEligibleCandidate(Variable* variable);

    // Processes all 'load', 'store' and 'call' instructions in the block
	// in order to identify all accesses that use an aggregate.
    void ProcessBlock(Block* block);

    // Verifies if the 'load' or 'store' instruction targets
	// a scalarization candidate, and if true, it creates the 
	// appropriate access information and associates it with the candidate.
    void ProcessLoadStore(Instruction* instr, Operand* op,
                          bool isLoad, bool isVolatile);

    // Verifies if the 'call' instruction targets a scalarization 
	// candidate, either as an aggregate copy/initialization,
	// or as an argument to another kind of function.
    void ProcessCall(CallInstr* instr);

    // Verifies if the 'copyMemory' call targets scalarization
	// candidates, both as a destination and as a source.
    void ProcessCopyMemory(CallInstr* instr);

    // Verifies if the 'setMemory' initializes a scalarization
	// candidate. This often happens for large aggregates 
	// initialized with a constant (usually zero).
    void ProcessSetMemory(CallInstr* instr);

    // Walks the use-definition chain starting with the operand
	// and collects the indexes used by the addressing instructions
	// which lead to a scalarization candidate.
	// Returns 'true' if the base is a eligible candidate.
	// 'variable' is set to the base variable, 'offset' to the
	// offset into the variable computed by the access path.
    bool CreateAccessPath(Operand* op, AccessPath& path, 
                          Variable*& variable, int& offset,
                          bool usedByAddr = false);

    bool CreateAccessPathForElem(ElementInstr* elemInstr, AccessPath& path, 
                                 Variable*& variable, int& offset, bool usedByAddr);

    bool CreateAccessPathForIndex(IndexInstr* indexInstr, AccessPath& path, 
                                  Variable*& variable, int& offset, bool usedByAddr);

    bool CreateAccessPathForAddr(AddressInstr* addrInstr, AccessPath& path, 
                                 Variable*& variable, int& offset, bool usedByAddr);

    // Returns 'true' if all components of the aggregates
	// should be replaced. This is done for medium-sized records
	// and relatively small arrays.
    bool ShouldDoCompleteReplacement(Candidate& candidate);

    // Returns 'true' if any component of the aggregate should be
	// replaced, and adds to 'replaced' the access path for theses
	// components. Uses a heuristic to estimate the accesses
	// that are the most important.
    bool ShouldDoPartialReplacement(Candidate& candidate, 
                                    PathDict& replaced);
									
	// Returns the estimated hotness of an access. If possible
	// it uses profile information, else it tries to boost
	// accesses that are found inside a loop.
    int EstimateHottness(Access& access);
									
    // The driver of the replacement step. Replaces load/store
	// accesses first and creates a component tree (if required),
	// then replaces copy/set and call operations.
    void ReplaceAggregates();

    // Creates variables for the scalarized components 
	// of the aggregate. If 'completeReplacement' is 'false' only
	// the components whose path is in 'replaced' are considered.
    void CreateReplacements(Candidate& candidate, bool completeReplacement,
                            PathDict& replaced);

    // Creates variables that are used to replace a scalarized component.
	// If the access path targets a record/array, variables are
	// created, recursively, for each of their components.
    void CreateVariable(const Type* type, AccessPath& path,
                        CandidateData* data, bool completeReplacement,
                        PathDict& replaced);

    // Replace a load/store access with a load/store from the variable
	// that was created for the accessed component.
    void ReplaceAccess(Access& access, CandidateData* data);

    // Replaces a copy access. See the implementation for details
	// about the used algorithm.
    void ReplaceCopyAccess(Candidate& candidate);

    // Copies the common components from the source to the
	// destination list. The copied components are marked,
	// so they are no longer considered by the rest of the algorithm.
    void CopyCommonComponents(NodeList& destList, NodeList& sourceList,
                              int startDestOffset, int startSourceOffset,
                              CopyAccess& copyAccess);
                             
	// Copies the components that were not found in both the source
	// and destination list, and regions that are not covered
	// by components (either because they were not scalarized,
	// or because there is unused space between them - happens for
	// records, because of alignment constrains).
    void CopyRemainingComponents(NodeList& sourceList, int startDestOffset, 
                                 int startSourceOffset, int finalSourceOffset,
                                 CopyAccess& copyAccess);

    // Replaces an access that initializes an entire aggregate
	// by initializing each scalarized component with the 
	// appropriate value, and initializing the regions that are
	// not covered the usual way, using 'setMemory'.
    void ReplaceSetAccess(Candidate& candidate, CopyAccess& access);

    // If the callee may read from the aggregate, we need to 
	// copy back the scalarized components before the 'call'.
	// If the callee may write to the aggregate, all scalarized
	// components need to be reloaded need to be reloaded.
    void PatchCallAccess(Candidate& candidate);    

    // Creates a tree that represents the components of the aggregate
	// and their properties (type, offset size, path, replacement, etc.).
	// If the type is a record/array it create nodes in the tree
	// for each of the child components.
    TreeNode* CreateTree(const Type* type, int offset, 
                         CandidateData* data, AccessPath& path);

	// Creates the component tree for the specified aggregate.
    void CreateTree(Candidate& candidate);

    // Returns 'true' if there is any tree node whose region
	// intersects with the region defined by 'node' and 'size'.
    bool HasCommonRegion(TreeNode* node, int offset, int size);

    // Adds to the list all nodes whose region intersects with
	// the with the region defined by 'node' and 'size'.
	// This also considers the children of the node, recursively.
    void AddInRegionToList(TreeNode* node, int offset, 
                           int size, NodeList& list);

    // Returns the tree node targeted by the specified access path.
    TreeNode* GetNodeForAccessPath(Candidate& candidate, AccessPath& path);

    // Generates code that writes all replaced components
	// back to their aggregate (this includes all children 
	// of the specified node).
    void WriteReplacementsBack(TreeNode* node, Variable* variable,
                               Instruction* beforeInstr);

    // Generates code that computes the address specified
	// by the access path (creates 'index' and 'elem' instructions).
    Operand* GetAddressForPath(AccessPath& path, Variable* variable,
                               Instruction* beforeInstr);

    // Generates code that loads the value of the specified replacement
	// variable. Returns the operand that contains the loaded value.
    Operand* LoadReplacement(Variable* replacement, Instruction* beforeInstr);

    // Generates code that reloads from the aggregate the component
	// that is targeted by the specified access path.
    void ReloadReplacement(Variable* variable, AccessPath& path,
                           Variable* replacement, Instruction* beforeInstr);

    // Generates code that reloads all replaced components
	// starting with the specified node (this includes its children).
    void ReloadAllReplacements(TreeNode* node, Variable* variable,
                               Instruction* beforeInstr);

    // Generates code that computes the required offsets 
	// relative to the source/destination operands,
	// then a call to the 'copyMemory' intrinsic.
    void GenerateMemoryCopy(Operand* destAddress, Operand* sourceAddress,
                            int fromOffset, int toOffset, int size,
                            Instruction* beforeInstr);

	// Generates code that copies a region of memory from the 
	// source to the destination of the specified access path.
    void GenerateMemoryCopy(CopyAccess& access, int fromOffset, 
                            int toOffset, int size,
                            Instruction* beforeInstr);
							
	// Generates code that initializes the specified region
	// using a call to the 'setMemory' intrinsic.
    void GenerateMemorySet(Operand* destAddress, Operand* valueOp,
                           int toOffset, int size, Instruction* beforeInstr);

    // Generates code that loads a replacement and writes it
	// to an aggregate at the specified offset.
    void WriteReplacementToMemory(Variable* replacement, CopyAccess& access, 
                                  int toOffset, Instruction* beforeInstr);

    // Generates code that loads a replacement and stores it
	// to another replacement with the same type.
    void MakeReplacementCopy(Variable* destReplacement,
                             Variable* sourceReplacement, 
                             Instruction* beforeInstr);

    // Returns the next component node in the list.
    TreeNode* AdvanceToNextComponent(NodeList& list, int& nodeIndex);

    // Creates an integer constant whose value is equal 
	// to the "extension" of 'value'.
    Operand* CreateConstant(const Type* requiredType, __int64 value);

    // Tries to transform the instruction so that fever aggregates
	// are marked as address-taken.
    bool ImproveFunction();

    // Tries to replace a load from a 'phi' with loads
	// of the incoming operands in the corresponding blocks.
    bool ImproveLoadFromPhi(LoadInstr* loadInstr, PhiInstr* phiInstr);

    // Tries to replace a load from a 'quest' with loads
	// from the true and false operands.
    bool ImproveLoadFromQuestion(LoadInstr* loadInstr, 
								 QuestionInstr* questInstr);

    // Returns 'true' if all users of the specified temporary
	// are 'load' instructions.
    bool UsedOnlyByLoad(Temporary* temp);

    // Returns 'true' if the specified address may be written to
	// by any instruction until the beginning of the block is reached.
    bool MayBeWrittenTo(Operand* addressOp, Instruction* startInstr);

    void MakeEligible(Variable* variable) {
        DebugValidator::IsNotNull(variable);
        eligibleCandidates_.SetBit(variable->Id());
    }

    bool IsEligible(Variable* variable) {
        DebugValidator::IsNotNull(variable);
        return eligibleCandidates_.IsSet(variable->Id());
    }

    void ResetEligible(Variable* variable) {
        DebugValidator::IsNotNull(variable);
        eligibleCandidates_.ResetBit(variable->Id());
    }

    int EligibleCount() const {
        return eligibleCandidates_.SetBitsCount();
    }

    // Methods for debugging.
    void Dump();
    void AccessReplaced(Access& access);
    void CopyAccessReplaced(CopyAccess& access);
    void CallAccessReplaced(CallAccess& access);
    void AccessImproved(Instruction* instr);
    
public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif