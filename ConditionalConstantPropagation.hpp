// ConstantPropagation.hpp
// Copyright (c) Lup Gratian
//
// Implements the well-known Sparse Conditional Constant Propagation algorithm.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_SCCP_HPP
#define PC_OPTIMIZATION_SCCP_HPP

#include "BlockCloner.hpp"
#include "../Analysis/LatticePropagator.hpp"
#include "../Analysis/ConstantFolder.hpp"
#include "../Analysis/OperandInfo.hpp"
#include "../Analysis/TypeInfo.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/IRDominators.hpp"
#include "../Analysis/CFGWalker.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Constants.hpp"
#include "../IR/References.hpp"
#include "../IR/Operand.hpp"
#include "../IR/GlobalVariable.hpp"
#include "../IR/IRGenerator.hpp"
#include "../IR/Function.hpp"
#include "../IR/Unit.hpp"
#include "../IR/IRPrinter.hpp"
#include "../IR/Block.hpp"
#include "../Base/String.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/DefaultComparer.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/Log.hpp"
#include "../Base/MakePair.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace Target;
using namespace CompilationPass;

namespace Optimization {

// Define a pair of two operands.
MAKE_PAIR(OperandPair, Operand*, FirstOperand, Operand*, SecondOperand);

// The lattice cell that stores the value of an operand
// (TOP, BOTTOM or a constant). 
class ConstantLatticeCell : public LatticeCell {
private:
    Constant* constant_;

public:
    explicit ConstantLatticeCell(CellType type) :
            LatticeCell(type), constant_(nullptr) {}

    explicit ConstantLatticeCell(Constant* constant) :
            LatticeCell(Cell_Value), constant_(constant) {}

    virtual ~ConstantLatticeCell() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    bool IsConstant() const {
        return Type() == Cell_Value;
    }

    Constant* AsConstant() {
        if(IsConstant()) return constant_;
        else return nullptr;
    }

    IntConstant* AsIntConstant() {
        if(IsConstant()) return AsConstant()->As<IntConstant>();
        else return nullptr;
    }

    bool IsZeroInt() const {
        return IsConstant() && constant_->IsZeroInt();
    }

    bool IsOneInt() const {
        return IsConstant() && constant_->IsOneInt();
    }

    bool IsMinusOneInt() const {
        return IsConstant() && constant_->IsMinusOneInt();
    }

    virtual string ToString() const {
        if(IsConstant()) {
            return "constant: " + constant_->ToString();
        }
        else return LatticeCell::ToString();
    }
};


// Implements a cache for the lattice cells.
// This speeds up the algorithm, because it allows us 
// to keep the cells between successive runs.
class ConstantLatticeCellCache {
private:
    ConstantLatticeCell* topCell_;
    ConstantLatticeCell* bottomCell_;
    Dictionary<Constant*, ConstantLatticeCell*> constCells_;

public:
    ConstantLatticeCellCache();

    ~ConstantLatticeCellCache();

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    ConstantLatticeCell* GetTopCell() {
        return topCell_;
    }

    ConstantLatticeCell* GetBottomCell() {
        return bottomCell_;
    }

    ConstantLatticeCell* GetCellForConstant(Constant* constant);
};

// Forward declaration.
class ConstantPropagation;

// The lattice that implements the SCCP algorithm.
// Simulates the instructions using the target arithmetic.
// The lattice cells are unique and cached.
class ConstantLattice : public Lattice {
private:
    typedef Analysis::TypeInfo TI;
    typedef Dictionary<Operand*, ConstantLatticeCell*> FixedConstantDict;

    ConstantPropagation* parent_;
    Function* funct_;
    TargetInfo* target_;
    ConstantLatticeCellCache* cellCache_;
    FixedConstantDict fixedPhiConsts_;
    ConstantFolder folder_;
    IRGenerator irGen_;
    bool hasFixedPhiConsts_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Methods that return the TOP and BOTTOM lattice cells.
    virtual LatticeCell* GetTopCell() {
        return cellCache_->GetTopCell();
    }

    virtual LatticeCell* GetBottomCell() {
        return cellCache_->GetBottomCell();
    }

    // Returns a lattice cell that represents the specified constant.
    virtual LatticeCell* GetCellForConstant(Constant* constant) {
        return cellCache_->GetCellForConstant(constant);
    }

    // Returns a lattice cell that represents a constant
    // if we know that the parameter is always one, else BOTTOM.
    virtual LatticeCell* GetCellForParameter(Parameter* parameter);

    virtual LatticeCell* GetInitializationCell(Operand* op) {
        auto cell = Lattice::GetInitializationCell(op);
        if(cell) return cell;
        else return GetTopCell();
    }

    virtual bool AreCellsEqual(LatticeCell* cellA, LatticeCell* cellB);

    virtual LatticeCell* MergeCells(LatticeCell* cellA, LatticeCell* cellB);

    // Methods for simulating the instructions.
    virtual void Transfer(Instruction* instr);
    
    virtual LatticeCell* TransferPhi(PhiInstr* instr, bool isOptimistic);

    bool TransferArithmeticLogical(Instruction* instr);
    bool TransferArithmetic(Instruction* instr, 
                            ConstantLatticeCell* constCellA,
                            ConstantLatticeCell* constCellB);
    bool TransferLogical(Instruction* instr, 
                         ConstantLatticeCell* constCellA,
                         ConstantLatticeCell* constCellB);
    bool TransferArithmeticLogicalPhi(Instruction* instr,
                               ConstantLatticeCell* constCellA,
                               ConstantLatticeCell* constCellB);
    bool TransferArithmeticLogicalQuest(Instruction* instr,
                                        ConstantLatticeCell* constCellA,
                                        ConstantLatticeCell* constCellB);
    bool TransferConversion(ConversionInstr* instr);
    bool TransferComparison(CmpInstrBase* instr);
    bool TransferComparisonOneConst(CmpInstrBase* instr,
                                    ConstantLatticeCell* constCellA,
                                    ConstantLatticeCell* constCellB);

    bool TransferComparisonPhi(CmpInstrBase* instr,
                               ConstantLatticeCell* constCellA,
                               ConstantLatticeCell* constCellB);

    bool TransferLoad(LoadInstr* instr);
    bool TransferCall(CallInstr* instr);
    bool TransferQuestion(QuestionInstr* instr);

    // Returns the global variable that acts as the source,
    // or 'nullptr' if no variable is found.
    VariableReference* GetBaseGlobal(Operand* op, __int64& offset);

    virtual int AddFeasibleSuccessors(ControlInstr* instr, 
                                      TSuccessorList& successorList);

    // Adds all successors of the block to the list.
    int AddAllSuccessors(Block* block, TSuccessorList& successorList);

    // Methods for updating the cell of the result operand.
    void UpdateCell(Instruction* instr, LatticeCell* cell) {
        if(instr->HasDestinationOp()) {
            Propagator()->UpdateCell(instr, cell);
        }
    }

    void MarkBottom(Instruction* instr) {
        UpdateCell(instr, GetBottomCell());
    }

    // Methods for obtaining cells for constants.
    // Used when folding branches.
    LatticeCell* GetCellForZero(Operand* op) {
        auto& consts = funct_->ParentUnit()->Constants();
        auto constant = consts.GetInt(op->GetType(), 0);
        return GetCellForConstant(constant);
    }

    LatticeCell* GetCellForMinusOne(Operand* op) {
        auto& consts = funct_->ParentUnit()->Constants();
        auto constant = consts.GetInt(op->GetType(), 0);
        return GetCellForConstant(constant);
    }

    LatticeCell* GetCellForOne(Operand* op) {
        auto& consts = funct_->ParentUnit()->Constants();
        auto constant = consts.GetInt(op->GetType(), 1);
        return GetCellForConstant(constant);
    }

    // Returns 'true' if block 'a' dominates block 'b'.
    bool Dominates(Block* a, Block* b);

public:
    ConstantLattice(Function* function, ConstantLatticeCellCache* cache, 
                    TargetInfo* target, ConstantPropagation* parent) :
            funct_(function), cellCache_(cache), target_(target), 
            parent_(parent), hasFixedPhiConsts_(false) {
        irGen_ = IRGenerator(function->ParentUnit());
        folder_ = ConstantFolder(&irGen_, target_);
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    ConstantLatticeCell* GetCell(Operand* op) {
        auto cell = Propagator()->GetCell(op);
        return static_cast<ConstantLatticeCell*>(cell);
    }

    void SetCell(Operand* op, ConstantLatticeCell* cell) {
        DebugValidator::IsNotNull(op);
        DebugValidator::IsNotNull(cell);
        Propagator()->SetCell(op, cell);
    }

    bool IsEdgeExecutable(Block* from, Block* to) {
        DebugValidator::IsNotNull(from);
        DebugValidator::IsNotNull(to);
        return Propagator()->IsExecutableEdge(FlowEdge(from, to));
    }

    bool HasFixedPhiConstants() const {
        return hasFixedPhiConsts_;
    }

    void SetHasFixedPhiConstants(bool value) {
        hasFixedPhiConsts_ = value;
    }

    FixedConstantDict& FixedPhiConstants() {
        return fixedPhiConsts_;
    }

    // Forces he 'phi' instruction to be always replaced 
    // with the specified instruction during the SCCP algorithm.
    // Used when evaluating the specialization benefit.
    void AddFixedPhiConstant(PhiInstr* instr, Constant* constant);

    // If the 'phi' instruction is forced to be a constant
    // it returns that constant, else 'nullptr'.
    ConstantLatticeCell* GetFixedPhiConstant(PhiInstr* instr);
};


// Represents a region that might be specialized
// so that more constants are discovered.
class ConstantRegion {
private:
    // Stores how many instructions where constant folded.
    struct InstructionCounts {
        int SimpleCount;
        int MulCount;
        int DivCount;
        int FloatingCount;

        InstructionCounts() : SimpleCount(0), MulCount(0), 
                              DivCount(0), FloatingCount(0) {}
    };

    // An estimated execution time for some instruction categories.
    static const int SIMPLE_WEIGHT = 1;
    static const int MUL_WEIGHT = 2;
    static const int DIV_WEIGHT = 5;
    static const int FLOATING_WEIGHT = 6;
    
    ConstantRegion(const ConstantRegion& other);
    ConstantRegion& operator= (const ConstantRegion& other);

    typedef Dictionary<Operand*, Constant*> ConstantMap;

    ConstantRegion* parent_;
    List<ConstantRegion*> children_;
    List<Block*> blocks_;
    ConstantMap constantOps_;
    List<Operand*> replacementOps_;
    List<OperandPair> probableOpPairs_;
    shared<BlockCloner> cloner_;
    Block* specializedPred_;
    InstructionCounts counts_;
    int alwaysIncomingConstCount_;
    int withParentRegionConstCount_;
    int totalInstrs_;
    int constantInstrs_;
    int constantAfterInstrs_;
    bool selected_;

public:
    ConstantRegion(ConstantRegion* parent = nullptr) :
            parent_(parent), totalInstrs_(0), constantInstrs_(0),
            constantAfterInstrs_(0), selected_(false) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    Block* SpecializedPredecessor() {
        return specializedPred_;
    }

    void SetSpecializedPredecessor(Block* block) {
        specializedPred_ = block;
    }

    BlockCloner* Cloner() {
        return cloner_;
    }

    void SetCloner(shared<BlockCloner> value) {
        cloner_ = value;
    }

    void AddBlock(Block* block) {
        DebugValidator::IsFalse(blocks_.Contains(block));
        blocks_.Add(block);
    }

    bool ContainsBlock(Block* block) const {
        return blocks_.Contains(block);
    }

    void RemoveBlock(Block* block) {
        DebugValidator::IsFalse(blocks_.Contains(block));
        blocks_.Remove(block);
    }

    int BlockCount() const {
        return blocks_.Count();
    }

    Block* GetBlock(int index) {
        return blocks_[index];
    }

    List<Block*>& Blocks() {
        return blocks_;
    }

    ConstantRegion* Parent() {
        return parent_;
    }

    void SetParent(ConstantRegion* value) {
        parent_ = value;
    }

    bool HasParent() const {
        return parent_;
    }

    void AddChild(ConstantRegion* region) {
        DebugValidator::IsNotNull(region);
        DebugValidator::IsFalse(HasChild(region));
        children_.Add(region);
    }

    bool HasChild(ConstantRegion* region, bool includeChildren = true) {
        DebugValidator::IsNotNull(region);
        for(int i = 0; i < children_.Count(); i++) {
            if((children_[i] == region) ||
               (includeChildren && children_[i]->HasChild(region))) {
                return true;
            }
        }

        return false;
    }

    int ChildCount() const {
        return children_.Count();
    }

    ConstantRegion* GetChild(int index) {
        return children_[index];
    }

    List<Operand*>& ReplacementOperands() {
        return replacementOps_;
    }

    int ConstantInstructions() const {
        return constantInstrs_;
    }

    void SetConstantInstructions(int value) {
        DebugValidator::IsLargerOrEqual(value, 0);
        constantInstrs_ = value;
    }

    int ConstantAfterInstructions() const {
        return constantAfterInstrs_;
    }

    void SetConstantAfterInstructions(int value) {
        DebugValidator::IsLargerOrEqual(value, 0);
        constantAfterInstrs_ = value;
    }

    int ConstantDifference(bool includeChildren = false) const {
        int difference = constantAfterInstrs_ - constantInstrs_;

        if(includeChildren) {
            for(int i = 0; i < children_.Count(); i++) {
                difference += children_[i]->ConstantDifference(includeChildren);
            }
        }

        return difference;
    }

    int TotalInstructions() const {
        return totalInstrs_;
    }

    void SetTotalInstructions(int value) {
        DebugValidator::IsLargerOrEqual(value, 1);
        totalInstrs_ = value;
    }

    int RemainingInstructions(bool includeChildren = false) const {
        int remaining = TotalInstructions() - ConstantDifference();

        if(includeChildren) {
            for(int i = 0; i < children_.Count(); i++) {
                remaining += children_[i]->RemainingInstructions(includeChildren);
            }
        }

        return remaining;
    }

    void AddConstantOperand(Operand* op, Constant* constantOp) {
        DebugValidator::IsNotNull(op);
        DebugValidator::IsNotNull(constantOp);
        constantOps_.Add(op, constantOp);
    }

    Constant* GetAsConstant(Operand* op) {
        DebugValidator::IsNotNull(op);
        Constant* constant;

        if(constantOps_.TryGetValue(op, &constant)) {
            return constant;
        }
        else if(parent_) {
            return parent_->GetAsConstant(op);
        }
        else return nullptr;
    }

    bool IsConstant(Operand* op) {
        return GetAsConstant(op) != nullptr;
    }

    int AlwaysIncomingConstantCount() const {
        return alwaysIncomingConstCount_;
    }

    void SetAlwaysIncomingConstantCount(int value) {
        alwaysIncomingConstCount_ = value;
    }

    int WithParentRegionIncomingConstantCount() const {
        return withParentRegionConstCount_;
    }

    void SetWithParentRegionIncomingConstantCount(int value) {
        withParentRegionConstCount_ = value;
    }

    ConstantMap& ConstantOperands() {
        return constantOps_;
    }

    bool IsSelected() const {
        return selected_;
    }

    void SetIsSelected(bool value) {
        selected_ = value;
    }

    List<OperandPair>& ProbableOperands() {
        return probableOpPairs_;
    }

    void CountFoldedInstruction(Instruction* instr) {
        DebugValidator::IsNotNull(instr);

        if(instr->IsMul()) {
            counts_.MulCount++;
        }
        else if(instr->IsDiv() || instr->IsUdiv() ||
                instr->IsMod() || instr->IsUmod()) {
            counts_.DivCount++;
        }
        else if(instr->IsFloatArithmetic()) {
            counts_.FloatingCount++;
        }
        else counts_.SimpleCount++;
    }

    float GetFoldedInstructionsWeight(bool includeChildren = false) const {
        float value = (float)((counts_.SimpleCount * SIMPLE_WEIGHT) +
                              (counts_.MulCount * MUL_WEIGHT) +
                              (counts_.DivCount * DIV_WEIGHT) +
                              (counts_.FloatingCount * FLOATING_WEIGHT)) /
                      (float)((std::min(counts_.SimpleCount, 1) * SIMPLE_WEIGHT) +
                              (std::min(counts_.MulCount, 1) * MUL_WEIGHT) +
                              (std::min(counts_.DivCount, 1) * DIV_WEIGHT) +
                              (std::min(counts_.FloatingCount, 1) * FLOATING_WEIGHT));
        
        if(includeChildren) {
            for(int i = 0; i < children_.Count(); i++) {
                value += children_[i]->GetFoldedInstructionsWeight(true);
            }

            value = value / (1 + children_.Count());
        }
        
        return value;
    }

    void ResetFoldedInstructionCounts() {
        counts_.SimpleCount = 0;
        counts_.MulCount = 0;
        counts_.DivCount = 0;
        counts_.FloatingCount = 0;
    }
};


// The SCCP driver.
class ConstantPropagation : public Pass {
private:
    // Helper used to sort the regions based on their score.
    struct RegionSortData {
        ConstantRegion* Region;
        float Score;
        int Cost;
        bool IsTree;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        RegionSortData() {}

        RegionSortData(ConstantRegion* region, float score, 
                       int cost, bool isTree = false) :
                Region(region), Score(score), Cost(cost), IsTree(isTree) {}

        RegionSortData(const RegionSortData& other) :
                Region(other.Region), Score(other.Score), Cost(other.Cost),
                IsTree(other.IsTree) {}

        bool operator< (const RegionSortData& other) const {
            // We want the regions to be sorted descending.
            return Score > other.Score;
        }
    };

    typedef IRDominatorTree::TDominatorNode DominatorNode;
    typedef Dictionary<Operand*, Constant*> ConstantMap;
    typedef StaticList<Constant*, 2> ConstantList;
    typedef List<Operand*> OperandList;
    typedef List<OperandPair> ProbableOperandList;
    typedef List<RegionSortData> SortedRegionList;
    typedef List<ConstantRegion*> RegionList;
    typedef List<Block*> BlockList;
    typedef ConstantPropagation CCP;

    //! MAKE THEM CONTROLS!
    static const int MAX_REGION_BLOCK_COUNT = 32;
    static const int MAX_FUNCTION_INSTRUCTIONS_CLONED = 192;
    static const int CONSTANT_INSTRUCTION_BENEFIT = 2;
    static const int REMAINING_INSTRUCTION_PENALITY = 1;
    static const int MIN_EDGE_COUNT_DIFFERENCE = 30;
    static const int AVERAGE_BLOCK_SIZE = 4;
    static const int MIN_CLONE_LIMIT = AVERAGE_BLOCK_SIZE * 2;
    static const float MIN_REGION_SCORE;
    static const float MIN_OPERAND_PROBABILITY;
    static const float PROBABLE_OPERAND_PENALTY;
    static const int MAX_PROBABLE_OPERANDS = 2;

    LatticePropagator propagator_;
    shared<ConstantLattice> lattice_;
    shared<IRDominatorTree> domTree_;
    shared<ConstantLatticeCellCache> cache_;
    List<ConstantRegion*> regions_;
    List<ConstantRegion*> regionRoots_;
    Function* funct_;
    int constantsFound_;
    int constantBranchesFound_;
    bool hasFixedPhiConsts_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Scans all instructions in the function and replaces
    // them with constant, if it's the case.
    void PatchFunction(Function* function);

    // If the instruction turns out to be a constant
    // it replaces the result operand, then deletes the instruction.
    void PatchInstruction(Instruction* instr);

    // If the branch turned out to be unnecessary
    // it replaces the condition operand with the appropriate constant.
    // The dead blocks will be removed later by the CFG Cleaner.
    void PatchBranching(Instruction* instr);

    // Scans the functions and looks for regions that can benefit
    // from specialization (creating a clone of the region where
    // a 'phi' result is replaced by a constant, leading to constant folding).
    int FindConstantRegions();

    bool ShouldOptimizeRegion(Block* regionEntryBlock, 
                              ProbableOperandList& probableOps);

    // Looks at the predecessors of the specified block and selects
    // the one from which control reaches in most cases.
    // Edge count profile information is used here.
    Block* FindMaximumIncomingBlock(Block* block);

    // Selects a subset of the found regions to be specialized.
    // The algorithm is based on the "knapsack" idea, each region
    // having a benefit/cost ratio (the score).
    void SelectConstantRegions(RegionList& selectedRegions);

    void MarkRegionAsSelected(RegionSortData& regionInfo,
                              RegionList& selectedRegions);

    // Selects the regions that should be considered for specialization.
    // Regions with a very low improvement score are not selected.
    void SelectCandidateRegions(SortedRegionList& list);

    // Sorts the selected region descending based on their score. 
    void SortCandidateRegions(SortedRegionList& list);

    // Marks all the regions in the specified tree as "selected".
    void SelectRegionTree(ConstantRegion* region, RegionList& selectedRegions);

    // Computes the region score based on the benefit/cost ratio.
    // If 'includeSubregions' is specified the score applies to the entire
    // region tree. The estimated number of remaining instructions 
    // after specialization is incremented by 'instrCountAdjustment'.
    float ComputeRegionScore(ConstantRegion* region, bool includeSubregions = false,
                             int instrCountAdjustment = 0);

    // Estimates the specialization benefit for an entire region tree
    // using a more precise, but slower algorithm (practically it creates
    // a clone of the SCCP driver, runs the SCCP algorithm on the region,
    // then counts how many instructions became constants or where unreachable).
    bool AnalyzeRegionTree(RegionSortData& regionInfo, 
                           int actualCost, int maximumCost, int& newCost);

    // Recomputes the number of remaining/constant/unreachable
    // instructions after the SCCP algorithm was run using the
    // specified lattice. 'reachableBlocks' indicates the blocks
    // that are still reachable after specialization.
    void RecomputeInstructionCounts(ConstantRegion* region,
                                    ConstantLattice* lattice, 
                                    SparseBitVector& reachableBlocks,
                                    int& instructionCount,
                                    int& constInstructionCount,
                                    int& unreachableInstructionCount,
                                    bool includeSubregions);

    // Returns 'true' if the specified operand is a constant
    // after specialization for it's region is done.
    bool IsConstantAfterSpecialization(bool ignoreOriginalConstants, 
                                       ConstantLattice* lattice, Operand* op);

    // Determines the blocks that are reachable from the function entry
    // after the SCCP algorithm was run using the specified lattice.
    void AnalyzeReachableBlocks(Block* block, ConstantLattice* lattice,
                                SparseBitVector& reachableBlocks);
    
    // Creates a clone of the current SCCP driver which is used
    // to evaluate a region tree. Forces the 'phi' instructions
    // of the region entries to be replaced by constants.
    shared<ConstantPropagation> 
    CreateRegionEvaluator(ConstantRegion* region, bool includeSubregions);

    // Forces the 'phi' instructions in the region entry block
    // to be replaced by constants when running the SCCP algorithm
    // using the specified lattice. If 'includeSubregions' is set
    // it also (recursively) replaces the 'phi's in the subregions.
    void SetFixedPhiConstants(ConstantRegion* region, bool includeSubregions,
                              ConstantLattice* lattice);

    // Clones the selected regions and inserts them into the function,
    // making the required CFG changes. For regions that are specialized
    // only if an operand has a specific value a test block is inserted.
    void CloneConstantRegions(RegionList& selectedRegions);

    // Makes the required changes to connect a cloned region to the CFG.
    void RedirectRegionEntry(Block* regionEntry, Block* clonedRegionEntry,
                             Block* specializedPredecessor);

    // Makes the required changes to connect a cloned region 
    // that needs a test block to the CFG.
    void RedirectRegionEntryWithTestBlock(Block* regionEntry, Block* clonedRegionEntry,
                                          Block* specializedPredecessor, Block* testBlock);

    // Creates a new block where the incoming operands are tested
    // against the expected constants.
    Block* CreateTestBlock(ConstantRegion* region);

    // Clones the blocks that form the specified region.
    // If the region is a subregion, and the parent region was also cloned,
    // the blocks are cloned only in the parent's clone.
    Block* CloneBlocks(ConstantRegion* region, BlockList& clonedBlocks);

    // Returns the predecessor that forms with the entry block 
    // of the region the most executed edge.
    // If the region is a subregion, and the parent region was also cloned,
    // the returned block will be part of the parent's clone.
    Block* GetSpecializedPredecessor(ConstantRegion* region);

    // Creates a region that starts from the specified block.
    // The regions contains the entry block and all the blocks
    // that are dominated by it. After the region is created
    // the number of incoming constant 'phi' operands is set.
    ConstantRegion* CreateRegion(Block* startBlock, int alwaysCount,
                                 int withParentRegionCount);

    // Adds to the region the blocks that are dominated by the block.
    void AddDominatedBlocks(DominatorNode* node, ConstantRegion* region);

    // Returns the number of blocks a region starting with the
    // specified block would have.
    int ComputeRegionSize(DominatorNode* node);

    int ComputeRegionSize(Block* block) {
        DebugValidator::IsNotNull(block);
        DebugValidator::IsNotNull(domTree_.Raw());
        return ComputeRegionSize(domTree_->GetBlockNode(block));
    }

    // If the region entry block is embedded inside another region
    // it returns that region, else 'nullptr'. If there are multiple 
    // parent candidates, it returns the nearest one.
    ConstantRegion* GetParentRegion(Block* block);

    ConstantRegion* GetParentRegion(ConstantRegion* rootRegion, Block* block);

    //! TODO: implement
    // Returns the estimated execution count of the specified block.
    int GetBlockExecutionCount(Block* block) { return block->InstructionCount();}

    // TODO: Implement
    // TODO: This should be provided by the profile module.
    int GetMaximumExecutionCount() {
        int maximum = -1;

        for(auto block = funct_->FirstBlock(); block; block = block->NextBlock()) {
            if(block->InstructionCount() > maximum) {
                maximum = block->InstructionCount();
            }
        }

        return std::max(1, maximum);
    }

    // Returns the estimated execution count of the specified edge.
    int GetEdgeCount(Block* fromBlock, Block* toBlock) {
        //! IMPLEMENT
        if(*fromBlock->Name() == "#entry") return 0;
        return 5;
    }

    // Computes the benefit of specializing the specified region
    // by estimating how many instructions will turn to constants.
    void ComputeRegionBenefit(ConstantRegion* region);

    // Implements a fast algorithm that estimates the number of instructions
    // that would turn into constants if the specified block is specialized.
    void EstimateConstants(Block* block, ConstantMap& constantMap,
                           int& constantInstrs, int& constantAfterInstrs,
                           ConstantFolder& folder, ConstantRegion* region);

    bool CollectConstantOperands(Instruction* instr, ConstantMap& constantMap,
                                 ConstantList& constantOps, bool& hasUndefined);

    // Computes the cost of specializing the region, equal to the number
    // of instructions that need to be cloned in each block.
    void ComputeRegionCost(ConstantRegion* region);

    // Computes the average execution count of the blocks 
    // found in the specified region, optionally including any subregion.
    float ComputeAverageExecutionCount(ConstantRegion* region, 
                                       bool includeSubregions);

    // Adds the region to the list of found regions.
    void AddRegion(ConstantRegion* region);

    // Returns 'true' if the specified instruction can be considered
    // a constant when evaluated with the operands taken from 'constantMap'.
    bool InstructionIsConstant(Instruction* instr, ConstantMap& constantMap,
                               Constant*& constantResult);

    // Verifies if any of the 'phi' instructions in the block 
    // has incoming operands that are constants from 'maxIncomingBlock'.
    // The operands that replace the 'phi' in the specialized region
    // are added to 'replacementOps'. Operands that are likely to have
    // a specific constant value are added to 'probableOps'.
    bool HasIncomingConstantPhi(Block* block, Block* maxIncomingBlock,
                                int& alwaysCount, int& withParentRegionCount,
                                List<Operand*>& replacementOps,
                                List<OperandPair>& probableOps);

    // Returns the most probable value the specified operand could have,
    // or 'nullptr' if such value can not be determined or its
    // probability is low.
    bool GetProbableValue(Operand* op, Block* testBlock, 
                          ValueProbability& probableValue);

    int GetCloneLimit();

    // Evaluates the specified instruction using for the operands
    // the values found in 'constantOps'.
    Constant* GetConstantResult(Instruction* instr, bool hasUndefined,
                                ConstantList& constantOps,
                                ConstantFolder& folder);

    bool IsZero(Operand* op, ConstantMap& constMap) {
        return lattice_->GetCell(op)->IsZeroInt() ||
               (constMap.ContainsKey(op) && constMap[op]->IsZeroInt());
    }

    bool IsMinusOne(Operand* op, ConstantMap& constMap) {
        return lattice_->GetCell(op)->IsMinusOneInt() ||
               (constMap.ContainsKey(op) && constMap[op]->IsMinusOneInt());
    }

    Constant* TryGetConstant(Operand* op, ConstantMap& constMap) {
        if(auto constantOp = op->As<Constant>()) {
            return constantOp;
        }
        else if(auto cellConstOp = lattice_->GetCell(op)->AsConstant()) {
            return cellConstOp;
        }
        else if(constMap.ContainsKey(op)) {
            return constMap[op];
        }
        else return nullptr;
    }

    // Methods for obtaining certain constants.
    IntConstant* GetOneInt() {
        return funct_->ParentUnit()->Constants().GetInt32(1);
    }

    IntConstant* GetZeroInt() {
        return funct_->ParentUnit()->Constants().GetInt32(0);
    }

    IntConstant* GetMinusOneInt() {
        return funct_->ParentUnit()->Constants().GetInt32(-1);
    }

    IntConstant* GetIntConstant(const Type* type, __int64 value) {
        return funct_->ParentUnit()->Constants().GetInt(type, value);
    }

    UndefinedConstant* GetUndefined(Operand* op) {
        DebugValidator::IsNotNull(op);
        return funct_->ParentUnit()->Constants().GetUndefined(op->GetType());
    }

    bool MightBeConstant(bool isConstant, Instruction* instr) {
        return (isConstant == false) &&
               (instr->IsArithmetic() || instr->IsLogical());
    }

    void InstructionSimplified(Instruction* instr);

    void DumpRegions();

public:
    ConstantPropagation() : 
            cache_(new ConstantLatticeCellCache()) {}

    ConstantPropagation(shared<ConstantLatticeCellCache> cache) :
            cache_(cache) {}

    ~ConstantPropagation() {
        for(int i = 0; i < regions_.Count(); i++) {
            delete regions_[i];
        }
    }

    void Initialize(Function* function);

    void Execute(Function* function);

    void IterateToFixedpoint(bool limited = false, bool resume = false);

    int ConstantInstructionCount() const;

    ConstantLattice* Lattice() {
        return lattice_;
    }

    void Dump();
};

} // namespace Optimization
#endif