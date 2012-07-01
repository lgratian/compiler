// LatticePropagator.hpp
// Copyright (c) Lup Gratian
//
// Defines the SSA propagator, which implements an algorithm
// similar to Sparse Conditional Constant Propagation, but using
// a lattice that can be customized.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SSA_PROPAGATOR_HPP
#define PC_ANALYSIS_SSA_PROPAGATOR_HPP

#include "SparseBitVector.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Constants.hpp"
#include "../IR/References.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../Base/String.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/DefaultComparer.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

// The type of the value of a cell, in a lattice-theoretic sense.
enum CellType {
	Cell_Bottom,
	Cell_Top,
	Cell_Value
};


// Base class for the values used by a lattice.
// A "ConstantCell" might include the constant, for example.
class LatticeCell {
private:
    LatticeCell(const LatticeCell& other);
    LatticeCell& operator= (const LatticeCell& other);

protected:
	CellType type_;
	
public:
    LatticeCell(CellType type) : type_(type) {}

    virtual ~LatticeCell() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	CellType Type() const {
		return type_;
	}
	
	void SetType(CellType type) {
		type_ = type;
	}

    bool IsTop() const {
        return type_ == Cell_Top;
    }

    bool IsBottom() const {
        return type_ == Cell_Bottom;
    }
	
	virtual string ToString() const {
        if(IsTop()) return "TOP";
        else if(IsBottom()) return "BOTTOM";
        else return "VALUE";
	}
};


// Represents an edge in the CFG.
struct FlowEdge {
	Block* First;
	Block* Second;
	
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	FlowEdge() {}

	FlowEdge(Block* first, Block* second) : 
            First(first), Second(second) {}

	FlowEdge(const FlowEdge& other) : 
            First(other.First), Second(other.Second) {}
	
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	unsigned GetHashCode() const {
        unsigned hash = 17;
        hash = hash * 37 + HashCalculator::GetHashCode(&First, sizeof(First));
        hash = hash * 37 + HashCalculator::GetHashCode(&Second, sizeof(Second));
        return hash;
    }

    FlowEdge& operator= (const FlowEdge& other) {
        First = other.First;
        Second = other.Second;
        return *this;
    }
	
    bool operator== (const FlowEdge& other) const {
        return (First == other.First) &&
               (Second == other.Second);
    }

    bool operator!= (const FlowEdge& other) const {
        return operator== (other) == false;
    }

    bool operator< (const FlowEdge& other) const {
        return true; // Required by 'DefaultComparer'.
    }
};

// Forward declarations.
class LatticePropagator;


// Base class for the lattice used by the propagator.
// Should implement methods that "evaluate" the instructions
// using the lattice values. Default implementations for some
// of these methods are provided ('phi' evaluation, for example).
class Lattice {
private:
    Lattice(const Lattice& other);
    Lattice& operator= (const Lattice& other);

protected:
	LatticePropagator* propagator_; // The propagator that uses the lattice.

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Methods that should return the TOP and BOTTOM lattice cells.
    virtual LatticeCell* GetTopCell() = 0;
    virtual LatticeCell* GetBottomCell() = 0;
    
    // Should return a lattice cell that represents the specified constant.
    // No default implementation is provided because the cell is highly
    // dependent on the lattice used.
    virtual LatticeCell* GetCellForConstant(Constant* constant) = 0;

    // Should return a lattice cell that represents the specified parameter.
    // By default it creates a BOTTOM value cell.
    virtual LatticeCell* GetCellForParameter(Parameter* parameter) {
        return GetBottomCell();
    }

    // Should return the maximum number of incoming operands
    // a 'phi' instruction can have before the propagator gives up
    // and uses a BOTTOM cell for its result operand. 
    virtual int PhiLimit() { 
        return 64; 
    }

    // Various helper methods.
    LatticeCell* MarkBottom(Instruction* instr);
    LatticeCell* MarkTop(Instruction* instr);
    LatticeCell* GetCell(Instruction* instr);
    LatticeCell* GetCell(Operand* op);
    void UpdateCell(Instruction* instr, LatticeCell* cell);
    bool IsExecutableEdge(Block* fromBlock, Block* toBlock);

public:
    typedef StaticList<Block*, 8> TSuccessorList;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Lattice() {}

    virtual ~Lattice() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Method called when the SSA propagation started.
	virtual void PropagationStarted() {}

    // Method called when the SSA propagation ended
    // because it reached the fixedpoint.
	virtual void PropagationEnded() {}

    // Method called when the maximum lattice cell update count has been reached.
    virtual void CellLimitReached(Temporary* temp) {}

    // Returns the SSA propagator that uses the lattice.
	LatticePropagator* Propagator() {
		return propagator_;
	}

    void SetPropagator(LatticePropagator* value) {
        propagator_ = value;
    }

	// Returns the cell that should be used the first time
    // when an operand is encountered. The default implementation 
    // handles parameters and constants using the customizable
    // 'GetCellForParameter' and 'GetCellForConstant' methods,
    // and creates a TOP value cell for all other cases.
	virtual LatticeCell* GetInitializationCell(Operand* op);

    // Should return 'true' if the two cells are equal.
    // The default implementation checks for TOP and BOTTOM cells.
	virtual bool AreCellsEqual(LatticeCell* cellA, LatticeCell* cellB);
	
    // Should return the cell that represents the "meet"
    // of the values represented by the cells.
	virtual LatticeCell* MergeCells(LatticeCell* cellA, LatticeCell* cellB);
	
    // Should perform the action required by the lattice 
    // on the specified instruction. No default implementation is provided,
    // because it's highly dependent on the type of the lattice.
	virtual void Transfer(Instruction* instr) = 0;

    // Should perform the action required by the lattice
    // on the specified 'phi' instruction. By default it tries to merge
    // the lattice values of all incoming operands, ignoring operands
    // that were not yet processed in 'optimistic' mode.
	virtual LatticeCell* TransferPhi(PhiInstr* instr, bool isOptimistic);
	
    // Should add to the list all successor blocks that are reachable
    // based on the lattice value of the condition operand.
    // By default it adds all successors to the list.
    // Returns the number of added successors.
	virtual int AddFeasibleSuccessors(ControlInstr* instr, 
                                      TSuccessorList& successorList);
};


// The SSA propagator. "Optimistic" mode means that the unknown incoming
// operands of a 'phi' instructions are presumed to be TOP, allowing
// certain algorithms to get better results this way.
class LatticePropagator {
private:
    typedef Lattice::TSuccessorList TSuccessorList;

	Lattice* lattice_;                           // The associated lattice.
	Dictionary<Operand*, LatticeCell*> cells_;   // The cell associated with each operand.
	Dictionary<FlowEdge, bool> executableEdges_; // The edges in the CFG that are executable.
    List<Block*> blockWorklist_;                 // The blocks that need processing.
    SparseBitVector inBlockWorklist_;   // The blocks that are in the worklist.
    SparseBitVector wasBlockProcessed_; // the blocks that were processed at least once.
    List<Instruction*> instructionWorklist_; // The instructions that need processing.
    Dictionary<Instruction*, bool> inInstructionWorklist_;
    Dictionary<Instruction*, int> instructionInterations_;
    bool optimistic_;       // 'true' if the propagation is "optimistic".
    bool limitIterations_;  // 'true' if the number of times an instruction 
                            // is reprocessed should be limited.
    int maxIterationCount_; // The reprocessing iteration limit.
    
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Adds all instructions that use the temporary to the worklist.
	void AddUsersToWorklist(Temporary* temp);
	
    // Adds all successors that are reachable to the worklist.
    void AddExecutableSuccessors(Block* block, TSuccessorList& successorList);
    
    // Initializes the propagation algorithm.
    void Initialize(Function* function);

    // Simulates the instructions in the block, if necessary.
    void ProcessBlock(Block* block);

    // Simulates the instruction using the lattice.
    void ProcessInstruction(Instruction* instr);

    // Increments the iteration count for the instruction.
    void UpdateIterationCount(Instruction* instr);

    // The propagation driver.
    void Process();
	
public:
    LatticePropagator(Lattice* lattice = nullptr) : 
            lattice_(lattice), limitIterations_(false) {}

    virtual ~LatticePropagator() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    void SetLattice(Lattice* lattice) {
        DebugValidator::IsNotNull(lattice);
        lattice_ = lattice;
    }

    // Runs the propagation algorithm.
    void Start(Function* function, bool optimistic = true);

    // Resumes the propagation algorithm.
    void Resume();

    // Methods for querying/setting the lattice cell associated 
    // with an operand or with the result operand of an instruction.
	LatticeCell* GetCell(Operand* op);
	LatticeCell* GetCell(Instruction* instr);

	void SetCell(Operand* op, LatticeCell* cell);
	void SetCell(Instruction* instr, LatticeCell* cell);
	
    // Returns 'true' if the specified CFG edge is marked as executable.
    bool IsExecutableEdge(FlowEdge edge) {
        return executableEdges_.ContainsKey(edge);
    }

    // Updates the cell associated with the operand or with
    // the result operand of an instruction. Returns 'true'
    // if the cell is different from the previous one and the users
    // were added to the instruction worklist for processing.
    bool UpdateCell(Instruction* instr, LatticeCell* cell);

    // Adds the specified block to the worklist (in not added already). 
    // If 'predecessorBlock' is specified it marks the edge as executable.
    void AddBlockToWorklist(Block* block, Block* predecessorBlock = nullptr);

    // If 'true' the number of times an instruction is
    // reprocessed is limited, even though its operands change.
    bool LimitIterations() const {
        return limitIterations_;
    }

    void SetLimitIterations(bool value) {
        limitIterations_ = value;
    }

    // Returns the number of times that an instruction
    // should be reprocessed when its operands change.
    int MaximumIterationCount() const {
        return maxIterationCount_;
    }

    void SetMaximumIterationCount(int value) {
        maxIterationCount_ = value;
    }
};

} // namespace Analysis
#endif