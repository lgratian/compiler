// RecursionElimination.cpp
// Copyright (c) Lup Gratian
//
// Implements tail-recursion elimination. This turns a tail call
// to itself to a jump to the entry point of the function.
// In order to convert many more functions it may insert accumulator
// variables (handles cases like recursive factorial, for example).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_RECURSION_ELIMINATION_HPP
#define PC_OPTIMIZATION_RECURSION_ELIMINATION_HPP

#include "BlockUtils.hpp"
#include "../Analysis/IntArithmetic.hpp"
#include "../Analysis/StdLibRecognizer.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Tag.hpp"
#include "../IR/Unit.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/Dictionary.hpp"
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

class RecursionElimination : public Pass {
private:
    // Stores information about a tail call.
    // 'Additive' contains the instructions that compose
    // the additive accumulator, 'Multiplicative' the ones
    // that form the multiplicative accumulator. For example, in
    // return a + b + c*f(n - 1);
    // 'a' and 'b' are part of 'Additive', 'c' of 'Multiplicative'.
    struct TailCallInfo {
        CallInstr* TailCall;
        ReturnInstr* TailReturn;
        StaticList<Operand*, 2> Additive;
        StaticList<Operand*, 2> Multiplicative;
    };

    Function* funct_;
    StaticList<TailCallInfo, 8> tailCalls_;    // List of tail calls found.
    StaticList<ReturnInstr*, 8> returnInstrs_; // Not part of tail calls.
    Dictionary<ReturnInstr*, bool> processedReturns_;
    Block* newEntryBlock_;
    Block* oldEntryBlock_;
    PhiInstr* additiveAccumulator_;
    PhiInstr* multiplicativeAccumulator_;
    StaticList<PhiInstr*, 16> parameterPhis_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Returns 'true' if we should try to transform the function.
    // It's not possible if we have variables that are aggregates, for example.
    bool IsFunctionCandidate();
    
    // Analyzes all calls in the function in order to identify
    // tail recursions. Here we only collect the candidate calls.
    // Returns 'true' if any tail call was found.
    bool AnalyzeCalls();

    // Returns 'true' if the specified function is a tail-recursive call,
    // and stores details about the call into 'info'.
    bool IsTailCall(CallInstr* callInstr, TailCallInfo& info);

    // Returns 'true' if the call needs accumulators to be eliminated.
    // Checks for '(a + b + ...) + (x * y * ...) * f(...)'.
    bool IsTailCallWithAccumulator(CallInstr* callInstr, ReturnInstr* retInstr,
                                   TailCallInfo& info);

    // Returns 'true' if the call can be eliminated after all instructions
    // found between the 'call' and 'ret' can be moved before the 'call'.
    bool IsTailCallWithMoveBefore(CallInstr* callInstr, ReturnInstr* retInstr,
                                  TailCallInfo& info);

    // Returns 'true' if the specified operand
    // doesn't depend on the iteration number of the function.
    bool IsCallInvariant(Operand* op, CallInstr* callInstr);

    // 
    bool CollectCallInvariant(Operand* op, CallInstr* callInstr,
                              TailCallInfo& info, bool additive);

    // Returns 'true' if the instruction can be moved
    // before the specified 'call' instruction.
    bool CanBeMovedBeforeCall(Instruction* instr, CallInstr* callInstr);

    // Eliminates the tail call described by the information object,
    // This also creates code for accumulators if required.
    void EliminateTailCall(TailCallInfo& info);

    // Removes all instructions that become dead after recursion
    // elimination (this includes the 'call' and 'ret').
    void RemoveDeadInstructions(TailCallInfo& info);

    // Transforms the arguments of the 'call' into incoming
    // operands for the 'phi' instructions in the old entry block.
    void CopyArguments(CallInstr* callInstr);

    // Generates code that update the accumulators
    // using the specified information.
    void GenerateAccumulatorUpdate(TailCallInfo& info);
    
    // Replaces uses of parameters with uses of the 'phi' instruction
    // that were added in the old entry block.
    void ReplaceParameters();

    // Generates code that computes the correct returned value for
    // 'ret' instructions that are not part of tail calls. 
    void RewriteOtherReturns();

    // Methods to create 'add'/'mul' instructions (floating too).
    // The instructions are placed before 'beforeInstr'.
    Operand* CreateAdd(Operand* opA, Operand* opB, Instruction* beforeInstr);
    Operand* CreateMul(Operand* opA, Operand* opB, Instruction* beforeInstr);

    // Creates a new entry block and connects it with the previous one.
    Block* CreateNewEntryBlock();

    // Creates the 'phi' instructions in the old entry block
    // that "combine" the original parameters and the arguments
    // from the calls that will be eliminated.
    void CreateParameterPhis();

    // Creates a properly initialized accumulator instruction.
    void CreateAccumulator(PhiInstr*& accumulator, bool isAdditive);

    // Returns 'true' if accumulators are needed
    // in order to eliminated the tail-recursive call.
    bool NeedsAccumulator(TailCallInfo& info) {
        return (info.Additive.Count() > 0) ||
               (info.Multiplicative.Count() > 0);
    }

    void CallEliminated(Block* block);

public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif