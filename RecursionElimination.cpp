// RecursionElimination.cpp
// Copyright (c) Lup Gratian
//
// Implements the Recursion Elimination pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "RecursionElimination.hpp"

namespace Optimization {

void RecursionElimination::Execute(Function* function) {
    funct_ = function;
    newEntryBlock_ = nullptr;
    additiveAccumulator_ = nullptr;
    multiplicativeAccumulator_ = nullptr;

    // Give up early if we can't do tail-recursion elimination.
    if(IsFunctionCandidate() == false) {
        return;
    }

    if(AnalyzeCalls()) {
        // We found candidate calls. Create a new entry block
        // where the accumulators (if required) are placed.
        oldEntryBlock_ = funct_->FirstBlock();
        newEntryBlock_ = CreateNewEntryBlock();

        // The arguments of the tail calls are "collected"
        // and merged with the initial parameters using 'phi' instructions.
        CreateParameterPhis();

        // Process each tail call.
        for(int i = 0; i < tailCalls_.Count(); i++) {
            EliminateTailCall(tailCalls_[i]);            
        }

        // Replace all parameters with the corresponding
        // 'phi' instruction results.
        ReplaceParameters();

        // Now all 'ret' instructions that are not part
        // of a tail call need to be rewritten in order
        // to return the correct value.
        // ret a -> ret (aAccum + mAccum * x)
        RewriteOtherReturns();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RecursionElimination::IsFunctionCandidate() {
    // We don't check for tail-recursion elimination opportunities 
    // if the function has variables that could not be converted
    // to SSA form (this includes aggregates), or if the function 
    // has variable number of arguments.
    if((funct_->VariableCount() > 0) ||
        funct_->IsVarargs()) {
        return false;
    }

    // Neither of the parameters should have it's address taken,
    // because they could be modified in unexpected ways.
    for(int i = 0; i < funct_->ParameterCount(); i++) {
        if(funct_->GetParameterVariable(i)->IsAddressTaken()) {
            return false;
        }
    }

    // The function shouldn't contain calls to 'alloca',
    // 'setjmp' or 'longjmp'.
    StdlibRecognizer recognizer;
    bool valid = true;

    funct_->ForEachInstruction([&valid, &recognizer]
                               (Instruction* instr) -> bool {
        if(auto callInstr = instr->As<CallInstr>()) {
            StdlibType type = recognizer.Recognize(callInstr);

            if((type == Stdlib_setjmp) ||
               (type == Stdlib_longjmp) ||
               (type == Stdlib_alloca)) {
                valid = false;
                return false; // End foreach.
            }
        }
        
        return true;
    });

    return valid;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RecursionElimination::AnalyzeCalls() {
    // Scan all instructions and search for tail-recursive calls.
    bool hasTailCall = false;

    for(auto block = funct_->FirstBlock(); block; block = block->NextBlock()) {
        for(auto instr = block->FirstInstruction(); instr; 
            instr = instr->NextInstruction()) {
            if(auto callInstr = instr->As<CallInstr>()) {
                TailCallInfo info;

                if(IsTailCall(callInstr, info)) {
                    // We process it later. Mark the 'ret' instruction as visited.
                    tailCalls_.Add(info);
                    processedReturns_.Add(info.TailReturn, true);
                    hasTailCall = true;
                }
            }
            else if(auto retInstr = instr->As<ReturnInstr>()) {
                // Add the 'ret' to the list if it isn't
                // part of a tail-call.
                if(processedReturns_.ContainsKey(retInstr) == false) {
                    returnInstrs_.Add(retInstr);
                }
            }
        }
    }

    return hasTailCall;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RecursionElimination::IsTailCall(CallInstr* callInstr, TailCallInfo& info) {
    // If the 'call' doesn't call our function then it can't
    // be a tail-call. If it is a recursive call then we require
    // one of the following:
    // 1. The 'call' is followed by a return.
    // 2. The 'call' is of the form: (a + b + ...) + (x * y * ..) * f(...)
    //    Either of the additive or multiplicative part can be missing.
    // 3. Between the 'call' and the 'return' are only instruction
    //    that can be moved before the call. This happens seldom, because
    //    it means that the returned value doesn't depend on what the
    //    called function may return.
    if(auto calledFunct = callInstr->GetCalledFunction()) {
        if(calledFunct != funct_) {
            return false;
        }
    }
    else return false;

    // Make sure that the block ends with a 'ret'.
    auto branchInstr = callInstr->ParentBlock()->BranchInstruction();
    auto retInstr = branchInstr->As<ReturnInstr>();
    if(retInstr == nullptr) {
        return false;
    }

    // Check situation 1.
    if(callInstr->NextInstruction() == retInstr) {
        info.TailCall = callInstr;
        info.TailReturn = retInstr;
        return true;
    }

    // Check situation 2.
    if(IsTailCallWithAccumulator(callInstr, retInstr, info)) {
        return true;
    }

    // Check situation 3.
    return IsTailCallWithMoveBefore(callInstr, retInstr, info);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RecursionElimination::IsTailCallWithAccumulator(CallInstr* callInstr, 
                                                     ReturnInstr* retInstr,
                                                     TailCallInfo& info) {
    // If we don't have a returned value we have nothing to do.
    if(callInstr->IsVoid() || retInstr->IsVoid()) {
        return false;
    }

    // Walk from the call until we reach the return instruction.
    // Collect any 'add'/'fadd' or 'mul'/'fmul' instruction on the way,
    // and give up if we see something invalid/dangerous.
    bool sawAdd = false;
    bool sawMul = false;
    auto currentOp = callInstr->ResultOp();

    while(currentOp && currentOp->HasSingleUser()) {
        DebugValidator::IsTrue(currentOp->IsTemporary());
        auto user = currentOp->As<Temporary>()->GetUser(0);

        if((user->IsMul() || user->IsFmul()) && 
           (sawAdd == false)) {
            // Add the multiplier to the list.
            auto multiplier = user->GetSourceOp(0) == currentOp ?
                              user->GetSourceOp(1) : user->GetSourceOp(0);
            currentOp = user->GetDestinationOp();

            // Make sure that the multiplier doesn't change
            // the value after the first iteration.
           
            if(CollectCallInvariant(multiplier, callInstr, info, false)) {
                sawMul = true;
            }
            else return false;
        }
        else if(user->IsAdd() || user->IsFadd()) {
            // Add the addend to the list.
            auto addend = user->GetSourceOp(0) == currentOp ?
                          user->GetSourceOp(1) : user->GetSourceOp(0);
            currentOp = user->GetDestinationOp();

            // Make sure that the multiplier doesn't change
            // the value after the first iteration.
            if(CollectCallInvariant(addend, callInstr, info, true)) {
                sawAdd = true;
            }
            else return false;
        }
        else if(user == retInstr) {
            // We reached the 'ret' instruction,
            // it means that all is OK.
            break;
        }
        else {
            // We have an invalid instruction, or a situation
            // like 'mul' after we already saw an 'add'.
            return false; 
        }
    }

    info.TailCall = callInstr;
    info.TailReturn = retInstr;
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RecursionElimination::CollectCallInvariant(Operand* op, CallInstr* callInstr,
                                                TailCallInfo& info, bool additive) {
    // Check if the operand is call-invariant.
    if(IsCallInvariant(op, callInstr)) {
        if(additive) info.Additive.Add(op);
        else info.Multiplicative.Add(op);
        return true;
    }

    // If all operands are call-invariant we don't care
    // about the instruction opcode, as long as it's an
    // arithmetic, logical or conversion instruction.
    if(auto definingInstr = op->DefiningInstruction()) {
        bool allInvariant = true;

        for(int i = 0; i < definingInstr->SourceOpCount(); i++) {
            if(IsCallInvariant(definingInstr->GetSourceOp(i), callInstr) == false) {
                allInvariant = false;
                break;
            }
        }

        if(allInvariant) {
            if(additive) info.Additive.Add(op);
            else info.Multiplicative.Add(op);
            return true;
        }
    }

    // Check if we have a series of additions/multiplications
    // that involve only call-invariant operands.
    if(auto arithInstr = op->DefiningInstrAs<ArithmeticInstr>()) {
        bool valid = ((arithInstr->IsAdd() || arithInstr->IsFadd()) && additive) ||
                      (arithInstr->IsMul() || arithInstr->IsFmul());
        if(valid == false) {
            return false;
        }

        return CollectCallInvariant(arithInstr->LeftOp(), callInstr,
                                    info, additive) &&
               CollectCallInvariant(arithInstr->RightOp(), callInstr,
                                    info, additive);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RecursionElimination::IsCallInvariant(Operand* op, CallInstr* callInstr) {
    // Check if the value of the operand changes depending
    // on the iteration number. This is similar to checking
    // if an operand is loop-invariant, with the difference
    // the we're here extremely conservative.
    if(op->IsConstant() || op->IsParameter()) {
        return true;
    }

    if(auto loadInstr = op->DefiningInstrAs<LoadInstr>()) {
        // If we load from a constant global variable
        // we know that nothing can modify the value.
        //! TODO: USE ALIAS INFO
        if(auto variableRef = loadInstr->SourceOp()->As<VariableReference>()) {
            if(auto globalVar = variableRef->GetGlobal()) {
                return globalVar->HasInitializer() &&
                       globalVar->IsConstant();
            }
        }
    }

    // If the block where the 'call' is found is the
    // 'case' target of a 'switch', and the operand is the 
    // condition operand of the 'switch' then we know it's value
    // and that it doesn't depend on the iteration number.
    auto callBlock = callInstr->ParentBlock();

    if(callBlock->HasSinglePredecessor()) {
        auto predecessorBlock = callBlock->PredecessorAt(0);

        if(auto switchInstr = predecessorBlock->BranchInstruction()->As<SwitchInstr>()) {
            bool found = false;

            for(int i = 0; i < switchInstr->CaseCount(); i++) {
                if(switchInstr->GetCase(i).Target->Target() == callBlock) {
                    // There should be a single 'case'
                    // that targets the block with the 'call'.
                    if(found) return false;
                    else found = true;
                }
            }

            return found && (switchInstr->ConditionOp() == op);
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RecursionElimination::IsTailCallWithMoveBefore(CallInstr* callInstr, 
                                                    ReturnInstr* retInstr,
                                                    TailCallInfo& info) {
    // Check if all instructions between 'call' and 'ret'
    // can be moved before the 'call'. If it's possible
    // then we can apply tail-recursion elimination.
    for(auto instr = callInstr->NextInstruction(); 
        instr && (instr != retInstr); instr = instr->NextInstruction()) {
        if(CanBeMovedBeforeCall(instr, callInstr) == false) {
            // Instruction can't be moved, give up.
            return false;
        }
    }

    // It's possible to move the instructions, now do it.
    auto block = callInstr->ParentBlock();

    while(callInstr->NextInstruction() != retInstr) {
        auto instr = callInstr->NextInstruction();
        callInstr->RemoveFromBlock();
        block->InsertInstructionBefore(instr, callInstr);
    }

    // Make sure we don't have any remaining operands.
    info.Additive.Clear();
    info.Multiplicative.Clear();
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool RecursionElimination::CanBeMovedBeforeCall(Instruction* instr, 
                                                CallInstr* callInstr) {
    // Make sure that the instruction doesn't use
    // the result operand of the 'call'.
    auto callResultOp = callInstr->ResultOp();

    for(int i = 0; i < instr->SourceOpCount(); i++) {
        if(instr->GetSourceOp(i) == callResultOp) {
            return false;
        }
    }

    // 'load' instructions can be moved before only if 
    // we're sure that it can't be modified by the 'call'.
    if(auto loadInstr = instr->As<LoadInstr>()) {
        // Don't touch volatile operations.
        if(loadInstr->IsVolatile() || funct_->IsNoWrite()) {
            return true;
        }

        // If we load from a global constant we're sure
        // nothing can write to it.
        if(auto variableRef = loadInstr->SourceOp()->As<VariableReference>()) {
            if(auto globalVar = variableRef->GetGlobal()) {
                return globalVar->HasInitializer() &&
                       globalVar->IsConstant();
            }
        }

        //! TODO: USE ALIAS ANALYSIS
        return false;
    }
    else if(instr->IsArithmetic() ||
            instr->IsLogical()    ||
            instr->IsConversion() ||
            instr->IsConversion()) {
        // It is always safe to move these instructions.
        return true;
    }
    else {
        // Any other instruction is not safe to move.
        return false;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RecursionElimination::EliminateTailCall(TailCallInfo& info) {
    // 1. Add incoming operands for all arguments.
    CopyArguments(info.TailCall);

    // 2. Create code that updates the accumulators,
    //    and add incoming operand for them (if it's the case).
    if(NeedsAccumulator(info)) {
        // Make sure that the accumulators exist.
        CreateAccumulator(additiveAccumulator_, true);
        CreateAccumulator(multiplicativeAccumulator_, false);
        GenerateAccumulatorUpdate(info);
    }

    // 3. Remove the dead instructions (includes the 'call' and 'ret').
    auto callBlock = info.TailCall->ParentBlock();
    RemoveDeadInstructions(info);

    // 4. Connect the parent block with the old entry block
    //    using a 'goto' instruction.
    auto& refs = funct_->ParentUnit()->References();
    auto oldEntryRef = refs.GetBlockRef(oldEntryBlock_);
    GotoInstr::GetGoto(oldEntryRef, callBlock);
    CallEliminated(callBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RecursionElimination::RemoveDeadInstructions(TailCallInfo& info) {
    // Remove all instructions that start with the 'call'
    // until the end of the block.
    Instruction* instr = info.TailCall;

    do {
        auto nextInstr = instr->NextInstruction();
        instr->RemoveFromBlock(true /* free */);
        instr = nextInstr;
    } while(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RecursionElimination::GenerateAccumulatorUpdate(TailCallInfo& info) {
    DebugValidator::IsTrue(info.Additive.Count() > 0 ||
                           info.Multiplicative.Count() > 0);
    
    auto parentBlock = info.TailCall->ParentBlock();
    auto& refs = funct_->ParentUnit()->References();
    auto blockRef = refs.GetBlockRef(parentBlock);
    
    // The additive accumulator is computed as follows:
    // (a + b + ...) + f(...)
    // aAccum += (a + b + ...) * mAccum
    if(info.Additive.Count() > 0) {
        auto sumOp = info.Additive[0];
        bool isFloating = sumOp->IsFloating();

        for(int i = 1; i < info.Additive.Count(); i++) {
            sumOp = CreateAdd(sumOp, info.Additive[i], info.TailCall);
        }

        // Multiply the sum with the multiplicative accumulator,
        auto mulAccumOp = multiplicativeAccumulator_->ResultOp();
        auto mulOp = CreateMul(sumOp, mulAccumOp, info.TailCall);

        // Add the whole result to the existing additive accumulator.
        auto addAccumOp = additiveAccumulator_->ResultOp();
        auto addOp = CreateAdd(addAccumOp, mulOp, info.TailCall);
        additiveAccumulator_->AddOperand(addOp, blockRef);
    }

    // The multiplicative accumulator is computed as follows:
    // (a * b * ...) * f(...)
    // mAccum = (a * b * ...) * mAccum
    if(info.Multiplicative.Count() > 0) {
        auto parentBlock = info.TailCall->ParentBlock();
        auto productOp = info.Multiplicative[0];
        bool isFloating = productOp->IsFloating();

        for(int i = 1; i < info.Multiplicative.Count(); i++) {
            productOp = CreateMul(productOp, info.Multiplicative[i], 
                                  info.TailCall);
        }

        // Multiply the product with the multiplicative accumulator,
        auto mulAccumOp = multiplicativeAccumulator_->ResultOp();
        auto mulOp = CreateMul(productOp, mulAccumOp, info.TailCall);
        multiplicativeAccumulator_->AddOperand(mulOp, blockRef);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RecursionElimination::ReplaceParameters() {
    // As it is now, the function still uses the parameters
    // in SSA form directly, but now it should use
    // the 'phi' instructions from the old entry block.
    // We use a dictionary to map from parameter to 'phi' result.
    Dictionary<Parameter*, Operand*> paramToPhi;
    DebugValidator::AreEqual(funct_->ParameterCount(),
                             parameterPhis_.Count());

    for(int i = 0; i < funct_->ParameterCount(); i++) {
        paramToPhi.Add(funct_->GetParameter(i),
                       parameterPhis_[i]->ResultOp());
    }

    // Scan all instructions and do the replacements.
    auto oldEntryBlock = oldEntryBlock_;

    funct_->ForEachInstruction([&paramToPhi, oldEntryBlock]
                               (Instruction* instr) -> bool {
        if(instr->IsPhi() && (instr->ParentBlock() == oldEntryBlock)) {
            // The 'phi's int the old entry block
            // shouldn't have their operands changed.
            return true;
        }

        for(int i = 0; i < instr->SourceOpCount(); i++) {
            if(auto parameter = instr->GetSourceOp(i)->As<Parameter>()) {
                DebugValidator::IsTrue(paramToPhi.ContainsKey(parameter));
                instr->ReplaceSourceOp(i, paramToPhi[parameter]);
            }
        }

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RecursionElimination::RewriteOtherReturns() {
    // If the function has no return value (it's 'void')
    // then we have nothing to do.
    if(funct_->IsVoid()) {
        return;
    }
    
    for(int i = 0; i < returnInstrs_.Count(); i++) {
        // Rewrite the return to the following form:
        // ret a -> ret (aAccum + mAccum * a)
        auto returnInstr = returnInstrs_[i];
        auto resultOp = returnInstr->ReturnedOp();
        DebugValidator::IsNotNull(resultOp);

        if(multiplicativeAccumulator_) {
            // mAccum * a
            auto mulOp = Temporary::GetTemporary(funct_->ReturnType());
            auto mAccum = multiplicativeAccumulator_->ResultOp();
            resultOp = CreateMul(resultOp, mAccum, returnInstr);
        }

        if(additiveAccumulator_) {
            auto addOp = Temporary::GetTemporary(funct_->ReturnType());
            auto aAccum = additiveAccumulator_->ResultOp();
            resultOp = CreateAdd(resultOp, aAccum, returnInstr);
        }

        returnInstr->SetReturnedOp(resultOp);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RecursionElimination::CopyArguments(CallInstr* callInstr) {
    DebugValidator::AreEqual(callInstr->ArgumentCount(), 
                             parameterPhis_.Count());

    // Add the arguments to the corresponding 'phi' instructions
    // that "collect" the values from the tail calls.
    auto& refs = funct_->ParentUnit()->References();
    auto blockRef = refs.GetBlockRef(callInstr->ParentBlock());

    for(int i = 0; i < callInstr->ArgumentCount(); i++) {
        parameterPhis_[i]->AddOperand(callInstr->GetArgument(i),
                                      blockRef);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RecursionElimination::CreateParameterPhis() {
    DebugValidator::IsNotNull(oldEntryBlock_);
    DebugValidator::IsNotNull(newEntryBlock_);

    // Create a 'phi' instruction for each argument of the function,
    // in the order in which they appear. These 'phi's are used
    // to "collect" the values that come from tail calls.
    int capacity = tailCalls_.Count() + 1;
    auto& refs = funct_->ParentUnit()->References();
    auto newEntryRef = refs.GetBlockRef(newEntryBlock_);

    for(int i = 0; i < funct_->ParameterCount(); i++) {
        auto resultOp = Temporary::GetTemporary(funct_->ParameterTypes()[i]);
        auto phiInstr = PhiInstr::GetPhi(resultOp, capacity);
        oldEntryBlock_->InsertInstructionFirst(phiInstr);
        parameterPhis_.Add(phiInstr);

        // Add as the first incoming operand the parameter
        // used on the first iteration.
        phiInstr->AddOperand(funct_->GetParameter(i), newEntryRef);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RecursionElimination::CreateAccumulator(PhiInstr*& accumulator, 
                                             bool isAdditive) {
    DebugValidator::IsFalse(funct_->IsVoid());

    // Create a 'phi' that acts as the accumulator
    // (if it wasn't created yet).
    if(accumulator == nullptr) {
        auto resultOp = Temporary::GetTemporary(funct_->ReturnType());
        accumulator = PhiInstr::GetPhi(resultOp);
        oldEntryBlock_->InsertInstructionFirst(accumulator);

        // The additive accumulator is initialized with 0,
        // while the multiplicative one with 1.
        auto& consts = funct_->ParentUnit()->Constants();
        auto initConst = isAdditive ? consts.GetInt(funct_->ReturnType(), 0) :
                                      consts.GetInt(funct_->ReturnType(), 1);
        auto& refs = funct_->ParentUnit()->References();
        auto newEntryRef = refs.GetBlockRef(newEntryBlock_);
        accumulator->AddOperand(initConst, newEntryRef);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* RecursionElimination::CreateNewEntryBlock() {
    // Create a new entry block for the function.
    string name = BlockUtils::CreateUniqueName("#treEntry", funct_->Symbols());
    auto previousEntryBlock = funct_->FirstBlock();
    auto block = Block::GetBlock(name);
    funct_->InsertFirstBlock(block);

    // Link the block to the previous entry block.
    auto& references = funct_->ParentUnit()->References();
    auto previousEntryRef = references.GetBlockRef(previousEntryBlock);
    GotoInstr::GetGoto(previousEntryRef, block);
    return block;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* RecursionElimination::CreateAdd(Operand* opA, Operand* opB, 
                                         Instruction* beforeInstr) {
    auto parentBlock = beforeInstr->ParentBlock();
    auto addOp = Temporary::GetTemporary(opA->GetType());
    Instruction* addInstr;

    if(addOp->IsFloating() == false) {
        addInstr = AddInstr::GetAdd(opA, opB, addOp);
    }
    else addInstr = FaddInstr::GetFadd(opA, opB, addOp);

    parentBlock->InsertInstructionBefore(addInstr, beforeInstr);
    return addOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* RecursionElimination::CreateMul(Operand* opA, Operand* opB, 
                                         Instruction* beforeInstr) {
    auto parentBlock = beforeInstr->ParentBlock();
    auto mulOp = Temporary::GetTemporary(opA->GetType());
    Instruction* addInstr;

    if(mulOp->IsFloating() == false) {
        addInstr = MulInstr::GetMul(opA, opB, mulOp);
    }
    else addInstr = FmulInstr::GetFmul(opA, opB, mulOp);

    parentBlock->InsertInstructionBefore(addInstr, beforeInstr);
    return mulOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void RecursionElimination::CallEliminated(Block* block) {
#if 1
	auto function = block->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	Log::Warning("Tail call eliminated in " + functionName + ":" + blockName);
#endif
}

} // namespace Optimization