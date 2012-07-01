// SimpleConstantPropagation.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'SimpleConstantPropagation' pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "SimpleConstantPropagation.hpp"
#include "../Analysis/OperandInfo.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

void SimpleConstantPropagation::Execute(Function* function) {
    InitializeWorklist(function);
    FoldAndPropagate(function);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SimpleConstantPropagation::InitializeWorklist(Function* function) {
    auto instrEnum = function->GetInstructionEnum();

    // Add all the available instructions to the worklist.
    while(instrEnum.IsValid()) {
        auto instr = instrEnum.Next();
		
		// Add the instruction only if it's not dead. Dead instructions 
        // will be removed later by the dead-code elimination pass.
		if(GetSafetyInfo()->IsDefinitelyDead(instr) == false) {
			worklist_.Add(instr);
            inWorklist_.Add(instr, true);
		}
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SimpleConstantPropagation::FoldAndPropagate(Function* function) {
    IRGenerator irGen(function->ParentUnit());
    ConstantFolder folder(&irGen, GetTarget());

    Analysis::OperandInfo info(function->ParentUnit());

    while(worklist_.IsNotEmpty()) {
        auto instr = worklist_.RemoveLast();
        inWorklist_.Remove(instr);

        // Try to constant-fold the instruction.
        Operand* result = folder.Fold(instr);
        
        if(result) {
            // The instruction could be folded, now we need to propagate
            // the constant to all the instruction that use this instruction.
            PropagateToUsers(instr->GetDestinationOp(), result);

            // Delete the instruction, it's no longer needed.
            instr->RemoveFromBlock(true /* free */);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SimpleConstantPropagation::PropagateToUsers(Temporary* temp, Operand* result) {
	// If the result is not used anywhere we don't need to propagate the result.
	// The function will eventually be removed by a subsequent cleaning pass.
	if(temp == nullptr) {
        return;
    }
	
    // Add all the users to the worklist, because they may be
    // candidates for folding now.
    auto userEnum = temp->GetUserEnumerator();

    while(userEnum.IsValid()) {
        Instruction* user = userEnum.Next();
        worklist_.Add(user);
        inWorklist_.Add(user, true);
    }

    // Replace the previous temporary by the constant in all the users.
    temp->ReplaceWith(result);
}

} // namespace Optimization