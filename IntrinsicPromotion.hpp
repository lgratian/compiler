// IntrinsicPromotion.hpp
// Copyright (c) Lup Gratian
//
// Promotes a function call to an intrinsic, if possible.
// This works with standard library functions only.
// A dead-code elimination pass should be executed after this one.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_INRINSIC_PROMOTION_HPP
#define PC_OPTIMIZATION_INRINSIC_PROMOTION_HPP

#include "../IR/Instructions.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/Temporary.hpp"
#include "../IR/References.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Analysis/StdLibRecognizer.hpp"
#include "../Analysis/PatternMatching.hpp"
#include "../Analysis/OperandInfo.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Log.hpp"
using namespace IR;
using namespace Analysis;

/*
/* Recognize the following case, assuming d <= u:

	 if (a <= u)
	   b = MAX (a, d);
	 x = PHI <b, u>

	 This is equivalent to

	 b = MAX (a, d);
	 x = MIN (b, u);  *
 
*/

namespace Optimization {

class IntrinsicPromotion {
private:
    // Tries to promote a 'memmove' call to a 'moveMemory' intrinsic.
    void PromoteMemmove(CallInstr* callInstr, Unit* unit, IRGenerator* irGen);

    // Tries to promote a 'memcpy' call to a 'copyMemory' intrinsic.
    void PromoteMemcpy(CallInstr* callInstr, Unit* unit, IRGenerator* irGen);
    
    // Tries to promote a 'strcpy' or 'strncpy' call to a 'copyMemory' intrinsic.
    void TryToPromoteStrcpy(CallInstr* callInstr, Unit* unit, IRGenerator* irGen,
                            bool isStrncpy = false);

    // Tries to promote a 'quest' instruction to abs/min/max.
    void PromoteQuestion(QuestionInstr* instr);

    // Methods for detecting and creating abs/min/max intrinsics.
    Operand* DetectAbs(QuestionInstr* instr);
    Operand* CreateAbs(Operand* op, bool isSigned, Unit* unit);

    Operand* DetectMin(QuestionInstr* instr);
    Operand* CreateMin(Operand* opA, Operand* opB, bool isSigned, Unit* unit);

    Operand* DetectMax(QuestionInstr* instr);
    Operand* CreateMax(Operand* opA, Operand* opB, bool isSigned, Unit* unit);

    // Creates a 'call' instruction that targets the specified intrinsic.
    CallInstr* CreateIntrinsicCall(Intrinsic* intrinsic, Operand* resultOp);

    void IntrinsicDetected(Instruction* instr, const string& name);

public:
    void Execute(Block* block);

    void Execute(Function* function);
};

} // namespace Optimization
#endif

