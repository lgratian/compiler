// SimpleConstantPropagation.hpp
// Copyright (c) Lup Gratian
//
// Implements a simple, but fast constant folding and propagation pass.
// Uses a worklist-based algorithm to propagate the changes.
// A dead-code elimination pass should be executed after this one.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_SIMPLE_CONSTANT_PROPAGATION_HPP
#define PC_OPTIMIZATION_SIMPLE_CONSTANT_PROPAGATION_HPP

#include "../IR/Instructions.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/Temporary.hpp"
#include "../IR/References.hpp"
#include "../IR/IRGenerator.hpp"
#include "../IR/Unit.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Analysis/ConstantFolder.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Log.hpp"
using namespace IR;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class SimpleConstantPropagation : public Pass {
private:
    StaticList<Instruction*, 512> worklist_;
    Dictionary<Instruction*, bool> inWorklist_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Initializes the worklist with all the instructions in the function.
    void InitializeWorklist(Function* function);

    // Tries to fold and to propagate the constants until the worklist is empty.
    void FoldAndPropagate(Function* function);

    // Propagates a folded instruction to all its users.
    void PropagateToUsers(Temporary* temp, Operand* result);

public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif