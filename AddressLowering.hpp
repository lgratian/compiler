// AddressLowering.hpp
// Copyright (c) Lup Gratian
//
// Lowers instructions that compute an address to their actual implementation.
// This applies to 'addr', 'index' and 'elem' instructions.
// Lowering allows performing subexpression elimination, loop-invariant code motion
// and strength reduction on address instruction too.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_ADDRESS_LOWERING_HPP
#define PC_OPTIMIZATION_ADDRESS_LOWERING_HPP  

//! TODO: This should take into consideration the address modes
//! of the target architecture!!!

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Analysis/TypeInfo.hpp"
#include "../Base/String.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace Target;
using namespace CompilationPass;

namespace Optimization {

class AddressLowering : public Pass {
private:
    IRGenerator irGen_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Computes the address obtained by incrementing the base by
    // 'indexOp * size' bytes.
    // t1 = ptop baseOp, int8*
    // t2 = mul indexOp, size
    // t3 = addr t1, t2
    Operand* ComputeAddress(Operand* baseOp, Operand* indexOp, __int64 size,
                            Instruction* instr);

    // Returns the operand cast to the specified pointer type.
    Operand* CastToType(Operand* op, const Type* type, Instruction* instr);

    __int64 Size(const Type* type) {
        return Analysis::TypeInfo::GetSize(type, GetTarget());
    }

    // Methods for lowering the instructions.
    Instruction* LowerAddress(AddressInstr* instr);
    Instruction* LowerIndex(IndexInstr* instr);
    Instruction* LowerElement(ElementInstr* instr);

public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif