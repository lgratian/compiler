#ifndef PC_ANALYSIS_PROFILE_INFO_HPP
#define PC_ANALYSIS_PROFILE_INFO_HPP  

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
using namespace IR;

namespace Analysis {

class ProfileInfo {
public:
    static const int NO_INFO = -1;

    int GetExecutionFrequency(Block* block) {
        return NO_INFO;
    }

    // 0/100   -1 unknown
    int GetExecutionFrequency(Block* fromBlock, Block* toBlock) {
        //return 98;
        return NO_INFO;
    }

    float GetNormalizedExecutionFrequency(Block* fromBlock, Block* toBlock) {
        int edgeFrequency = GetExecutionFrequency(fromBlock, toBlock);
        int blockFrequency = GetExecutionFrequency(toBlock);
        return (float)edgeFrequency / (float)blockFrequency;
    }

    int GetDynamicExecutionFrequency(Instruction* instr) {
        return NO_INFO;
    }
};

} // namespace Analysis
#endif