// LatticePropagator.cpp
// Copyright (c) Lup Gratian
//
// Implements the region specialization algorithm part of the SCCP pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConditionalConstantPropagation.hpp"
#include "../IR/IRPrinter.hpp"
#include <fstream>

namespace Optimization {

const float ConstantPropagation::MIN_REGION_SCORE = 0.25;
const float ConstantPropagation::MIN_OPERAND_PROBABILITY = 0.7;
const float ConstantPropagation::PROBABLE_OPERAND_PENALTY = 0.1;

void ConstantPropagation::CloneConstantRegions(RegionList& selectedRegions) {
    // Each selected region is cloned and the 'phi instructions
    // in the region entry blocks (the original and the cloned ones)
    // are adjusted to reflect the new control flow.
    for(int i = 0; i < selectedRegions.Count(); i++) {
        auto region = selectedRegions[i];

        // Check if a test block needs to be inserted before the region.
        // The test block checks if the incoming operands have the
        // expected (probable) values, and if they do, jumps to the
        // region clone, otherwise to the original region.
        Block* testBlock = region->ProbableOperands().Count() > 0 ?
                           CreateTestBlock(region) : nullptr;

        // The region is cloned either in the original function,
        // or in the cloned parent region, if it's the case.
        BlockList clonedBlocks;
        auto regionEntry = CloneBlocks(region, clonedBlocks);
        auto& blockMap = region->Cloner()->GetBlockMap();
        auto clonedRegionEntry = blockMap[regionEntry];
        auto specializedPred = GetSpecializedPredecessor(region);

        // Redirect the selected edge to the region clone.
        // If we have a test block the redirection is more complicated.
        if(testBlock == nullptr) {
            RedirectRegionEntry(regionEntry, clonedRegionEntry, specializedPred);
        }
        else {
            RedirectRegionEntryWithTestBlock(regionEntry, clonedRegionEntry,
                                             specializedPred, testBlock);
        }

        // Each cloned block must be processed by the SCCP algorithm,
        // so we add them to the block worklist.
        propagator_.AddBlockToWorklist(regionEntry);
        propagator_.AddBlockToWorklist(specializedPred);
        propagator_.AddBlockToWorklist(clonedRegionEntry, specializedPred);

        for(int i = 1; i < region->BlockCount(); i++) {
            propagator_.AddBlockToWorklist(clonedBlocks[i]);
            propagator_.AddBlockToWorklist(blockMap[clonedBlocks[i]]);
        }

        // If we have a test block we add it to the worklist too,
        // and also replace the (unknown) incoming operands 
        // with their probable constant values.
        if(testBlock) {
            propagator_.AddBlockToWorklist(testBlock, specializedPred);
            auto& probableOps = region->ProbableOperands();

            clonedRegionEntry->ForEachPhiInstruction([region, &probableOps]
                                                     (PhiInstr* instr) -> bool {
                for(int i = 0; i < probableOps.Count(); i++) {
                    if(probableOps[i].FirstOperand == instr->GetOperand(0)) {
                        instr->ReplaceOperand(i, probableOps[i].SecondOperand);
                        break;
                    }
                }
                return true;
            });
        }
    }

    IRPrinter printer(funct_);
    std::wofstream f("d:\\test\\out_opt2.irl");
    f<<printer.ToString().Chars();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::RedirectRegionEntry(Block* regionEntry, 
                                              Block* clonedRegionEntry,
                                              Block* specializedPredecessor) {
    // The 'phi' operand that is incoming from the selected
    // predecessor is removed because control flows from it
    // to the cloned region.
    regionEntry->ForEachPhiInstruction([specializedPredecessor]
                                       (PhiInstr* instr) -> bool {
        instr->RemoveOperand(specializedPredecessor);
        return true;
    });

    // Any 'phi' operand that is not incoming from the selected
    // predecessor is removed.
    clonedRegionEntry->ForEachPhiInstruction([specializedPredecessor]
                                             (PhiInstr* instr) -> bool {
        for(int i = 0; i < instr->OperandCount(); i++) {
            if(instr->GetOperandBlock(i)->Target() != specializedPredecessor) {
                instr->RemoveOperand(i);
                i--;
            }
        }
        return true;
    });

    // The selected predecessor must jump to the entry block
    // of the cloned region instead of the original successor.
    specializedPredecessor->ReplaceSuccessor(regionEntry, clonedRegionEntry);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::RedirectRegionEntryWithTestBlock(Block* regionEntry, 
                                                           Block* clonedRegionEntry,
                                                           Block* specializedPredecessor, 
                                                           Block* testBlock) {
    // The 'phi' operand that is incoming from the selected
    // predecessor is now incoming from the test block.
    auto& refs = funct_->ParentUnit()->References();
    auto testBlockRef = refs.GetBlockRef(testBlock);

    regionEntry->ForEachPhiInstruction([specializedPredecessor, testBlockRef]
                                       (PhiInstr* instr) -> bool {
        for(int i = 0; i < instr->OperandCount(); i++) {
            if(instr->GetOperandBlock(i)->Target() == specializedPredecessor) {
                instr->ReplaceOperandBlock(i, testBlockRef);
                break;
            }
        }
        return true;
    });

    // Any 'phi' operand that is not incoming from the selected
    // predecessor is removed. For the one that is, we replace
    // the associated block with the test block.
    clonedRegionEntry->ForEachPhiInstruction([specializedPredecessor, testBlockRef]
                                             (PhiInstr* instr) -> bool {
        for(int i = 0; i < instr->OperandCount(); i++) {
            if(instr->GetOperandBlock(i)->Target() != specializedPredecessor) {
                instr->RemoveOperand(i);
                i--;
            }
            else {
                instr->ReplaceOperandBlock(i, testBlockRef);
            }
        }
        return true;
    });

    // The selected predecessor must jump to the test block.
    specializedPredecessor->ReplaceSuccessor(regionEntry, testBlock);

    // If all incoming values match the probable ones control flows
    // from the test block to the entry block of the cloned region,
    // otherwise to the entry block of the original region.
    auto conditionOp = testBlock->LastInstruction()->GetDestinationOp();
    auto clonedRegionEntryRef = refs.GetBlockRef(clonedRegionEntry);
    auto regionEntryRef = refs.GetBlockRef(regionEntry);
    IfInstr::GetIf(conditionOp, clonedRegionEntryRef, regionEntryRef, testBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* ConstantPropagation::CloneBlocks(ConstantRegion* region, 
                                        BlockList& clonedBlocks) {
    Block* regionEntry;
    shared<BlockCloner> cloner = new BlockCloner();

    // If the is a sub-region (part of a region tree, for example)
    // and the parent region was also cloned we clone this region
    // inside its cloned parent region. Otherwise we clone it
    // inside the original function at the appropriate place.
    if(region->HasParent() && region->Parent()->IsSelected()) {
        DebugValidator::IsNotNull(region->Parent()->Cloner());
        
        // We need to known which block is the entry in the parent clone.
        auto& parentBlockMap = region->Parent()->Cloner()->GetBlockMap();
        regionEntry = parentBlockMap[region->GetBlock(0)];

        // Make a list with the blocks we need to clone
        // from the already cloned parent region.
        List<Block*> blocksToClone(region->BlockCount());

        for(int i = 0; i < region->BlockCount(); i++) {
            blocksToClone.Add(parentBlockMap[region->GetBlock(i)]);
            clonedBlocks.Add(region->GetBlock(i));
        }

        cloner->CloneBlocks(blocksToClone, funct_, regionEntry);
    }
    else {
        regionEntry = region->GetBlock(0);
        clonedBlocks.AddRange(region->Blocks());
        cloner->CloneBlocks(region->Blocks(), funct_, regionEntry);
    }

    // The cloner is needed when cloning sub-regions.
    region->SetCloner(cloner);
    return regionEntry;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* ConstantPropagation::GetSpecializedPredecessor(ConstantRegion* region) {
    // This returns the block from which the control flows 
    // to the specialized region. If the regions is a sub-region,
    // the block might be part of the cloned parent region.
    if(region->HasParent() && region->Parent()->IsSelected()) {
        auto& parentBlockMap = region->Parent()->Cloner()->GetBlockMap();

        return parentBlockMap.ContainsKey(region->SpecializedPredecessor()) ?
               parentBlockMap[region->SpecializedPredecessor()] :
               region->SpecializedPredecessor();
    }
    else return region->SpecializedPredecessor();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* ConstantPropagation::CreateTestBlock(ConstantRegion* region) {
    // Create an unique name for the block.
    string name;
    int ct = 0;

    do {
        name = string::Format(L"#region_test%d", ct);
        ct++;
    } while(funct_->Symbols().Contains(&name));

    // Create the block and insert it before the first block
    // in the region (it will be execute before).
    Block* block = Block::GetBlock(name);
    funct_->InsertBlockBefore(block, region->GetBlock(0));

    // Create the instructions that compare the incoming operands
    // with their most probable constant values. If all operands
    // have these values then we jump to the specialized region.
    auto& probableOps = region->ProbableOperands();
    Operand* result = nullptr;

    for(int i = 0; i < probableOps.Count(); i++) {
        // Create the comparison.
        auto testedOp = probableOps[i].FirstOperand;
        auto probableValue = probableOps[i].SecondOperand;
        auto cmpResult = Temporary::GetTemporary(IntegerType::GetInt32());
        CmpInstr::GetCmp(Order_Equal, testedOp, probableValue, 
                         cmpResult, block);

        // If there are more operands to be tested the comparison
        // results need to be combined using an 'and'.
        if(result) {
            auto andResult = Temporary::GetTemporary(IntegerType::GetInt32());
            AndInstr::GetAnd(result, cmpResult, andResult, block);
            result = andResult;
        }
        else result = cmpResult;
    }

    return block;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ConstantPropagation::FindConstantRegions() {
    // Here we try to determine the regions where specialization opportunities
    // arise after the region is cloned (may include its subregions too).
    // The blocks are processed in reverse-postorder. In this way
    // we guarantee that the nesting relationship of the regions
    // is properly discovered.
    CFGInfo<Block, Function> cfgInfo(funct_->FirstBlock(), false /* edgeInfoNeeded */);
    auto& postorderList = cfgInfo.PostorderList();

    for(int i = postorderList.Count() - 1; i >= 0; i--) {
        auto block = const_cast<Block*>(postorderList[i]);

        // We're looking for 'phi' instructions that have a constant
        // incoming operand from an edge with high execution probability.
        // Single incoming operand 'phi's are ignored.
        if(block->HasSinglePredecessor() ||
           block->IsFunctionEntry()) continue;

        // A potential region begins in a block that starts
        // with a 'phi' instruction with constant operands.
        if(block->HasPhi() == false) continue;

        // As a candidate we select the incoming edge with the greatest count.
        Block* maxIncomingBlock = FindMaximumIncomingBlock(block);

        // Check if we have at least one incoming constant 
        // from the most taken edge. If we have none there is really
        // nothing that can be specialized in the cloned blocks.
        OperandList replacementOps;
        ProbableOperandList probableOps;
        bool isCandidate = false;
        int alwaysCount = 0;
        int withParentRegionCount = 0;

        if(HasIncomingConstantPhi(block, maxIncomingBlock, alwaysCount, 
                                  withParentRegionCount, replacementOps,
                                  probableOps) == false) {
            // There is no benefit in specializing the region.
            continue;
        }

        // Check if it might be profitable to optimize the region.
        if(ShouldOptimizeRegion(block, probableOps) == false) {
            continue;
        }

        // Create the region and estimate the profitability.
        auto region = CreateRegion(block, alwaysCount, withParentRegionCount);
        region->SetSpecializedPredecessor(maxIncomingBlock);
        region->ReplacementOperands().AddRange(replacementOps);
        region->ProbableOperands().AddRange(probableOps);

        ComputeRegionBenefit(region);
        ComputeRegionCost(region);
        AddRegion(region);
    }

    return regions_.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantPropagation::ShouldOptimizeRegion(Block* regionEntryBlock, 
                                               ProbableOperandList& probableOps) {
    // If the specialized region would depend on the value
    // of many probable values it probably doesn't pay off.
    if(probableOps.Count() > MAX_PROBABLE_OPERANDS) {
        return false;
    }

    // If the region contains a large number of blocks we skip it
    // because the probability to have an improvement is very small.
    if(ComputeRegionSize(regionEntryBlock) > MAX_REGION_BLOCK_COUNT) {
        return false;
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* ConstantPropagation::FindMaximumIncomingBlock(Block* block) {
    Block* maxIncomingBlock = nullptr;

    int maxEdgeCount = -1;
    int previousMaxEdgeCount = -1;
    auto& predecessorEnum = block->GetPredecessorEnum();

    while(predecessorEnum.IsValid()) {
        auto predecessorBlock = predecessorEnum.Next();
        int edgeCount = GetEdgeCount(predecessorBlock, block);

        if(edgeCount > maxEdgeCount) {
            // Found an edge that is more frequently taken.
            maxIncomingBlock = predecessorBlock;
            previousMaxEdgeCount = maxEdgeCount;
            maxEdgeCount = edgeCount;
        }
    }

    return maxIncomingBlock;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::SelectConstantRegions(RegionList& selectedRegions) {
    // We need to select a subset of the regions that lead to the maximum 
    // benefit,  while avoiding to increase the size of the function too much
    // (note that this is similar to the Knapsack problem).
    // We approximate this by sorting based on benefit/cost (the "score"), 
    // then taking the first regions until the sum of the cloned instructions 
    // is greater than the clone limit.
    SortedRegionList candidateList;
    int maximumCost = GetCloneLimit();
    int actualCost = 0;
    int position = 0;

    // Select the region and region trees we should consider,
    // then sort them decreasingly based on their score.
    SelectCandidateRegions(candidateList);
    SortCandidateRegions(candidateList);

    while((actualCost < maximumCost) &&
          (position < candidateList.Count())) {
        auto regionInfo = candidateList[position++];
        
        // Skip the region if it was already selected 
        // (as part of a previous region tree).
        if(regionInfo.Region->IsSelected()) {
            continue;
        }

        // For region trees we try harder to prove that their cost
        // is justified, because the benefits of specializing whole trees
        // are usually larger than specializing each region separately.
        int newCost = actualCost + regionInfo.Cost;
        bool selected = (newCost <= maximumCost) && 
                        (regionInfo.Score >= MIN_REGION_SCORE);

        if(selected == false) {
            // If the cost difference is not too large we do
            // a better estimate of the benefit/cost ratio.
            selected = AnalyzeRegionTree(regionInfo, actualCost, 
                                         maximumCost, newCost);
        }

        if(selected) {
            // Specializing the entire region tree seems profitable.
            MarkRegionAsSelected(regionInfo, selectedRegions);
            actualCost = newCost;
        }
    }

#if 1
    //! REMOVE THIS
    selectedRegions.ForEach([selectedRegions](ConstantRegion* region) -> bool {
        Log::Info(string::Format(L"Selected region %X with const_diff %f", 
                                 region, region->ConstantDifference()));
        return true;
    });
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::MarkRegionAsSelected(RegionSortData& regionInfo,
                                               RegionList& selectedRegions) {
    if(regionInfo.IsTree) {
        SelectRegionTree(regionInfo.Region, selectedRegions);
    }
    else {
        selectedRegions.Add(regionInfo.Region);
        regionInfo.Region->SetIsSelected(true);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantPropagation::AnalyzeRegionTree(RegionSortData& regionInfo, 
                                            int actualCost, int maximumCost,
                                            int& newCost) {
    // We have a region tree and the benefit/cost ratio isn't high enough
    // to justify its specialization; in this case we try a more precise
    // constant approximation algorithm that uses a second instance
    // of the ConstantPropagation class and also determines
    // the blocks that are reachable in the tree (the instructions in
    // unreachable blocks are not taken into consideration anymore because
    // they will be deleted afterwards anyway).
    auto evaluator = CreateRegionEvaluator(regionInfo.Region, regionInfo.IsTree);
    evaluator->IterateToFixedpoint();

    // Estimate the blocks that are reachable.
    SparseBitVector reachableBlocks;
    AnalyzeReachableBlocks(funct_->FirstBlock(), evaluator->Lattice(),
                           reachableBlocks);

    // Count how many instructions need to be cloned and how many
    // of them will be replaced by constants.
    int instructionCount = 0;
    int constInstructionCount = 0;
    int unreachableInstructionCount = 0;

    RecomputeInstructionCounts(regionInfo.Region, evaluator->Lattice(), 
                               reachableBlocks, instructionCount, constInstructionCount, 
                               unreachableInstructionCount, regionInfo.IsTree);

    // Recompute the region score only if necessary.
    float score = regionInfo.Score;

    if(score < MIN_REGION_SCORE) {
        score = ComputeRegionScore(regionInfo.Region, true, 
                                   -unreachableInstructionCount);
    }

    // Compute the new specialization cost.
    newCost = instructionCount - constInstructionCount;
    return ((newCost + actualCost) <= maximumCost) &&
           (score >= MIN_REGION_SCORE);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::RecomputeInstructionCounts(ConstantRegion* region,
                                                     ConstantLattice* lattice, 
                                                     SparseBitVector& reachableBlocks,
                                                     int& instructionCount,
                                                     int& constInstructionCount,
                                                     int& unreachableInstructionCount,
                                                     bool includeSubregions) {
    // Compute the number of instructions that need to be cloned
    // and how many of these will be replaced by constants.
    // If the constants that are incoming in the region depend
    // on the fact that the parent region is cloned then we don't count
    // any constants in the region, unless we're certain that they exist.
    int constInstructionCountStart = constInstructionCount;
    bool ignoreConstants = region->HasParent() &&
                           region->AlwaysIncomingConstantCount() == 0;

    // Recount how many folded instructions we have based on their type.
    region->ResetFoldedInstructionCounts();

    for(int i = 0; i < region->BlockCount(); i++) {
        auto block = region->GetBlock(i);
        instructionCount += block->InstructionCount();

        if(reachableBlocks.IsNotSet(block->Id())) {
            // The block will not be reachable, skip it,
            // but remember how many instructions will be dead.
            unreachableInstructionCount += block->InstructionCount();
            continue;
        }

        // Check which instructions will be replaced by constants.
        for(auto instr = block->FirstInstruction(); instr; 
            instr = instr->NextInstruction()) {
            // Consider only instructions that compute a value.
            if(instr->HasDestinationOp()) {
                if(IsConstantAfterSpecialization(ignoreConstants, lattice, 
                                                 instr->GetDestinationOp())) {
                    // The instruction will definitely be replaced by a constant.
                    constInstructionCount++;
                    region->CountFoldedInstruction(instr);
                }
            }
        }
    }

    // Update the number of constant instructions in the region
    // (used for updating the region score).
    region->SetConstantAfterInstructions(constInstructionCount - 
                                         constInstructionCountStart);

    // Recount the instructions in the child regions, if requested.
    if(includeSubregions) {
        for(int i = 0; i < region->ChildCount(); i++) {
            RecomputeInstructionCounts(region->GetChild(i), lattice, reachableBlocks,
                                       instructionCount, constInstructionCount,
                                       unreachableInstructionCount, true);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantPropagation::IsConstantAfterSpecialization(bool ignoreOriginalConstants, 
                                                        ConstantLattice* lattice, 
                                                        Operand* op) {
    // Check the original lattice first, if we're allowed.
    if((ignoreOriginalConstants == false) &&
       lattice_->GetCell(op)->IsConstant()) {
        return true;
    }

    return lattice->GetCell(op)->IsConstant();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<ConstantPropagation>
ConstantPropagation::CreateRegionEvaluator(ConstantRegion* region,
                                           bool includeSubregions) {
    // We create a clone of this pass; the lattice values that have been
    // determined until now are taken and used by the new pass, while the 'phi'
    // instructions in the region entries are replaced by the corresponding constants.
    shared<ConstantPropagation> evaluator = new ConstantPropagation(cache_);
    evaluator->Initialize(funct_);

    funct_->ForEachInstruction([&evaluator, this](Instruction* instr) -> bool {
        if(instr->HasDestinationOp()) {
            auto destOp = instr->GetDestinationOp();
            auto currentCell = lattice_->GetCell(destOp);
            evaluator->Lattice()->SetCell(destOp, currentCell);
        }
        return true;
    });

    // Now set the 'phi' instructions that become constants
    // (recursively if each subregion needs to be considered).
    evaluator->Lattice()->SetHasFixedPhiConstants(true);
    SetFixedPhiConstants(region, includeSubregions, evaluator->Lattice());
    return evaluator;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::SetFixedPhiConstants(ConstantRegion* region, 
                                               bool includeSubregions,
                                               ConstantLattice* lattice) {
    // We force the lattice to use the constant that will replace
    // the 'phi' instructions after the specialization of the region.
    DebugValidator::IsLarger(region->BlockCount(), 0);
    auto regionEntry = region->GetBlock(0);
    int position = 0;

    regionEntry->ForEachPhiInstruction([region, lattice, &position]
                                       (PhiInstr* instr) -> bool {
        if(auto destOp = instr->GetDestinationOp()) {
            auto incomingOp = region->ReplacementOperands()[position];

            if(auto constant = incomingOp->As<Constant>()) {
                // Force the replacement of this 'phi'. By updating the cell
                // we force the algorithm to re-evaluate all instructions
                // that depend on the 'phi'.
                lattice->AddFixedPhiConstant(instr, constant);
            }
        }

        position++;
        return true;
    });

    // Recourse if we fix the constant for the entire region tree.
    if(includeSubregions) {
        for(int i = 0; i < region->ChildCount(); i++) {
            SetFixedPhiConstants(region->GetChild(i), includeSubregions, lattice);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::AnalyzeReachableBlocks(Block* block, ConstantLattice* lattice,
                                                 SparseBitVector& reachableBlocks) {
    // If we already marked the block as reachable we have a cycle.
    if(reachableBlocks.IsSet(block->Id())) {
        return;
    }
    else reachableBlocks.SetBit(block->Id());

    // Visit only the blocks that the lattice determined to be reachable.
    block->ForEachSuccessor([&](Block* successor, int index) -> bool {
        if(lattice->IsEdgeExecutable(block, successor)) {
            AnalyzeReachableBlocks(successor, lattice, reachableBlocks);
        }
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::SelectRegionTree(ConstantRegion* region, 
                                           RegionList& selectedRegions) {
    region->SetIsSelected(true);
    selectedRegions.Add(region);

    for(int i = 0; i < region->ChildCount(); i++) {
        SelectRegionTree(region->GetChild(i), selectedRegions);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::SelectCandidateRegions(SortedRegionList& list) {
    // We first consider the region trees, for which we compute a score
    // (the sum of the score of each child region). Then we add each region,
    // with the condition that it doesn't depend on the parent region being cloned.
    for(int i = 0; i < regionRoots_.Count(); i++) {
        auto region = regionRoots_[i];
        float score = ComputeRegionScore(region, true);

        // We allow low scores here because in this case we want 
        // to re-estimate the constants using a more accurate algorithm.
        if(score > 0.01) {
            int remaining = region->RemainingInstructions(true);
            list.Add(RegionSortData(region, score, remaining, true));
        }
    }

    for(int i = 0; i < regions_.Count(); i++) {
        // At least one constant value should be always incoming, else
        // there is no benefit in specializing the region.
        auto region = regions_[i];
        float score = ComputeRegionScore(region, false);
        
        if((region->AlwaysIncomingConstantCount() > 0) && 
            (score > MIN_REGION_SCORE)) {
            bool inList = list.Exists([region](RegionSortData& data) -> bool {
                              return data.Region == region;
                          });
            if(inList == false) {
                int remaining = region->RemainingInstructions(true);
                list.Add(RegionSortData(region, score, remaining, false));
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::SortCandidateRegions(SortedRegionList& list) {
    // The list usually has only a few items, so selection sort is fast enough.
    // We sort in descending order based on the score of the region(tree).
    for(int i = 0; i < list.Count(); i++) {
        for(int j = i + 1; j < list.Count(); j++) {
            // A region tree has higher priority than one of its child regions.
            bool iChildOfj = (list[j].Region == list[i].Region) || 
                              list[j].Region->HasChild(list[i].Region);
            bool moveTreeToFront = (list[i].IsTree == false) && 
                                    list[j].IsTree && iChildOfj;

            bool jChildOfi = (list[i].Region == list[j].Region) || 
                              list[i].Region->HasChild(list[j].Region);
            bool moveRegionToFront = (list[i].Score < list[j].Score) &&
                                     ((jChildOfi == false) || (list[i].IsTree == false));

            if(moveTreeToFront || moveRegionToFront) {
                std::swap(list[i], list[j]);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::ComputeRegionBenefit(ConstantRegion* region) {
    // The benefit of replacing all 'phi' instructions in the entry block 
    // of the regions is estimated by counting the number of instructions 
    // that will become constant after this operation. We maintain a map 
    // of the operands that indicates if they are constant or not.
    IRGenerator irGen(funct_->ParentUnit());
    ConstantFolder folder(&irGen, GetTarget());
    auto entryBlock = region->GetBlock(0);
    int constantInstrs = 0;
    int constantAfterInstrs = 0;
    int phiCount = 0;

    entryBlock->ForEachPhiInstruction([&constantAfterInstrs, &phiCount, 
                                       region, this](PhiInstr* instr) -> bool {
        // Consider only instructions that compute a value.
        if(instr->HasDestinationOp()) {
            auto incomingOp = region->ReplacementOperands()[phiCount++];
            auto cell = lattice_->GetCell(incomingOp);

            if(auto constant = cell->AsConstant()) {
                region->ConstantOperands().Add(instr->GetDestinationOp(), constant);
            }
        }
        return true;
    });

    // Scan all instructions and estimate which will become constants
    // in each block. A fast and simple algorithm is used, but it's
    // accurate enough in most cases.
    for(int i = 0; i < region->BlockCount(); i++) {
        EstimateConstants(region->GetBlock(i), region->ConstantOperands(),
                          constantInstrs, constantAfterInstrs, folder, region);
    }

    // Update the information about the region.
    region->SetConstantInstructions(constantInstrs);
    region->SetConstantAfterInstructions(constantAfterInstrs);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::EstimateConstants(Block* block, ConstantMap& constantMap,
                                            int& constantInstrs, int& constantAfterInstrs,
                                            ConstantFolder& folder, ConstantRegion* region) {
    // We do a rough estimate of the instructions that become constant
    // after the 'phi's are replaced by the corresponding constants.
    block->ForEachInstruction([&constantAfterInstrs, &constantInstrs,
                               &constantMap, &folder, region, this]
                               (Instruction* instr) -> bool {
        if(instr->IsPhi()) {
            return true; // Skip.
        }
        else if(instr->HasDestinationOp()) {
            auto cell = lattice_->GetCell(instr->GetDestinationOp());

            // If we already know the instruction is a constant
            // we don't get any extra benefit from it after specialization.
            if(cell->IsConstant()) {
                constantInstrs++;
                return true;
            }

            // Check if the instruction would turn into a constant
            // after we replace it's operands with what we have in the map.
            bool hasUndefined;
            ConstantList constantOps;
            Constant* constantResult = nullptr;

            bool isConstant = CollectConstantOperands(instr, constantMap, 
                                                      constantOps, hasUndefined);

            // Certain instructions can turn into a constant
            // even if both operands are not constants (and a, 0 -> 0).
            // If we have such a case 'constantResult' will be assigned.
            if(MightBeConstant(isConstant, instr)) {
                isConstant = InstructionIsConstant(instr, constantMap,
                                                   constantResult);
            }

            if(isConstant) {
                // The instruction is a constant. If we don't know already 
                // the constant (0, 1, -1) we compute it now if we have 
                // an arithmetic/logical//conversion/comparison instruction,
                // else we use 'undef' (it's still considered a constant).
                if(constantResult == nullptr) {
                    constantResult = GetConstantResult(instr, hasUndefined,
                                                       constantOps, folder);
                }

                constantMap.Add(instr->GetDestinationOp(), constantResult);
                region->CountFoldedInstruction(instr);
                constantAfterInstrs++;
                
                Log::Info("Folding opportunity in " + *instr->ParentBlock()->Name());
            }
        }
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantPropagation::CollectConstantOperands(Instruction* instr, 
                                                  ConstantMap& constantMap,
                                                  ConstantList& constantOps, 
                                                  bool& hasUndefined) {
    hasUndefined = false;

    for(int i = 0; i < instr->SourceOpCount(); i++) {
        auto op = instr->GetSourceOp(i);
        auto constantOp = TryGetConstant(op, constantMap);

        if(constantOp) {
            if(constantOp->IsUndefinedConstant()) {
                hasUndefined = true;
            }
            else constantOps.Add(constantOp);
        }
        else {
            // The instruction can't be a constant
            // (there are some exceptions though, like a * 0 = 0).
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantPropagation::InstructionIsConstant(Instruction* instr, 
                                                ConstantMap& constantMap,
                                                Constant*& constantResult) {
    // We apply some arithmetic/logical identities to discover more constants.
    auto leftOp = instr->GetSourceOp(0);
    auto rightOp = instr->GetSourceOp(1);

    // mul a, 0 -> 0
    // and a, 0 -> 0
    if((instr->IsMul() || instr->IsAnd()) &&
        (IsZero(leftOp, constantMap) || 
         IsZero(rightOp, constantMap))) {
        constantResult = GetZeroInt();
        return true;
    }
    // div/udiv/mod/umod/shl/shr/ushr 0, a -> 0       
    else if((instr->IsDiv() || instr->IsUdiv() ||
             instr->IsMod() || instr->IsUmod() ||
             instr->IsShl() || instr->IsShr()  || instr->IsUshr()) && 
             IsZero(leftOp, constantMap)) {
        constantResult = GetZeroInt();
        return true;
    }
    // or a, -1 -> -1
    else if(instr->IsOr() &&
            (IsMinusOne(leftOp, constantMap) ||
             IsMinusOne(rightOp, constantMap))) {
        constantResult = GetMinusOneInt();
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Constant* ConstantPropagation::GetConstantResult(Instruction* instr, 
                                                 bool hasUndefined,
                                                 ConstantList& constantOps,
                                                 ConstantFolder& folder) {
    // We evaluate the instruction because we know all operands.
    // If one of the operands is 'undef' then the result is also 'undef'.
    if(hasUndefined) {
        return GetUndefined(instr->GetDestinationOp());
    }
    else if(instr->IsArithmetic() ||
            instr->IsLogical()) {
        DebugValidator::AreEqual(constantOps.Count(), 2);
        auto result = folder.FoldBinary(instr->GetOpcode(),
                                        constantOps[0], constantOps[1]);
        DebugValidator::IsNotNull(result);
        return result->As<Constant>();
    }
    else if(auto cmpInstr = instr->As<CmpInstrBase>()) {
        DebugValidator::AreEqual(constantOps.Count(), 2);
        auto result = folder.FoldCompare(cmpInstr->GetOpcode(),
                                         constantOps[0], constantOps[1],
                                         cmpInstr->Order());
        DebugValidator::IsNotNull(result);
        return result->As<Constant>();
    }
    else if(auto convInstr = instr->As<ConversionInstr>()) {
        DebugValidator::AreEqual(constantOps.Count(), 1);
        auto result = folder.FoldConversion(convInstr->GetOpcode(),
                                            constantOps[0],
                                            convInstr->CastType());
        // Sometimes the folder is not able to generate a constant
        // even if the operands are constants (pointers, for example).
        if(result) return result->As<Constant>();
    }
    
    // 'undef' is also considered a constant, but we might miss
    // some arithmetic identities that lead to a constant.
    return GetUndefined(instr->GetDestinationOp());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::ComputeRegionCost(ConstantRegion* region) {
    // The cost of duplicating a region is the sum of the number 
    // of instructions in all blocks, from which we subtract 
    // the instructions that are known to always be constants
    // (they will be replaced at the end of the pass anyway).
    int totalInstrs = 0;
    int constantInstrs = 0;

    for(int i = 0; i < region->BlockCount(); i++) {
        auto block = region->GetBlock(i);
        totalInstrs += block->InstructionCount();

        block->ForEachInstruction([&constantInstrs, this]
                                  (Instruction* instr) -> bool {
            if(instr->HasDestinationOp()) {
                auto cell = lattice_->GetCell(instr->GetDestinationOp());
                
                if(cell->IsConstant()) {
                    constantInstrs++;
                }
            }
            return true;
        });
    }

    region->SetTotalInstructions(totalInstrs - constantInstrs);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
float ConstantPropagation::ComputeRegionScore(ConstantRegion* region,
                                              bool includeSubregions,
                                              int instrCountAdjustment) {
    // The "score" of the region depends on how much benefit we obtain
    // after specialization compared to the penalty incurred by the increase
    // in code size (this may affect the instruction cache, for example).

    // The benefit scales with the number of new constants
    // discovered, and is greater for a hot region.
    float benefit = (float)region->ConstantDifference(includeSubregions) *
                    CONSTANT_INSTRUCTION_BENEFIT;

    // Adjust the benefit based on the type of the folded instructions
    // (we want to eliminate expensive ones, such as division).
    benefit *= region->GetFoldedInstructionsWeight(includeSubregions);

    // If we need to insert a value test make the benefit smaller
    // (this happens if an incoming operand is not statically known
    //  to be a constant, but it's probable to be a specific value).
    if(region->ProbableOperands().Count() > 0) {
        float totalPenalty = region->ProbableOperands().Count() *
                             PROBABLE_OPERAND_PENALTY;
        benefit *= 1.0f - totalPenalty;
    }

    // The penalty is the number of instructions that must be cloned.
    int remainingInstrs = region->RemainingInstructions(includeSubregions) - 
                          instrCountAdjustment;
    float penality = std::max(remainingInstrs * REMAINING_INSTRUCTION_PENALITY, 1);

    // We compute the average hotness of the blocks found in the region 
    // because we don't know exactly where the new constants are found.
    float averageBlockHotness = ComputeAverageExecutionCount(region, includeSubregions);

    // The average is normalized relative to the "hottest" block in the function.
    averageBlockHotness /= GetMaximumExecutionCount();
    return (benefit / penality) * (1.0f + averageBlockHotness);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
float ConstantPropagation::ComputeAverageExecutionCount(ConstantRegion* region,
                                                        bool includeSubregions) {
    // We compute the average execution count for all the blocks in the region,
    // including all subregions, if requested. Note that the execution count 
    // of a block is taken into consideration a single time.
    SparseBitVector processedBlocks;
    int distinctBlocks = 0;
    float value = 0;

    // A worklist algorithm is used.
    List<ConstantRegion*> queue;
    queue.Add(region);

    while(queue.Count() > 0) {
        int last = queue.Count() - 1;
        auto region = queue[last];
        queue.RemoveAt(last);

        for(int i = 0; i < region->BlockCount(); i++) {
            auto block = region->GetBlock(i);

            if(processedBlocks.IsSet(block->Id()) == false) {
                // This is the first time we see the block.
                value += GetBlockExecutionCount(block);
                processedBlocks.SetBit(block->Id());
                distinctBlocks++;
            }
        }

        // If the subregions need to be processed we add them to the worklist.
        if(includeSubregions) {
            for(int i = 0; i < region->ChildCount(); i++) {
                queue.Add(region->GetChild(i));
            }
        }
    }

    DebugValidator::IsLarger(distinctBlocks, 0);
    return value / distinctBlocks;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantPropagation::HasIncomingConstantPhi(Block* block, 
                                                 Block* maxIncomingBlock,
                                                 int& alwaysCount, 
                                                 int& withParentRegionCount,
                                                 List<Operand*>& replacementOps,
                                                 List<OperandPair>& probableOps) {
    // Check how many of the operands incoming from 'maxIncomingBlock'
    // are always constant or constant when the parent region is specialized.
    auto parentRegion = GetParentRegion(block);
    auto instr = block->FirstInstruction();

    while(auto phiInstr = instr->As<PhiInstr>()) {
        auto incomingOp = phiInstr->GetOperandFromBlock(maxIncomingBlock);

        if(incomingOp->IsConstant()) {
            alwaysCount++;
        }
        else if(parentRegion && parentRegion->IsConstant(incomingOp)) {
            withParentRegionCount++;
        }
        else {
            ValueProbability probableValue;
            
            // Get the most probable value of the operand;
            // If the probability is high enough we use it instead;
            if(GetProbableValue(incomingOp, block, probableValue)) {
                DebugValidator::IsTrue(probableValue.Value->IsConstant());
                probableOps.Add(OperandPair(incomingOp, probableValue.Value));
                incomingOp = probableValue.Value;
            }
        }

        replacementOps.Add(incomingOp);
        instr = instr->NextInstruction();
    }

    // The region is selected if we found at least one constant.
    return (alwaysCount > 0)           ||
           (withParentRegionCount > 0) ||
           (probableOps.Count() > 0);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantPropagation::GetProbableValue(Operand* op, Block* testBlock, 
                                           ValueProbability& probableValue) {
    OperandInfo opInfo(funct_->ParentUnit());
    OperandInfo::ValueList valueList;

    // TODO: fix this!!!!
#if 0
    if(auto loadInstr = op->DefiningInstrAs<LoadInstr>()) {
    if(loadInstr->SourceOp()->IsVariableReference()) {  
        probableValue = ValueProbability(GetZeroInt(), Probability_Percent, 0.8f);
        return true;
    }
    }
#endif

    if(opInfo.GetProbableIntConstant(op, valueList, testBlock) > 0) {
        if(valueList[0].Probability > MIN_OPERAND_PROBABILITY) {
            probableValue = valueList[0];
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ConstantRegion* ConstantPropagation::CreateRegion(Block* startBlock,
                                                  int alwaysCount,
                                                  int withParentRegionCount) {
    DebugValidator::IsNotNull(startBlock);

    // When we create a region we check if it's found
    // inside another region (using the dominator tree).
    auto parent = GetParentRegion(startBlock);
    auto region = new ConstantRegion(parent);

    AddDominatedBlocks(domTree_->GetBlockNode(startBlock), region);
    region->SetAlwaysIncomingConstantCount(alwaysCount);
    region->SetWithParentRegionIncomingConstantCount(withParentRegionCount);
    return region;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::AddRegion(ConstantRegion* region) {
    regions_.Add(region);

    if(region->Parent()) {
        region->Parent()->AddChild(region);
    }
    else regionRoots_.Add(region);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantPropagation::AddDominatedBlocks(DominatorNode* node,
                                             ConstantRegion* region) {
    DebugValidator::IsNotNull(node);
    DebugValidator::IsNotNull(region);
    region->AddBlock(node->Block());

    for(int i = 0; i < node->Children().Count(); i++) {
        AddDominatedBlocks(node->Children()[i], region);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ConstantPropagation::ComputeRegionSize(DominatorNode* node) {
    DebugValidator::IsNotNull(node);
    int count = node->Children().Count();

    for(int i = 0; i < node->Children().Count(); i++) {
        count += ComputeRegionSize(node->Children()[i]);
    }

    return count;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ConstantRegion* ConstantPropagation::GetParentRegion(Block* block) {
    // Search for a region whose entry block dominates
    // the specified block. If we found one we look (recursively)
    // at its children to find the one that is the nearest parent.
    for(int i = 0; i < regionRoots_.Count(); i++) {
        auto parent = GetParentRegion(regionRoots_[i], block);
        if(parent) return parent;
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ConstantRegion* ConstantPropagation::GetParentRegion(ConstantRegion* rootRegion,
                                                     Block* block) {
    // Check if this region dominates the block.
    // If it does we check if we have a child region that dominates it.
    if(domTree_->Dominates(rootRegion->GetBlock(0), block) == false) {
        return nullptr;
    }

    for(int i = 0; i < rootRegion->ChildCount(); i++) {
        auto childParent = GetParentRegion(rootRegion->GetChild(i), block);
        if(childParent) return childParent;
    }

    return rootRegion;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ConstantPropagation::GetCloneLimit() {
    // The maximum number of cloned instructions depends
    // on the current size of the function.
    return std::max(MIN_CLONE_LIMIT,
                    AVERAGE_BLOCK_SIZE * funct_->BlockCount());
}

}