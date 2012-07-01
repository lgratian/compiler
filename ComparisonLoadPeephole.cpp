// ComparisonLoadPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements peephole optimizations for comparisons that involve
// values loaded from constant global variables.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

//? TRY TO DO THIS FOR MATRIX - if two conditions are needed, but are on the
//? same line/column we can simplify (i == 3 && (j == 0 || j == 2))
//? Maximum cost should be 9 (~cost to compute addr on a Intel Core2/Nehalem, OoO?)
// 1 2 3   if(a[i][j] == 6) -> if((i == 1) && (j == 2))
// 4 5 6   if(a[i][j] != 6) -> if((i != 1) || (j != 2))
// 7 8 9
//
// 1 2 3   if(a[i][j] == 2) -> if((i == 0 && j == 1) || (i == 1 && j == 2))
// 1 4 2   if(a[i][j] == 4) -> if((i == 1 && j == 1) || (i == 2 && j == 1)) ->
// 1 4 7                       if((j == 1) && (i == 1 || i == 2)) ->
//                          -> if((j == 1) && (i != 0)) -> if only one elem remains
//         if(a[i][j] == 1) -> if(j == 0) - because 'i' can be any row
//                             (BUT IS IT VALID TO PRESUME THIS??? YES, IT'S UNDEFINED :P)

namespace Optimization {

Operand* Peephole::HandleIntegerCmpLoad(LoadInstr* instr, IntConstant* intConst, 
								        Operand* resultOp, OrderType order, 
										bool isSigned, IntConstant* intConstInstr,
										Opcode opcode) {
	// Consider the following case:
	// const int v[4] = {1,2,3,4};
	// if(v[i] == 2) {...}
	// Because we're indexing in a constant array, and the value to for test is a constant,
	// instead of loading the value from the global variable and testing it we can do:
	// if(i == 1) {...} - test for the position where 2 appears.

	// Some more (complicated) examples:
	// char v[] = "abcdb";     if(v[i] == 'b')  ->  if(i == 1 || i == 4)
    // char v[] = "abcdb";     if(v[i] != 'b')  ->  if(i != 1 && i != 4)
	// char v[] = "abbbbcdd";  if(v[i] == 'b')  ->  if(i >= 1 && i <= 4)
	// char v[] = "abbbbcdd";  if(v[i] != 'b')  ->  if(i < 1 || i > 4)
	// char v[] = "abbbbcddb"; if(v[i] == 'b')  ->  if((i >= 1 && i <= 4) || (i == 8))
	// Note that we allow only indexing in single-dimensional arrays, but they can have
	// records as the element type. This simplifies the tests, and allowing
	// multi-dimensional arrays wouldn't make too much sense anyway.
	GlobalVariable* globalVar;
	Operand* indexOp;
	IndexList subindexList;
	__int64 maxElems = GetMaxConstArraySize();

	if(IsLoadFromConstArray(instr, globalVar, indexOp, 
                            subindexList) == false) {
		// We are not loading from a constant global.
		return nullptr;
	}

	// If the array is too large give up; it would take too long to compute the ranges,
	// and probably too many elements would match the condition anyway.
	if(globalVar->HasInitializerList()) {
		if(globalVar->GetInitializerList()->Count() > maxElems) {
			return nullptr;
		}
	}
	else {
		auto stringConst = globalVar->GetInitializer()->Value()->As<StringConstant>();
		if(stringConst->Value().Length() > maxElems) {
			return nullptr;
		}
	}

	// After we're certain that we index in a single-dimensional array, we need to
	// determine where the constant we compare with appears in the array.
	// Because it can appear in several places, and in consecutive positions,
	// we associate with it a set of ranges. As an example, for
	// char v[] = "abbbbcddb" the following set of ranges can be created:
	// 'a': [0, 0]
	// 'b': [1, 4] U [8, 8]
	// 'c': [5, 5]
	// 'd': [6, 7]
	RangeList ranges;
	int rangeCount = ComputeConstRanges(globalVar, intConst, order, ranges, 
										subindexList, intConstInstr, opcode);

	if(rangeCount == 0) {
		// The value doesn't appear in the constant at all, so we can return the result:
		return GetBool(false, resultOp);
	}

	// Because there can be many ranges, we define a maximum MAX_COST for the
	// generated tests; if the cost is higher than MAX_COST we give up,
	// because generating all the tests may be slower than loading the value from
	// the global the usual way (this is related to cache spatial locality).
	// The maximum cost is MAX_COST = 5. Note that for all other orders than
	// 'equal' and 'not equal' the maximum cost is 64, because the code generator
	// is able to optimize the generated tests.
	bool isNotEqual = order == Order_NotEqual;
	bool isEqual = order == Order_Equal;
	bool isNotEquality = (isNotEqual || isEqual) == false;

	int cost = ComputeRangesCost(ranges, isNotEquality);

	if(isNotEquality && (cost > 64)) return nullptr;
	else if(cost > 5) return nullptr;
	
	// Now generate the range tests, and the code that combines their results.
	StaticList<Operand*, 64> rangeOps;
	auto resultType = resultOp->GetType();

	for(int i = 0; i < ranges.Count(); i++) {
		ConstantRange& range = ranges[i];

		if(isNotEquality) {
			// For <, <=, > and >= create a test for each of the possible values.
			for(int i = 0; i < range.Count; i++) {
				auto constantOp = irGen_.GetIntConst(resultType, range.StartIndex + i);
				rangeOps.Add(CreateCompare(indexOp, constantOp, Order_Equal,
										   resultType, false));
			}
		}
		else { 
            rangeOps.Add(CreateRangeTest(range, indexOp, isEqual, resultType));
        }
	}

	// If we have a single range, we're done.
	if(rangeOps.Count() == 1) {
		// Only a single range, nothing needs to be combined.
		return rangeOps[0];
	}

	// Generate the code that combines the results of the individual range tests.
	// ==/</<=/>/>=  R1 | R2 | ... Rn
	// !=            R1 & R2 & ... Rn
	Operand* result = rangeOps[0];

	for(int i = 1; i < rangeOps.Count(); i++) {
		auto combOp = irGen_.GetTemporary(resultType);

		if(isNotEqual) {
			irGen_.GetAnd(result, rangeOps[i], combOp);
		}
		else irGen_.GetOr(result, rangeOps[i], combOp);
		result = combOp;
	}

	return LOG(result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::IsLoadFromConstArray(LoadInstr* instr, GlobalVariable*& globalVar,
									Operand*& indexOp, IndexList& subindexList) {
	// If the 'load' is marked as 'volatile' we shall not eliminate it.
	// The loaded value should have integer type.
	if(instr->IsVolatile()) {
        return false;
    }

	if(instr->ResultOp()->IsInteger() == false) {
        return false;
    }

	// We handle only a 'load' that targets an 'index' or 'addr' instruction directly.
	// More complicated cases are probably not worth it.
	if((instr->SourceOp()->DefiningInstrIs<IndexInstr>()   ||
		instr->SourceOp()->DefiningInstrIs<AddressInstr>() ||
		instr->SourceOp()->DefiningInstrIs<ElementInstr>()) == false) {
	    return false;
	}

	// The base operand should be a constant global variable with an initializer,
	// or, if allowed, a series of 'index'/'elem' instructions that lead to a global
	/// variable. This allows the handling of nested records/arrays.
    auto definingInstr = instr->SourceOp()->DefiningInstruction();
	auto addrInstr = static_cast<AddressInstr*>(definingInstr);
	Operand* baseOp = addrInstr->BaseOp();
	indexOp = addrInstr->IndexOp();

	if(ShouldLoadFromConstStruct() && (addrInstr->IsAddress() == false)) {
		Operand* lastBaseOp = instr->SourceOp();
		Operand* lastIndexOp = nullptr;
		bool lastWasIndex = true;
		int level = 0;

		while(true) {
			// Limit the number of nesting levels; a record with many levels
			// probably has many value, so it doesn't pay off to continue.
			if(level > MaxConstNestedLevels()) {
                return false;
            }

			if(auto elemInstr = lastBaseOp->DefiningInstrAs<ElementInstr>()) {
				subindexList.Add(elemInstr->GetFieldIndex());
				lastBaseOp = elemInstr->BaseOp();
				lastWasIndex = false;
				level++;
			}
			else if(auto indexInstr = lastBaseOp->DefiningInstrAs<IndexInstr>()) {
				// The index into the array needs to be a constant, 
				// except for the last index (the one that targets the constant global).
				auto intConst = indexInstr->IndexOp()->As<IntConstant>();

				if((intConst == nullptr) && 
				   (indexInstr->BaseOp()->IsVariableReference() == false)) {
                   return false;
                }

				if(intConst) {
                    subindexList.Add(intConst->Value());
                }

				lastBaseOp = indexInstr->BaseOp();
				lastIndexOp = indexInstr->IndexOp();
				lastWasIndex = true;
				level++;
			}
			else if(lastBaseOp->IsVariableReference()) {
				// We can continue only if the global is an array.
				if(lastWasIndex == false) {
                    return false;
                }

				baseOp = lastBaseOp;
				indexOp = lastIndexOp;
				break;
			}
			else {
				// All other cases are not supported, give up.
				return false;
			}
		}
	}

	// Now we really need to have a constant global.
	auto variableRef = baseOp->As<VariableReference>();

	if(variableRef == nullptr) {
        return false;
    }

	if(variableRef->IsGlobalVariableRef() == false) {
        return false;
    }

	globalVar = variableRef->GetGlobal();

	if((globalVar->IsConstant() == false)     ||
	   (globalVar->HasInitializer() == false) ||
	   globalVar->IsTentative()) {
       return false;
    }

	// If the initializer has wired "features" like 'adjust' give up.
	auto initializer = globalVar->GetInitializer();

	if(initializer->HasAdjustment()) {
        return false;
    }

	// Test the initializer. If we're loading 'int8' values we accept a string constant.
	if(globalVar->HasInitializerList()) {
        return true;
    }
	else return instr->ResultOp()->IsInt8() &&
				globalVar->GetInitializer()->Value()->IsStringConstant();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Peephole::ComputeConstRanges(GlobalVariable* globalVar, IntConstant* intConst,
								 OrderType order, RangeList& ranges, 
								 IndexList& subindexList, IntConstant* intConstInstr,
								 Opcode opcode) {
	// Travers all the elements; if the value is the one we're searching for,
	// add it to the current range. If not, end the current range.
	__int64 value = intConst->Value();
	ConstantRange* currentRange = nullptr;

	// We can have either a initializer list, or a string constant.
	if(globalVar->HasInitializerList()) {
		InitializerList* initList = globalVar->GetInitializerList();

		for(int i = 0; i < initList->Count(); i++) {
			Initializer* initializer = (*initList)[i];

			// If we have nested records/array, we must index into them first.
			for(int j = subindexList.Count() - 1; j >=  0; j--) {
				DebugValidator::IsTrue(initializer->IsInitializerList());
				auto nestedInitList = static_cast<InitializerList*>(initializer);
				initializer = (*nestedInitList)[subindexList[j]];
			}

			__int64 initValue = initializer->Value()->As<IntConstant>()->Value();
			AddToCurrentRange(initValue, value, i, currentRange, ranges,
							  order, intConstInstr, opcode);
		}
	}
	else {
		// This is a string constant.
		auto stringConst = globalVar->GetInitializer()->Value()->As<StringConstant>();
		DebugValidator::IsNotNull(stringConst);
		auto& strData = stringConst->Value();

		for(int i = 0; i < strData.Length(); i++) {
			AddToCurrentRange(strData[i], value, i, currentRange, ranges,
							  order, intConstInstr, opcode);
		}
	}

	return ranges.Count();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Peephole::AddToCurrentRange(__int64 value, __int64 requiredValue, __int64 index,
								 ConstantRange*& currentRange, RangeList& ranges,
								 OrderType order, IntConstant* intConstInstr, 
								 Opcode opcode) {
	// Try to match the value to the required one. We have four cases:
	// 1. ==, !=, and no instruction involved
	// 2. <, <=, >, >=, and no instruction involved
	// 3. 4. as above, but with an instruction involved
	bool match = false;

	if((order == Order_Equal) || (order == Order_NotEqual)) {
		if(intConstInstr) {
			// a[i] % 2 == 0
			match = MatchesRequiredValue(value, requiredValue, order,
										 intConstInstr, opcode);
		}
		else {
			// a[i] == 3
			// We test for both orders for 'equal', but different code will be generated.
			match = value == requiredValue;
		}
	}
	else if(intConstInstr) {
		// a[i] << 3 < 1
		match = MatchesRequiredValue(value, requiredValue, order, 
									 intConstInstr, opcode);
	}
	else {
		// a[i] > 8
		switch(order) {
			case Order_Less:           { match = value <  requiredValue; break; }
			case Order_LessOrEqual:    { match = value <= requiredValue; break; }
			case Order_Greater:        { match = value >  requiredValue; break; }
			case Order_GreaterOrEqual: { match = value >= requiredValue; break; }
			default: DebugValidator::Unreachable();
		}
	}

	if(match) {
		if(currentRange) {
			// The previous element was the searched value too.
			currentRange->Count++;
		}
		else {
			// A new range must be started now.
			ranges.Add(ConstantRange(value, index, 1));
			currentRange = &ranges[ranges.Count() - 1];
		}
	}
	else if(currentRange) {
		// The previous element was the searched value, but this one
		// isn't anymore; end the current range.
		currentRange = nullptr;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::MatchesRequiredValue(__int64 value, __int64 requiredValue,
                                    OrderType order, IntConstant* intConstInstr, 
                                    Opcode opcode) {
	DebugValidator::IsNotNull(intConstInstr);

	// Apply the instruction to the value, then compare it to the required one.
	// Use the constant folder, it handles all cases already.
	auto intConst = irGen_.GetIntConst(intConstInstr->GetType(), value);
	auto result = folder_.FoldBinary(opcode, intConst, 
                                     intConstInstr, nullptr);
	DebugValidator::IsNotNull(result);
	__int64 resultValue = result->As<IntConstant>()->Value();
	
	switch(order) {
		case Order_Equal:          { return resultValue <  requiredValue; break; }
		case Order_Less:           { return resultValue <  requiredValue; break; }
		case Order_LessOrEqual:    { return resultValue <= requiredValue; break; }
		case Order_Greater:        { return resultValue >  requiredValue; break; }
		case Order_GreaterOrEqual: { return resultValue >= requiredValue; break; }
		default: DebugValidator::Unreachable();
	}

	return false; // Should not be reached.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Peephole::ComputeRangesCost(RangeList& ranges, bool isNotEquality) {
	// How the cost is computed for a range [a, b]:
	// COST_SINGLE (a == b): 1        COST_RANGE (a != b): 3
	// COST_COMBINATION (R1 &&/|| R2): 1
	// TOTAL_COST: (NUM_SINGLE(RS) * COST_SINGLE) + (NUM_RANGE(RS) * COST_RANGE) +
	//			   ((NUM(RS) - 1) * COST_COMBINATION), where 'RS' is the set of all ranges.
	//
	// Example 1: TOTAL_COST('if(i >= 1 && i <= 4)') = 2*1 + 1*1 = 3
	// Example 2: TOTAL_COST('if((i >= 1 && i <= 4) || (i == 8))') = 1*3 + 1*1 + 1*1 = 5
	// An exception is for cases when the order is not 'equal' or 'not equal';
	// in this case we consider only the sum of the number of elements in the ranges.
	int cost = 0;

	for(int i = 0; i < ranges.Count(); i++) {
		if(ranges[i].Count == 1) {
            cost += 1;
        }
		else if(isNotEquality) {
            cost += ranges[i].Count;
        }
		else cost += 3;
	}
	
	if(isNotEquality) {
        return cost;
    }
	else return cost + (1 * (ranges.Count() - 1));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::CreateRangeTest(ConstantRange& range, Operand* indexOp, bool isEquality,
								   const Type* resultType) {
	// The type of the test that must be generated depends on the range
	// and on the order of the test ('equal' or 'not equal'). For range [a, b]:
	//       | a == b |      a != b     | Range combination
	// -----------------------------------------------------
	//  ==   | i == a | i >= a & i <= b | R1 | R2
	//  !=   | i != a | i < a  | i > b  | R1 & R2
	auto type = indexOp->GetType();

	if(isEquality) {
		if(range.Count == 1) {
			// i == a
			auto constantOp = irGen_.GetIntConst(type, range.StartIndex);
			return CreateCompare(indexOp, constantOp, Order_Equal, 
                                 resultType, false);
		}
		else {
			// i >= a && i <= b
			auto leftConstOp = irGen_.GetIntConst(type, range.StartIndex);
			auto rightConstOp = irGen_.GetIntConst(type, range.StartIndex + range.Count);
			
            auto leftResult = CreateCompare(indexOp, leftConstOp, Order_GreaterOrEqual,
											resultType, false);
			auto rightResult = CreateCompare(indexOp, rightConstOp, Order_Less,
											 resultType, false);

			auto andOp = irGen_.GetTemporary(resultType);
			irGen_.GetAnd(leftResult, rightResult, andOp);
			return andOp;
		}
	}
	else {
		if(range.Count == 1) {
			// i != a
			auto constantOp = irGen_.GetIntConst(type, range.StartIndex);
			return CreateCompare(indexOp, constantOp, Order_NotEqual, 
                                 resultType, false);
		}
		else {
			// i < a || i > b (>=)
			auto leftConstOp = irGen_.GetIntConst(type, range.StartIndex);
			auto rightConstOp = irGen_.GetIntConst(type, range.StartIndex + range.Count);
			
            auto leftResult = CreateCompare(indexOp, leftConstOp, Order_Less,
											resultType, false);
			auto rightResult = CreateCompare(indexOp, rightConstOp, Order_GreaterOrEqual,
											 resultType, false);
			
            auto orOp = irGen_.GetTemporary(resultType);
			irGen_.GetOr(leftResult, rightResult, orOp);
			return orOp;
		}
	}
}

} // namespace Optimization