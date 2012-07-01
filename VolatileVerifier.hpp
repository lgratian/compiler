// VolatileVerifier.hpp
// Copyright (c) Lup Gratian
//
// Verifies if operations involving volatile memory locations are preserved.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_VOLATILE_VERIFIER_HPP
#define PC_ANALYSIS_VOLATILE_VERIFIER_HPP

#include "../IR/Instructions.hpp"
#include "../IR/Function.hpp"
#include "../IR/Unit.hpp"
#include "../Base/Log.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/String.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class VolatileVerifier {
private:
	Dictionary<Function*, int> volatileCount_; // The number of volatile operations.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Returns the number of volatile operations from the specified function.
	int GetVolatileCount(Function* function) {
		if(volatileCount_.ContainsKey(function)) {
			return volatileCount_[function];
		}
		else return 0;
	}

	// Sets the number of volatile operations for the specified function.
	void SetVolatileCount(Function* function, int value) {
		if(volatileCount_.ContainsKey(function)) {
			volatileCount_[function] = value;
		}
		else volatileCount_.Add(function, value);
	}

	// Counts the number of volatile operations found in the specified function.
	int CountVolatileOperations(Function* function);

	// Reports about a volatile operation count mismatch.
	void ReportMismatch(Function* function, int prevCount, int newCount);

public:
	void Execute(Unit* unit);

	void Dump();
};

} // namespace Analysis
#endif