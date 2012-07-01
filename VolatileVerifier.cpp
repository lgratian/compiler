// VolatileVerifier.cpp
// Copyright (c) Lup Gratian
//
// Implements the VolatileVerifier class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "VolatileVerifier.hpp"

namespace Analysis {

int VolatileVerifier::CountVolatileOperations(Function* function) {
	DebugValidator::IsTrue(function->IsDefinition());
	auto instrEnum = function->GetInstructionEnum();
	int count = 0;

	while(instrEnum.IsValid()) {
		auto instr = instrEnum.Next();

		if(auto loadInstr = instr->As<LoadInstr>()) {
			if(loadInstr->IsVolatile()) count++;
		}
		else if(auto storeInstr = instr->As<StoreInstr>()) {
			if(storeInstr->IsVolatile()) count++;
		}
	}

	return count;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VolatileVerifier::Execute(Unit* unit) {
	DebugValidator::IsNotNull(unit);

	for(auto function = unit->Functions().First(); function; function = function->Next) {
		if(function->Value->IsDefinition() == false) {
			// This is a declaration, skip it.
			continue;
		}

		int prevCount = GetVolatileCount(function->Value);
		int newCount = CountVolatileOperations(function->Value);
		SetVolatileCount(function->Value, newCount);

		if(prevCount > newCount) {
			// Some volatile operations were removed!
			ReportMismatch(function->Value, prevCount, newCount);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VolatileVerifier::ReportMismatch(Function* function, int prevCount, int newCount) {
	string text = "Volatile operation mismatch in " + *function->Name();
	text += string::Format(L" (previous = %d, now = %d", prevCount, newCount);
	Log::Error(text);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VolatileVerifier::Dump() {
	typedef Dictionary<Function*, int>::TPair TPair;
	string text = "";

	volatileCount_.ForEach([&text](TPair& pair) -> bool {
		text += *pair.Key->Name();
		text += string::Format(L": %d", pair.Value);
		return true;
	});

	ObjectDumper(text, "Volatile Operations").Dump();
}

} // namespace Analysis