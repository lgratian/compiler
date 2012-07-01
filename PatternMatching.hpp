// PatternMatching.hpp
// Copyright (c) Lup Gratian
//
// Implements a collection of classes that can be used to verify if a series
// of instructions represent a specific expression.
// For example, to verify if an instruction, together with the previous ones,
// form the expression '(-a) * (-b)' we can use the following:
// Operand *a, *b;
// if(Match<MulInstr>(Match<SubInstr>(MatchInt(0), MatchAny(&a)),
//                    Match<SubInstr>(MatchInt(0), MatchAny(&b)))(instr)) {...}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_PATTERN_MATCHING_HPP
#define PC_ANALYSIS_PATTERN_MATCHING_HPP

#include "OperandInfo.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Operand.hpp"
#include "../IR/References.hpp"
#include "../IR/Constants.hpp"
#include "../IR/IRTypes.hpp"
#include "../Base/DebugValidator.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

// The base class for the IR matchers.
class Matcher {
protected:
	// Used by the derived classes to handle the instruction/operand that must be matched.
	virtual bool MatchImpl(Instruction* instr)  { return false; }
	virtual bool MatchImpl(Operand* op)  { return false; }

public:
	// Should return 'true' if the object matches instructions.
	virtual bool IsInstructionMatcher() const {
		return false;
	}

	// Should be used to obtain the result.
	bool operator() (Instruction* instr)  {
		return MatchImpl(instr);
	}

	bool operator() (Operand* op)  {
		return MatchImpl(op);
	}
};


// Matches an instruction with two operands.
template <class T>
class Match : public Matcher {
private:
	 Matcher& matcherA_;
	 Matcher& matcherB_;

public:
	Match(Matcher& matcherA, Matcher& matcherB) : 
			matcherA_(matcherA), matcherB_(matcherB) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual bool IsInstructionMatcher() const {
		return true;
	}

	virtual bool MatchImpl(Instruction* instr)  {
		// First check the type of the instruction.
		if(instr == nullptr) return false;
		if(instr->Is<T>() == false) return false;

		// The type of the instruction matches, now check the operands.
		Operand* opA = instr->GetSourceOp(0);

		if(matcherA_.IsInstructionMatcher()) {
			if(opA->HasDefiningInstruction() == false) return false;
			if(matcherA_(opA->DefiningInstruction()) == false) {
				return false;
			}
		}
		else if(matcherA_(opA) == false) {
			return false;
		}

		Operand* opB = instr->GetSourceOp(1);
		if(matcherB_.IsInstructionMatcher()) {
			if(opB->HasDefiningInstruction() == false) return false;
			if(matcherB_(opB->DefiningInstruction()) == false) {
				return false;
			}
		}
		else if(matcherB_(opB) == false) {
			return false;
		}

		return true;
	}

	virtual bool MatchImpl(Operand* op)  {
		if(op->HasDefiningInstruction()) {
			return MatchImpl(op->DefiningInstruction());
		}
		else return false;
	}
};


// Matches an instruction having the specified opcode.
class MatchOpcode : public Matcher {
private:
	Opcode opcode_;

public:
	MatchOpcode(Opcode opcode) : opcode_(opcode) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual bool IsInstructionMatcher() const {
		return true;
	}

	virtual bool MatchImpl(Instruction* instr)  {
		return instr->GetOpcode() == opcode_;
	}

	virtual bool MatchImpl(Operand* op)  {
		if(op->HasDefiningInstruction()) {
			return op->DefiningInstruction()->GetOpcode() == opcode_;
		}
		else return false;
	}
};


// Matches a conversion instruction (operand and cast type).
template <class T>
class MatchConversion : public Matcher {
private:
	 Matcher& matcher_;
	 const Type* type_;
	 const Type** matchedType_;

public:
	MatchConversion(Matcher& matcher, const Type* type = nullptr, 
					const Type** matchedType = nullptr) : 
			matcher_(matcher), matchedType_(matchedType) {}
			
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual bool MatchImpl(Instruction* instr)  {
		// First check the type of the instruction.
		if(instr == nullptr) return false;
		auto convInstr = instr->As<ConversionInstr>();
		if(convInstr == nullptr) return false;
		if(convInstr->Is<T>() == false) return false;

		// The type of the instruction matches, now check the operands.
		Operand* op = convInstr->GetSourceOp(0);

		if(matcher_.IsInstructionMatcher()) {
			if(op->HasDefiningInstruction() == false) return false;
			if(matcher_(op->DefiningInstruction()) == false) {
				return false;
			}
		}
		else if(matcher_(op) == false) {
			return false;
		}

		// Match the type, if required.
		if(type_) return convInstr->CastType() == type_;
		else {
			if(matchedType_) *matchedType_ = convInstr->CastType();
			return convInstr;
		}
	}

	virtual bool MatchImpl(Operand* op)  {
		if(op->HasDefiningInstruction()) {
			return MatchImpl(op->DefiningInstruction());
		}
		else return false;
	}
};


// Matches any operand.
class MatchAny : public Matcher {
private:
	Operand** matchedOp_;

public:
	MatchAny(Operand** matchedOp = nullptr) : matchedOp_(matchedOp) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual bool MatchImpl(Operand* op)  {
		// Match any kind of operand.
		if(matchedOp_) {
			// The client wants to know which operand it is.
			*matchedOp_ = op;
		}

		return true;
	}
};


// Matches an integer operand having a fixed value.
class MatchInt : public Matcher {
private:
	__int64 value_;
	Type* type_;
    Block* testBlock_;

public:
	MatchInt(__int64 value,  Block* testBlock = nullptr,
             Type* type = nullptr) : 
            value_(value), testBlock_(testBlock), type_(type) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual bool MatchImpl(Operand* op)  {
		// The value and the type must match.
        IntConstant* intConst = op->As<IntConstant>();

        if((intConst == nullptr) && testBlock_) {
            OperandInfo opInfo(testBlock_->ParentFunction()->ParentUnit());
            intConst = opInfo.GetIntConstant(op, testBlock_);
        }

		if(intConst) {
			if(intConst->Value() != value_) return false;
			if(type_ && (intConst->GetType() != type_)) return false;
			return true;
		}

		return false;
	}
};


// Matches an float operand having a fixed value.
class MatchFloat : public Matcher {
private:
	double value_;
	Type* type_;

public:
	MatchFloat(double value,  Type* type = nullptr) : value_(value), type_(type) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual bool MatchImpl(Operand* op)  {
		// The value and the type must match.
		if(auto floatConst = op->As<FloatConstant>()) {
			if(floatConst->Value() != value_) return false;
			if(type_ && (floatConst->GetType() != type_)) return false;
			return true;
		}

		return false;
	}
};


// Matches any integer ant operand.
class MatchIC : public Matcher {
private:
	Type* type_;
	IntConstant** matchedOp_;
    Block* testBlock_;

public:
	MatchIC(IntConstant** matchedOp = nullptr, Block* testBlock = nullptr,
            Type* type = nullptr) : 
			matchedOp_(matchedOp), testBlock_(testBlock), type_(type) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual bool MatchImpl(Operand* op)  {
		// The value and the type must match.
		IntConstant* intConst = op->As<IntConstant>();

        if((intConst == nullptr) && testBlock_) {
            OperandInfo opInfo(testBlock_->ParentFunction()->ParentUnit());
            intConst = opInfo.GetIntConstant(op, testBlock_);
        }

		if(intConst) {
			if(type_ && (intConst->GetType() != type_)) return false;

			if(matchedOp_) {
				// The client wants to know which operand it is.
				*matchedOp_ = intConst;
			}
			
			return true;
		}
		else return false;
	}
};


// Matches any floating ant operand.
class MatchFC : public Matcher {
private:
	Type* type_;
	FloatConstant** matchedOp_;

public:
	MatchFC(FloatConstant** matchedOp = nullptr,  Type* type = nullptr) :
			matchedOp_(matchedOp), type_(type) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual bool MatchImpl(Operand* op)  {
		// The value and the type must match.
		if(auto floatConst = op->As<FloatConstant>()) {
			if(type_ && (floatConst->GetType() != type_)) return false;
			if(matchedOp_) {
				// The client wants to know which operand it is.
				*matchedOp_ = op->As<FloatConstant>();
			}
			
			return true;
		}
		else return false;
	}
};


// Matches a 'nullptr' operand, ignoring the type.
class MatchNullptr : public Matcher {
private:
	NullConstant** matchedOp_;

public:
	MatchNullptr(NullConstant** matchedOp = nullptr) : matchedOp_(matchedOp) {}

	virtual bool MatchImpl(Operand* op)  {
		if(op->IsNullConstant()) {
			if(matchedOp_) {
				// The client wants to know which operand it is.
				*matchedOp_ = op->As<NullConstant>();
			}

			return true;
		}
		else return false;
	}
};


// Matches a 'undef' operand, ignoring the type.
class MatchUndef : public Matcher {
private:
	UndefinedConstant** matchedOp_;

public:
	MatchUndef(UndefinedConstant** matchedOp = nullptr) : matchedOp_(matchedOp) {}

	virtual bool MatchImpl(Operand* op)  {
		if(op->IsUndefinedConstant()) {
			if(matchedOp_) {
				// The client wants to know which operand it is.
				*matchedOp_ = op->As<UndefinedConstant>();
			}

			return true;
		}
		else return false;
	}
};


// Matches a specific operand.
class MatchOp : public Matcher {
private:
	Operand* op_;

public:
	MatchOp(Operand* op) : op_(op) {}

	virtual bool MatchImpl(Operand* op)  {
		return op == op_;
	}
};

} // namespace Analysis
#endif