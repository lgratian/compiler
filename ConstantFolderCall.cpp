// ConstantFolderCall.cpp
// Copyright (c) Lup Gratian
//
// Implements the constant folder for 'call' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConstantFolder.hpp"

namespace Analysis {

Operand* ConstantFolder::HandleCall(CallInstr* callInstr) {
	// A call to an 'undef' operand, or to a null pointer yields 
    // undefined behavior, so we're allowed to do everything we want.
	if(callInstr->TargetOp()->IsUndefinedConstant() ||
	   callInstr->TargetOp()->IsNullConstant()) {
		// We should return 'undef' only if the function returns a value.
		if(callInstr->ResultOp()) {
			return GetUndefined(callInstr->ResultOp());
		}
		else return nullptr;
	}

	// The target must be a reference to a known function.
	auto functionRef = callInstr->TargetOp()->As<FunctionReference>();
	
    if(functionRef == nullptr) {
        return nullptr;
    }

    // First test for intrinsics; we fold them using the stdlib functions.
    if(auto intrinsic = functionRef->Target()->As<Intrinsic>()) {
        return HandleIntrinsic(callInstr, intrinsic);
    }

	// Check if the function is a known standard library function,
	// and try to evaluate it at compile time. Very useful for functions like
	// 'sin', 'cos', 'floor', etc.
	StdlibType functionType = StdlibRecognizer::Recognize(functionRef->Target());

	if(functionType == Stdlib_None) {
        return nullptr;
    }

	// We treat for now only functions from 'math.h'.
	// The functions are evaluated using the implementation
    // provided by the compiler, so note that the results 
    // are dependent on the machine where the program is compiled.
	switch(functionType) {
 		case Stdlib_fabs:  return EvaluateMathOneParam(callInstr, std::fabs);
		case Stdlib_sqrt:  return EvaluateMathOneParam(callInstr, std::sqrt);
		case Stdlib_exp:   return EvaluateMathOneParam(callInstr, std::exp);
		case Stdlib_floor: return EvaluateMathOneParam(callInstr, std::floor);
		case Stdlib_ceil:  return EvaluateMathOneParam(callInstr, std::ceil);
		case Stdlib_log:   return EvaluateMathOneParam(callInstr, std::log);
		case Stdlib_log10: return EvaluateMathOneParam(callInstr, std::log10);
		case Stdlib_sin:   return EvaluateMathOneParam(callInstr, std::sin);
		case Stdlib_asin:  return EvaluateMathOneParam(callInstr, std::asin);
		case Stdlib_cos:   return EvaluateMathOneParam(callInstr, std::cos);
		case Stdlib_acos:  return EvaluateMathOneParam(callInstr, std::acos);
		case Stdlib_tan:   return EvaluateMathOneParam(callInstr, std::tan);
		case Stdlib_atan:  return EvaluateMathOneParam(callInstr, std::atan);

		case Stdlib_abs:   return EvaluateMathOneParamInt(callInstr, std::abs);
        case Stdlib_labs:  return EvaluateMathOneParamLong(callInstr, std::labs);

		case Stdlib_sqrtf:  return EvaluateMathOneParamFloat(callInstr, std::sqrtf);
		case Stdlib_expf:   return EvaluateMathOneParamFloat(callInstr, std::expf); 
		case Stdlib_floorf: return EvaluateMathOneParamFloat(callInstr, std::floorf);
		case Stdlib_ceilf:  return EvaluateMathOneParamFloat(callInstr, std::ceilf);
		case Stdlib_logf:   return EvaluateMathOneParamFloat(callInstr, std::logf); 
		case Stdlib_log10f: return EvaluateMathOneParamFloat(callInstr, std::log10f);
		case Stdlib_sinf:   return EvaluateMathOneParamFloat(callInstr, std::sinf); 
		case Stdlib_asinf:  return EvaluateMathOneParamFloat(callInstr, std::asinf);
		case Stdlib_cosf:   return EvaluateMathOneParamFloat(callInstr, std::cosf); 
		case Stdlib_acosf:  return EvaluateMathOneParamFloat(callInstr, std::acosf);
		case Stdlib_tanf:   return EvaluateMathOneParamFloat(callInstr, std::tanf); 
		case Stdlib_atanf:  return EvaluateMathOneParamFloat(callInstr, std::atanf);

		case Stdlib_pow:   return EvaluateMathTwoParams(callInstr, std::pow);
		case Stdlib_fmod:  return EvaluateMathTwoParams(callInstr, std::fmod);
        case Stdlib_atan2: return EvaluateMathTwoParams(callInstr, std::atan2);

		case Stdlib_powf:   return EvaluateMathTwoParamsFloat(callInstr, std::powf);
		case Stdlib_fmodf:  return EvaluateMathTwoParamsFloat(callInstr, std::fmodf);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleIntrinsic(CallInstr* callInstr, Intrinsic* intrinsic) {
    if(auto mathIntrinsic = intrinsic->As<MathIntrinsic>()) {
        switch(mathIntrinsic->GetMathKind()) {
            case MathIntr_Abs8:
            case MathIntr_Abs16:
            case MathIntr_Abs32:
            case MathIntr_Abs64: {
                auto argument = callInstr->GetArgument(0);
                bool is64Bit = mathIntrinsic->GetMathKind() == MathIntr_Abs64;

                // abs(quest c, 5, -5) -> 5
                if(auto questInstr = argument->DefiningInstrAs<QuestionInstr>()) {
                    return HandleAbsOnQuestion(questInstr, is64Bit);
                }
                else return EvaluateAbs(callInstr);
            }
            case MathIntr_Min8:
            case MathIntr_Min16:
            case MathIntr_Min32:
            case MathIntr_Min64: {
                return EvaluateMin(callInstr);
            }
            case MathIntr_Max8:
            case MathIntr_Max16:
            case MathIntr_Max32:
            case MathIntr_Max64: {
                return EvaluateMax(callInstr);
            }
            case MathIntr_Fabs:  return EvaluateMathOneParam(callInstr, std::fabs);
            case MathIntr_Atan:  return EvaluateMathOneParam(callInstr, std::atan);
            case MathIntr_Exp:   return EvaluateMathOneParam(callInstr, std::exp);
            case MathIntr_Pow:   return EvaluateMathTwoParams(callInstr, std::pow);
            case MathIntr_Log10: return EvaluateMathOneParam(callInstr, std::log10);
            case MathIntr_Sqrt:  return EvaluateMathOneParam(callInstr, std::sqrt);
            case MathIntr_Sqrtf: return EvaluateMathOneParamFloat(callInstr, std::sqrtf);
            case MathIntr_Log:   return EvaluateMathOneParam(callInstr, std::log);
            case MathIntr_Sin:   return EvaluateMathOneParam(callInstr, std::sin);
            case MathIntr_Tan:   return EvaluateMathOneParam(callInstr, std::tan);
            case MathIntr_Cos:   return EvaluateMathOneParam(callInstr, std::cos);
            case MathIntr_Atan2: return EvaluateMathTwoParams(callInstr, std::atan2);
        }
    }
        
    // Any other intrinsic is not foldable.
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateAbs(CallInstr* callInstr) {
    auto intConst = AsIntConstant(callInstr->GetArgument(0),
                                  callInstr->ParentBlock());
    if(intConst == nullptr) {
        return nullptr;
    }

    __int64 result = std::abs(intConst->Value());
    return irGen_->GetIntConst(intConst->GetType(), result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMin(CallInstr* callInstr) {
    auto intConstA = AsIntConstant(callInstr->GetArgument(0),
                                   callInstr->ParentBlock());
    auto intConstB = AsIntConstant(callInstr->GetArgument(1),
                                   callInstr->ParentBlock());
    if((intConstA && intConstB) == false) {
        return nullptr;
    }

    bool isSigned = callInstr->GetArgument(2)->IsOneInt();
    __int64 result;
    
    if(isSigned) {
        result = std::min(intConstA->Value(), intConstB->Value());
    }
    else result = std::min((unsigned __int64)intConstA->Value(), 
                           (unsigned __int64)intConstB->Value());
    return irGen_->GetIntConst(intConstA->GetType(), result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMax(CallInstr* callInstr) {
    auto intConstA = callInstr->GetArgument(0)->As<IntConstant>();
    auto intConstB = callInstr->GetArgument(1)->As<IntConstant>();

    if((intConstA && intConstB) == false) {
        return nullptr;
    }

     bool isSigned = callInstr->GetArgument(2)->IsOneInt();
    __int64 result;
    
    if(isSigned) {
        result = std::max(intConstA->Value(), intConstB->Value());
    }
    else result = std::max((unsigned __int64)intConstA->Value(), 
                           (unsigned __int64)intConstB->Value());
    return irGen_->GetIntConst(intConstA->GetType(), result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::ParametersValid(CallInstr* callInstr, double& value) {
	auto floatConst = callInstr->GetArgument(0)->As<FloatConstant>();

	if(floatConst == nullptr) {
        return false;
    }

	if(FA::IsNaN(floatConst) || FA::IsInfinity(floatConst)) {
        return false;
    }
	
	value = FA::LimitToType(floatConst);
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathOneParam(CallInstr* callInstr, 
                                              MathOneParam function) {
	double value;

	if(ParametersValid(callInstr, value)) {
		double result = function(value);
		return irGen_->GetDoubleConst(result);
	}
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathOneParamInt(CallInstr* callInstr,
                                                 MathOneParamInt function) {
	auto intConst = AsIntConstant(callInstr->GetArgument(0),
                                  callInstr->ParentBlock());
    if(intConst == nullptr) {
        return nullptr;
    }

	double result = function(intConst->Value());
    return irGen_->GetInt32Const(result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathOneParamLong(CallInstr* callInstr,
                                                  MathOneParamLong function) {
	auto intConst = AsIntConstant(callInstr->GetArgument(0),
                                  callInstr->ParentBlock());
    if(intConst == nullptr) {
        return nullptr;
    }

	double result = function(intConst->Value());
    return irGen_->GetInt64Const(result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathOneParamFloat(CallInstr* callInstr, 
												   MathOneParamFloat function) {
	double value;
	if(ParametersValid(callInstr, value)) {
		double result = function(value);
		return irGen_->GetDoubleConst(result);
	}
    
    return nullptr;    
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::ParametersValid(CallInstr* callInstr, 
                                     double& value1, double& value2) {
	auto floatConst1 = callInstr->GetArgument(0)->As<FloatConstant>();
	auto floatConst2 = callInstr->GetArgument(1)->As<FloatConstant>();
	
	// If the operands are not constants, or they are NaN or Infinity we give up.
	if((floatConst1 && floatConst2) == false) {
        return false;
    }

	if(FA::IsNaN(floatConst1) || FA::IsInfinity(floatConst1) ||
	   FA::IsNaN(floatConst2) || FA::IsInfinity(floatConst2)) {
		return false;
	}

	value1 = FA::LimitToType(floatConst1);
	value2 = FA::LimitToType(floatConst2);
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathTwoParams(CallInstr* callInstr, 
                                               MathTwoParams function) {
	double value1;
	double value2;

	if(ParametersValid(callInstr, value1, value2)) {
		double result = function(value1, value2);
		return irGen_->GetDoubleConst(result);
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathTwoParamsFloat(CallInstr* callInstr, 
													MathTwoParamsFloat function) {
	double value1;
	double value2;

	if(ParametersValid(callInstr, value1, value2)) {
		double result = function(value1, value2);
		return irGen_->GetDoubleConst(result);
	}
	else return nullptr;
}

} // namespace Analysis