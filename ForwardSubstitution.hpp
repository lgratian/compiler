//! - Estimate register pressure ("Value-driven redundancy elimination" PHD)
//! - Insert computation where pressure is high (but only if it reduces the pressure!!!)
//? - candidates are cheap instructions (add, sub, and, xor, shl, index/addr with Const, elem, zext, sext, etc.)
//? - Should be useful especially for x86