//! Heuristic
//?     o  callee should be small (<20 instr, < 5 block)
//?     o  bonus if callee has contains loops (bonus for each loop, use flags from 'goto' to detect)
//?     o  bigger bonus if caller contains loops too (where call is made)
//?     o  don't inline if block contains call to 'abort'/'exit', 'ret -1'
//?     o  big bonus if arguments are constant (points for each parameter)
//?     o  bonus if leaf call (callee doesn't contain 'call' at all)
//?     o  bonus if large number of parameters (reduces init code)
//?     o  small bonus if call with record parameter
