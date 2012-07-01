//! Uses value profiles to specialize code for the common values
//? Compute benefit/cost
//?     o use block frequency
//?           - candidates only in frequently executed blocks
//?     o use dynamic instruction count
//?     o use estimated latency (add - 1, mul - 2, div - 10, etc.)
//?     o check if constant folding possible
//?     o check if simplifications can be done
//?           - div / C, and a, 1, mul a, Pow_2, etc.
//?     o check if value used by memcpy, memset
//?           - can create improved version
//?     o handle switch
//?           - "peel" most used case
//?     o handle loops
//?           - may allow loop unrolling
//?           - clone loop and unroll/other simplifications
//?           - for (i = m; i < n; i++)
//?     o prove that two pointers don't overlap
//?           - only if a large number of loads can be eliminated
//?     o insert code prefetching
//?           - prefetch "in the future" of the hot path
//?           - prefetch called function
//?           - do this before loop
//?     o consider profiling loads