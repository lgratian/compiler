// Pointer arguments whose values are only read can be replaced directly by the values
// (but only for small values!)
// Large record passed by value should be passed by reference if only read from.