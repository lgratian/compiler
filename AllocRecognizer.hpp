// Tries to recognize functions that behave like 'malloc'
// and sets the 'alloc' flag. Helps alias analysis for programs that
// wrap the call to 'malloc'.

//! Conditions
//? 1. allocated pointer is returned
//? 2. no store instruction that targets the pointer - doesn't actually matter
//? 3. no call that uses the pointer - important
// - simple linear scan