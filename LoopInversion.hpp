// Transforms loops to allow PRE and other optimizations on loops.
// Transforms
// while(c) { do_smth(); } into
// if(c) do { do_smth(); } while(c);