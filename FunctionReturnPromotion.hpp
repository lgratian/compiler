// int* g();
// f() {
//	int* p = g();
//	if(*p == 2) { do_smth(); }
//}

// If the returned pointer is only read from, we can return the int value directly.
// If only a certain field is returned from a large record don't return the whole record.