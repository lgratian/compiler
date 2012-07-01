// simple interprocedural pass to determine if pair of parameters 
// can point to to same locations (do analysis in caller, propagate to calle
// and intersect - if for all cases no alias -> no alias)