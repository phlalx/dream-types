merge : { i1 : { 'x1 => 'x2  } ; i2 : { 'x2 => 'x3 } }
    -> { o : { 'x1 => 'x3 } } 
gen_int : {} -> { o1 : { a : 'alpha ; 'y1  => a : pre(Int) ; 'y1 } } 
gen_float : {} -> { o2 : { b : 'alpha ; y2 => b : pre(Float) ; 'y2 } }
%
gen_int.o1 = merge.i1
gen_float.o2 = merge.i2
