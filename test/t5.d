c : { a : { r : pre(Int) ; abs } } -> 
    { b : { r : pre(Int) ; abs } } 
%
composite d is
   { i = c.a } -> {  o = d.b }
with 
   c.b = c.a

