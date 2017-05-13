
type info = Inf of string * int * int  | Noinf

exception Dummy  
    
type 'a withinfo = {i: info; v: 'a}
    
let dummyinfo  = Noinf
  
let create_info f l c = Inf (f, l, c)

let print_info =
  function
      Inf (f,l,c) ->
	print_string f; print_string ":"; print_int l; print_string "."; 
	print_int c; print_string ":"
    | Noinf -> print_string "<Unknown file and line>: "

let error fi s = 
  print_info fi ; 
  print_string " " ; 
  print_string s ; 
  print_newline () ; 
  exit 0

let err s = 
  print_string s ; 
  print_newline () ; 
  exit 0

	

let error_no_exit fi s = 
  print_info fi ; 
  print_string " " ; 
  print_string s ; 
  print_newline () ;
  raise Dummy

		   

    



