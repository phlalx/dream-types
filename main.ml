open Error
open Ktype

let output = ref ""
let in_file = ref "" 
let out_file = ref "" 
let in_filed = ref stdin 
let out_filed = ref stdout
let res_typecheck = ref false
let version = "Dream Constraints Solver V0.01\n" 
let syntax_tree = { compdecl = [] ; consts = [] }

let args =    
  [("-v", Arg.Unit (fun _ -> print_string version ; exit 1) ,"version") ;
   ("-o", Arg.String (fun x -> out_file := x ) ,"output name")]

let parse_args () =
  let inFile = ref (None : string option) in Arg.parse args
  ( fun s -> match !inFile with
   | Some(_) -> err "You must specify exactly one input file"
   | None -> inFile := Some(s) ) "";
  match !inFile with
   | None -> err "You must specify an input file"
   | Some(s) -> in_file := s

let main =
  parse_args () ;
  let open_file f =
  try 
    open_in f 
  with
    _ -> err ("Could not find " ^ !in_file)
  in
  in_filed := open_file !in_file ; 
  let lexbuf = Lexer.create !in_file !in_filed in
  let result =
    try 
      Parser.main Lexer.token lexbuf 
    with 
      Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error"
  in
  Parsing.clear_parser (); 
  close_in !in_filed;
  let equations = gen_unificand result in
  if (String.length (!out_file) != 0) then 
    out_filed := open_out !out_file 
  else
  out_filed := stdout;
  let output s = output_string !out_filed (s ^"\n") in
  output ( "Components" );
  output ( Ktype.constraints_to_string result ) ;
  output ( "Sorting" );
  output ( "-------" );
  let f (s,t) =
  let (env,equations,res) =  Sorts.comp_sort_check t in
  if res then (
    output ( "Component " ^ s ^ " is well sorted" ) ;
    output ( "Environment" ) ;
    output ( Sorts.environment_to_string env  ) ;
    output ("Sorting equations" ) ;
    output ( Sorts.equations_to_string equations ) 
  ) else (
    output ( "No principal sortings" )  ;
    output ( "Environment" ) ;
    output ( Sorts.environment_to_string env  ) ;
    output ("Sorting equations" ) ;
    output ( Sorts.equations_to_string equations ) ;
  ) ; res
in
let sortable = ref true in
List.iter (fun x ->sortable :=(!sortable & (f x))) result.compdecl;
if !sortable then (
  output ( "Typing equations" ) ;
  output ( "----------------" ) ;
  output ( Typecheck.equations_to_string equations ) ;

  let (solution,res) =  Typecheck.type_check equations in

  if res then (
    output ( "Solution" ) ;
    output ( "--------" ) ;
    output ( Typecheck.equations_to_string solution ) 
  ) else (
    output ( "No solution" )  ;
    output ( "-----------" ) ;
    output ( Typecheck.equations_to_string solution ) 
  ) ;
) ;
close_out !out_filed     
