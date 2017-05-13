{
open Parser
open Error

exception Eof

let keywords = [
  ("abs", fun i -> Parser.ABS  i);
  ("pre", fun i -> Parser.PRE i);
  (":", fun i -> Parser.COL  i);
  (".", fun i -> Parser.DOT  i);
  (",", fun i -> Parser.COM  i);
  ("=", fun i -> Parser.EQ  i);
  ("%", fun i -> Parser.PERCENT i);
  (";", fun i -> Parser.SCOL  i);
  ("[", fun i -> Parser.LBRA i);
  ("]", fun i -> Parser.RBRA i);
  ("{", fun i -> Parser.LBR  i);
  ("(", fun i -> Parser.LPAR  i);
  (")", fun i -> Parser.RPAR i);
  ("}", fun i -> Parser.RBR  i);
  ("->", fun i -> Parser.ARROW  i);
  ("=>", fun i -> Parser.BARROW  i);
]
		      
let keywords_table = Hashtbl.create 1024
		    
let _ = List.iter (fun (str,f) -> Hashtbl.add keywords_table str f) keywords
    
let create_id i str =
  try (Hashtbl.find keywords_table str) i
  with _ ->
    if (String.get str 0) >= 'A' && (String.get str 0) <= 'Z' then
      Parser.UNAME {i=i;v=str}
    else 
      if (String.get str 0) = '\'' then
	Parser.VAR {i=i;v=str}
      else
	Parser.NAME {i=i;v=str}
	
let lineno   = ref 1
and depth    = ref 0
and start    = ref 0
and filename = ref ""
and startLex = ref dummyinfo
		 
let create inFile stream =
  if not (Filename.is_implicit inFile) then 
    filename := inFile
  else 
    filename := Filename.concat (Sys.getcwd()) inFile ;
  lineno := 1; start := 0; Lexing.from_channel stream
    
let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)
  
let info lexbuf =
  create_info (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)
    
let text = Lexing.lexeme

let stringBuffer = ref (String.create 2048)
let stringEnd = ref 0

let resetStr () = stringEnd := 0

let addStr ch =
  let x = !stringEnd in
  let buffer = !stringBuffer
in
  if x = String.length buffer then
    begin
      let newBuffer = String.create (x*2) in
      String.blit buffer 0 newBuffer 0 x;
      String.set newBuffer x ch;
      stringBuffer := newBuffer;
      stringEnd := x+1
    end
  else
    begin
      String.set buffer x ch;
      stringEnd := x+1
    end

let getStr () = String.sub (!stringBuffer) 0 (!stringEnd)

}    

rule token = parse
  [' ' '\009' '\012']+     { token lexbuf }

| [' ' '\009' '\012']*("\r")?"\n" { newline lexbuf; token lexbuf }

| "*)" { error (info lexbuf) "Unmatched end of comment" }

| "(*" { depth := 1; startLex := info lexbuf; comment lexbuf; token lexbuf }

| ['A'-'Z' 'a'-'z' ] ['A'-'Z' 'a'-'z' '_' '0'-'9']* 
  { create_id (info lexbuf) (text lexbuf) }

| '\''  ['A'-'Z' 'a'-'z' ] ['A'-'Z' 'a'-'z' '_' '0'-'9']* 
  { create_id (info lexbuf) (text lexbuf) }


| "->"  { create_id (info lexbuf) (text lexbuf) }
| "=>"  { create_id (info lexbuf) (text lexbuf) }

| ['{' '}' '[' ']' '(' ')' '%' ':' '.' ';' '=' '^' ','  ] 
  { create_id (info lexbuf) (text lexbuf) }

| "\"" { resetStr(); startLex := info lexbuf; string lexbuf }

| eof { Parser.EOF(info lexbuf) }

| _  { error (info lexbuf) "Illegal character" }

and comment = parse
  "(*"
    { depth := succ !depth; comment lexbuf }
| "*)"
    { depth := pred !depth; if !depth > 0 then comment lexbuf }
| eof
    { error (!startLex) "Comment not terminated" }
| [^ '\n']
    { comment lexbuf }
| "\n"
    { newline lexbuf; comment lexbuf }

and string = parse
 '\\' { addStr(escaped lexbuf); string lexbuf }
| '\n' { addStr '\n'; newline lexbuf; string lexbuf }
| eof  { error (!startLex) "String not terminated" }
| _    { addStr (Lexing.lexeme_char lexbuf 0); string lexbuf }

and escaped = parse
  'n'	 { '\n' }
| 't'	 { '\t' }
| '\\'	 { '\\' }
| '"'    { '\034'  }
| '\''	 { '\'' }
| ['0'-'9']['0'-'9']['0'-'9']
    {
      let x = int_of_string(text lexbuf) in
      if x > 255 then
	error (info lexbuf) "Illegal character constant"
      else
	Char.chr x
    }
| [^ '"' '\\' 't' 'n' '\'']
    { error (info lexbuf) "Illegal character constant" }




