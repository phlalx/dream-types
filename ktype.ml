open Error

type t =
    TRecord of t * t
  | TRow of string * t * t
  | TAbsF
  | TAbsR 
  | TVr of string
  | TPres of t
  | TBase of string
  | TSerialized of t

type ct = { input : (string * t) list ; output : (string * t) list }

type constraints = { 
  mutable compdecl : (string * ct) list ; 
  mutable consts   : (string * string * string * string) list
}

type itf = Client | Server

let rec freshvars c t =
  match t with
      TVr s -> TVr (c ^ "_" ^ s)
    | TRow (s,t,t') -> TRow (s, freshvars c t, freshvars c t')
    | TPres t -> TPres (freshvars c t)
    | TRecord (t, t') -> TRecord ((freshvars c t), (freshvars c  t'))
    | x -> x
	
let freshconstraints c =
  let g u (c,t) = (c, freshvars u t ) in
  let f (c,t) = (c,  {input  = List.map  (g c) t.input ; 
		      output = List.map  (g c) t.output } ) 
  in
    c.compdecl <- List.map f c.compdecl 

let gen_unificand (consts :  constraints) =
    
  let find_comp compname l =
   try 
     List.assoc compname l
   with
       Not_found -> Error.err ("no component with name " ^ compname)
  in
    
  let find_itf compname itfname itftype l =
    try
      List.assoc itfname l  
    with
	Not_found ->
	  match itftype with
	      Client -> 
		Error.err ("no client interface with name " ^ itfname ^ 
		" in component " ^ compname) 
	    | Server ->
		Error.err ( "no server interface with name " ^ itfname ^ 
		" in component " ^ compname )
  in
  let gen_equation c o c' i =
    let tc  = find_comp c consts.compdecl
    and tc' = find_comp c' consts.compdecl
    in let tyo =  find_itf c o Client tc.output
    and tyi = find_itf c' i Server tc'.input 
    in
      (Error.dummyinfo, tyo, tyi )
	
  in
  let rec g lconst res  =
    match lconst with
	[] -> res
      | (c,o,c',i) :: t -> (gen_equation c o c' i) :: (g t res)
	  
  in 
    freshconstraints consts ;
    g consts.consts []

let count = ref 0

let generate () = 
  incr count ; 
  TVr ("?X" ^ (Int32.to_string (Int32.of_int !count)) )

let rec term_to_string t =
  match t with
    | TRecord (TAbsR, t) -> "{" ^ (term_to_string t) ^ "}"
    | TRecord (t,t') ->"{" ^(term_to_string t) ^ "=>" ^ (term_to_string t')^"}"
    | TRow (s,t,t') -> s ^ ":" ^ (term_to_string t) ^";" ^(term_to_string t')
    | TAbsF -> "abs" 
    | TAbsR -> "abs" 
    | TVr s -> s 
    | TPres t -> "Pres(" ^ (term_to_string t) ^")"
    | TBase t -> t 
    | TSerialized t ->  "Ser(" ^ (term_to_string t) ^")"

let interface_to_string l = 
  let iter (u,v) =
    u ^ ":" ^ (term_to_string v)
  in
    String.concat ";" (List.map iter l)
	
let ct_to_string c =
  let aux u = "{" ^ (interface_to_string u) ^ "}"
  in (aux c.input) ^ "->" ^ (aux c.output) 

let rec compdecl_to_string (n,t) = 
  n ^ ":" ^ (ct_to_string t)
    
and cont_to_string (a,b,c,d) =
  a ^ "." ^ b ^ " = " ^ c ^ "." ^ d

and liste_to_string assoc l = 
  String.concat "\n" (List.map assoc l)

let constraints_to_string p = 
  (liste_to_string compdecl_to_string p.compdecl) ^ "\n"
  ^ (liste_to_string cont_to_string p.consts) ^ "\n"

