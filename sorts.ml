open Ktype
open Error

type sort =
    Field 
  | Row of sort
  | Type
  | SVar of string
  | Empty
  | Cons of string * sort

type equation =  sort * sort
    
exception Unsolvable of equation list 

type environment = (string * sort) list

let scount = ref 0

let sgenerate () = 
  incr scount ; 
  SVar ("?X" ^ (Int32.to_string (Int32.of_int !scount)) )

let getsort x env =
    List.assoc x env 
    	
(* concatene 2 environments et genere les contraintes
   correspondantes                                     *)
let rec concenvs (e1 : environment) (e2 : environment) 
  : environment * (equation list)  = 
  let rec insert (l,s : string * sort)  (e : environment) : 
    environment * equation list =
    match e with
	[] -> [(l,s)], []
      | (l',s') :: e' ->
	  if l = l' then
	    e, [(s, s')]
	  else
	    let e'',c' = insert (l,s)  e'
	    in ((l',s') :: e''), c'  
  in    
    match e1 with
	[] -> e2, []
      | (l,s) :: e1t -> 
	  let e2', c = insert (l,s) e2 in
	  let e3, c' = concenvs e1t e2'
	  in e3, (c @ c') 


let rec infersort (e : environment) (t : t) (c : equation list) =
    match t with
	TPres t' -> 
	  let res, const, newe = infersort e t' c  in
	    Field, ((res, Type) :: const), newe
      | TSerialized t' -> 
	  let res, const, newe = infersort e t' c in
	    Type, ((res, Type) :: const), newe
      | TVr s  -> (
	  try 
	    let t = getsort s e in t,[],e
	  with
	      Not_found -> 
		let sv = sgenerate () in sv,[], ((s,sv) :: e) ) 
      | TBase s -> Type, [], e 
      | TRecord (t,t') ->
	  let res, const, ne = infersort e t c in
	  let res', const', ne' = infersort e t' c in
	  let ne'', const'' = concenvs ne ne' in
	     Type, (( res, Row Empty) 
			:: ( res', Row Empty) 
			:: const  @ const'  @ const''), ne'' 
      | TRow (s,f,r) ->
	  let res, const, ne = infersort e f c in
	  let res', const', ne' = infersort e r  c in
	  let ne'', const'' = concenvs ne ne' in
	  let sv = sgenerate () in
	    Row sv, (res, Field) 
	      :: (res', Row (Cons (s,sv))) 
	      :: (const @ const'  @ const'' ) , ne''
      | TAbsF -> Field, [], e
      | TAbsR -> let sv = sgenerate () in Row sv, [], e


let rec infersortcomp (e : environment) (comp : ct) (c : equation list) :
  (equation list) * environment =
  let terms = comp.input @ comp.output in
  let rec u (e : environment) (ts : (string * Ktype.t) list) 
    (c : equation list) =
    match ts with
	[] -> c, e
      | (s,t) :: ts' -> 
	  let sort, c1, e1 = infersort e t c in
	  let e2, c2 = concenvs e1 e in
	    u e2 ts' ((sort,Type) :: (c2 @ c1 @ c) )
  in u e terms c


let occursinsort (tyX : string) tyT =
  let rec oterm tyT =
    match tyT with
	Field -> false 
      | Row v -> oterm v
      | Type -> false
      | SVar s -> tyX = s
      | Empty -> false
      | Cons (l,s) -> oterm s
  in oterm tyT

let substinsort 
  ( tyX : string ) 
  ( tyT : sort )
  ( tyS : sort ) 
  =
  let rec ssort t =
    match t with
      | Field -> Field
      | Empty -> Empty
      | Cons (l,s) -> Cons (l, ssort s)
      | Row v -> Row (ssort v)
      | Type -> Type
      | SVar s -> if s = tyX then tyT else SVar s
  in ssort tyS
       
let substinconstr tyX tyT constr =
  List.map
    (fun (tyS1,tyS2) ->
       (substinsort tyX tyT tyS1, substinsort tyX tyT tyS2))
    constr

let unify  (constr : equation list) : equation list =
  let rec u constr = 
    match constr with
	[] -> []
      | (  tyS,  (SVar tyX) ) :: rest ->
          if tyS = SVar tyX then 
	    u rest
          else if occursinsort tyX tyS then
            raise (Unsolvable constr)
          else
            (u (substinconstr tyX tyS rest)) @ [ (SVar tyX) ,tyS ]
      | (  (SVar tyX),  tyT ) :: rest ->
          if tyT = SVar tyX then 
	    u rest
          else if occursinsort tyX tyT then
             raise (Unsolvable constr)
          else
	    (u (substinconstr tyX tyT rest)) @ [ (SVar tyX),tyT]
      | (   Type,  Type) :: rest -> u rest
      | (   Row s1,  Row s2) :: rest -> u ((s1,s2) :: rest)
      | (   Field,  Field) :: rest -> u rest
      | (   Empty,  Empty) :: rest -> u rest
      | (  Cons (l1,s1), Cons(l2,s2)) :: rest ->
	  if l1 = l2 then
	    u ((s1,s2) :: rest)
	  else
	    let sv = sgenerate () in
	      u (( s1, Cons (l2, sv) ) :: ( s2, Cons (l1, sv)) :: rest)
      | ( _,_) :: _  ->  raise (Unsolvable constr)
  in u constr



let comp_sort_check (comp : Ktype.ct) : environment * (equation list) * bool = 
  let c, e = infersortcomp [] comp [] in
    try
      e, (unify c), true
    with
	Unsolvable u -> e, u , false


(********************************* to_string *)

let rec labels_to_string s =
  match s with
    | Empty -> ""
    | Cons (l,s') ->  
	if s' = Empty then 
	  l
	else
	  l ^ "," ^ (labels_to_string s')
    | SVar s -> s
    | _ -> assert false
	
let rec sort_to_string s =
  match s with
      Field -> "field"
    | Row s -> "row(" ^ (sort_to_string s) ^ ")"
    | Type -> "type"
    | SVar s -> s
    | Empty -> labels_to_string s
    | Cons (l,s') -> labels_to_string s 

let equation_to_string ( t,t') =
  (sort_to_string t) ^ " = " ^ (sort_to_string t')

let equations_to_string l =
  String.concat "\n" (List.map equation_to_string l)

let environment_to_string e =
  let assoc_to_string (id,s) = id ^ "," ^ (sort_to_string s) in
    String.concat ";" (List.map assoc_to_string e ) 

