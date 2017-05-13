open Error
open Ktype 

type equation = Error.info * Ktype.t * Ktype.t 

exception Unsolvable of equation list 

let occursinterm (tyX : string) tyT =
  let rec oterm tyT =
    match tyT with
	TVr s -> s = tyX
      | TPres t -> oterm t
      | TSerialized t -> oterm t
      | TRow (s,t,t') -> (oterm t) || (oterm t')
      | _ -> false
  in oterm tyT

let substinterm 
  ( tyX : string ) 
  ( tyT : t )
  ( tyS : t ) 
  =
  let rec sterm t =
    match t with
      | TVr s -> if s = tyX then tyT else TVr s
      | TPres t -> sterm t
      | TSerialized t -> sterm t
      | TRow (s,f,r) -> TRow (s, sterm f, sterm r)
      | TRecord (t,t') -> TRecord (sterm t, sterm t')
      | x -> x
  in sterm tyS
   
let substinconstr tyX tyT constr =
  List.map
    (fun (inf ,tyS1,tyS2) ->
       (inf, substinterm tyX tyT tyS1, substinterm tyX tyT tyS2))
    constr

let unify fi msg constr =
  let rec u constr = 
    match constr with
	[] -> []
      | ( inf,  tyS,  (TVr tyX) ) :: rest ->
          if tyS = TVr tyX then 
	    u rest
          else if occursinterm tyX tyS then
            raise (Unsolvable constr)
          else
            (u (substinconstr tyX tyS rest)) @ [inf, (TVr tyX) ,tyS ]
      | ( inf,  (TVr tyX),  tyT ) :: rest ->
          if tyT = TVr tyX then 
	    u rest
          else if occursinterm tyX tyT then
             raise (Unsolvable constr)
          else
	    (u (substinconstr tyX tyT rest)) @ [inf, (TVr tyX),tyT]
      | ( inf,  (TBase x),  (TBase y)) :: rest ->
	  if (x = y) then
	    u rest
	  else
	    raise (Unsolvable constr)
	      
      | ( inf,  (TRecord (x, y)),  (TRecord (x', y'))) :: rest ->
	    u ((inf,x,x') :: (inf,y,y') :: rest)

     | ( inf,  TAbsR,  TAbsR) :: rest -> u rest
     | ( inf,  TAbsF,  TAbsF) :: rest -> u rest

     | ( inf,  TPres t,  TPres t') :: rest ->
	    u ((inf, t, t') :: rest)

     | ( inf,  TSerialized t,  TSerialized t') :: rest ->
	    u ((inf, t, t') :: rest)

      | (inf, TRow (s,f,r), TRow (s',f',r') ) :: rest ->
	  if s = s' then
	    u ((inf,f,f') :: (inf,r,r') ::  rest) 
	  else
	    let freshv = generate () in
	      u ( (inf, r, TRow (s',f',freshv) )  ::
		  (inf, r', TRow (s,f,freshv) )  ::
		  rest )

      | (inf, TRow (s,f,r), TAbsR ) :: rest ->
	  u ((inf, TAbsR, TRow (s,f,r)) :: rest)

      | (inf, TAbsR, TRow (s,f,r)) :: rest ->
	  u ((inf, TAbsF, f) :: (inf, TAbsR, r) :: rest)

      | ( inf, _, _) :: rest ->
	  raise (Unsolvable constr)
  in u constr

let equation_to_string (_,x,y) =
  (Ktype.term_to_string x) ^ " = " ^ (Ktype.term_to_string y)

let equations_to_string cts =
  String.concat "\n"  (List.map equation_to_string cts)
   
let type_check p = 
  try
    (unify dummyinfo "" p), true
  with
      Unsolvable u -> u , false

