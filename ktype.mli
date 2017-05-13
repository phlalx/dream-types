
(* types *)
type t =
  | TRecord of t * t
  | TRow of string * t * t
  | TAbsF
  | TAbsR
  | TVr of string
  | TPres of t
  | TBase of string
  | TSerialized of t

(* component types *)
type ct = { input : (string * t) list ; output : (string * t) list }

(* typing and connexion declarations *)
type constraints = { 
  mutable compdecl : (string * ct ) list ; 
  mutable consts   : (string * string * string * string) list
}

val term_to_string : t -> string

val generate : unit -> t

val gen_unificand : constraints -> (Error.info * t * t) list 

val ct_to_string : ct -> string
   
val constraints_to_string : constraints -> string

