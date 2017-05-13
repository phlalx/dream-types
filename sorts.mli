
type sort =
    Field 
  | Row of sort
  | Type
  | SVar of string
  | Empty
  | Cons of string * sort

type environment = (string * sort) list

type equation = sort * sort

val equations_to_string : equation list -> string

val environment_to_string : environment -> string

val comp_sort_check : Ktype.ct -> environment  * (equation list) * bool

