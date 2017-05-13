
type equation = Error.info * Ktype.t * Ktype.t 

val type_check : equation list -> (equation list) * bool

val equations_to_string  : equation list -> string
