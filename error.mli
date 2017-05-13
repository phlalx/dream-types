
type info

exception Dummy  

val dummyinfo : info

val create_info : string -> int -> int -> info

val print_info : info -> unit

type 'a withinfo = { i: info; v: 'a }

val error : info -> string -> 'a

val err : string -> 'a

val error_no_exit : info -> string -> 'a


