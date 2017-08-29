open Types

type t = secondary [@@deriving sexp_of]

val to_int : t -> int
val of_int : int -> t
