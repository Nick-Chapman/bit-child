open! Core
open Import

(* Fixed size bit-vectors *)

module F(X : sig val size : int end) : sig
    
  type t [@@deriving sexp_of]

  (* When a vector is converted-to or displayed-as a list of bits, the
     list is in `natural' order: left->right from MSB to LSB.  *)

  val of_bits_exn : bit list -> t
  val of_num : int -> t
  val zero : t

  (* The int used for indexing is 0-LSB, (size-1)-MSB *)
  val get_exn : t -> int -> bit
  val set_exn : t -> int -> bit -> t
    
  val bits : t -> bit list
  val to_num : t -> int

end
