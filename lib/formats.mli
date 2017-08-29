open! Core
open Import

module Mode : sig
  type t = mode
end

module Oper : sig
  type t
  val case : t -> truth_table
  val tt : truth_table -> t
  val const : bit -> t
  val not_arg1 : t
  val not_arg2 : t
  val (lor) : t
  val (land) : t
  val xor : t
  val encode : t -> bit list
end 

module Preg : sig
  type t
  val case : t -> primary
  val a : t
  val b : t
  val c : t
  val d : t
end

module Sreg : sig
  type t
  val case : t -> secondary
  val general_purpose_memory : int -> t
  val data_memory_access : int -> t
  val ga : t
  val g_wr : t
  val xo3 : t
  val xo2 : t
  val xo1 : t
  val xo0 : t
end 

module Paddr : sig
  type t [@@deriving sexp_of]
  val zero : t
  val (++) : t -> int -> t
end 

module Su : sig
  type t = su [@@deriving sexp_of]
end

module Instruction : sig 
  type t [@@deriving sexp_of]
  val case : t -> instruction
  val branch : Su.t -> Paddr.t -> t
  val log_op : Mode.t -> target:Preg.t -> Oper.t -> Preg.t -> Sreg.t -> t 
end

module Prom : sig
  type t
  val case : t -> instruction list

  val create : Instruction.t list -> t
  val of_data_string : string -> t
  val to_program_data : t -> string
  val print_internal : t -> unit
end
