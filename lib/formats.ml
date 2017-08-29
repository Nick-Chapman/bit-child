open Core
open Import

module Mode : sig 

  type t = mode
  val encode : t -> bit list
  val decode : bit * bit -> t

end = struct

  type t = mode
  
  let encode = function
    | Mode1 -> [L;H]
    | Mode2 -> [H;L]
    | Mode3 -> [H;H]

  let decode = function
    | L,L -> failwith "impossible, LL is a Branch"
    | L,H -> Mode1
    | H,L -> Mode2
    | H,H -> Mode3

end

module Oper : sig

  type t [@@deriving sexp_of]

  val case : t -> truth_table

  val tt : truth_table -> t
  val const : bit -> t
  val not_arg1 : t
  val not_arg2 : t
  val (lor) : t
  val (land) : t
  val xor : t

  val encode : t -> bit list
  val decode : bit * bit * bit * bit -> t

end = struct

  module Bit4 = Vec.F(struct let size = 4 end)

  type t = Bit4.t [@@deriving sexp_of]

  let case t = 
    match Bit4.bits t with
    | [ll;lh;hl;hh] -> {ll;lh;hl;hh}
    | _ -> failwith "impossible: bit4 must have 4 bits"

  let create (o3,o2,o1,o0) = Bit4.of_bits_exn [o3;o2;o1;o0]

  let tt {ll;lh;hl;hh} = create (ll,lh,hl,hh)

  let const = function
    | L -> create (L,L,L,L)
    | H -> create (H,H,H,H)

  let not_arg1 = create (L,L,H,H)
  let not_arg2 = create (L,H,L,H)
  let (land)   = create (H,L,L,L)
  let (lor)    = create (H,H,H,L)
  let xor =      create (L,H,H,L)

  let encode = Bit4.bits
  let decode = create

end

module Preg : sig

  type t [@@deriving sexp_of]

  val case : t -> primary

  val a : t
  val b : t
  val c : t
  val d : t

  val encode : t -> bit list
  val decode : bit * bit -> t

end = struct

  type t = primary [@@deriving sexp_of]

  let case x = x

  let a = A
  let b = B
  let c = C
  let d = D

  let encode = function
    | A -> [L;L]
    | B -> [L;H]
    | C -> [H;L]
    | D -> [H;H]

  let decode = function
    | (L,L) -> a
    | (L,H) -> b
    | (H,L) -> c
    | (H,H) -> d

end

module Sreg : sig

  type t [@@deriving sexp_of]

  val case : t -> secondary

  val general_purpose_memory : int -> t
  val data_memory_access : int -> t
  val ga : t
  val g_wr : t
  val xo3 : t
  val xo2 : t
  val xo1 : t
  val xo0 : t

  val encode : t -> bit list
  val decode6 : bit list -> t

end = struct

  type t = Secondary.t [@@deriving sexp_of]

  let case x = x

  let general_purpose_memory n = M n
  let data_memory_access n = G n

  let ga  = Ga
  let g_wr= Btn_a__G_wr
  let xo3 = Xo 3
  let xo2 = Xo 2
  let xo1 = Xo 1
  let xo0 = Xo 0

  module Bit6 = Vec.F(struct let size = 6 end)

  let encode t =
    Bit6.bits (Bit6.of_num (Secondary.to_int t))

  let decode6 xs =
    Secondary.of_int (Bit6.to_num (Bit6.of_bits_exn xs))

end


module Paddr : sig (* 13 bit program addr *)

  type t [@@deriving sexp_of]
  val to_num : t -> int

  val zero : t
  val (++) : t -> int -> t

  val encode : t -> bit list
  val decode13 : bit list -> t

end = struct

  type t = int [@@deriving sexp_of]

  let to_num x = x

  let zero = 0
  let (++) a b = a + b

  module Bit13 = Vec.F(struct let size = 13 end)

  let encode n = Bit13.bits (Bit13.of_num n)
    
  let decode13 xs = Bit13.to_num (Bit13.of_bits_exn xs)

end

module Su : sig

  type t = su [@@deriving sexp_of]

  val encode : t -> bit list
  val decode : bit -> t

end = struct

  type t = su [@@deriving sexp_of]

  let encode = function
    | Supervisor -> [L]
    | User -> [H]

  let decode = function
    | L -> Supervisor
    | H -> User

end

module Byte : sig

  type t
  val of_bits_exn : bit list -> t
  val to_char : t -> char
  val to_hex_string : t -> string

end = struct

  module Bit8 = Vec.F(struct let size = 8 end)
  type t = Bit8.t
  let of_bits_exn = Bit8.of_bits_exn
  let to_num t =
    List.fold (Bit8.bits t) ~init:0 ~f:(
      fun acc -> 
	function 
	| L -> 2 * acc
	| H -> 2 * acc + 1)
  let to_char t = Char.of_int_exn (to_num t)
  let to_hex_string t = sprintf "%02x" (to_num t)

end


module Instruction : sig 

  type t [@@deriving sexp_of]

  val case : t -> instruction

  val branch : Su.t -> Paddr.t -> t
  val log_op : mode -> target:Preg.t -> Oper.t -> Preg.t -> Sreg.t -> t 

  val encode : t -> bit list
  val decode16 : bit list -> t

  val to_byte_pair : t -> Byte.t * Byte.t
  val to_hex_string : t -> string

end = struct

  type op_fields = {
    target : Preg.t;
    oper : Oper.t;
    primary : Preg.t;
    sec : Sreg.t;
  } [@@deriving sexp_of]
    
  module T = struct
    type t =
    | Branch of (Su.t * Paddr.t)
    | Op of mode * op_fields
	[@@deriving sexp_of]
  end

  let case = function
    | T.Branch (su,a) -> Branch { su; address = Paddr.to_num a }
    | T.Op (mode,x) -> 
      let target = Preg.case x.target in
      let oper = Oper.case x.oper in
      let primary = Preg.case x.primary in
      let secondary = Sreg.case x.sec in
      Log_op { mode; target; oper; primary; secondary }
      
  include T

  let branch su paddr = Branch (su,paddr)

  let log_op mode ~target oper pri sec : t = 
    T.Op (mode, {
      target;
      oper;
      primary = pri;
      sec;
    })

  module Bit16 = Vec.F(struct let size = 16 end)

  let to_bits = 
    function
    | Branch (us,paddr) -> [L;L] @ Su.encode us @ Paddr.encode paddr
    | Op(mode,i) ->
      List.concat [
	Mode.encode mode;
	Preg.encode i.target;
	Oper.encode i.oper;
	Preg.encode i.primary;
	Sreg.encode i.sec;
      ]
	
  let encode t = Bit16.bits (Bit16.of_bits_exn (to_bits t))

  let decode16 xs =
      let bits = Bit16.bits (Bit16.of_bits_exn xs) in
      match bits  with

      | L::L::c::xs ->
	let su = Su.decode c in
	let paddr = Paddr.decode13 xs in
	branch su paddr
	  
      | [m1;m0; q1;q0; o3;o2;o1;o0; a1;a0; b5;b4;b3;b2;b1;b0]
	->
	let mode = Mode.decode (m1,m0) in
	let target = Preg.decode (q1,q0) in
	let oper = Oper.decode (o3,o2,o1,o0) in
	let primary = Preg.decode (a1,a0) in
	let sec = Sreg.decode6 [b5;b4;b3;b2;b1;b0] in
	log_op mode ~target oper primary sec

      | _ -> failwith "impossible - instruction must be 16 bits"


  let to_byte_pair t =
    match chop 8 (encode t) with
    | [h;l] -> (Byte.of_bits_exn h, Byte.of_bits_exn l)
    | _ -> failwith "Instruction.byte_pair"  

  let to_hex_string t =
    let (h,l) = to_byte_pair t in
    Byte.to_hex_string h ^ Byte.to_hex_string l

end

module Prom : sig

  type t [@@deriving sexp_of]
  
  val case : t -> instruction list

  val create : Instruction.t list -> t
  val of_data_string : string -> t
  val to_program_data : t -> string
  val print_internal : t -> unit

end = struct

  type t = ROM of Instruction.t list

  let case (ROM xs) =
    List.map xs ~f:Instruction.case

  let create xs = ROM xs

  let sexp_of_t _ = Sexp.Atom "rom"

  let of_data_string data =
    let n = String.length data in
    let rec loop i acc =
      if i >= n then ROM (List.rev acc) else
	let high = data.[i] in
	let low = data.[i+1] in
	let n = Char.to_int high * 256 + Char.to_int low in
	let bits = bits_of_nat n in
	let instruction = Instruction.decode16 (extend 16 bits) in
	loop (i+2) (instruction::acc)
    in
    loop 0 []

  let to_program_data (ROM xs) : string =
    let bytes = List.concat_map xs ~f:(fun x ->
      let (h,l) = Instruction.to_byte_pair x in [h;l]
    ) in
    let chars = List.map bytes ~f:Byte.to_char in
    String.of_char_list chars

  let print_internal (ROM xs) =
    List.iteri xs ~f:(fun i x ->
      printf "%02d - %s -- %s -- %s\n%!" i
	(bits_to_string (Instruction.encode x))
	(Instruction.to_hex_string x)
	(String.tr ~target:'\n' ~replacement:' '
	   (Sexp.to_string_hum (Instruction.sexp_of_t x))))

end

