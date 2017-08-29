open Core
open Import

module My_Formats = struct

  (* types for various bit-vector sizes *)
  module Bit4 = Vec.F(struct let size = 4 end)
  module Bit12 = Vec.F(struct let size = 12 end)
  module Bit13 = Vec.F(struct let size = 13 end)
  module Bit30 = Vec.F(struct let size = 30 end)

  module Pfile = Bit4

  module Oper : sig

    type t [@@deriving sexp_of]
    
    val of_form : truth_table -> t

    val of_bit4 : Bit4.t -> t

    val apply : t -> bit -> bit -> bit

    val to_num : t -> int

  end = struct

    type t = Bit4.t

    let of_bit4 x = x

    let of_bit_quad (o3,o2,o1,o0) = Bit4.of_bits_exn [o3;o2;o1;o0]
      
    let of_form {ll;lh;hl;hh} = of_bit_quad (ll,lh,hl,hh)

    let apply t left right =
      let n = 
	match left,right with
	| L,L -> 0
	| L,H -> 1
	| H,L -> 2
	| H,H -> 3
      in
      Bit4.get_exn t n

    let to_num = Bit4.to_num

    type operation = 
    | And
    | Or
    | Xor 
    | Not_arg1
    | Not_arg2
    | Const0
    | Const1
    | Oper_bits of Bit4.t
	[@@deriving sexp_of]

    let to_operation t =
      match Bit4.bits t with
      | [H;L;L;L] -> And
      | [H;H;H;L] -> Or
      | [L;H;H;L] -> Xor
      | [L;L;H;H] -> Not_arg1
      | [L;H;L;H] -> Not_arg2
      | [L;L;L;L] -> Const0
      | [H;H;H;H] -> Const1
      | _         -> Oper_bits t

    let sexp_of_t t = sexp_of_operation (to_operation t)

  end


  let o = 0

  (* These 5 offset defs can go in any order *)
  let o,pfile_offset    = o+4,o
  let o,gen_offset      = o+30,o
  let o,aux_offset      = o+4,o
  let o,stage_offset  = o+4,o
  let o,addr_offset     = o+12,o

  let bits_width = o

  module Bits = Vec.F(struct let size = bits_width end)

  let pfile_mask bits = 
    Pfile.of_num ((Bits.to_num bits lsr pfile_offset) land 0xF)

  let gen_mask bits = 
    Bit30.of_num ((Bits.to_num bits lsr gen_offset) land 0x3FFFFFFF)

  let aux_mask bits = 
    Oper.of_bit4 (Bit4.of_num ((Bits.to_num bits lsr aux_offset) land 0xF))

  let staging_mask bits = 
    Bit4.of_num ((Bits.to_num bits lsr stage_offset) land 0xF)

  let addr_mask bits = 
    Bit12.of_num ((Bits.to_num bits lsr addr_offset) land 0xFFF)


  module Preg : sig

    type t [@@deriving sexp_of]

    val of_form : primary -> t

    val d : t

    val to_offset : t -> int 

  end = struct

    type t = int

    let a = pfile_offset
    let b = a+1
    let c = a+2
    let d = a+3

    let of_form = function
      | A -> a
      | B -> b
      | C -> c
      | D -> d

    let to_offset t = t 

    type pretty = primary [@@deriving sexp_of]

    let to_pretty t =
      if t = a then A else
	if t = b then B else
	  if t = c then C else
	    if t = d then D else
	      failwith "Preg.to_pretty"

    let sexp_of_t t = sexp_of_pretty (to_pretty t)


  end

  module Sreg : sig (* 6 bit secondary reg identifier *)

    type t = secondary [@@deriving sexp_of]

    val of_form : secondary -> t

  end = struct

    type t = secondary [@@deriving sexp_of]

    let of_form x = x

  end

  module Paddr : sig (* 13 bit program addr *)

    type t [@@deriving sexp_of]
    
    val of_form : int -> t

    val zero : t
    val next : t -> t
    val to_num : t -> int

  end = struct

    type t = int [@@deriving sexp_of]
      
    let of_form x = x

    let zero = 0
    let next n = n+1
    let to_num t = t

  end


  module Su = Formats.Su

  module Instruction = struct

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

    let of_form (x:instruction) : T.t =
	match x with
	| Branch { su; address } -> T.Branch (su,Paddr.of_form address)
	| Log_op { mode; target; oper; primary; secondary } ->

	  let target = Preg.of_form target in
	  let oper = Oper.of_form oper in
	  let primary = Preg.of_form primary in
	  let sec = Sreg.of_form secondary in

	  Op (mode, { target; oper; primary; sec })

    include T

    let to_string t = 
      String.tr ~target:'\n' ~replacement:' ' (Sexp.to_string_hum (sexp_of_t t))


  end



  module Prom : sig

    type t [@@deriving sexp_of]

    val of_form : instruction list -> t

    val read : t -> Paddr.t -> Instruction.t

    val decompile : t -> Instruction.t list

  end = struct

    type t = ROM of Instruction.t array

    let of_form xs =
      ROM (Array.of_list (List.map xs ~f:Instruction.of_form))

    let sexp_of_t _ = Sexp.Atom "rom"

    let read (ROM a) paddr =
      let i = Paddr.to_num paddr in
      try
	a.(i)
      with _ -> failwithf "Prom.read, i=%d, #xs=%d" i (Array.length a) ()

    let decompile (ROM a) = Array.to_list a

  end


  module Dword : sig (* 4 bit data word *)
    type t = Bit4.t [@@deriving sexp_of] 
  end = struct
    type t = Bit4.t [@@deriving sexp_of] 
  end


  module Daddr : sig (* 12 bit data addr *)
    type t
    include Comparable with type t := t
    val of_bit12 : Bit12.t -> t
  end = struct
    module T = struct
      type t = int [@@deriving sexp,compare]
    end
    include T
    include Comparable.Make(T)
    let of_bit12 x = Bit12.to_num x
  end


  module Dmem : sig 
    type t [@@deriving sexp_of]
    val empty : t
    val get : t -> Daddr.t -> Dword.t
    val set : t -> Daddr.t -> Dword.t -> t
  end = struct
    type t = Dword.t Daddr.Map.t [@@deriving sexp_of]
    let empty = Daddr.Map.empty
    let get mem addr = 
      match Map.find mem addr with
      | Some x -> x
      | None -> Bit4.zero
    let set mem addr data =
      Map.add mem ~key:addr ~data
  end


end
open My_Formats
(*----------------------------------------------------------------------*)


module State : sig

  type t [@@deriving sexp_of]

  val initialize : Prom.t -> t
  val run : t -> int * t (* num_steps * final state *)
(*TODO: put num_steps in state *)
end = struct

  type t = {
    pc : Paddr.t;
    prom : Prom.t;
    dmem : Dmem.t;

    pfile : Pfile.t		    ; (* d,c,b,a *)
    general_purpose : Bit30.t       ; (* m29,m28,..,m0 *)
    aux_logic : Oper.t              ; (* o3,o2,o1,o0 *)
    dmem_write_staging : Bit4.t     ; (* gd,gc,gb,ga *)
    dmem_addr : Bit12.t             ; (* g11,g10,..,g0 *)

  } [@@deriving sexp_of]

  type state = t

  let bits_from_state s =
    Bits.of_num (
      (Pfile.to_num s.pfile		lsl pfile_offset) lor
      (Bit30.to_num s.general_purpose	lsl gen_offset) lor
      (Oper. to_num s.aux_logic  	lsl aux_offset) lor
      (Bit4. to_num s.dmem_write_staging lsl stage_offset) lor
      (Bit12.to_num s.dmem_addr         lsl addr_offset)
    )


  let state_from_bits ~prom ~pc ~dmem ~bits = 
    let aux_logic = aux_mask bits in
    let dmem_write_staging = staging_mask bits in
    let general_purpose = gen_mask bits in
    let pfile = pfile_mask bits in
    let dmem_addr = addr_mask bits in
    { pc; prom; dmem; pfile; 
      general_purpose;
      aux_logic; dmem_write_staging; dmem_addr }

  let initialize prom = 
    let pc = Paddr.zero in
    let dmem = Dmem.empty in
    let bits = Bits.zero in
    state_from_bits ~prom ~pc ~dmem ~bits

  let d_offset = Preg.to_offset Preg.d

  let chunk_size = 1000

  type run_res =
  | Finished of (int * state)
  | Continue of state


  let __ x = Continue x

  let rec execute ~num_steps ~prom ~pc ~bits ~dmem : run_res =
    
    if num_steps = chunk_size
    then
      let s = state_from_bits ~prom ~pc ~dmem ~bits in
      Continue s
    else

      let num_steps = num_steps + 1 in

      let instruction = Prom.read prom pc in

      match instruction with
      | Branch (_su,paddr) ->
	if Bits.get_exn bits d_offset = H
	then 
	  if paddr = pc 
	  then 
	    let s = state_from_bits ~prom ~pc ~dmem ~bits in
	    Finished (num_steps, s)
	  else 
	    let pc = paddr in
	    execute ~num_steps ~prom ~pc ~bits ~dmem
	else 
	  let pc = Paddr.next pc in
	  execute ~num_steps ~prom ~pc ~bits ~dmem

      | Op (mode,i) ->
	let pc = Paddr.next pc in
	let v1 = Bits.get_exn bits (Preg.to_offset i.primary) in
	let v2 = 
	  match
	    match i.sec with
	    | M n                                     -> `state (n+gen_offset)
	    | Xo n                                    -> `state (n+aux_offset)
	    | G n                                     -> `state (n+addr_offset)
	    | Ga                                      -> `mem 0
	    | Gb                                      -> `mem 1
	    | Gc                                      -> `mem 2
	    | Gd                                      -> `mem 3
	    |        Sa__Usr_exec                     -> `state (0+pfile_offset)
	    |        Sb__Usr_mem                      -> `state (1+pfile_offset)
	    |        Sc__Usr_fb                       -> `state (2+pfile_offset)
	    |        Sd__Snd_en                       -> `state (3+pfile_offset)
	    |     Pulse__Fb_page                      -> `fail
	    |   Bus_lsb__Bus_stg                      -> `fail
	    |  Scan_lsb__Sd_cs                        -> `fail
	    |     Sd_in__Sd_out                       -> `fail
	    |    Btn_up__Sd_clk                       -> `fail
	    |  Btn_down__Ld_pc__Ld_ram__St_ram__Exec  -> `fail
	    |  Btn_left__Bus_clk                      -> `fail
	    | Btn_right__Scan_clk                     -> `fail
	    |     Btn_a__G_wr                         -> `const L
	    |     Btn_b__Btn_clr                      -> `fail
	  with
	  | `const x -> x
	  | `state n  -> Bits.get_exn bits n
	  | `mem n -> 
	    let dmem_addr = addr_mask bits in
	    Bit4.get_exn (Dmem.get dmem (Daddr.of_bit12 dmem_addr)) n
	  | `fail -> 
	    failwithf !"todo: execute, read: %{sexp:secondary}" i.sec ()

	in

	let result = Oper.apply i.oper v1 v2 in

	let bits = 
	  match mode with
	  | Mode1 -> Bits.set_exn bits (Preg.to_offset i.target) result
	  | Mode2 -> bits
	  | Mode3 ->
	    let aux_logic = aux_mask bits in
	    let aux_result = Oper.apply aux_logic v1 v2 in
	    Bits.set_exn bits (Preg.to_offset i.target) aux_result
	in

	match mode with
	| Mode1 -> execute ~num_steps ~prom ~pc ~bits ~dmem

	| Mode2
	| Mode3 ->
	  match
	    match i.sec with
	    | M n                                     -> `state (n+gen_offset)
	    | Xo n                                    -> `state (n+aux_offset)
	    | G n                                     -> `state (n+addr_offset)
	    | Ga                                      -> `state (0+stage_offset)
	    | Gb                                      -> `state (1+stage_offset)
	    | Gc                                      -> `state (2+stage_offset)
	    | Gd                                      -> `state (3+stage_offset)
	    |     Pulse__Fb_page                      -> `fail
	    |   Bus_lsb__Bus_stg                      -> `fail
	    |  Scan_lsb__Sd_cs                        -> `fail
	    |     Sd_in__Sd_out                       -> `fail
	    |    Btn_up__Sd_clk                       -> `fail
	    |  Btn_down__Ld_pc__Ld_ram__St_ram__Exec  -> `fail
	    |  Btn_left__Bus_clk                      -> `fail
	    | Btn_right__Scan_clk                     -> `fail
	    |     Btn_a__G_wr                         -> `strobe_memory_write
	    |     Btn_b__Btn_clr                      -> `fail
	    |        Sa__Usr_exec                     -> `fail
	    |        Sb__Usr_mem                      -> `fail
	    |        Sc__Usr_fb                       -> `fail
	    |        Sd__Snd_en                       -> `fail
	  with
	  | `state n ->
	    let bits = Bits.set_exn bits n result in
	    execute ~num_steps ~prom ~pc ~bits ~dmem

	  | `strobe_memory_write ->
	    begin match result with
	    | L -> execute ~num_steps ~prom ~pc ~bits ~dmem
	    | H ->
	      let staging = staging_mask bits in
	      let addr = addr_mask bits in
	      let dmem = Dmem.set dmem (Daddr.of_bit12 addr) staging in
	      execute ~num_steps ~prom ~pc ~bits ~dmem
	    end

	  | `fail ->
	    failwithf !"todo: execute, write %{sexp:secondary}" i.sec ()

  let run =
    let rec chunk_loop ~num_chunks state : int * state =
      match
	let num_steps = 0 in
	let prom = state.prom in
	let pc = state.pc in
	let dmem = state.dmem in
	let bits = bits_from_state state in
	execute ~num_steps ~prom ~pc ~bits ~dmem
      with
      | Finished (num_steps,state) -> 
	let total_steps = num_chunks * chunk_size + num_steps in
	(total_steps,state)
	  
      | Continue state ->
	(*printf !"intermediate_state=%{sexp:t}\n" state;*)
	chunk_loop ~num_chunks:(num_chunks+1) state
    in
    chunk_loop ~num_chunks:0
	    
end

(*let run1000 s =
  let rec loop i =
    let s = State.run s in
    if i=1000 then s else loop (i+1)
  in loop 1*)



let sim ~filename () =

  let () = printf "sim: filename= %s\n" filename in
  let data = In_channel.read_all filename in

  let prom : Prom.t =
    let p = Formats.Prom.of_data_string data in
    let instructions = Formats.Prom.case p in
    Prom.of_form instructions

  in

  let () = 
    let xs = Prom.decompile prom in
    List.iteri xs ~f:(fun i x ->
      printf !"%02d - %s\n" i (Instruction.to_string x));
    ()
  in
  let () = 
    let init_state = State.initialize prom in
    let start_time = Time.now () in
    let num_steps,final_state = State.run init_state  in
    let end_time = Time.now () in
    let () = 
      printf !"--------------------------------------------------\n";
      printf !"final_state=%{sexp:State.t}\n" final_state;
      printf !"--------------------------------------------------\n";
    in
    let duration = Time.diff end_time start_time in
    printf !"num_steps=%d, duration=%{sexp:Time.Span.t}\n" num_steps duration;
  in
  ()

let go() = 
  let () = printf "Sim.go()...\n" in
  let filename = "paint.prog" in
  let () = printf "- using hardcoded prog: paint.prog\n" in
  sim ~filename ()

(* temp *)
let _ = Prom.of_form

