open Core
open Import
open Formats


module Pri = Preg
module Sec = Sreg
module Program_address = Paddr
module Operation = Oper

module Assemble : sig

  val jump : Program_address.t -> Instruction.t
    
  val assign_pri_const : Pri.t * bit -> Instruction.t
  val assign_pri_not_pri : Pri.t * Pri.t -> Instruction.t

  val assign_mode3 : Pri.t * Pri.t * Operation.t * Sec.t -> Instruction.t

  val assign_seconday_const : Sec.t * bit -> Instruction.t

  val assign_seconday_const_vec : Sec.t list * bit list -> Instruction.t list
  val assign_mode3_vec : Pri.t * Pri.t * Operation.t * Sec.t list -> Instruction.t list

end = struct

  open Instruction

  let jump a = 
    branch User a (* Must determine user/super while assembling *)

  let any_preg = Preg.a
  let any_sreg = Sreg.general_purpose_memory 0

  let assign_pri_const (target,bit) : Instruction.t = 
    log_op Mode1 ~target (Oper.const bit) any_preg any_sreg

  let assign_pri_not_pri (target,arg) : Instruction.t = 
    log_op Mode1 ~target (Oper.not_arg1) arg any_sreg

  let assign_mode3 (target, prim, oper, sec) =
    log_op Mode3 ~target oper prim sec

  let assign_seconday_const (sec,bit) : Instruction.t =
    log_op Mode2 ~target:any_preg (Oper.const bit) any_preg sec

  let assign_seconday_const_vec (secs,bits) : Instruction.t list  =
    List.map (List.zip_exn secs bits) ~f:assign_seconday_const

  let assign_mode3_vec (target, prim, op, secs) = (* rev: unroll right->left *)
    List.map (List.rev secs) ~f:(fun sec ->
      assign_mode3 (target,prim,op,sec))

end	   

let _ = Assemble.assign_mode3


module Code : sig 
    
  type located_instruction
  type t

  val locate : Program_address.t -> Instruction.t -> located_instruction
  val create : located_instruction list -> t
    
  val length : t -> int
  val seq : t list -> t

  val to_prom : t -> Prom.t

end = struct
    
  type located_instruction = Loc of Program_address.t * Instruction.t
  [@@deriving sexp_of]
    
  type t = Code of located_instruction list
  [@@deriving sexp_of]
  let deCode (Code xs) = xs

  let locate a i = Loc (a,i)
  let create xs = Code xs

  let length (Code xs) = List.length xs

  let seq ts = Code (List.concat (List.map ts ~f:deCode))

  let delocate = List.map ~f:(function Loc(_,i) -> i)

  let to_prom (Code xs) = Prom.create (delocate xs)

end


let (++) = Program_address.(++)


module Emitter : sig

  type t
  val run : t -> Program_address.t -> Code.t

  val emit1 : Instruction.t -> t 
  val emitV : Instruction.t list -> t
  val seq : t list -> t

  val label_start : (Program_address.t -> t) -> t
  val label_end : (Program_address.t -> t) -> t

end = struct

  type t = F of (Program_address.t -> Code.t)
  let deF (F x) = x

  let run t = deF  t
    
  let emit1 x = F (fun a -> Code.create [Code.locate a x])

  let emitV xs = F (fun a ->
    Code.create (List.mapi xs ~f:(fun i x -> Code.locate (a++i) x)))

  let seq = 
    let rec loop a = function
      | [] -> Code.create []
      | t::ts -> let code1 = deF t a in
		 let code2 = loop (a ++ Code.length code1) ts in
		 Code.seq [code1; code2]
    in 
    fun ts -> F (fun a -> loop a ts)
      
  let label_start f =
    F (fun a -> deF (f a) a)

  let label_end f =
    F (fun a -> 
      let thrown_away = deF (f Program_address.zero) a in
      let size = Code.length thrown_away in
      let code = deF (f (a ++ size)) a in
      assert (size = Code.length code);
      code)

end

let _ = Emitter.label_end

module Symbols = struct

  let _ = Sreg.general_purpose_memory

  let ra = Preg.a
  let rb = Preg.b
  let rc = Preg.c
  let rd = Preg.d

  let g = Sreg.data_memory_access

  let g8 = g 8
  let g7 = g 7
  let g6 = g 6
  let g5 = g 5
  let g4 = g 4
  let g3 = g 3
  let g2 = g 2
  let g1 = g 1
  let g0 = g 0

  let ga = Sreg.ga

  let g_wr = Sreg.g_wr

  let xo = Sreg.[xo3;xo2;xo1;xo0]
    
end

let _ = Symbols.(ra,rb)

module Paint_example = struct

  open Symbols
  open Assemble
  open Emitter

  let emit_wait_for_ever =
    label_start (fun forever ->
      emit1 (jump forever))
		  

  let emitter = 
    (* Paint the screen red and wait forever *)

    let gbits = [g7;g6;g5;g4;g3;g2;g1;g0] in
    let _ = gbits in

    seq [
      (*[xo3,xo2,xo1,xo0] <= $get_op pri & sec unroll*)
      emitV (assign_seconday_const_vec (xo, Oper.encode (Oper.(land))));

      (*g8 <- 1*)
      emit1 (assign_seconday_const (g8,H));

      (*ga <- 1*)
      emit1 (assign_seconday_const (ga,H));

      (*paint_loop:*)
      label_start (fun paint_loop -> seq [

	(*g_wr <- 1*)
	emit1 (assign_seconday_const (g_wr,H));

	(*c <- 1*)
	emit1 (assign_pri_const (rc,H));

	(*gbits <= gbits ^ c, c <= c $$ gbits unroll*)
	emitV (assign_mode3_vec (rc,rc,Oper.xor,gbits));

	(*d <- ~c*)
	emit1 (assign_pri_not_pri (rd,rc));

        (*jmp paint_loop*)
	emit1 (jump paint_loop);

      ]);
      (*d <- 1*)
      emit1 (assign_pri_const (rd,H));

      (*forever:*)
      (*jmp forever*)
      emit_wait_for_ever;
    ]
      
    
end


module Test_show_some_things = struct

    let assemble () =
      printf "Test_show_some_things.assemble()...\n";
      let code = 
	printf "- hardcoded to use paint example \n";
	Emitter.run Paint_example.emitter (Program_address.zero ++ 0)
      in
      let prom = Code.to_prom code in
      let () = 
	printf "- writing code to stdout...\n";
	Prom.print_internal prom
      in
      let () =
	let program_data = Prom.to_program_data prom in
	let prom2 = Prom.of_data_string program_data in
	let res = (prom = prom2) in
	printf "- checking round-trip: %s\n" (if res then "ok" else "FAILED");

	let target_file = "paint.prog" in
	printf "- writing bitchild program to: %s\n" target_file;
	Out_channel.write_all target_file ~data:program_data;
      in
      ()

end

let go() = 
  let () = printf "Ass.go()...\n" in
  Test_show_some_things.assemble ()
