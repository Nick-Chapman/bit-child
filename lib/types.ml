open! Core

type bit = H | L [@@deriving sexp_of]

type mode = Mode1 | Mode2 | Mode3 [@@deriving sexp_of]

type truth_table = { ll:bit; lh:bit; hl:bit; hh:bit }

type primary = A | B | C | D [@@deriving sexp_of]

type secondary = 
| M  of int (*0..29 -- general purpose *)
| Xo of int (*0..3  -- auxillary logic *)
| G  of int (*0..11 -- memory address  *)

(* Read: current word at memory address *)
(* Write: word to be written to memory when "g_wr" is strobed *)
| Ga 
| Gb 
| Gc 
| Gd

(* registers with unrelated read/write purpose, separated by "__" *)
|        Sa__Usr_exec
|        Sb__Usr_mem
|        Sc__Usr_fb
|        Sd__Snd_en
|     Pulse__Fb_page
|   Bus_lsb__Bus_stg
|  Scan_lsb__Sd_cs
|     Sd_in__Sd_out
|    Btn_up__Sd_clk
|  Btn_down__Ld_pc__Ld_ram__St_ram__Exec (* ext:0..3 selects write action *)
|  Btn_left__Bus_clk
| Btn_right__Scan_clk
|     Btn_a__G_wr
|     Btn_b__Btn_clr

[@@deriving sexp_of]

type su = Supervisor | User [@@deriving sexp_of]

type branch = {
  su : su;
  address : int;
}

type log_op = {
  mode : mode;
  target : primary;
  oper : truth_table;
  primary : primary;
  secondary : secondary;
}

type instruction =
| Branch of branch
| Log_op of log_op
