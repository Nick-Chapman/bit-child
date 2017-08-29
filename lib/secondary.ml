open Core
include Import

type t = secondary [@@deriving sexp_of]

let to_int = function
  | M n                                     ->    n
  | Xo n                                    -> 30+n
  |        Sa__Usr_exec                     -> 34
  |        Sb__Usr_mem                      -> 35
  |        Sc__Usr_fb                       -> 36
  |        Sd__Snd_en                       -> 37
  |     Pulse__Fb_page                      -> 38
  |   Bus_lsb__Bus_stg                      -> 39
  |  Scan_lsb__Sd_cs                        -> 40
  |     Sd_in__Sd_out                       -> 41
  |    Btn_up__Sd_clk                       -> 42
  |  Btn_down__Ld_pc__Ld_ram__St_ram__Exec  -> 43
  |  Btn_left__Bus_clk                      -> 44
  | Btn_right__Scan_clk                     -> 45
  |     Btn_a__G_wr                         -> 46
  |     Btn_b__Btn_clr                      -> 47
  | Ga                                      -> 48
  | Gb                                      -> 49
  | Gc                                      -> 50
  | Gd                                      -> 51
  | G n                                     -> 52+n

let of_int n = 
  let in_range (a,b) = n>=a && n<=b in
  match n with
  | _ when in_range(0,29) -> M n
  | _ when in_range(30,33) -> Xo (n-30)
  | 34 ->          Sa__Usr_exec
  | 35 ->          Sb__Usr_mem
  | 36 ->          Sc__Usr_fb
  | 37 ->          Sd__Snd_en
  | 38 ->       Pulse__Fb_page
  | 39 ->     Bus_lsb__Bus_stg
  | 40 ->    Scan_lsb__Sd_cs
  | 41 ->       Sd_in__Sd_out
  | 42 ->      Btn_up__Sd_clk
  | 43 ->    Btn_down__Ld_pc__Ld_ram__St_ram__Exec
  | 44 ->    Btn_left__Bus_clk
  | 45 ->   Btn_right__Scan_clk
  | 46 ->       Btn_a__G_wr
  | 47 ->       Btn_b__Btn_clr
  | 48 -> Ga
  | 49 -> Gb
  | 50 -> Gc
  | 51 -> Gd
  | _ when in_range(52,63) -> G (n-52)
  | _ ->
    failwith "Secondary.decode, arg not in range 0..63"
