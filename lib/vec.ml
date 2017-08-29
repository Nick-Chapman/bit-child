open! Core
open Import

module F(X : sig val size : int end) = struct

  open X
  let () = assert (size >= 0)
  let () = assert (size <= 62)

  let max = 1 lsl (size) - 1

  type t = int [@@deriving sexp_of]

  let create_exn n = 
    assert (n>=0);
    if (n>max) then 
      failwithf "Vec, create_exn, n=%d, max=%d, size=%d" n max size ();
    assert (n<=max);
    n

  let of_num = create_exn

  let bits t = extend size (bits_of_nat t)
    
  let check_length xs =
    let n = List.length xs in
    if n = size then () else
      failwithf "Vec(%d).check, got: %d" size n ()

  let of_bits_exn xs = check_length xs; create_exn (nat_of_bits xs)

  let zero = 0

  let get_exn t n =
    (*assert (n >= 0);
    assert (n < size);*)
    if t land (1 lsl n) = 0 then L else H

  let set_exn t n replacement_bit =
    match replacement_bit with
    | H -> t lor (1 lsl n)
    | L -> t land (lnot (1 lsl n))

  let to_num t = t

  type rep = bit list [@@deriving sexp_of]

  let sexp_of_t t = sexp_of_rep (bits t)

end
