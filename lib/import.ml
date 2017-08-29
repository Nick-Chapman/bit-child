open! Core

include Types

let chop size : 'a list -> 'a list list =
  let rec loop n ~acc xs =
    if n <= size then List.rev (xs::acc) else
      let (front,rest) = List.split_n xs size in
      loop (n - size) ~acc:(front::acc) rest
  in
  fun xs -> loop (List.length xs) ~acc:[] xs

let extend size xs =
  let n = List.length xs in
  assert (n<=size);
  let pad = List.init (size-n) ~f:(fun _ -> L) in
  pad @ xs

let bits_of_nat n =
  assert (n>=0);
  let rec loop acc n =
    if n = 0 
    then acc
    else loop ((if (n mod 2) = 0 then L else H) :: acc) (n / 2)
  in loop [] n

let nat_of_bits xs =
  List.fold xs ~init:0 ~f:(
    fun acc -> 
      function 
      | L -> 2 * acc
      | H -> 2 * acc + 1)

let bits_to_string xs =
  String.of_char_list
    (List.map xs ~f:(function H -> '1' | L -> '0'))
