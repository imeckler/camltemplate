open Core

type alphabet = A | B | C | D

let elt_of_char = function
    | 'a' -> `In A
    | 'A' -> `Inv A
    | 'b' -> `In B
    | 'B' -> `Inv B
    | 'c' -> `In C
    | 'C' -> `Inv C
    | 'd' -> `In D
    | 'D' -> `Inv D
    | _   -> failwith "elt_of_char: Parse failure"

let char_of_elt = function
  | `In A  -> 'a'
  | `Inv A -> 'A'
  | `In B  -> 'b'
  | `Inv B -> 'B'
  | `In C -> 'c'
  | `Inv C -> 'C'
  | `In D -> 'd'
  | `Inv D -> 'D'

let parse_word w : alphabet Free_group.t =
  List.init (String.length w) ~f:(fun i -> elt_of_char w.[i])

let parse_word_arr w =
  Array.init (String.length w) ~f:(fun i -> elt_of_char w.[i])

let rel : alphabet Free_group.t ref = ref [`In A; `In B; `Inv A; `Inv B; `In C; `In D; `Inv C; `Inv D]

let set_word w = rel := parse_word w

let () = set_global "setWord" (Js.wrap_callback (fun jsw -> set_word (Js.to_string jsw)))

let reduce w =
  let w_arr = parse_word_arr w in
  let red_w = Dehn.reduce ~rel:(!rel) w_arr in
  println (String.init (Array.length red_w) ~f:(fun i -> char_of_elt red_w.(i)))
  (*
  | Some red_w ->
  | None -> println "No reduction!" *)

let () = reduce "b"

let () = set_global "reduceWord" (Js.wrap_callback (fun jsw -> reduce (Js.to_string jsw)))

let w_equal w1 w2 =
  let w1_f, w2_f = parse_word w1, parse_word w2 in
  match Dehn.reduce ~rel:(!rel) 
    (Array.of_list (Free_group.mul w1_f (Free_group.inv w2_f))) with
  | [||] -> Js._true
  | _    -> Js._false

let () = set_global "eqWord" (Js.wrap_callback (fun jsw1 jsw2 -> w_equal (Js.to_string jsw1) (Js.to_string jsw2)))
