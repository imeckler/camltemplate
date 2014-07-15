open Core

type word_arr = char Free_group.Elt.t array

let is_upper c = let n = Char.code c in
  n >= 65 && n <= 90

let elt_of_char c =
  if is_upper c
  then `Inv (Char.lowercase c)
  else `In c

let char_of_elt = function
  | `In c -> c
  | `Inv c -> Char.uppercase c

let parse_word w : char Free_group.t =
  List.init (String.length w) (fun i -> elt_of_char w.[i])

let parse_word_arr w : word_arr =
  Array.init (String.length w) (fun i -> elt_of_char w.[i])

let word_arr_to_string w_arr =
  String.init (Array.length w_arr) ~f:(fun i -> char_of_elt w_arr.(i))

module Partial (M : sig
  val rel : char Free_group.t
end) = struct
  module D = Dehn.Make (struct include M type t = char end)

  type t =
    { nodes : ((unit, char) Graph.Node.t * word_arr) Stringtbl.t
    ; graph : (unit, char) Graph.Mutable.t
    }

  exception Match of (unit, char) Graph.Node.t * string

  let find_node {nodes; graph=_} w =
    try
      Stringtbl.iter nodes ~f:(fun ~key:representative ~data:(node, w'_arr) ->
        if D.equal w'_arr w
        then raise (Match (node, representative))
        else ());
        None
    with Match (node, representative) -> (
      if String.length representative > Array.length w
      then begin
        (* We've found a shorter representative for the same group element,
         * so we may as well swap them to make future calls to D.equal more
         * efficient. *)
        Stringtbl.remove nodes representative;
        Stringtbl.add nodes ~key:(word_arr_to_string w) ~data:(node, w)
      end;
      Some node)

  let extend 

end

