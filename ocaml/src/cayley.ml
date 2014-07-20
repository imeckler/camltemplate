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

module Partial (D : sig
  val reduce : word_arr -> word_arr
  val equal  : word_arr -> word_arr -> bool
end) = struct
  module G = Graph.Sigma

  type t =
    { nodes : ((unit, char) G.Node.t * word_arr) Stringtbl.t
    ; graph : (unit, char) G.t
    }

  type state =
    { word_arr : word_arr
    ; node     : (unit, char) G.Node.t
    }

  let create () =
    let graph = G.create () in
    let node  = G.add_node graph () in
    let nodes = Stringtbl.create () in
    Stringtbl.add nodes ~key:"" ~data:(node, [||]);
    ({graph; nodes}, {word_arr = [||]; node})
  ;;

  let find_node =
    let module M = struct exception Match of (unit, char) G.Node.t * string end in
    fun {nodes; graph=_} w ->
    try
      Stringtbl.iter nodes ~f:(fun ~key:representative ~data:(node, w'_arr) ->
        if D.equal w'_arr w
        then raise (M.Match (node, representative))
        else ());
        None
    with M.Match (node, representative) -> (
      if String.length representative > Array.length w
      then begin
        (* We've found a shorter representative for the same group element,
         * so we may as well swap them to make future calls to D.equal more
         * efficient. *)
        Stringtbl.remove nodes representative;
        Stringtbl.add nodes ~key:(word_arr_to_string w) ~data:(node, w)
      end;
      Some node)
  ;;

  let str_snoc s c =
    let n = String.length s in
    let t = String.create (n + 1) in
    for i = 0 to n - 1 do t.[i] <- s.[i] done;
    t.[n] <- c;
    t
  ;;

  let arr_snoc arr x =
    let arr' = Array.copy arr in
    Array.push x arr';
    arr'
  ;;

  let extend ({nodes; graph} as t) curr c =
    let w_arr  = arr_snoc curr.word_arr c in
    let w_node = match find_node t w_arr with
      | Some node -> node
      | None      -> (
        let node = G.add_node graph () in
        Stringtbl.add nodes ~key:(word_arr_to_string w_arr) ~data:(node, w_arr);
        node)
    in
    { word_arr = w_arr; node = w_node }
  ;;

  let rec extend_to_depth t gens curr d =
    if d = 0
    then ()
    else Array.iter gens ~f:(fun c ->
      extend_to_depth t gens (extend t curr (`In c)) (d - 1);
      extend_to_depth t gens (extend t curr (`Inv c)) (d - 1))
  ;;
end

