open Core

let sub_cyclic arr pos len =
  let n = Array.length arr in
  if len > n
  then failwith "sub_cyclic: len > length of array"
  else Array.init len ~f:(fun i -> arr.((pos + i) mod n))

module Make (R : sig 
  type t
  val rel : t Free_group.t
end) = struct
(*
  let cyclic_permutations arr =
    let n = Array.length arr in
    Iterator.create n ~f:(fun start_i ->
      Array.init n ~f:(fun j -> arr.((start_i + j) mod n)))

*)

  (* TODO : Not used
  let cyclic_permutations =
    let n = Array.length rel_arr in
    Array.init n ~f:(fun start_i ->
      Array.init n ~f:(fun j -> rel_arr.((start_i + j) mod n)))
      *)


  let invert_array arr = let n = Array.length arr in
    Array.init n ~f:(fun i -> Free_group.inv_one arr.(n - 1 - i))

  let rel_arr         = Array.of_list R.rel
  let rel_len         = Array.length rel_arr
  let inv_rel_arr     = invert_array rel_arr
  let exposed_len     = (rel_len / 2) + 1 (* + (rel_len mod 2) *)
  let replacement_len = rel_len - exposed_len

  let find_in_range (start, fin) f =
    let rec loop i =
      if i > fin
      then None
      else if f i then Some i else loop (i + 1)
    in
    loop start

  let matches len (arr1, start1) (arr2, start2) =
    let n1, n2 = Array.length arr1, Array.length arr2 in
    let rec loop i =
      if i = len
      then true
      else arr1.((i + start1) mod n1) = arr2.((i + start2) mod n2) && loop (i + 1)
    in
    loop 0

  let () = assert (matches 3 ([|1;2;3;4|], 1) ([|3;4;1;2|], 3))

  let find_reduction w =
    let n = Array.length w in
    let rec loop start_pos =
      if start_pos >= n
      then None
      else
        match find_in_range (0, rel_len) (fun rel_start_pos ->
          matches exposed_len (w, start_pos) (rel_arr, rel_start_pos))
        with
        | Some rel_start_pos -> Some (
            `Relator, start_pos, rel_start_pos)
        | None -> (match find_in_range (0, rel_len) (fun inv_rel_start_pos ->
            matches exposed_len (w, start_pos) (inv_rel_arr, inv_rel_start_pos)) with
          | Some inv_rel_start_pos -> Some (
              `Inv_relator, start_pos, inv_rel_start_pos)
          | None -> loop (start_pos + 1))
    in
    loop 0

  let show_reduction = function
    | None -> "None"
    | Some (rel_or_inv, w_start_pos, rel_start_pos) ->
        Printf.sprintf "Some (%s, %d, %d)"
          (if rel_or_inv = rel_arr then "Rel" else "Inv")
          w_start_pos rel_start_pos

  let reduce_once w =
    match find_reduction w with
    | None -> None
    | Some (rel_or_inv, w_start_pos, rel_start_pos) ->
        let other = match rel_or_inv with `Relator -> inv_rel_arr | `Inv_relator -> rel_arr in
        let replacement_start_pos = rel_len - rel_start_pos in
        Some (
          Array.append
            (sub_cyclic w (w_start_pos + exposed_len) (Array.length w - exposed_len))
            (sub_cyclic other replacement_start_pos replacement_len))
(*             (invert_array (sub_cyclic arr (rel_start_pos + exposed_len) replacement_len))) *)

  let rec reduce w =
    let w = Array.of_list (Free_group.reduce (Array.to_list w)) in
    match reduce_once w with
    | None -> w
    | Some w' -> reduce w'

  let equal w1 w2 =
    let n1 = Array.length w1 in let n2 = Array.length w2 in
    Array.length (
      reduce (
        Array.init (n1 + n2) ~f:(fun i ->
          if i < n1 then w1.(i) else Free_group.inv_one w2.(n2 - 1 - (i - n1)))))
      = 0

(*
  let blit_cyclic ~src ~src_pos ~dest ~dest_pos ~len =
    let dest_len = Array.length dest in
    let src_len  = Array.length src in
    let rec loop i =
      if i = len then ()
      else begin
        dest.((dest_pos + i) mod dest_len) <- src.((src_pos + i) mod src_len);
        loop (i + 1)
      end
    in
    loop 0

      let rel = match rel_or_inv with `Relator -> rel_arr | `Inv_relator -> inv_rel_arr in
      blit_cyclic ~src:rel ~dest:w ~src_pos:rel_start_pos ~dest_pos:w_start_pos ~len:replacement_len;
      true

*)
end

let reduce : type a. rel:(a Free_group.t) -> a Free_group.Elt.t array -> a Free_group.Elt.t array = fun ~rel w ->
  let module M = Make(struct type t = a let rel = rel end) in
  M.reduce w

