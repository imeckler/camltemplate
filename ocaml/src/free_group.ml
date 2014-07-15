open Core

module Elt = struct
  type 'a t = [`In of 'a | `Inv of 'a]
end

type 'a t = 'a Elt.t list

let mul = (@)

let id = []

let inj x = [`In x]

let inj_inv x = [`Inv x]

let product  = Array.to_list
let to_array = Array.of_list
let to_list x = x

let inv_one = function
  | `In x  -> `Inv x
  | `Inv x -> `In x

let inv = List.rev_map ~f:inv_one

let reduce xs =
  List.fold_left xs ~init:[] ~f:(fun acc x -> match acc with
    | []     -> [x]
    | (l::ls) -> if inv_one l = x then ls else x::acc
  ) |> List.rev

let unelt = function
  | `In x | `Inv x -> x

let kill gs = List.filter ~f:(fun x -> not (Array.mem gs (unelt x)))

module Infix = struct
  let ( * ) = mul
end

