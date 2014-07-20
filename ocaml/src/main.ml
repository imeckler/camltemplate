open Core

module C = Cayley.Partial(struct
    let rel = Cayley.parse_word "abABcdCD"
  end)

let cg, init_state = C.create ()

let () = C.extend_to_depth cg [|'a'; 'b'; 'c'; 'd'|] init_state 5
let () = println "done"

let get_element_by_id s =
  match Js.Opt.to_option (Dom_html.document##getElementById(Js.string s)) with
  | None -> failwith "get_element_by_id: Not found" | Some x -> x

let () = set_global "g" cg.C.graph

let () = Graph.Sigma.display cg.C.graph (get_element_by_id "container")
let () = Graph.Sigma.start_force_layout cg.graph

