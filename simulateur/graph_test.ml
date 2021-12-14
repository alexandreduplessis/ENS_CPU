open Graph

type graph' = int list array

let g1 = [| [1]; [1]; [1] |]
let g2 = [| []; [2]; [3]; [4]; [0; 1]; |]
let g3 = [| [1]; [2]; [3]; [] |]
let g4 = [| [3]; [0; 2; 4]; [3]; []; [6]; [1; 4; 7]; [3; 8]; [6; 9]; []; [8] |]

let tests : (graph' * bool) list = [g1, true; g2, true; g3, false; g4, false]

let graph_of_graph' a =
  let g = mk_graph () in
  for i = 0 to Array.length a - 1 do
    add_node g i
  done;
  Array.iteri (fun src -> List.iter (add_edge g src)) a;
  g


let check_topo_on g =
  let l = topological (graph_of_graph' g) in
  let inverse_l = Array.map (fun _ -> 0) g in
  List.iteri (fun i node -> inverse_l.(node) <- i) l;
  Array.for_all Fun.id (Array.mapi (fun i lst -> List.for_all (fun dst -> inverse_l.(i) < inverse_l.(dst)) lst) g)


let test i (g, b) =
  let g' = graph_of_graph' g in

  Format.printf "Test %d:\n- has_cycle: %s\n- topological_sort: %s\n"
    i
    (if has_cycle g' = b then "OK" else "FAIL")
    (if b then "(skipped because of the presence of a cycle)"
    else
        try if check_topo_on g then "OK" else "FAIL"
        with Cycle -> "FAIL (no cycle here)"
        | e -> Format.eprintf "FAIL (unknown exception)"; raise e)

let () = List.iteri test tests

