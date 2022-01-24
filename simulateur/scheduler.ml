open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp (id, expr) =
   let read_arg = (function
      | Avar x -> [x]
      | _ -> [])
   in match expr with
   | Earg x -> read_arg x
   | Enot x -> read_arg x
   | Ereg x -> [] (* pour éviter les cycles *)
   | Ebinop (_, x, y) -> (read_arg x) @ (read_arg y)
   | Emux (x, y, z) -> (read_arg x) @ (read_arg y) @ (read_arg z)
   | Eram (_, _ , x, _, _, _) -> (read_arg x)
   | Erom (_, _, x) | Eslice (_, _, x) -> read_arg x
   | Econcat (x, y) -> (read_arg x) @ (read_arg y)
   | Eselect (_, x) -> read_arg x;;

let schedule p =
   let graphe = mk_graph () in
   Env.iter (fun x _ -> add_node graphe x) p.p_vars;
   List.iter (fun (id, expr) -> List.iter (fun elt -> add_edge graphe elt id) (read_exp (id, expr))) p.p_eqs ;
   let rec attrib_eqs = function
      | [] -> []
      | d::f -> try (d, List.assoc d p.p_eqs) :: (attrib_eqs f) with Not_found -> attrib_eqs f
   in try {p_eqs = attrib_eqs (topological graphe); p_inputs = p.p_inputs; p_outputs = p.p_outputs; p_vars = p.p_vars}
   with Cycle -> raise Combinational_cycle
