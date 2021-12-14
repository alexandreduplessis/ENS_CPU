open Netlist_ast
type oneram = {lram : int ; wlram : int ; mutable valeurs : value array}

let print_only = ref false
let number_steps = ref (-1)

let to_int b = if b=false then 0 else 1

let cle_mem = Array.fold_left (fun n b -> 2*n+ to_int b) 0 (* Ecriture en base 10 du nombre binaire obtenu en concaténant *)

let rec read_input typ = (* lecture des entrées *)
  let l = read_line () in
  match typ with
  |TBit -> VBit (not(l="0"))
  |TBitArray n -> VBitArray (Array.init n (fun k->not(l.[k]='0')))

let rec simul_onestep p vars ram rom compt = function
  |0 -> ()
  |n -> begin
  let read_arg = function Aconst c -> c | Avar a -> Hashtbl.find vars a in
  let all_to_array c = match read_arg c with VBit b -> [|b|] | VBitArray b -> b in
  print_string ("Etape "^ string_of_int (compt+1-n)); print_newline (); List.iter (fun x-> print_string (x^"= "); Hashtbl.add vars x (read_input (Env.find x p.p_vars))) p.p_inputs; (* lecture des entrées *)
    List.iter (fun (id,exp) -> let rep = match exp with
      | Earg x -> read_arg x
      | Ereg x -> Hashtbl.find vars x
      | Enot x -> let VBit vx = read_arg x in VBit (not vx)
      | Ebinop (bin, x, y) -> let VBit vx = read_arg x and VBit vy = read_arg y in
      begin match bin with
        |Or -> VBit (vx || vy)
        |Xor -> VBit ((vx || vy) && not(vx && vy))
        |And -> VBit (vx && vy)
        |Nand -> VBit (not(vx && vy)) end
      | Emux (cond, x, y) -> let VBit vcond = read_arg cond in if vcond then read_arg x else read_arg y
      | Erom (_, _, ra) -> let ara = all_to_array ra in rom.(cle_mem ara)
      | Eram (_, _,ra,_ ,_ ,_ ) -> let ara = all_to_array ra in let ar = Hashtbl.find ram id in (ar.valeurs).(cle_mem ara)
      | Eselect (i, x) -> let ax = all_to_array x in VBit ax.(i) 
      | Econcat (x, y) -> let ax = all_to_array x and ay = all_to_array y in VBitArray (Array.append ax ay)
      | Eslice (i1, i2, x) -> let ax = all_to_array x in VBitArray (Array.sub ax i1 (i2-i1+1))
    in Hashtbl.add vars id rep) p.p_eqs; (* on remet à jour (ou on ajoute) les variables modifiées *)
    List.iter (fun (id,exp) -> match exp with (* Toutes les écritures en RAM sont faites à la fin : on reparcourt les équations *)
      | Eram (_, _, _, we,wa,wd) -> let VBit vwe = read_arg we and vwa = all_to_array wa and vwd = read_arg wd in
      if vwe then let ar = Hashtbl.find ram id in (ar.valeurs).(cle_mem vwa) <- vwd;
      | _ -> ()) p.p_eqs;
    print_newline (); List.iter (fun x -> print_string (x^" : "); let sortie = (Hashtbl.find vars x) in begin match sortie with VBit s -> print_int (to_int s)|VBitArray s -> Array.iter (fun x -> print_int (to_int x)) s end; print_newline ()) p.p_outputs; print_newline(); (* affichage des sorties *)
    simul_onestep p vars ram rom compt (n-1) end (* lance la prochaine étape *)

let simulator program n =
   let hram = Hashtbl.create 0 in (* table de hachage contenant les différentes RAM* *)
   let lrom = ref 0 and wlrom = ref 0 in (* je suppose que je n'ai qu'une seule ROM *)
   List.iter (fun (id,exp) -> match exp with Erom (n1, n2, _)-> lrom := (1 lsl n1); wlrom := n2 | Eram (n1, n2, _, _, _, _)-> Hashtbl.add hram id {lram = (1 lsl n1); wlram = n2; valeurs = (Array.make (1 lsl n1) (VBitArray (Array.make n2 false)))} | _->()) program.p_eqs; (* recherche des équations RAM et ROM, et initialisation de la RAM *)
  let h = Hashtbl.create 0 in (* table de hachage des variables *)
  Env.iter (fun x t -> Hashtbl.add h x (match t with TBit -> VBit false | TBitArray at -> VBitArray (Array.make at false))) program.p_vars;
  simul_onestep program h hram (Array.init !lrom (fun i -> begin print_string ("ROM - mot "^(string_of_int i)^" ="); let t = read_input (TBitArray !wlrom) in (let VBitArray a = t in ()); t end)) n n (* on initialise toutes les variables, on demande la ROM en entrée *)

let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
