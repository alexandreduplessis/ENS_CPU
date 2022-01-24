open Netlist_ast
open String

let number_steps = ref (-1)

let read_arg x = match x with (* renvoie le nom de variable ou le bitset correspondant *)
   | Avar y -> y
   | Aconst (VBit b) -> if b then "bitset<1>{\"1\"}" else "bitset<1>{\"0\"}"
   | Aconst (VBitArray ba) -> 
   let bb = String.concat "" (Array.to_list (Array.map (fun x -> (string_of_int (Bool.to_int x))) ba)) in 
   let taille = Array.length ba in "bitset<" ^ (string_of_int taille) ^ ">{\"" ^ bb ^ "\"}" 

let rec simulation_pass1 p hashram (ident, expr) = ident ^ "=" ^ (match expr with
  | Earg x                       -> read_arg x
  | Ereg x                       -> "reg_" ^ x
  | Enot x                       -> "~" ^ read_arg x
  | Ebinop (Or,  x,  y)          -> read_arg x ^ "|" ^ read_arg y
  | Ebinop (And,  x,  y)         -> read_arg x ^ "&" ^ read_arg y
  | Ebinop (Xor,  x,  y)         -> read_arg x ^ "^" ^ read_arg y
  | Ebinop (Nand,  x,  y)        -> "~" ^ "(" ^ read_arg x ^ "&" ^ read_arg y ^ ")"
  | Econcat (x, y)               -> let b = read_arg x in let a = read_arg y in Printf.sprintf "bitset<(%s).size()>{(%s).to_string() + (%s).to_string()}" ident a b
  | Eslice (i1, i2, x)           -> let xx = (read_arg x) in Printf.sprintf "bitset<(%s).size()>{((%s).to_string()).substr (%s.size()- %d -1, %d)}" ident xx xx i2 (i2-i1+1)
  | Eselect (i, x)               -> Printf.sprintf "bitset<1>{(%s.to_string())[%s.size() - %d -1]}" (read_arg x) (read_arg x) i
  | Emux (choice, a, b)          -> Printf.sprintf "(%s == bitset<1>(1)) ? %s : %s" (read_arg choice) (read_arg a) (read_arg b)
  | Eram(addr_size, word_size, read_addr, _, _, _)-> let id = (Hashtbl.find hashram ident) in 
    let addr = read_arg read_addr in  (* on fait seulement la lecture *)
    Printf.sprintf "bitset<%d>{((%s).to_string()).substr ((%s).to_ulong()*%d, (%d))}" word_size id addr word_size word_size
  | Erom(addr_size, word_size, read_addr)-> let addr = read_arg read_addr in Printf.sprintf "rom[(%s).to_ulong()]" addr
  ) ^ ";\n"

let simulation_pass2 p hashram (ident, expr) = match expr with (* traite la maj des variables reg et l'écriture dans la ram *)
    | Ereg id -> "reg_" ^ id ^ " = " ^ id ^ ";\n"
    | Eram(_, word_size, _, write_enable, write_addr, data) -> let id = (Hashtbl.find hashram ident) in let waddr = (read_arg write_addr) in Printf.sprintf "if((%s).to_ulong()) {%s = bitset<(%s).size()>{((%s).to_string()).replace((%s).to_ulong()*%d, %d, (%s).to_string())};};\n" (read_arg write_enable) id id id waddr word_size word_size (read_arg data) (* toutes les écritures sont faites à la fin *)
    | _ -> ""

let entete =
"#include <iostream>
#include <bitset>
#include <fstream>
#include <string>
using namespace std;

int main(int argc, char** argv) {
"

let read_file file_name = (* retourne le nombre de lignes d'un fichier *)
  let in_channel = open_in file_name in
  let compt = ref 0 in
  try 
    while true do
      let _ = input_line in_channel in compt := !compt + 1
    done; !compt
  with End_of_file ->
     close_in in_channel; !compt

let read_inputs p fchan = List.iter (fun id -> Format.fprintf fchan (* lecture des entrées *)
	"\t\tcout << \"%s = ? \";\n\t\tcin >> %s;\n" id id) p.p_inputs

let print_outputs p fchan = List.iter (fun id -> Format.fprintf fchan (* affichage des sorties *)
	"cout << \"%s = \" << %s << \"\\n\";\n" id (read_arg (Avar id))) p.p_outputs

let compile filename =
  try
    let p = Netlist.read_file filename in
    let file_name = Filename.chop_suffix filename ".net" in
    let out = open_out (file_name ^ ".c") in
    let close_all () = close_out out in
    let fchan = Format.formatter_of_out_channel out in
    let p = Scheduler.schedule p in (* réorganise les équations avec le scheduler *)
    Format.fprintf fchan "%s" entete;
    let tailles = Hashtbl.create 50 in (* stocke les tailles de chaque variable d'entrée *)
    Env.iter (fun id ty -> match ty with
    	| TBit -> Hashtbl.add tailles id 1; Format.fprintf fchan "\tbitset<1> %s;\n" id
    	| TBitArray i -> Hashtbl.add tailles id i; Format.fprintf fchan "\tbitset<%d> %s;\n" i id) p.p_vars;
    let hashram = Hashtbl.create 17 in
    let taille_rom = read_file "rom" in (* lit le fichier rom pour connaître le nombre de lignes et initialiser le tableau c++ *)
    let i = ref 0 in (* pour compter les instructions ROM et lever une erreur si > 1 *)
    List.iter (fun (ident, expr) -> match expr with
   	| Ereg x -> let taille = Hashtbl.find tailles x in Format.fprintf fchan "\tbitset<%d> reg_%s = 0;\n" taille x; (* chaque variable aparaissant dans une instruction reg est en double, une pour le cycle courant, et une pour le cycle précédent *)
	| Eram(addr_size, word_size, read_addr, write_enable, write_addr, data) ->	   
	  Hashtbl.add hashram ident ("ram_" ^ ident);
	  Format.fprintf fchan "\tbitset<%d> %s = {0};\n" ((1 lsl addr_size)*word_size) ("ram_" ^ ident)  (* initialise les RAM *)
	| Erom(addr_size, word_size, read_addr) when !i=0 -> i:=1;
	  Format.fprintf fchan (* lit le fichier rom *)
"
   fstream newfile;
   newfile.open(\"rom\",ios::in);
   bitset<%d> rom [%d];
   string romstr;
   int compt = 0;
   if (newfile.is_open()){
      while(getline(newfile, romstr)){
         rom[compt] = bitset<%d> {romstr} ;
         compt +=1;
      }
	   newfile.close();
   };\n" word_size taille_rom word_size (* crée la ROM *)
	 | Erom _ -> failwith "Erreur : deux accès à la ROM"
    | _ -> ()
	) p.p_eqs;
    if (!number_steps = -1) then (* boucle à l'infini *)
      Format.fprintf fchan "%s" "\twhile (1) {\n"
    else (* l'utilisateur a précisé un nombre de steps *)
      Format.fprintf fchan "\tfor (int step = 0; step < %d; step++) {\n" !number_steps
    ;
    read_inputs p fchan;
    List.iter (fun eq -> Format.fprintf fchan "%s" (("\t\t")^(simulation_pass1 p hashram eq))) p.p_eqs; (* passe 1 de la simulation *)
    List.iter (fun eq -> Format.fprintf fchan "%s" (("\t\t")^(simulation_pass2 p hashram eq))) p.p_eqs; (* passe 2 pour reg et ram *)
    print_outputs p fchan;
    Format.fprintf fchan "%s" "\tcout.flush();"; (* permet la récupération des sorties par la clock *)
    Format.fprintf fchan "%s" "\t}\n\treturn 0;\n}\n";
    Format.fprintf fchan "@.";
    close_all ();
    let commande = "g++ " ^ (file_name ^ ".c") ^ " -o " ^ file_name in
    ignore (Unix.system commande)
  with
    | Netlist.Parse_error s -> Format.eprintf "Erreur : %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Nombres d'étapes à simuler."]
    compile
    ""
;;

main ()
