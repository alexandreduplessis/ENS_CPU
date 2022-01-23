open Netlist_ast
open String

let number_steps = ref (-1)

let read_arg x = match x with
   | Avar y -> y
   | Aconst (VBit b) -> if b then "bitset<1>{\"1\"}" else "bitset<1>{\"0\"}"
   | Aconst (VBitArray ba) -> 
   let bb = String.concat "" (Array.to_list (Array.map (fun x -> (string_of_int (Bool.to_int x))) ba)) in 
   let taille = Array.length ba in "bitset<" ^ (string_of_int taille) ^ ">{" ^ bb ^ "}" 

let rec simulate_eqs p memories(ident, expr) = ident ^ "=" ^ (match expr with
  | Earg x                       -> read_arg x
  | Ereg x                       -> "reg_" ^ x
  | Enot x                       -> "~" ^ read_arg x
  | Ebinop (Or,  x,  y)          -> read_arg x ^ "|" ^ read_arg y
  | Ebinop (And,  x,  y)         -> read_arg x ^ "&" ^ read_arg y
  | Ebinop (Xor,  x,  y)         -> read_arg x ^ "^" ^ read_arg y
  | Ebinop (Nand,  x,  y)        -> "~" ^ "(" ^ read_arg x ^ "&" ^ read_arg y ^ ")"
  | Econcat (x, y)               -> let a = read_arg x in let b = read_arg y in Printf.sprintf "bitset<(%s).size()>{(%s).to_string() + (%s).to_string()}" ident a b
  | Eslice (i1, i2, x)           -> Printf.sprintf "bitset<(%s).size()>{((%s).to_string()).substr (%d, %d)}" ident (read_arg x) i1 (i2-i1+1)
  | Eselect (i, x)               -> Printf.sprintf "bitset<1>{(%s.to_string())[%d]}" (read_arg x) i
  | Emux (choice, a, b)          -> Printf.sprintf "(%s == bitset<1>(1)) ? %s : %s" (read_arg choice) (read_arg b) (read_arg a)
  | Eram(addr_size, word_size, read_addr, _, _, _)-> let id = (Hashtbl.find memories ident) in 
    let addr = read_arg read_addr in  
    Printf.sprintf "bitset<%d>{((%s).to_string()).substr ((%s).to_ulong()*%d, (%d))}" word_size id addr word_size word_size 
  (* a optimiser en faisant la conversion to_ulong en ocaml *)
  | Erom(addr_size, word_size, read_addr)-> let id = (Hashtbl.find memories ident) in let addr = read_arg read_addr in Printf.sprintf "bitset<%d>{((%s).to_string()).substr ((%s).to_ulong()*%d, %d)}" word_size id addr word_size word_size
  (* a optimiser en faisant la conversion to_ulong en ocaml *)
  | _                            -> failwith "not implemented"
  ) ^ ";\n"

let compile_eq2 p memories (ident, expr) = match expr with
    | Ereg id -> "reg_" ^ id ^ " = " ^ id ^ ";\n"
    | Eram(_, word_size, _, write_enable, write_addr, data) -> let id = (Hashtbl.find memories ident) in let waddr = (read_arg write_addr) in Printf.sprintf "if((%s).to_ulong()) {%s = bitset<(%s).size()>{((%s).to_string()).replace((%s).to_ulong()*%d, %d, (%s).to_string())};};\n" (read_arg write_enable) id id id waddr word_size word_size (read_arg data)
    | _ -> ""

let header =
"#include <iostream>
#include <bitset>
#include <fstream>
#include <string>
using namespace std;

string read_rom(){
	fstream newfile;
  	newfile.open(\"rom\",ios::in);
  	string rom;
  	if (newfile.is_open()){
		getline(newfile, rom);
		newfile.close();
   	}
   	return rom;
}

int main(int argc, char** argv) {
"

let read_inputs p ff = List.iter (fun id -> Format.fprintf ff
	"\t\tcout << \"%s = ? \";\n\t\tcin >> %s;\n" id id) p.p_inputs


let print_outputs p ff = List.iter (fun id -> Format.fprintf ff
	"cout << \"%s = \" << %s << \"\\n\";\n" id (read_arg (Avar id))) p.p_outputs

let compile filename =
  try
    let p = Netlist.read_file filename in
    let out_name = (Filename.chop_suffix filename ".net") ^ ".c" in
    let out_exe_name = Filename.chop_suffix filename ".net" in
    let out = open_out out_name in
    let close_all () =
      close_out out
    in
    let ff = Format.formatter_of_out_channel out in
    let p = Scheduler.schedule p in
    Format.fprintf ff "%s" header;
    let tailles = Hashtbl.create 50 in (* stocke les tailles de chaque variable d'entrée *)
    Env.iter (fun id ty -> match ty with
    	| TBit -> Hashtbl.add tailles id 1; Format.fprintf ff "\tbitset<1> %s;\n" id
    	| TBitArray i -> Hashtbl.add tailles id i; Format.fprintf ff "\tbitset<%d> %s;\n" i id) p.p_vars;
    let memories = Hashtbl.create 17 in
    let i = ref 0 in
    List.iter (fun (ident, expr) -> match expr with
   	| Ereg x -> let taille = Hashtbl.find tailles x in Format.fprintf ff "\tbitset<%d> reg_%s = 0;\n" taille x;
	| Eram(addr_size, word_size, read_addr, write_enable, write_addr, data) ->	   
	  Hashtbl.add memories ident ("ram_" ^ ident);
	  Format.fprintf ff "\tbitset<%d> %s = {0};\n" ((1 lsl addr_size)*word_size) ("ram_" ^ ident)  (* initialise les RAM *)
	| Erom(addr_size, word_size, read_addr) when !i=0 -> i:=1; (* ne mettre qu'une rom, la gestion ici paraît bizarre *)
	  Hashtbl.add memories ident ("rom_" ^ ident);
	  Format.fprintf ff "\tbitset<(%d)> rom_%s = bitset<(%d)>{read_rom()};\n" ((1 lsl addr_size)*word_size) ident (1 lsl addr_size) (* crée la ROM *)
	 | Erom _ -> failwith "Erreur : deux accès à la ROM"
    | _ -> ()
	) p.p_eqs;
    if (!number_steps = -1) then
      Format.fprintf ff "%s" "\twhile (1) {\n"
    else
      Format.fprintf ff "\tfor (int step = 0; step < %d; step++) {\n" !number_steps
    ;
    read_inputs p ff;
    List.iter (fun eq -> Format.fprintf ff "%s" (("\t\t")^(simulate_eqs p memories eq))) p.p_eqs;
    List.iter (fun eq -> Format.fprintf ff "%s" (("\t\t")^(compile_eq2 p memories eq))) p.p_eqs;
    print_outputs p ff;
    Format.fprintf ff "%s" "\t}\n\treturn 0;\n}\n";
    Format.fprintf ff "@.";
    close_all ();
    let command = "g++ " ^ out_name ^ " -o " ^ out_exe_name ^ "; ./" ^ out_exe_name in
    ignore (Unix.system command)
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Nombres d'étapes à simuler."]
    compile
    ""
;;

main ()
