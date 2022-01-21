open Netlist_ast
open String

let number_steps = ref (-1)

let read_arg x = match x with
   | Avar y -> y
   | Aconst (VBit b) -> if b then "bitset<1>{\"1\"}" else "bitset<1>{\"0\"}"
   | Aconst (VBitArray ba) -> let bb = String.concat "" (Array.map (fun x -> string_of_int (to_int x)) ba) let taille = Array.length ba in "bitset<" ^ (string_of_int taille) ^ ">{" ^ bb ^ "}" 

let rec simulate_eqs p memories(ident, expr) = ident ^ "=" ^ (match expr with
  | Earg x                       -> read_arg x
  | Ereg x                       -> "reg_" ^ x
  | Enot x                       -> "~" ^ read_arg x
  | Ebinop (Or,  x,  y)          -> read_arg x ^ "|" ^ read_arg y
  | Ebinop (And,  x,  y)         -> read_arg x ^ "&" ^ read_arg y
  | Ebinop (Xor,  x,  y)         -> read_arg x ^ "^" ^ read_arg y
  | Ebinop (Nand,  x,  y)        -> "~" ^ "(" ^ read_arg x ^ "&" ^ read_arg y ^ ")"
  | Econcat (x, y)               -> match x with
  	| "bitset<%s.size()+>  " (shift_left (read_arg x) (arg_length p y)) ^ "|" ^ (read_arg y)
  | Eslice (i1, i2, x)           -> (shift_right (read_arg x) ((arg_length p x) - i2 - 1)) ^ "&" ^ (mask 0 (i2 - i1 + 1))
  | Eselect (i, x)               -> (shift_right (read_arg x) ((arg_length p x) - i - 1)) ^ "&1"
  | Eram(_, _, read_addr, _, _, _)       -> (Hashtbl.find memories ident) ^ "[" ^ (read_arg read_addr) ^ "]"
  | Erom(_, _, read_addr)           -> (Hashtbl.find memories ident) ^ "[" ^ (read_arg read_addr) ^ "]"
  | _                            -> failwith "not implemented"
  ) ^ "\n;"

let compile_eq2 p memories (ident, expr) = match expr with
    | Ereg id -> "reg_" ^ id ^ " = " ^ id ^ ";\n" 
    | Eram(_, _, _, write_enable, write_addr, data) -> (Hashtbl.find memories ident) ^ "[" ^ (read_arg write_addr) ^ "]^=((-" ^ (read_arg write_enable) ^ ")&(" ^ (read_arg data) ^ "^" ^ (Hashtbl.find memories ident) ^ "[" ^ (read_arg write_addr) ^ "]));\n"
    | _ -> ""

let header =
"#include <stdio.h>
#include <stdlib.h>
#include <bitset>


void scan_input(


void read_file(char* filename, int addr_size, int word_size, bitset* array) {
        }
    }
}

int rom_id = 1;
int main(int argc, char** argv) {
"

let read_inputs p ff = List.iter (fun id -> Format.fprintf ff
	"cout << \"%s = ? \"; cin >> %s;\n" id id) p.p_inputs


let print_outputs p ff = List.iter (fun id -> Format.fprintf ff
	"cout << \"%s = \" << %s;\n" id (read_arg (Avar id))) p.p_outputs

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
     with
    let tailles = Hashtbl.create 50 in (* stocke les tailles de chaque variable d'entrée *)
    Env.iter (fun id ty -> match ty with
    	| TBit -> Hashtbl.add tailles id 1; Format.fprintf ff "bitset<1> %s = 0;\n" id
    	| TBitArray i -> Hashtbl.add tailles id i; Format.fprintf ff "bitset<%d> %s = 0;\n" i id) p.p_vars;
    let memories = Hashtbl.create 17 in
    let i = ref 0 in
    List.iter (fun (ident, expr) -> match expr with
   	| Ereg x -> let taille = Hashtbl.find tailles x in Format.fprintf ff "bitset<%d> reg_%s = 0;\n" taille x;
	| Eram(addr_size, word_size, read_addr, write_enable, write_addr, data) ->	   
	  Hashtbl.add memories ident ("ram_" ^ ident);
	  Format.fprintf ff "bitset<%d> = {0};\n" (1 lsl addr_size) ("ram_" ^ ident)  (* initialise les RAM *)
	| Erom(addr_size, word_size, read_addr) when !i=0 -> i:=1; 
	  Hashtbl.add memories ident ("rom_" ^ ident);
	  Format.fprintf ff "bitset<(%d)> %;\n" (1 lsl addr_size) ("rom_" ^ ident);
	  Format.fprintf ff "read_file(argv[rom_id++], %d, %d, %s);\n" addr_size word_size ("rom_" ^ ident) (* crée la ROM *)
    | _ -> ()
	) p.p_eqs;
    if (!number_steps = -1) then
      Format.fprintf ff "%s" "while (1) {\n"
    else
      Format.fprintf ff "for (int step = 0; step < %d; step++) {\n" !number_steps
    ;
    read_inputs p ff;
    List.iter (fun eq -> Format.fprintf ff "%s" (simulate_eqs p memories eq)) p.p_eqs;
    List.iter (fun eq -> Format.fprintf ff "%s" (compile_eq2 p memories eq)) p.p_eqs;
    print_outputs p ff;
    Format.fprintf ff "%s" "}\nreturn 0;\n}\n";
    Format.fprintf ff "@.";
    close_all ();
    let command = "gcc -std=c99 -O2 " ^ out_name ^ " -o " ^ out_exe_name in
    ignore (Unix.system command)
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
