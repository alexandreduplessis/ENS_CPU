open Netlist_ast

let number_steps = ref (-1)

let ident_length p ident =
  try
    let t = Env.find ident p.p_vars in
    match t with
      | TBit -> 1
      | TBitArray n -> n
  with
      Not_found -> failwith ("arg not found: " ^ ident)

let arg_length p = function
  | Avar id -> ident_length p id
  | Aconst (VBit _) -> 1
  | Aconst (VBitArray ba) -> Array.length ba

let sl x shift_amount =
  assert (shift_amount >= 0);
  if shift_amount >= 64 then 0L
  else Int64.shift_left x shift_amount

let mask a b =
  Int64.(to_string (sub (sl one b) (sl one a)))

let shift_left arg amount =
  if amount = 0 then arg else "((" ^ arg ^ ")<<" ^ (string_of_int amount) ^ ")"

let shift_right arg amount =
  assert (amount >= 0);
  if amount = 0 then arg else "((" ^ arg ^ ")>>" ^ (string_of_int amount) ^ ")"

let read_arg x = match x with
   | Avar y -> y
   | Aconst (VBit b) -> if b then "1" else "0"
   | Aconst (VBitArray ba) -> Int64.(to_string (Array.fold_left (fun a b -> logor (sl a 1) (if b then one else zero)) zero ba))

let rec simulate_eqs p memories(ident, expr) = ident ^ "=" ^ (match expr with
  | Earg x                       -> read_arg x
  | Ereg x                       -> "reg_"^x
  | Enot x                       -> "~"^read_arg x
  | Ebinop (Or,  x,  y)          -> read_arg x ^ "|" ^ read_arg y
  | Ebinop (And,  x,  y)         -> read_arg x ^ "&" ^ read_arg y
  | Ebinop (Xor,  x,  y)         -> read_arg x ^ "^" ^ read_arg y
  | Ebinop (Nand,  x,  y)        -> "~" ^ "(" ^ read_arg x ^ "&" ^ read_arg y ^ ")"
  | Econcat (x, y)               -> (shift_left (read_arg x) (arg_length p y)) ^ "|" ^ (read_arg y)
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

void read_file(char* filename, int addr_size, int word_size, unsigned long long int* array) {
    FILE* fp;
    fp = fopen(filename, \"r\");
    if (fp == NULL) {
        perror(\"Error while opening ROM\\n\");
        exit(1);
    }
    int num_chars = ((1 << addr_size) * word_size + 7) / 8;
    unsigned long long int current = 0;
    int num_read_bits = 0;
    int read_index = 0;
    for (int i = 0; i < num_chars; i++) {
        current = (current << 8) | fgetc(fp);
        num_read_bits += 8;
        while (num_read_bits >= word_size) {
            unsigned long long int mask = (num_read_bits == 64) ? 0 : (1 << num_read_bits);
            num_read_bits -= word_size;
            mask -= 1 << num_read_bits;
            array[read_index] = (current & mask) >> num_read_bits;
            read_index++;
        }
    }
}

int rom_id = 1;
int main(int argc, char** &argv) {
"

let read_inputs p ff = List.iter (fun id -> Format.fprintf ff
	"printf(\"%s = ?\\n\"); scanf(\"%%llu\", &%s);\n" id id) p.p_inputs

let print_outputs p ff = List.iter (fun id -> Format.fprintf ff
	"printf(\"%s = %%llu\\n\", %s);\n" id (read_arg (Avar id))) p.p_outputs

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
    Env.iter (fun id _ -> Format.fprintf ff "unsigned long long int %s = 0;\n" id) p.p_vars;
    let memories = Hashtbl.create 17 in
    List.iter (fun (ident, expr) -> match expr with
   | Ereg x -> Format.fprintf ff "unsigned long long int reg_%s = 0;\n" x;
	| Eram(addr_size, word_size, read_addr, write_enable, write_addr, data) ->	   
	  Hashtbl.add memories ident ("ram_" ^ ident);
	  Format.fprintf ff "unsigned long long int %s[%d] = {0};\n" ("ram_" ^ ident) (1 lsl addr_size) (* initialise les RAM *)
	| Erom(addr_size, word_size, read_addr) ->
	  Hashtbl.add memories ident ("rom_" ^ ident);
	  Format.fprintf ff "unsigned long long int %s[%d];\n" ("rom_" ^ ident) (1 lsl addr_size);
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
