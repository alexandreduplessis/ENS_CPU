open Format
open Lexing
open Ast

module Smap = Map.Make(String);;

let labelMap = ref Smap.empty;;

let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

let print_line s = print_string (s^"\n")

let options =
  ["-o", Arg.String (set_file ofile),
   "<file>  Pour indiquer le mom du fichier de sortie"]

let usage = "usage: assembleur [option] file.s"

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let rec int_to_bin n = if n = 0 then "" else match n mod 2 with
    0 -> (int_to_bin (n/2))^"0"
  | 1 -> (int_to_bin (n/2))^"1";;

let rec imm_to_bin n = if n >= 32768 then imm_to_bin (n - 65536)
  else
    if n < 0 then (int_to_bin (n + 65536))
	else let s = (int_to_bin n) in (String.make (16 - String.length s) '0')^s

let reg_to_bin = function
  | "r0" -> "0000"
  | "r1" -> "0001"
  | "r2" -> "0010"
  | "r3" -> "0011"
  | "r4" -> "0100"
  | "r5" -> "0101"
  | "r6" -> "0110"
  | "r7" -> "0111"
  | "r8" -> "1000"
  | "r9" -> "1001"
  | "ra" -> "1010"
  | "rb" -> "1011"
  | "rc" -> "1100"
  | "rd" -> "1101"
  | "re" -> "1110"
  | "rf" -> "1111";;

let r4d_to_bin (rd, ra, rb, rr) =
  (reg_to_bin rd)^(reg_to_bin ra)^(reg_to_bin rb)^(reg_to_bin rr)^(String.make 12 '0')^"\n"

let r3d_to_bin (ra, rb, rd) =
  (reg_to_bin rd)^(reg_to_bin ra)^(reg_to_bin rb)^(String.make 16 '0')^"\n"

let r2d_to_bin (ra, rd) =
  (reg_to_bin rd)^(reg_to_bin ra)^(String.make 20 '0')^"\n"

let r2Id_to_bin (ra, rd, imm) = match imm with
	| Jconst imm -> (reg_to_bin rd)^(reg_to_bin ra)^(String.make 4 '0')^(imm_to_bin imm)^"\n"
  | Jlabel label -> if Smap.mem label !labelMap then (reg_to_bin rd)^(reg_to_bin ra)^(String.make 4 '0')^(imm_to_bin (Smap.find label !labelMap))^"\n"
	else raise (Ast.Syntax_error ("La label "^label^" n'est pas defini."))
  

let rId_to_bin (rd, imm) =
  (reg_to_bin rd)^(String.make 8 '0')^(imm_to_bin imm)^"\n"
  
let rImm_to_bin  = function 
  | Jconst imm -> (String.make 12 '0')^(imm_to_bin imm)^"\n"
  | Jlabel label -> if Smap.mem label !labelMap then (String.make 12 '0')^(imm_to_bin (Smap.find label !labelMap))^"\n"
	else raise (Ast.Syntax_error ("La label "^label^" n'est pas defini."))



let compile_instr = function
  | Add (rd, ra, rb) -> "0000000000"^(r3d_to_bin (rd, ra, rb))
  | Sub (rd, ra, rb) -> "0000000001"^(r3d_to_bin (rd, ra, rb))
  | Mul (rd, ra, rb)  -> "0000000010"^(r3d_to_bin (rd, ra, rb))
  | Div (rd, ra, rb, rr) -> "0000000011"^(r4d_to_bin (rd, ra, rb, rr))
  | Xor (rd, ra, rb) -> "0000000100"^(r3d_to_bin (rd, ra, rb))
  | Or (rd, ra, rb) -> "0000000101"^(r3d_to_bin (rd, ra, rb))
  | And (rd, ra, rb) -> "0000000110"^(r3d_to_bin (rd, ra, rb))
  | Shiftl (rd, ra, rb) -> "0000001000"^(r3d_to_bin (rd, ra, rb))
  | Shiftr (rd, ra, rb) -> "0000001001"^(r3d_to_bin (rd, ra, rb))
  | Addi (rd, ra, imm) -> "0000010000"^(r2Id_to_bin (rd, ra, imm))
  | Subi (rd, ra, imm) -> "0000010001"^(r2Id_to_bin (rd, ra, imm))
  | Xori (rd, ra, imm) -> "0000010100"^(r2Id_to_bin (rd, ra, imm))
  | Ori (rd, ra, imm) -> "0000010101"^(r2Id_to_bin (rd, ra, imm))
  | Andi (rd, ra, imm) -> "0000010110"^(r2Id_to_bin (rd, ra, imm))
  | Shiftli (rd, ra, imm) -> "0000011000"^(r2Id_to_bin (rd, ra, imm))
  | Shiftri (rd, ra, imm) -> "0000011001"^(r2Id_to_bin (rd, ra, imm))
  | Not (rd, ra) -> "0000000111"^(r2d_to_bin (rd, ra))
  | Load (rd, ra, imm) -> "0001000000"^(r2Id_to_bin (rd, ra, imm))
  | Limm (rd, imm) -> "0000110000"^(rId_to_bin (rd, imm))
  | Store (rd, ra, imm) -> "0001000001"^(r2Id_to_bin (rd, ra, imm))
  | Move (rd, ra) -> "0000100000"^(r2d_to_bin (rd, ra))
  | Beq (rd, ra, imm) -> "0100000001"^(r2Id_to_bin (rd, ra, imm))
  | Bne (rd, ra, imm) -> "0110000001"^(r2Id_to_bin (rd, ra, imm))
  | Blt (rd, ra, imm) -> "1000000001"^(r2Id_to_bin (rd, ra, imm))
  | Ble (rd, ra, imm) -> "1010000001"^(r2Id_to_bin (rd, ra, imm))
  | Jmp (imm) -> "0010000000"^(rImm_to_bin (imm))
  | _ -> "";;

let rec compile ff prog =
  let i = ref 0 in
  List.iter ( fun instr -> match instr with 
      Label s -> labelMap := Smap.add s !i !labelMap;
	| _ -> i := !i + 1;
  ) prog;
  List.iter (fun instr -> let s = (compile_instr instr) in
	Format.fprintf ff "%s" s) prog


let () =
  Arg.parse options (set_file ifile) usage;

  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end;

  if not (Filename.check_suffix !ifile ".s") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .s\n@?";
    Arg.usage options usage;
    exit 1
  end;

  if !ofile="" then ofile := "rom";

  let f = open_in !ifile in

  let buf = Lexing.from_channel f in

  try
    let prog = Parser.prog Lexer.token buf in
    close_in f;
	
	let out = (open_out !ofile) in
	
	let ff = Format.formatter_of_out_channel out in
	
    compile ff prog;
	close_out out;
	
  with
    | Lexer.Lexing_error c ->
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse lexicale: %c@." c;
	exit 1
	| Ast.Syntax_error s ->
	localisation (Lexing.lexeme_start_p buf);
	print_string s;
	exit 1
    | Parser.Error ->
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse syntaxique@.";
	exit 1
    | Lexer.Error s -> 
	localisation (Lexing.lexeme_start_p buf);
	print_line s;
	exit 1




