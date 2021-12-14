open Format
open Lexing
open Ast

let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

let print_line s = print_string (s^"\n")

let options =
  ["-o", Arg.String (set_file ofile),
   "<file>  Pour indiquer le mom du fichier de sortie"]

let usage = "usage: arithc [option] file.s"

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let rec int_to_bin n = if n = 0 then "" else match n mod 2 with
    0 -> (int_to_bin (n/2))^"0"
  | 1 -> (int_to_bin (n/2))^"1";;

let rec imm_to_bin n = if n >= 65536 then imm_to_bin (n mod 65536)
  else let s = int_to_bin n in (String.make (16 - String.length s) '0')^s

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
  (reg_to_bin rd)^(reg_to_bin ra)^(reg_to_bin rb)^(reg_to_bin rr)^(String.make 11 '0')

let r3d_to_bin (rd, ra, rb) =
  (reg_to_bin rd)^(reg_to_bin ra)^(reg_to_bin rb)^(String.make 15 '0')

let r2d_to_bin (rd, ra) =
  (reg_to_bin rd)^(reg_to_bin ra)^(String.make 19 '0')

let r2Id_to_bin (rd, ra, imm) =
  (reg_to_bin rd)^(reg_to_bin ra)^(imm_to_bin imm)^(String.make 3 '0')

let rId_to_bin (rd, imm) =
  (reg_to_bin rd)^(String.make 4 '0')^(imm_to_bin imm)^(String.make 3 '0')


let compile_instr = function
  | Add (rd, ra, rb) -> "00001"^(r3d_to_bin (rd, ra, rb))
  | Sub (rd, ra, rb) -> "00010"^(r3d_to_bin (rd, ra, rb))
  | Mul (rd, ra, rb)  -> "00011"^(r3d_to_bin (rd, ra, rb))
  | Div (rd, ra, rb, rr) -> "00000"^(r4d_to_bin (rd, ra, rb, rr))
  | Xor (rd, ra, rb) -> "00101"^(r3d_to_bin (rd, ra, rb))
  | Or (rd, ra, rb) -> "00110"^(r3d_to_bin (rd, ra, rb))
  | And (rd, ra, rb) -> "00111"^(r3d_to_bin (rd, ra, rb))
  | Shiftl (rd, ra, rb) -> "01000"^(r3d_to_bin (rd, ra, rb))
  | Shiftr (rd, ra, rb) -> "01001"^(r3d_to_bin (rd, ra, rb))
  | Addi (rd, ra, imm) -> "01010"^(r2Id_to_bin (rd, ra, imm))
  | Xori (rd, ra, imm) -> "01011"^(r2Id_to_bin (rd, ra, imm))
  | Ori (rd, ra, imm) -> "01100"^(r2Id_to_bin (rd, ra, imm))
  | Andi (rd, ra, imm) -> "01101"^(r2Id_to_bin (rd, ra, imm))
  | Shiftli (rd, ra, imm) -> "01110"^(r2Id_to_bin (rd, ra, imm))
  | Shiftri (rd, ra, imm) -> "01111"^(r2Id_to_bin (rd, ra, imm))
  | Not (rd, ra) -> "10000"^(r2d_to_bin (rd, ra))
  | Load (rd, ra, imm) -> "10001"^(r2Id_to_bin (rd, ra, imm))
  | Limm (rd, imm) -> "10010"^(rId_to_bin (rd, imm))
  | Store (rd, ra, imm) -> "10011"^(r2Id_to_bin (rd, ra, imm))
  | Move (rd, ra) -> "10100"^(r2d_to_bin (rd, ra))
  | Beq (rd, ra, imm) -> "10101"^(r2Id_to_bin (rd, ra, imm))
  | Bne (rd, ra, imm) -> "10110"^(r2Id_to_bin (rd, ra, imm))
  | Blt (rd, ra, imm) -> "10111"^(r2Id_to_bin (rd, ra, imm))
  | Ble (rd, ra, imm) -> "11000"^(r2Id_to_bin (rd, ra, imm))
  | Jal (rd, imm) -> "11001"^(rId_to_bin (rd, imm))
  | Jalr (rd, ra, imm) -> "11010"^(r2Id_to_bin (rd, ra, imm));;

let rec compile prog = match prog with
    instr :: r -> print_line (compile_instr instr); compile r
  | [] -> ();;


let () =
  Arg.parse options (set_file ifile) usage;

  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end;

  if not (Filename.check_suffix !ifile ".s") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .java\n@?";
    Arg.usage options usage;
    exit 1
  end;

  if !ofile="" then ofile := Filename.chop_suffix !ifile ".s" ^ ".s";

  let f = open_in !ifile in

  let buf = Lexing.from_channel f in

  try
    let prog = Parser.prog Lexer.token buf in
    close_in f;

    compile prog
  with
    | Lexer.Lexing_error c ->
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse lexicale: %c@." c;
	exit 1
    | Parser.Error ->
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse syntaxique@.";
	exit 1
    | Lexer.Error s -> 
	localisation (Lexing.lexeme_start_p buf);
	print_line s;
	exit 1




