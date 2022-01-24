{
  open Lexing
  open Parser

  exception Lexing_error of char
  exception Error of string

  let kwd_tbl = ["add", ADD;
  		  "sub", SUB;
  		  "mul", MUL;
  		  "div", DIV;
  		  "addi", ADDI;
		  "subi", SUBI;
  		  "xor", XOR;
  		  "xori", XORI;
  		  "or", OR;
  		  "ori", ORI;
  		  "and", AND;
  		  "andi", ANDI;
  		  "not", NOT;
  		  "shl", SHIFTL;
  		  "shli", SHIFTLI;
  		  "shr", SHIFTR;
  		  "shri", SHIFTRI;
  		  "load", LOAD;
  		  "limm", LIMM;
  		  "sto", STORE;
  		  "mov", MOVE;
  		  "beq", BEQ;
  		  "bne", BNE;
  		  "blt", BLT;
  		  "ble", BLE;
  		  "jmp", JMP]
  		  
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> raise (Error ("L'instruction "^s^" n'existe pas."));;

  let reg_list = ["r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "ra"; "rb"; "rc"; "rd"; "re"; "rf"];;

  let reg s = if List.mem s reg_list then REG s else raise (Error (s^" n'est pas un registre"));;

  
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit)*
let integer = '-'? ['0'-'9']+
let space = [' ' '\t']

rule token = parse
  | "//" [^'\n']* { token lexbuf }
  | '\n'    { new_line lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id ':' { LABEL id }
  | ident as id { id_or_kwd id }
  | '%' (ident as id) { reg id }
  | '$' (ident as id) { LBL id }
  | '$' (integer as s) { CONST (int_of_string s) }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }
