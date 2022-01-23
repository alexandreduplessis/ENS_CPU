
(* The type of tokens. *)

type token = 
  | XOR
  | VAR
  | SLICE
  | SELECT
  | ROM
  | REG
  | RAM
  | OUTPUT
  | OR
  | NOT
  | NAND
  | NAME of (string)
  | MUX
  | INPUT
  | IN
  | EQUAL
  | EOF
  | CONST of (string)
  | CONCAT
  | COMMA
  | COLON
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Netlist_ast.program)
