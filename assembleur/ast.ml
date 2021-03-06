exception Syntax_error of string

type reg = string

type const = int

type r4d = reg * reg * reg * reg

type r3d = reg * reg * reg

type r2d = reg * reg

type jImm = Jconst of const | Jlabel of string

type r2Id = reg * reg * jImm

type rId = reg * const


type instr = 
  | Div of r4d
  | Add of r3d
  | Mul of r3d
  | Sub of r3d
  | Xor of r3d
  | Or of r3d
  | And of r3d
  | Shiftr of r3d
  | Shiftl of r3d
  | Addi of r2Id
  | Subi of r2Id
  | Xori of r2Id
  | Ori of r2Id
  | Andi of r2Id
  | Shiftri of r2Id
  | Shiftli of r2Id
  | Not of r2d
  | Load of r2d
  | Limm of rId
  | Store of rId
  | Move of r2d
  | Beq of r2Id
  | Bne of r2Id
  | Blt of r2Id
  | Ble of r2Id
  | Jmp of jImm
  | Label of string
  | Wait of jImm
type program = instr list
