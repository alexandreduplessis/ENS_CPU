%{
  open Ast
%}

%token <int> CONST
%token <string> REG, LABEL, LBL
%token ADD, ADDI, SUB, MUL, DIV, SUBI
%token AND, ANDI, OR, ORI, XOR, XORI, NOT
%token SHIFTL, SHIFTLI, SHIFTR, SHIFTRI
%token WAIT
%token LOAD, LIMM, STORE, MOVE
%token BEQ, BNE, BLT, BLE, JMP
%token EOF


%start prog

%type <Ast.program> prog

%%

prog:
| p = instrs EOF { List.rev p }
;

instrs:
| i = instr           { [i] }
| l = instrs i = instr { i :: l }
;

instr:
| DIV a = r4d { Div a }
| ADD a = r3d { Add a }
| MUL a = r3d { Mul a }
| SUB a = r3d { Sub a }
| XOR a = r3d { Xor a }
| OR a = r3d { Or a }
| AND a = r3d { And a }
| SHIFTR a = r3d { Shiftr a }
| SHIFTL a = r3d { Shiftl a }
| ADDI a = r2Id { Addi a }
| SUBI a = r2Id { Subi a }
| XORI a = r2Id { Xori a }
| ORI a = r2Id { Ori a }
| ANDI a = r2Id { Andi a }
| SHIFTRI a = r2Id { Shiftri a }
| SHIFTLI a = r2Id { Shiftli a }
| NOT a = r2d { Not a }
| LOAD a = r2d { Load a }
| LIMM a = rId { Limm a }
| STORE a = rId { Store a }
| MOVE a = r2d { Move a }
| BEQ a = r2Id { Beq a }
| BNE a = r2Id { Bne a }
| BLT a = r2Id { Blt a }
| BLE a = r2Id { Ble a }
| JMP a = rImm { Jmp a }
| label = LABEL { Label label }
| WAIT a = rImm { Wait a }


r4d:
| rd = REG ra = REG rb = REG rr = REG { rd, ra, rb, rr }

r3d:
| rd = REG ra = REG rb = REG  { rd, ra, rb }

r2d:
| rd = REG ra = REG { rd, ra }

r2Id:
| rd = REG ra = REG imm = CONST { rd, ra, Jconst imm }
| rd = REG ra = REG lbl = LBL { rd, ra, Jlabel lbl }

rId:
| rd = REG imm = CONST { rd, imm }

rImm:
| imm = CONST { Jconst imm }
| lbl = LBL { Jlabel lbl }
