from lib_carotte import *
from program_counter import program_counter
from registers import registers
from alu import alu
import utils
import params

def main():
    pc_reg = program_counter(
        Defer(1, lambda: is_jump),
        Defer(16, lambda: immi),
        {
            "jal": Defer(1, lambda: jal),
            "beq": Defer(1, lambda: beq),
            "bne": Defer(1, lambda: bne),
            "blt": Defer(1, lambda: blt),
            "bge": Defer(1, lambda: bge)
        }
    )
    op_code = ROM(params.rom.addr_size, params.rom.word_size, pc_reg)
    instr = op_code[28:38]
    immi = op_code[0:16]

    cond_jmp_instr = instr[7:10]
    # les trois derniers bits de l'instruction sont réservés
    # aux sauts et aux sauts conditionnels
    # ce fil empêche l'écriture des registres
    # et l'incrémentation du pc si l'instruction est un saut
    # ou un saut conditionnel
    is_jump = cond_jmp_instr[0] | cond_jmp_instr[1] | cond_jmp_instr[2]
    is_ram = instr[6]
    is_write_ram = is_ram & instr[0]
    is_read_ram = is_ram & ~instr[0]
    
    addr_dst = op_code[24:28]
    addr1 = op_code[16:20]
    addr2 = op_code[20:24]
    output = Defer(params.registers.size, lambda: wd_f)
    (op1, op2) = registers(addr1, addr2, output, addr_dst, (~is_jump & ~is_write_ram))
    
    # pour move (100000) et limm (110000), mettre op1 à 0
    op1_f = Mux(instr[5], Constant("0000000000000000"), op1)
    # pour addi, subi, xori... remplacer op2 par la constante
    op2_f = Mux(instr[4], immi, op2)
    
    ram = RAM(params.ram.addr_size, params.ram.word_size, immi, is_write_ram, immi, op1_f)
    
    (wd, c, not_zero_flag, stric_below_zero) = alu(instr, op1_f, op2_f)
    zero_flag = ~not_zero_flag
    
    wd_f = Mux(is_read_ram, ram, wd)
    
    jal = utils.decode(cond_jmp_instr, "001")
    beq = utils.decode(cond_jmp_instr, "010") & zero_flag
    bne = utils.decode(cond_jmp_instr, "011") & not_zero_flag
    blt = utils.decode(cond_jmp_instr, "100") & stric_below_zero
    bge = utils.decode(cond_jmp_instr, "101") & (stric_below_zero | zero_flag)
    
    #instr.set_as_output("instr")
    #addr_dst.set_as_output("addr_dst")
    #addr2.set_as_output("addr2")
    #addr1.set_as_output("addr1")
    #op1_f.set_as_output("op1_f")
    #op2_f.set_as_output("op2_f")
    #immi.set_as_output("immi")
    #instr[5].set_as_output('move_or_limm')
    
    #op_code.set_as_output("op_code")
    #is_write_ram.set_as_output("is_write_ram")
    #is_read_ram.set_as_output("is_read_ram")
    #wd.set_as_output("alu_output")
    #pc_reg.set_as_output("pc_reg")
    #is_jump.set_as_output("is_jump")
    #cond_jmp_instr.set_as_output("cond_jmp_instr")
