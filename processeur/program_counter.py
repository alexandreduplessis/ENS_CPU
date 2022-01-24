from lib_carotte import *
import params

def half_adder(a, c):
    b = a ^ c
    c = a & c
    return (b, c)

def incrementor(a):
    c = Constant("1")
    (s, c) = half_adder(a[0], c)
    for i in range(1, a.bus_size):
        (s_i, c) = half_adder(a[i], c)
        s = s + s_i
    return s

def cond(jmp_cond, jmp_addr, pc_reg):
    beq = Mux(
        jmp_cond["beq"],
        jmp_addr,
        pc_reg
    )
    bne = Mux(
        jmp_cond["bne"],
        jmp_addr,
        beq
    )
    jal = Mux(
        jmp_cond["jal"],
        jmp_addr,
        bne
    )
    
    return jal

def program_counter(is_jump, jmp_addr, jmp_cond):
    pc_reg = Reg(Defer(params.rom.addr_size, lambda: pc_var))
    inc_pc_reg = incrementor(pc_reg)
    cond_addr = cond(jmp_cond, jmp_addr, inc_pc_reg)
    pc_var = Mux(is_jump, cond_addr, inc_pc_reg)
    return pc_reg
