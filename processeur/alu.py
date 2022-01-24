from lib_carotte import *

def full_adder(a, b, c):
    tmp = a ^ b
    return (tmp ^ c, (tmp & c) | (a & b))

def n_adder(a, b, sub):
    assert(a.bus_size == b.bus_size)
    #### /!\ mettre c à 1 ssi soustraction
    c = sub
    b0 = b[0]^sub # for substraction
    (s, c) = full_adder(a[0], b0, c) # Treat the 0 case separately since variables have a bus size >= 1
    for i in range(1, a.bus_size):
        bi = b[i]^sub # for substraction
        (s_i, c) = full_adder(a[i], bi, c)
        s = s + s_i
    return (s, c)

def is_not_zero(a):
    not_zero_b = a[0]
    for i in range(1, a.bus_size):
        not_zero_b = a[i] | not_zero_b
    return not_zero_b

def left_shifter(op1, op2):
    for i in range(4):
        op1 = Mux(op2[i], Constant("0"*(2**i))+op1[0:16-2**i], op1)
    return op1

def right_shifter(op1, op2):
    for i in range(4):
        op1 = Mux(op2[i], op1[2**i:16]+Constant("0"*(2**i)), op1)
    return op1

def alu(instr, op1, op2):
    (s, c) = n_adder(op1, op2, instr[0])
    orr = op1 | op2
    xor = op1 ^ op2
    andd = op1 & op2
    nott = ~op1
    coor = Mux(instr[0], orr, xor)
    aoor = Mux(instr[1], andd, coor)
    noaoor = Mux(instr[0] & instr[1] & instr[2], nott, aoor)
    bonoaoor = Mux(instr[2], noaoor, s)
    shl = left_shifter(op1, op2)
    shr = right_shifter(op1, op2)
    shlor = Mux(instr[0], shr, shl)
    soboa = Mux(instr[3], shlor, bonoaoor)
    not_zero_flag = is_not_zero(soboa)
    stric_below_zero = soboa[15]
    return (soboa, c, not_zero_flag, stric_below_zero)
