from lib_carotte import *
import params

def n_mux(regs, addr):
    l = regs
    for d in range(params.registers.addr_size):
        tmp_l = []
        a = addr[d]
        for i in range(2**(params.registers.addr_size-d-1)):
            tmp1 = l[2*i]
            tmp2 = l[2*i+1]
            m = Mux(a, tmp2, tmp1)
            tmp_l.append(m)
        
        l = tmp_l
    
    return l[0]

def n_demux(we, wa):
    decoder = []
    for i in range(wa.bus_size):
        bit = wa[i]
        decoder.append((bit, ~bit))
    
    l = [[
        we & decoder[params.registers.addr_size-1][1],
        we & decoder[params.registers.addr_size-1][0],
    ]]
    for d in range(1, params.registers.addr_size):
        tmp_l = []
        for i in range(2**(d+1)):
            if i % 2 == 0:
                tmp_l.append(l[d-1][i // 2] & decoder[params.registers.addr_size-1-d][1])
            else:
                tmp_l.append(l[d-1][i // 2] & decoder[params.registers.addr_size-1-d][0])
        l.append(tmp_l)
    
    return l[-1]

class registerHolder:
    def __init__(self):
        self.regvars = [Variable(name="r"+str(i), bus_size=params.registers.size, autogen_name=False)
                        for i in range(params.registers.nb)]
            
def register(we, data, i):
    r = Reg(Defer(params.registers.size, lambda: rv))
    rv = Mux(we, data, r)
    if i in params.registers.out : rv.set_as_output("register_"+str(i))
    return r

def registers(r1addr, r2addr, wd, wa, we):
    #we.set_as_output("write_enable")
    wes = n_demux(we, wa)
    """for i, wer in enumerate(wes):
        wer.set_as_output("we_r_"+str(i))"""
    regs = [register(wes[i], wd, i) for i in range(params.registers.nb)]
    rr1 = n_mux(regs, r1addr)
    rr2 = n_mux(regs, r2addr)
    return rr1, rr2
