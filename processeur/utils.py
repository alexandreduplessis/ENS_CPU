def decode(instrs, instrt):
    assert(instrs.bus_size == len(instrt))
    instrs_vs = [instrs[i] for i in range(instrs.bus_size)]
    decoded = None
    if instrt[-1] == "1":
        decoded = instrs_vs[0]
    else:
        decoded = ~instrs_vs[0]
    
    for i in range(1, instrs.bus_size):
        if instrt[instrs.bus_size-1-i] == "1":
            decoded = decoded & instrs_vs[i]
        else:
            decoded = decoded & ~instrs_vs[i]
    
    return decoded
