from lib_carotte import *
import utils
import params
from program_counter import incrementor

def clock_div(is_not_wait, factor, clock_counter):
    activation_tmp = ~Xor(factor[factor.bus_size-1], clock_counter[factor.bus_size-1])
    for i in reversed(range(0,factor.bus_size-1)):
        activation_tmp = activation_tmp & ~Xor(factor[i], clock_counter[i])
    clock_counter = Mux(activation_tmp | is_not_wait,
                        Constant("0000000000000000"),
                        incrementor(clock_counter))
    return is_not_wait | activation_tmp, clock_counter
