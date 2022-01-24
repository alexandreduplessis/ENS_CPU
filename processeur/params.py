
class rom:
    word_size = 16 + 4 + 4 + 4 + 10
    addr_size = 16

class ram:
    word_size = 16
    addr_size = 16
    
class registers:
    nb = 16
    addr_size = 4
    size = 16
    out = {0, 1, 2, 3, 4, 5}
