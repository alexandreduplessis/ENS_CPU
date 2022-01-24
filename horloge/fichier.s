limm %r6 $59
secondes:
addi %r0 %r0 $1
beq %r0 %r6 $minutes
jmp $secondes
minutes:
limm %r0 $0
addi %r1 %r1 $1
jmp $secondes
