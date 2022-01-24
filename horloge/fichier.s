limm %r6 $60
limm %r7 $24
limm %r8 $12
limm %r3 $1
limm %r4 $1

limm %rd $31
sto %rd $1
limm %rd $28
sto %rd $2
limm %rd $31
sto %rd $3
limm %rd $30
sto %rd $4
limm %rd $31
sto %rd $5
limm %rd $30
sto %rd $6
limm %rd $31
sto %rd $7
limm %rd $31
sto %rd $8
limm %rd $30
sto %rd $9
limm %rd $31
sto %rd $10
limm %rd $30
sto %rd $11
limm %rd $31
sto %rd $12

secondes:
mov %r0 %rf
addi %rf %rf $60
beq %rf %r6 $minutes
mov %rf %r0
jmp $secondes

minutes:
limm %r0 $0
mov %r1 %rf
addi %rf %rf $60
beq %rf %r6 $heures
mov %rf %r1
jmp $secondes

heures:
limm %r1 $0
mov %r2 %rf
addi %rf %rf $24
beq %rf %r7 $jours
mov %rf %r2
jmp $secondes

jours:
limm %r2 $0
mov %r3 %rf
addi %rf %rf $1
load %r4 %r9
beq %rf %r9 $mois
mov %rf %r3
jmp $secondes

mois:
limm %r3 $1
mov %r4 %rf
addi %rf %rf $1
beq %rf %r8 $annees
mov %rf %r4
jmp $secondes

annees:
limm %r4 $1
addi %r5 %r5 $1
jmp $secondes
