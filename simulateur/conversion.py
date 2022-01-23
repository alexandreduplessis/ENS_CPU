total = int(input())
f = open("rom2", 'r')
chaine = ""
for line in f:
   chaine += line[:-1]
f.close()
l = len(chaine)
zero = total-l
f = open("rom", 'w')
f.write(chaine+"0"*zero)
f.close()
