# ENS_CPU
Faire tourner une horloge sur un processeur.
4 composants :
- assembleur
- processeur en lui-même, codé en python (utilise le compilateur carotte.py pour convertir en netlist.net)
- simulateur de netlist : compilateur net -> C++
- le programme de l'horloge (en assembleur, ou autre si on rajoute un compilateur)
