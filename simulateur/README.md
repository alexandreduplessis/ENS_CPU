Simulateur de netlist :
Gestion de :
- la RAM (une RAM par instruction RAM)
- la ROM (une seule ROM à lire au début)
- les registres
- opérations courantes

Pour tester sur un fichier test.net dans le dossier courant :
> ocamlbuild netlist_simulator.byte
> ./netlist_simulator.byte test.net

Lecture de la ROM : mot par mot (la lecture des variables est censée être intuitive)

Version du 11/11/2021
