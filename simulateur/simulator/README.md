- Organiser code, ajouter commentaires
- Créer une variable bitset<1>(1) dans le header
- Plusieurs ram superflu -> supprimer

# **Simulateur de netlist**
On a écrit un compilateur netlist vers C++ pour des questions d'optimisation.

**Utilisation**
- 'make clean' (on ne sait jamais)
- 'make'
- './compiler.native -n 3 nom_netlist.net' (compile et exécute)
Si on veut se limiter à 5 tours par exemple, il suffit de remplacer la 3e commande par './compiler.native -n 3 nom_netlist.net'.

**ROM**
La ROM doit être stockée dans le fichier 'rom', sous forme d'une seule ligne de 0 et de 1.
Pour des raisons de commodité, un script python a été créé pour créer, à partir d'un fichier 'rom2' sous forme de plusieurs lignes (vraisemblablement pour chaque mot de la ROM) et de la longueur théorique totale de la ROM, le fichier 'rom' correspondant, en rassemblant tout sur une ligne et en ajoutant le nombre de 0 nécessaire à la fin. (sert pour le débuggage)

**Entrées**
La lecture des entrées est intuitive.

_Version 23/01/2022_
