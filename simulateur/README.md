- Organiser code, ajouter commentaires

# **Simulateur de netlist**
On a écrit un compilateur netlist vers C++ pour des questions d'optimisation.

**Utilisation**

- `make clean` (on ne sait jamais)
- `make`
- `./compiler.native nom_netlist.net`(compile et exécute)
Si on veut se limiter à 5 tours par exemple, il suffit de remplacer la 3e commande par `./compiler.native -n 5 nom_netlist.net`.

**ROM**

La ROM doit être stockée dans le fichier `rom`, sous forme d'une ligne par mot.

**Entrées**
La lecture des entrées est intuitive.

_Version du 23/01/2022_
