# **Simulateur de netlist**
On a écrit un compilateur netlist vers C++ pour des questions d'optimisation.

## **Utilisation**

- `make clean` (on ne sait jamais)
- `make`
- `./compiler.native nom_netlist.net`(compile et exécute)

Si on veut se limiter à 5 tours par exemple, il suffit de remplacer la troisième commande par 

`./compiler.native -n 5 nom_netlist.net`


**ROM**

La ROM doit être stockée dans le fichier `rom`, sous forme d'une ligne par mot.

**Entrées**

La lecture des entrées est intuitive.

## **Description**

- Le fichier `graph.ml` contient notamment une implémentation de tri topologique en Ocaml.
- Le fichier `scheduler.ml` permet de trier les équations d'une netlist topologiquement, et lève une erreur `Combinational_cycle` en cas de détection de cycle.
- Le fichier principal `compiler.ml` compile un fichier `.net` en un fichier `.c`.

Le simulateur gère toutes les opérations du langage netlist, le nombre de RAM désiré, une ROM, et des registres. On procède en deux passes : une pour simuler toutes les équations sauf l'écriture dans la RAM, et une deuxième pour écrire dans la RAM et mettre à jour les registres. En effet ceux-ci sont gérés grâce à deux variables pour chaque variable à stocker, une pour le cycle en cours et une une pour le cycle précédent. Toutes les variables sont stockées sous forme de bitsets.


_Version du 24/01/2022_
