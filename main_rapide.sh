cd assembleur
dune clean
dune build assembleur.exe
cp ../horloge/fichier.s fichier.s
dune exec ./assembleur.exe fichier.s
cp rom ../simulateur/rom
cd ../simulateur
make clean
make
./compiler.native ../processeur/rapide.net
cp ../processeur/rapide ../gui/a.out
cp rom ../gui/rom
cd ../gui
python3 test.py
