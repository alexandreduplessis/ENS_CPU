cd assembleur
dune clean
dune build assembleur.exe
cp ../horloge/fichier.s fichier.s
dune exec ./assembleur.exe fichier.s
cp rom ../simulateur/rom
cd ../simulateur
make clean
make
./compiler.native ../processeur/reel.net
cp ../processeur/reel ../gui/a.out
cp rom ../gui/rom
cd ../gui
python3 test.py
