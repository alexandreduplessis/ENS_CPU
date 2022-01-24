cd assembleur
duune clean
dune build assembleur.exe
dune exec ./assembleur.exe fichier.s
cp rom ../simulateur/rom
cd ../simulateur
make clean
make
./compiler.native ../processeur/test.net
cp ../processeur/test ../gui/a.out
cp rom ../gui/rom
cd ../gui
python3 test.py
