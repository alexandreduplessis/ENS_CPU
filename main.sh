cd ../assembleur
dune build
dune exec ./assembleur fichier.s
cp ./rom ../simulateur/rom
cd ../simulateur/
make clean
make
./compiler.native ../processeur/test.net
cp ./test ../gui/a.out
cp ./rom ../gui/rom
cd ../gui
python3 test.py
