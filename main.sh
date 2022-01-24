dune exec ./assembleur.exe fichier.s
cp ./rom ../simulateur/rom
cd ../simulateur/
make clean
make
./compiler.native test.net
cp ./test ../gui/a.out
cp ./rom ../gui/rom
cd ../gui
python3 test.py
