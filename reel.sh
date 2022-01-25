cd assembleur
dune clean
dune build assembleur.exe
cp ../horloge/reel.s reel.s
dune exec ./assembleur.exe reel.s
cp rom ../simulateur/rom
cd ../simulateur
make clean
make
./compiler.native ../processeur/test.net
cp ../processeur/test ../gui/a.out
cp rom ../gui/rom
cd ../gui
python3 test.py
