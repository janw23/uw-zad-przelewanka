cd src
ocamlopt przelewanka.mli przelewanka.ml -o ../build/eval
#ocamlopt test.ml -o ../build/eval
cd ..
bash clean_src.sh
