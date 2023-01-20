mkdir -p artifacts
# DUMP
# dune exec bin/mcompc.exe -- ./main.mc -d
# COMPILE
dune exec bin/mcompc.exe -- ./main.mc -o artifacts/temp.bc > /dev/null
# LINK EVERYTHING
clang artifacts/temp.bc bin/rt-support.c -o artifacts/main
# REMOVE TEMPORARY FILES
rm artifacts/temp.bc
echo -e "\n====================OUTPUT===================="
./artifacts/main
