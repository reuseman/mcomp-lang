#!/bin/bash

# REQUIREMENTS: https://github.com/sharkdp/hyperfine

######################
# CLEAN
######################

# remove all the previous .out files
rm *.out

# remove any benchmark file ending with .md, .csv or .json
rm *.md
rm *.csv
rm *.json

if [ "$1" = "clean" ]; then
    exit 0
fi


######################
# COMPILE EVERYTHING
######################


# for each file in the current folder that ends with .mc
for file in *.mc
do
    # get the name of the file without the extension
    filename="${file%.*}"
    # COMPILE
    dune exec ../bin/mcompc.exe -- ./$file -o temp.bc -O > /dev/null
    # LINK EVERYTHING
    clang temp.bc ../bin/rt-support.c -o $filename"_mc.out"
    # REMOVE TEMPORARY FILES
    rm temp.bc
done


# for each file in the current folder that ends with .c, compile it with clang and the output name should be {filename}_c.out
for file in *.c
do
    # get the name of the file without the extension
    filename="${file%.*}"
    # COMPILE
    clang $file -o $filename"_c.out"
done



######################
# BENCHMARK
######################

# for each file ending with .mc
for file in *.mc
do
    # get the name of the file without the extension
    filename="${file%.*}"
    
    # get the mc executable that is called {filename}_mc.out
    mc_executable=$filename"_mc.out"

    # get the c executable that is called {filename}_c.out
    c_executable=$filename"_c.out"

    # run the benchmark with hyperfine
    hyperfine --warmup 3 --export-markdown $filename"_benchmark.md" "./$mc_executable" "./$c_executable"
done