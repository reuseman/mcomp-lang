#!/bin/bash

# REQUIREMENTS: https://github.com/sharkdp/hyperfine

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