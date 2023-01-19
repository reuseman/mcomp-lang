# remove all the previous .out files
rm *.out

# remove any benchmark file ending with .md, .csv or .json
rm *.md
rm *.csv
rm *.json



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