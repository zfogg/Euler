#!/bin/bash

# Ensure output folder exists.
if [ ! -d bin ]; then
    mkdir bin
fi

# Compile it all.
for f in $(ls | grep .hs); do
    ghc --make -fforce-recomp -rtsopts -O -threaded $f "lib/Euler.hs";
    mv "${f%.*}" bin; # Move the compiled executable to ./bin/.
done

# Remove garbage.
for i in *.hi *.o; do
    find . -name $i -exec rm -f {} \;
done
