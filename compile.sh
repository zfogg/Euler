#!/bin/bash

# Ensure output folder exists.
if [ ! -d "$bin" ]; then
    mkdir bin
fi

# Compile it all.
for i in $(ls | grep .hs); do
    ghc --make -fforce-recomp -rtsopts -O -threaded $i "./lib/Euler.hs";
    mv "${i%.*}" ./bin;
done

# Remove garbage.
for i in *.hi *.o; do
    find . -name $i -exec rm -f {} \;
done
