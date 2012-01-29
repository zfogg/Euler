#!/bin/bash

# Ensure output folder exists.
if [ ! -d bin ]; then mkdir bin; fi

# Compile it all.
for f in $(ls | egrep "projectEuler[0-9].*\.hs"); do
    # Check for dependencies:
    files=$f" "`grep "import Euler" $f -q && echo "lib/Euler.hs"`

    # Compile it all:
    ghc --make -fforce-recomp -rtsopts -O -threaded $files

    # Move the compiled executable to ./bin/.
    mv "${f%.*}" bin;
done

# Remove garbage.
for i in *.hi *.o; do
    find -name $i -exec rm -f {} \;
done
