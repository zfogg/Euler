GHC_FLAGS = -fforce-recomp -outputdir /tmp

GHC_RTS = -rtsopts -O2 -threaded

LIBS = ./lib/Euler.hs

SRC = $(wildcard *.hs)


%: %.hs
	ghc --make $(GHC_FLAGS) $(GHC_RTS) $@.hs $(LIBS) -o ./bin/$@

all: $(SRC)
	make $(SRC:.hs=)
