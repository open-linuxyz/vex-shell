# Makefile for building the shellruntime_ssh program using cabal

# Define the name of the executable
EXECUTABLE=shellruntime_ssh

# Define the name of the Haskell source file
SRC=Main.hs

.PHONY: all clean

all: $(EXECUTABLE)

# Rule to build the executable using cabal
$(EXECUTABLE): $(SRC)
	cabal build --ghc-options="-O2"
	mv dist-newstyle/build/*/ghc*/ssh-add-keys-*/x/ssh-add-keys/build/ssh-add-keys/ssh-add-keys $(EXECUTABLE)
	strip $(EXECUTABLE)

clean:
	cabal clean
	rm -f $(EXECUTABLE)
