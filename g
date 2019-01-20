rm -f Holfenstein.prof

#OPTFLAGS=-O3
OPTFLAGS=

rm -f gfx.o
gcc $OPTFLAGS -c -o gfx.o gfx.c

#cabal configure -f examples
#(cabal build lazyfoo-lesson-43 && dist/build/lazyfoo-lesson-43/lazyfoo-lesson-43) 2>&1 | tee out

# No prof, so no stack trace
#(ghc $OPTFLAGS -outputdir ifs -o Holfenstein Holfenstein.hs && ./Holfenstein) 2>&1 | tee out

# Doesn't work on linux, probably missing prof libraries
(ghc $OPTFLAGS -rtsopts -prof -Werror -ferror-spans -fprof-auto -fprof-cafs -outputdir ifs -o Holfenstein gfx.o Holfenstein.hs && ./Holfenstein -cx +RTS -p) 2>&1 | tee out
