#cabal configure -f examples
#(cabal build lazyfoo-lesson-43 && dist/build/lazyfoo-lesson-43/lazyfoo-lesson-43) 2>&1 | tee out
(ghc -rtsopts -prof -Werror -ferror-spans -fprof-auto -fprof-cafs -outputdir ifs -o Holfenstein Holfenstein.hs && ./Holfenstein -cx +RTS) 2>&1 | tee out
