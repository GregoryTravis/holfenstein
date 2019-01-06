#cabal configure -f examples
#(cabal build lazyfoo-lesson-43 && dist/build/lazyfoo-lesson-43/lazyfoo-lesson-43) 2>&1 | tee out
(ghc -outputdir ifs -o Holfenstein Holfenstein.hs && ./Holfenstein) 2>&1 | tee out
