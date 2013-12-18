#!/bin/bash

./bench.sh -mflags "-mem 4000000000" NewBench/*.hs
./bench.sh -llvm -mflags "-mem 4000000000" NewBench/*.hs

./bench-ghc.sh -nogc NewBench/*.hs
./bench-ghc.sh -nogc -noopt NewBench/*.hs
./bench-ghc.sh -ghc ghc6 -nogc NewBench/*.hs
./bench-ghc.sh -ghc ghc6 -nogc -noopt NewBench/*.hs

./bench-nhc.sh -nogc NewBench/*.hs

./bench-jhc.sh -nogc NewBench/*.hs

./bench-uhc.sh -nogc NewBench/*.hs
