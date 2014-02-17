#!/bin/bash
#
# Test script for the greedy tests.
# 

echo Compiling+running with GIC...
export CC=clang
export GICFLAGS="-gic-tc -mem 2200000000 -compact"
# export CFLAGS2="-DSSTACK"
# echo Ack:
# ./run_lar.sh Examples/NewBench/ack.hs
# echo Collatz:
# ./run_lar.sh Examples/Data/collatz.hs
# echo Digits:
# ./run_lar.sh Examples/Data/digits_of_e1.hs
# echo Fib:
# ./run_lar.sh Examples/NewBench/fib.hs
# echo Primes:
# ./run_lar.sh Examples/NewBench/primes.hs
echo Church:
./run_lar.sh Examples/NewBench/church.hs
echo Reverse:
./run_lar.sh Examples/Data/reverse.hs
# export GICFLAGS="-gic-tc -mem 24428000000"
# echo Ntak:
# ./run_lar.sh Examples/NewBench/ntak.hs
# echo Queens:
# ./run_lar.sh Examples/NewBench/queens.hs

# echo Compiling with GHC...
# ghc -O3 Examples/NewBench/ack.hs -o Ack
# ghc -O3 Examples/Data/collatz.hs -o Collatz
# ghc -O3 Examples/Data/digits_of_e1.hs -o Digits_of_e1
# ghc -O3 Examples/NewBench/fib.hs -o Fib
# ghc -O3 Examples/NewBench/ntak.hs -o Ntak
# ghc -O3 Examples/NewBench/primes.hs -o Primes
# ghc -O3 Examples/NewBench/church.hs -o Church
# ghc -O3 Examples/NewBench/queens.hs -o Queens
# ghc -O3 Examples/Data/reverse.hs -o Reverse
# echo Running...
# for p in Ack Collatz Digits_of_e1 Fib Ntak Primes Church Queens Reverse
# do
#     echo $p: 
#     time ./$p
# done
