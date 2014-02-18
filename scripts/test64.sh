#!/bin/bash
#
# Test script for the greedy tests.
# 

echo Compiling+running with GIC...
if [ "$CC" == "" ]
then
  export CC=gcc
fi

export CFLAGS2="-Wno-#warnings"
# export CFLAGS2="-DSSTACK"
export GICFLAGS="-gic-tc -mem 220000000 -compact"
ulimit -s 262144
echo -n "Ack: "
./run_lar.sh Examples/NewBench/ack.hs
echo -n "Collatz: "
./run_lar.sh Examples/Data/collatz.hs
echo -n "Fib: "
./run_lar.sh Examples/NewBench/fib.hs
echo -n "Primes: "
./run_lar.sh Examples/NewBench/primes.hs
echo -n "Church: "
./run_lar.sh Examples/NewBench/church.hs
export GICFLAGS="-gic-tc -mem 2442800000 -compact"
echo -n "Digits: "
./run_lar.sh Examples/Data/digits_of_e1.hs
echo -n "Reverse: "
./run_lar.sh Examples/Data/reverse.hs
export GICFLAGS="-gic-tc -mem 24428000000 -compact"
echo -n "Ntak: "
./run_lar.sh Examples/NewBench/ntak.hs
export GICFLAGS="-mem 24428000000 -compact"
echo -n "Queens: "
./run_lar.sh Examples/NewBench/queens.hs

if [ "$GHC" == "" ]
then
  export GHC=ghc
fi
echo Compiling with GHC:
$GHC --version

$GHC -fforce-recomp -O3 Examples/NewBench/ack.hs -o Ack
$GHC -fforce-recomp -O3 Examples/Data/collatz.hs -o Collatz
$GHC -fforce-recomp -O3 Examples/Data/digits_of_e1.hs -o Digits_of_e1
$GHC -fforce-recomp -O3 Examples/NewBench/fib.hs -o Fib
$GHC -fforce-recomp -O3 Examples/NewBench/ntak.hs -o Ntak
$GHC -fforce-recomp -O3 Examples/NewBench/primes.hs -o Primes
$GHC -fforce-recomp -O3 Examples/NewBench/church.hs -o Church
$GHC -fforce-recomp -O3 Examples/NewBench/queens.hs -o Queens
$GHC -fforce-recomp -O3 Examples/Data/reverse.hs -o Reverse
echo Running...
for p in Ack Collatz Digits_of_e1 Fib Ntak Primes Church Queens Reverse
do
    echo -n "$p: "
    TIME="\t%E" time ./$p
done
