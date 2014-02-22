#!/bin/bash
#
# Test script for the greedy tests.
# 

function testRun {
  ./run_lar.sh $1
  # Run using the local LTO-enabled Clang build.
  # ./compile-clang-flto.sh && ./a.out
}

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
testRun Examples/NewBench/ack.hs
echo -n "Collatz: "
testRun Examples/Data/collatz.hs
echo -n "Fib: "
testRun Examples/NewBench/fib.hs
echo -n "Primes: "
testRun Examples/NewBench/primes.hs
echo -n "Church: "
testRun Examples/NewBench/church.hs
export GICFLAGS="-gic-tc -mem 2442800000 -compact"
echo -n "Digits: "
testRun Examples/Data/digits_of_e1.hs
echo -n "Reverse: "
testRun Examples/Data/reverse.hs
export GICFLAGS="-gic-tc -mem 24428000000 -compact"
echo -n "Ntak: "
testRun Examples/NewBench/ntak.hs
echo -n "Quick-sort: "
testRun Examples/NewBench/quick-sort.hs
echo -n "Tree-sort: "
testRun Examples/NewBench/tree-sort.hs
export GICFLAGS="-mem 24428000000 -compact"
echo -n "Queens: "
testRun Examples/NewBench/queens.hs
echo -n "Queens-num: "
testRun Examples/NewBench/queens-num.hs

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
$GHC -fforce-recomp -O3 Examples/NewBench/queens-num.hs -o Queens_num
$GHC -fforce-recomp -O3 Examples/NewBench/quick-sort.hs -o Quick_sort
$GHC -fforce-recomp -O3 Examples/NewBench/tree-sort.hs -o Tree_sort
$GHC -fforce-recomp -O3 Examples/Data/reverse.hs -o Reverse
echo Running...
for p in Ack Collatz Digits_of_e1 Fib Ntak Primes Church Queens Queens_num Quick_sort Reverse Tree_sort
do
    echo -n "$p: "
    TIME="\t%E" time ./$p
done
