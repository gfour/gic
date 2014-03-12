#!/bin/bash
#
# Test script for the greedy tests.
# 

function testRun {
  ./run_lar.sh $1
  # Run using the local LTO-enabled Clang build.
  # ./compile-clang-flto.sh && ./a.out
}

function testCache {
    echo $1 vs $2
    local CFILE=$1.cache.txt
    echo > $CFILE
    echo $1 >> $CFILE
    valgrind --tool=cachegrind --main-stacksize=262144000 ./$1 >> $CFILE 2>&1
    echo $2 >> $CFILE
    valgrind --tool=cachegrind --main-stacksize=262144000 ./$2 >> $CFILE 2>&1
}

echo Compiling+running with GIC...
if [ "$CC" == "" ]
then
  export CC=gcc
fi

# FASTOP="-fop"
export CFLAGS2="-Wno-#warnings"
# export CFLAGS2="-DSSTACK"
export GICFLAGS="-gic-tc -mem 220000000 -compact $FASTOP"
echo Using:
echo gic $GICFLAGS
echo C compiler: $CC

ulimit -s 262144
echo -n "Ack: "
testRun Examples/NewBench/ack.hs
cp main.c gic_ack.c
cp a.out gic_ack
echo -n "Collatz: "
testRun Examples/Data/collatz.hs
cp main.c gic_collatz.c
cp a.out gic_collatz
echo -n "Fib: "
testRun Examples/NewBench/fib.hs
cp main.c gic_fib.c
cp a.out gic_fib
echo -n "Primes: "
testRun Examples/NewBench/primes.hs
cp main.c gic_primes.c
cp a.out gic_primes
echo -n "Church: "
testRun Examples/NewBench/church.hs
cp main.c gic_church.c
cp a.out gic_church
export GICFLAGS="-gic-tc -mem 442800000 -compact $FASTOP"
echo -n "Digits: "
testRun Examples/Data/digits_of_e1.hs
cp main.c gic_digits_of_e1.c
cp a.out gic_digits_of_e1
echo -n "Reverse: "
testRun Examples/Data/reverse.hs
cp main.c gic_reverse.c
cp a.out gic_reverse
export GICFLAGS="-gic-tc -mem 24428000000 -compact $FASTOP"
echo -n "Ntak: "
testRun Examples/NewBench/ntak.hs
cp main.c gic_ntak.c
cp a.out gic_ntak
echo -n "Quick-sort: "
testRun Examples/NewBench/quick-sort.hs
cp main.c gic_quick_sort.c
cp a.out gic_quick_sort
echo -n "Tree-sort: "
testRun Examples/NewBench/tree-sort.hs
cp main.c gic_tree_sort.c
cp a.out gic_tree_sort
export GICFLAGS="-mem 24428000000 -compact $FASTOP"
echo -n "Queens: "
testRun Examples/NewBench/queens.hs
cp main.c gic_queens.c
cp a.out gic_queens
echo -n "Queens-num: "
testRun Examples/NewBench/queens-num.hs
cp main.c gic_queens_num.c
cp a.out gic_queens_num

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

echo Testing cache behavior:
testCache Ack gic_ack
testCache Collatz gic_collatz
testCache Digits_of_e1 gic_digits_of_e1
testCache Fib gic_fib
testCache Ntak gic_ntak
testCache Primes gic_primes
testCache Church gic_church
testCache Queens gic_queens
testCache Queens_num gic_queens_num
testCache Quick_sort gic_quick_sort
testCache Tree_sort gic_tree_sort
testCache Reverse gic_reverse
