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

function measureCache {
echo Measuring cache of $1...
./scripts/perf-greedy.sh ./$1_full
./scripts/perf-greedy.sh ./$1_compact
}

# Takes three arguments: a string of flags, a string suffix for the generated
# program, and a factor that multiplies the memory supplied (compact==1).
function runBenchmarks {
echo Benchmarks [flags: \"$1\", suffix: $2, mem_factor: $3]
echo Using:
echo gic $GICFLAGS
echo C compiler: $CC
let mem=28000000*$3
export GICFLAGS="-gic-tc -mem $mem $1 $FASTOP"
echo -n "Ack: "
testRun Examples/NewBench/ack.hs
cp main.c gic_ack$2.c
cp a.out gic_ack$2
echo -n "Collatz: "
testRun Examples/Data/collatz.hs
cp main.c gic_collatz$2.c
cp a.out gic_collatz$2
echo -n "Fib: "
testRun Examples/NewBench/fib.hs
cp main.c gic_fib$2.c
cp a.out gic_fib$2
echo -n "Primes: "
testRun Examples/NewBench/primes.hs
cp main.c gic_primes$2.c
cp a.out gic_primes$2
echo -n "Church: "
testRun Examples/NewBench/church.hs
cp main.c gic_church$2.c
cp a.out gic_church$2
echo -n "Queens-num: "
testRun Examples/NewBench/queens-num.hs
cp main.c gic_queens_num$2.c
cp a.out gic_queens_num$2
let mem=440000000*$3
export GICFLAGS="-gic-tc -mem $mem $1 $FASTOP"
echo -n "Digits: "
testRun Examples/Data/digits_of_e1.hs
cp main.c gic_digits_of_e1$2.c
cp a.out gic_digits_of_e1$2
echo -n "Reverse: "
testRun Examples/Data/reverse.hs
cp main.c gic_reverse$2.c
cp a.out gic_reverse$2
let mem=1800000000*$3
export GICFLAGS="-gic-tc -mem $mem $1 $FASTOP"
echo -n "Quick-sort: "
testRun Examples/NewBench/quick-sort.hs
cp main.c gic_quick_sort$2.c
cp a.out gic_quick_sort$2
let mem=1100000000*$3
export GICFLAGS="-mem $mem $1 $FASTOP"
echo -n "Tree-sort: "
testRun Examples/NewBench/tree-sort.hs
cp main.c gic_tree_sort$2.c
cp a.out gic_tree_sort$2
echo -n "Queens: "
testRun Examples/NewBench/queens.hs
cp main.c gic_queens$2.c
cp a.out gic_queens$2
let mem=7628000000*$3
export GICFLAGS="-gic-tc -mem $mem $1 $FASTOP"
echo -n "Ntak: "
testRun Examples/NewBench/ntak.hs
cp main.c gic_ntak$2.c
cp a.out gic_ntak$2
}

echo Compiling+running with GIC...
if [ "$CC" == "" ]
then
  export CC=gcc
fi

ulimit -s 262144

# FASTOP="-fop"
export CFLAGS2="-Wno-#warnings -Wno-unused-value"
# export CFLAGS2="-DSSTACK"

echo Using CFLAGS2: $CFLAGS2
runBenchmarks "" "_full" 4
runBenchmarks "-compact" "_compact" 1

echo Measuring cache using perf:

measureCache "gic_ack"
measureCache "gic_collatz"
measureCache "gic_digits_of_e1"
measureCache "gic_fib"
measureCache "gic_ntak"
measureCache "gic_primes"
measureCache "gic_church"
measureCache "gic_queens"
measureCache "gic_queens_num"
measureCache "gic_quick_sort"
measureCache "gic_tree_sort"
measureCache "gic_reverse"

exit


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

echo Testing cache behavior with Cachegrind:
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
