#!/bin/bash
#
# Test script for the greedy tests.
# 

set -e

# Flags that enable parts of the benchmarking suite. Set with 1, clear with 0.
COMPILE_RUN=1                # compile & run the benchmarks
MEASURE_CACHE_COUNTERS=0     # measure properties using the hardware counters
MEASURE_CACHEGRIND=0         # measure properties using the Cachegrind tool
COMPARE_GHC=1                # compare runtime with GHC

if [ "$COMPILE_RUN" == "0" ] && [ "$MEASURE_CACHE_COUNTERS" == "0" ] && [ "$MEASURE_CACHEGRIND" == "0" ] && [ "$COMPARE_GHC" == "0" ] 
then
  echo All flags are set to 0, please edit the script to set at least one.
  exit
fi

control_c() {
  exit 1
}
trap control_c SIGINT

function testRun {
  ./run_lar.sh $1
  # Run using the local LTO-enabled Clang build.
  # ./compile-clang-flto.sh && ./a.out
}

function testCache {
    echo $1 vs $2 vs $3
    local CFILE=$1.cache.txt
    echo > $CFILE
    echo $1 >> $CFILE
    valgrind --tool=cachegrind --main-stacksize=262144000 ./$1 >> $CFILE 2>&1
    echo $2 >> $CFILE
    valgrind --tool=cachegrind --main-stacksize=262144000 ./$2 >> $CFILE 2>&1
    echo $3 >> $CFILE
    valgrind --tool=cachegrind --main-stacksize=262144000 ./$3 >> $CFILE 2>&1
}

function measureCache {
echo Measuring property $1 of $2...
./scripts/perf-greedy.sh $1 $2_full
./scripts/perf-greedy.sh $1 $2_compact
}

# Takes three arguments: a string of flags, a string suffix for the generated
# program, and a factor that multiplies the memory supplied (compact==1).
function runBenchmarks {
echo Benchmarks [flags: \"$1\", suffix: $2, mem_factor: $3]
echo Using:
echo gic $GICFLAGS $1
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
if [ "$COMPILE_RUN" == "0" ]
then
  echo NOTE: benchmark compilation/running is disabled.
else
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
fi

if [ "$MEASURE_CACHE_COUNTERS" == "0" ]
then
  echo NOTE: Cache measurements using pref are disabled.
else
  echo Measuring cache using perf:
  for property in "L1-dcache-loads" "L1-dcache-load-misses" "L1-dcache-stores" "L1-dcache-store-misses" "L1-dcache-prefetches" "L1-icache-loads" "L1-icache-load-misses" "LLC-loads" "LLC-load-misses" "LLC-stores" "LLC-store-misses" "dTLB-loads" "dTLB-load-misses" "dTLB-stores" "dTLB-store-misses" "iTLB-loads" "iTLB-load-misses" "branch-loads" "branch-load-misses" "cpu-cycles" "instructions" "cache-references" "cache-misses" "branch-instructions" "branch-misses" "bus-cycles" "ref-cycles"
  do
    for bench in "ack" "collatz" "digits_of_e1" "fib" "ntak" "primes" "church" "queens" "queens_num" "quick_sort" "tree_sort" "reverse"
    do
      echo Testing $bench, property $property
      measureCache $property ./gic_$bench
    done
  done
fi

if [ "$COMPARE_GHC" == "0" ]
then
  echo NOTE: GHC comparison is disabled.
else
  if [ "$GHC" == "" ]
  then
    export GHC=ghc
  fi
  echo Compiling with GHC:
  $GHC --version

  export GHCFLAGS="-fforce-recomp -O3 -XBangPatterns"

  $GHC $GHCFLAGS Examples/NewBench/ack.hs -o Ack
  $GHC $GHCFLAGS Examples/Data/collatz.hs -o Collatz
  $GHC $GHCFLAGS Examples/Data/digits_of_e1.hs -o Digits_of_e1
  $GHC $GHCFLAGS Examples/NewBench/fib.hs -o Fib
  $GHC $GHCFLAGS Examples/NewBench/ntak.hs -o Ntak
  $GHC $GHCFLAGS Examples/NewBench/primes.hs -o Primes
  $GHC $GHCFLAGS Examples/NewBench/church.hs -o Church
  $GHC $GHCFLAGS Examples/NewBench/queens.hs -o Queens
  $GHC $GHCFLAGS Examples/NewBench/queens-num.hs -o Queens_num
  $GHC $GHCFLAGS Examples/NewBench/quick-sort.hs -o Quick_sort
  $GHC $GHCFLAGS Examples/NewBench/tree-sort.hs -o Tree_sort
  $GHC $GHCFLAGS Examples/Data/reverse.hs -o Reverse
  echo Running...
  for p in Ack Collatz Digits_of_e1 Fib Ntak Primes Church Queens Queens_num Quick_sort Reverse Tree_sort
  do
    echo -n "$p: "
    TIME="\t%E" time ./$p
  done
fi

if [ "$MEASURE_CACHEGRIND" == "0" ]
then
  echo NOTE: Cachegrind measurements are disabled.
else
  echo Testing cache behavior with Cachegrind:
  testCache Ack gic_ack_full gic_ack_compact
  testCache Collatz gic_collatz_full gic_collatz_compact
  testCache Digits_of_e1 gic_digits_of_e1_full gic_digits_of_e1_compact
  testCache Fib gic_fib_full gic_fib_compact
  testCache Ntak gic_ntak_full gic_ntak_compact
  testCache Primes gic_primes_full gic_primes_compact
  testCache Church gic_church_full gic_church_compact
  testCache Queens gic_queens_full gic_queens_compact
  testCache Queens_num gic_queens_num_full gic_queens_num_compact
  testCache Quick_sort gic_quick_sort_full gic_quick_sort_compact
  testCache Tree_sort gic_tree_sort_full gic_tree_sort_compact
  testCache Reverse gic_reverse_full gic_reverse_compact
fi
