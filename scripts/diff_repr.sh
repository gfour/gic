#!/bin/bash

function progStats {
tail -n +6 $1_$2_stats.txt | head -n -3 | awk '{print $1,$2}'
}


function diffProg {
  echo Checking $1...
  progStats $1 "full" | grep $2
  progStats $1 "compact" | grep $2
}

function diffProgs {
  echo Checking property $PROPERTY...
  diffProg "gic_ack" $1
  diffProg "gic_collatz" $1
  diffProg "gic_digits_of_e1" $1
  diffProg "gic_fib" $1
  diffProg "gic_ntak" $1
  diffProg "gic_primes" $1
  diffProg "gic_church" $1
  diffProg "gic_queens" $1
  diffProg "gic_queens_num" $1
  diffProg "gic_quick_sort" $1
  diffProg "gic_tree_sort" $1
  diffProg "gic_reverse" $1
}

PROPERTY="L1-dcache-loads"
diffProgs $PROPERTY