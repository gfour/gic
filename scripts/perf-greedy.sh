#!/bin/bash
# perf stat -e L1-dcache-loads,L1-dcache-load-misses,L1-dcache-stores,L1-dcache-store-misses,L1-dcache-prefetches,L1-icache-loads,L1-icache-load-misses,LLC-loads,LLC-load-misses,LLC-stores,LLC-store-misses,dTLB-loads,dTLB-load-misses,dTLB-stores,dTLB-store-misses,iTLB-loads,iTLB-load-misses,branch-loads,branch-load-misses,cpu-cycles,instructions,cache-references,cache-misses,branch-instructions,branch-misses,bus-cycles,ref-cycles -o $2_$1_stats.txt $2
if [ "$1" == "" ] || [ "$2" == "" ]
then
  echo -e Usage: perf-greedy \<property\> \<program\>
  echo -e Run \'perf list\' to see the properties available.
else
  # Check if the property depends on another property for percent counts and set EXTRA.
  case "$1" in
    branch-misses)
      EXTRA=",branch-instructions"
      ;;
    dTLB-load-misses)
      EXTRA=",dTLB-loads"
      ;;
    iTLB-load-misses)
      EXTRA=",iTLB-loads"
      ;;
    L1-icache-load-misses)
      EXTRA=",L1-icache-loads"
      ;;
    cache-misses)
      EXTRA=",cache-references"
      ;;
    instructions)
      EXTRA=",cpu-cycles"
      ;;
    L1-dcache-load-misses)
      EXTRA=",L1-dcache-loads"
      ;;
    LLC-load-misses)
      EXTRA=",LLC-loads"
      ;;
  esac
  perf stat -e $1$EXTRA -o $2_$1_stats.txt $2
fi
