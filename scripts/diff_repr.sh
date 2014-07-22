#!/bin/bash

# Test script that reads the results of the counter measurements, and generates the comparison tables.

# Takes three parameters: the benchmark name, the representation, and the property to find.
function progStats {
# tail -n +6 $1_$2_stats.txt | head -n -3 | awk '{print $1,$2}'
STATS_FILE=$1_$2_$3_stats.txt
echo Reading stats file $STATS_FILE... > /dev/stderr
# tail -n +6 $STATS_FILE | head -n -3 | cut -b 1-17,19-44,50-54,82-86 --output-delimiter="  "
grep $3 $STATS_FILE | cut -b 1-17,19-44,50-54,82-86 --output-delimiter="  "
}

# Takes two parameters: the benchmark name and the property to find.
function diffProg {
  BINARY=gic_$1
  echo -e \# Reading property \'$2\' of $BINARY... > /dev/stderr
  STATS_FULL=`progStats $BINARY "full" $2 | grep $2`
  STATS_COMPACT=`progStats $BINARY "compact" $2 | grep $2`
  # echo \# $STATS_FULL
  # echo \# $STATS_COMPACT
  if [ "$FIELD_TYPE" = "NUM" ]
  then
    # Print out the numerical values.
    FIELD_NUM_FULL=`echo -n $STATS_FULL | awk '{printf "%d", $1}'`
    FIELD_NUM_COMPACT=`echo -n $STATS_COMPACT | awk '{printf "%d", $1}'`
    echo -e $1 '\t' $FIELD_NUM_FULL '\t' $FIELD_NUM_COMPACT
  elif [ "$FIELD_TYPE" = "PERCENT" ] 
  then
    # Print out the percent values.
    FIELD_PCENT_FULL=`echo -n $STATS_FULL | awk '{printf "%d.%d", $3, 44}'`
    FIELD_PCENT_COMPACT=`echo -n $STATS_COMPACT | awk '{printf "%d.%d", $3, $4}'`
    echo -e $1 '\t' $FIELD_PCENT_FULL '\t' $FIELD_PCENT_COMPACT
  else
    echo FIELD_TYPE is not set. > /dev/stderr
    exit
  fi
}

# Takes a single parameter, the property to measure.
function diffProgs {
  echo benchmark GIC_full GIC_compact
  diffProg "ack" $1
  diffProg "collatz" $1
  diffProg "digits_of_e1" $1
  diffProg "fib" $1
  diffProg "ntak" $1
  diffProg "primes" $1
  diffProg "church" $1
  diffProg "queens" $1
  diffProg "queens_num" $1
  diffProg "quick_sort" $1
  diffProg "tree_sort" $1
  diffProg "reverse" $1
}

# Generates the table of side-by-side comparison between the two representations.
function diffProperty {
  PROPERTY=$1
  FILE="diff_${PROPERTY}_${FIELD_TYPE}.txt"
  diffProgs $PROPERTY > $FILE
  # cat $FILE
  echo Results saved in $FILE.
}

FIELD_TYPE="NUM"
diffProperty "L1-dcache-loads"
diffProperty "L1-dcache-load-misses"
diffProperty "L1-dcache-stores"
diffProperty "L1-dcache-store-misses"
diffProperty "L1-dcache-prefetches"
diffProperty "L1-icache-loads"
diffProperty "L1-icache-load-misses"
diffProperty "LLC-loads"
diffProperty "LLC-load-misses"
diffProperty "LLC-stores"
diffProperty "LLC-store-misses"
diffProperty "dTLB-loads"
diffProperty "dTLB-load-misses"
diffProperty "dTLB-stores"
diffProperty "dTLB-store-misses"
diffProperty "iTLB-loads"
diffProperty "iTLB-load-misses"
diffProperty "branch-loads"
diffProperty "branch-load-misses"
diffProperty "cpu-cycles"
diffProperty "instructions"
diffProperty "cache-references"
diffProperty "cache-misses"
diffProperty "branch-instructions"
diffProperty "branch-misses"
diffProperty "bus-cycles"
diffProperty "ref-cycles"
FIELD_TYPE="PERCENT"
diffProperty "L1-dcache-load-misses"
diffProperty "L1-icache-load-misses"
diffProperty "LLC-load-misses"
diffProperty "dTLB-load-misses"
diffProperty "iTLB-load-misses"
diffProperty "cpu-cycles"
diffProperty "instructions"
diffProperty "cache-misses"
diffProperty "branch-misses"
