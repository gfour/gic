#!/bin/bash
#
# Compiles and runs a program using the "simple" LAR representation.
# 
# Reads the following environment variables:
# 
#   CC       : the C compiler to use (default=gcc)
#   GICFLAGS : flags passed on to gic (e.g. the type checker to use)
#   CFLAGS2  : extra flags passed to the C compiler
# 

set -e

# USE_GMP=
USE_GMP="-DHAS_GMP -lgmp"
if [ "$CC" = "" ]; then
    CC=gcc
fi

# add -DGC and -DSSTACK to the gcc flags if compiling with -semigc
USE_GC="-DGC -DSSTACK"
# USE_GC="-DGC -DSSTACK -DVERBOSE_GC"

CFLAGS="-O3 -I . -ggdb3"
# CFLAGS2 are used but undefined: extra flags to be filled in from the command-line
# e.g. add -DUSE_TAGS if compiling with -tag

./gic -semigc $GICFLAGS $1 $2 $3 $4 $5 $6 $7 $8 $9 > /dev/null

cat c/gc.c >> main.c

CMD="${CC} ${CFLAGS} ${CFLAGS2} main.c ${USE_GC} ${USE_GMP}"
# echo $CMD
$CMD

./a.out
