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

GC=0

if [ "$GC" = "1" ]; then
    # Add -DGC to the gcc flags to use the semi-space collector.
    USE_GC="-DGC"
    # USE_GC="-DGC -DVERBOSE_GC"
else
    USE_GC=""
    LIBUNWIND=""
fi

CFLAGS="-O3 -I . -ggdb3"
# CFLAGS2 are used but undefined: extra flags to be filled in from the command-line
# e.g. add -DUSE_TAGS if compiling with -tag

./gic -semigc $GICFLAGS $* > /dev/null

cat c/gc.c >> main.c

CMD="${CC} main.c ${CFLAGS} ${CFLAGS2} ${USE_GC} ${USE_GMP} ${LIBUNWIND}"
# echo $CMD
$CMD

./a.out
