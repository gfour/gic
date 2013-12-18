#!/bin/bash

set -e

#USE_GMP=
USE_GMP="-DHAS_GMP -lgmp"
if [ "$CC" = "" ]; then
#   CC=llvm-gcc-4.7
    CC=gcc
fi

# add -DGC to the gcc flags if compiling with -semigc
# add -DUSE_TAGS if compiling with -tag
CFLAGS="-O3 -I ."
# CFLAGS2 are used but undefined: extra flags to be filled in from the command-line

./gic -nogc $1 $2 $3 $4 $5 $6 $7 $8 $9

cat c/gc.c >> main.c

${CC} ${CFLAGS} ${CFLAGS2} main.c ${USE_GMP} && ./a.out
