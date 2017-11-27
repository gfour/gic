#!/bin/bash
#
# Compiles and runs a Haskell file using the Boehm-Demers-Weiser garbage
# collector. The collector is available from:
#
#   http://www.hpl.hp.com/personal/Hans_Boehm/gc/
#
# Reads the following environment variables:
# 
#   CC       : the C compiler to use (default=gcc)
#   GICFLAGS : flags passed on to gic (e.g. the type checker to use)
#   OMP      : set it to any value, to enable the OpenMP runtime
#

set -e

# If optimization level is less than 2, add -falign-functions (required
# for using tagged argument pointers).
# CFLAGS="-I . -O1 -falign-functions -ggdb3"
CFLAGS="-I . -O3 -ggdb3"
# CFLAGS=-Wpadded

# Use libgmp
USE_GMP=
# USE_GMP="-DHAS_GMP -lgmp"

# Use libgc 7.2 @ greedy
GC_INCLUDE="-I/var/tmp/gfour/gc-inst-7.2/include/"
GC_LIB="-pthread /var/tmp/gfour/gc-inst-7.2/lib/libgc.a"
# Use libgc @ ~ptheof
# GC_INCLUDE="-I/home/ptheof/gc-inst/include"
# GC_LIB=/home/ptheof/gc-inst/lib/libgc.a

# If set, enables the shadow stack.
# SSTACK="-DSSTACK"
SSTACK=

if [ "$CC" == "" ]; then
    CC=gcc
fi

if [ "$OMP" != "" ]; then
#   echo Using the OpenMP-based runtime.
#   USE_OMP="-DGC_REDIRECT_TO_LOCAL -DUSE_OMP -fopenmp -fsplit-stack"
    USE_OMP="-DGC_REDIRECT_TO_LOCAL -DUSE_OMP -fopenmp"
else
    USE_OMP=""
fi

if [ "$GICFLAGS" = ""  ]; then
    GICFLAGS="-ghc-tc"
fi

source find-gic.sh
echo "Using GIC=${GIC}"

${GIC} ${GICFLAGS} -cl $* > /dev/null
CMD="${CC} ${SSTACK} ${GC_INCLUDE} ${CFLAGS} ${USE_GMP} ${USE_OMP} main.c ${GC_LIB}"
# echo $CMD
$CMD
./a.out
