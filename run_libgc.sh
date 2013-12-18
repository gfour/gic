#!/bin/bash
#
# Compiles and runs a Haskell file using the Boehm-Demers-Weiser garbage
# collector. The collector is available from:
#
#   http://www.hpl.hp.com/personal/Hans_Boehm/gc/
#

set -e

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

if [ "$GICFLAGS" = ""  ]; then
    GICFLAGS="-ghc-tc"
fi

./gic ${GICFLAGS} -cl $1 > /dev/null
gcc ${GC_INCLUDE} ${CFLAGS} ${USE_GMP} main.c ${GC_LIB}
./a.out
