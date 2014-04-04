#!/bin/bash

# Compile a module Module.hs to Module.o through Module.c using the
# separate compilation mode of the LAR back-end of GIC.
#
# To call this script, omit the .hs module extension:
#
#   ./compile-module.sh Examples/Modules/example2/Main
#

# Use libgc 7.2 @ greedy
GC_INCLUDE="-I/var/tmp/gfour/gc-inst-7.2/include/"
GC_LIB="-pthread /var/tmp/gfour/gc-inst-7.2/lib/libgc.a"
# Use libgc @ ~ptheof
# GC_INCLUDE="-I/home/ptheof/gc-inst/include"
# GC_LIB=/home/ptheof/gc-inst/lib/libgc.a

# Use the allocator of semi-gc
# GC_INCLUDE="-I ./c/"

# STACKTRACE="+RTS -xc -RTS"
STACKTRACE=

# Needed for tagged pointers to work.
OPT="-falign-functions"

set -e

echo Compiling $1.g.o...
./gic $TC $STACKTRACE -cmod $1.hs && gcc $OPT $GC_INCLUDE -I . -c $1.c -o $1.g.o -ggdb3 -Wall
