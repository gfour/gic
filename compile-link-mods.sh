#!/bin/bash
# 
# Separately compiles two modules and then links them using the modular
# defunctionalization technique.
#

# Use libgc 7.2 @ greedy
GC_INCLUDE="-I/var/tmp/gfour/gc-inst-7.2/include/"
GC_LIB="-pthread /var/tmp/gfour/gc-inst-7.2/lib/libgc.a"
# Use libgc @ ~ptheof
# GC_INCLUDE="-I/home/ptheof/gc-inst/include"
# GC_LIB=/home/ptheof/gc-inst/lib/libgc.a

# Needed for tagged pointers to work.
OPT="-falign-functions"

set -e

source find-gic.sh
echo "Using GIC=${GIC}"

# generates a .hi file for the module (used by the GHC type checker)
function generateHI {
MOD=`basename $1`
echo Generating $1.hi...
pushd `dirname $1` > /dev/null
rm -f $MOD.o $MOD.c $MOD.hi
ghc -o /dev/null -ohi $MOD.hi -c $MOD.hs
rm -f $MOD.o $MOD.c
cp $MOD.hi ../../../
popd > /dev/null
}

# if using the GHC type checker, generate .hi files for the modules
if [ "$TC" = "-ghc-tc" ]; then
    generateHI $1
    generateHI $2
fi

./compile-module.sh $1
./compile-module.sh $2

echo Creating linker...

if [ "$TC" = "-ghc-tc" ]; then
    rm -f `basename $1`.hi `basename $2`.hi
fi

${GIC} -link $1 $2
echo Linking with gcc...
gcc $GC_INCLUDE -I . dfmod.c main-link.c "$1.g.o" "$2.g.o" $GC_LIB -o a.out -ggdb3 -Wall $OPT
echo -n LAR\ \ result:\  
./a.out

# GHCi test
echo -n GHCi result:\ 
echo "Main.result" | ghci -v0 $1.hs $2.hs
