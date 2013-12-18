#!/bin/bash
#
# Test the separate compilation features using the simple LAR representation.
# The GC variable controls whether semispace garbage collection is enabled.
#

set -e

# semispace GC (disabled)
GC="-nogc"
CFLAGS="-I ."

# semispace GC
# GC="-semigc"
# CFLAGS="-DGC -I ."

function compileLink {
    for module in $1 $2
    do
	echo Compiling module: ${module} --
	./gic -cmod -debug ${GC} ${module}.hs
	gcc -c ${CFLAGS} ${module}.c -o ${module}.o
    done
    echo Creating linker...
    ./gic ${GC} -link $1 $2
    echo Adding GC...
    cat c/gc.c >> main-link.c
    echo Linking with gcc...
    gcc ${CFLAGS} dfmod.c main-link.c $1.o $2.o -o a.out -ggdb3 -Wall

    echo -n LAR\ \ result:\  
    ./a.out

    # GHCi test
    echo -n GHCi result:\ 
    echo "Main.result" | ghci -v0 $1.hs $2.hs
}

echo --- Separate compilation tests [lar, ${GC}] ---
echo -- [Example1] --
compileLink Examples/Modules/Example1/ModuleA Examples/Modules/Example1/Main
echo -- [Example2] --
compileLink Examples/Modules/Example2/Lib Examples/Modules/Example2/Main
echo -- [Example2] --
compileLink Examples/Modules/Example3/Lib Examples/Modules/Example3/Main
echo -- [Example4] --
compileLink Examples/Modules/Example4/Lib Examples/Modules/Example4/Main
