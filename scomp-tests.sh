#!/bin/bash
#
# Test the separate compilation features using the optimized LAR representation.
#
# The TC variable contains the type checker to use. For polymorphic examples, the
# type checker of the GHC API is used. For examples with 'let', the built-in
# type inference engine is used.
# 

set -e

echo --- Separate compilation tests [lar-opt] ---
echo -- [Example1] --
export TC="-ghc-tc"
./compile-link-mods.sh Examples/Modules/Example1/ModuleA Examples/Modules/Example1/Main
echo -- [Example2] --
export TC="-ghc-tc"
./compile-link-mods.sh Examples/Modules/Example2/Lib Examples/Modules/Example2/Main
echo -- [Example3] --
export TC="-gic-tc"
./compile-link-mods.sh Examples/Modules/Example3/Lib Examples/Modules/Example3/Main
echo -- [Example4] --
export TC="-ghc-tc"
./compile-link-mods.sh Examples/Modules/Example4/Lib Examples/Modules/Example4/Main
