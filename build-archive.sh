#!/bin/bash
TARGET_DIR=gic-pack
mkdir -p $TARGET_DIR $TARGET_DIR/SLIC \
   $TARGET_DIR/SLIC/Distr $TARGET_DIR/SLIC/Front $TARGET_DIR/SLIC/ITrans \
   $TARGET_DIR/SLIC/LAR $TARGET_DIR/SLIC/Maude $TARGET_DIR/SLIC/TTD \
   $TARGET_DIR/SLIC/Front/LLifter \
   $TARGET_DIR/c $TARGET_DIR/erlang $TARGET_DIR/maude \
   $TARGET_DIR/Examples $TARGET_DIR/Examples/Num $TARGET_DIR/Examples/Data \
   $TARGET_DIR/Examples/NewBench \
   $TARGET_DIR/Examples/Polymorphic $TARGET_DIR/Examples/GADT \
   $TARGET_DIR/Examples/Modules $TARGET_DIR/Examples/Modules/Example1 \
   $TARGET_DIR/Examples/Modules/Example2 $TARGET_DIR/Examples/Modules/Example3 \
   $TARGET_DIR/Examples/Modules/Example4
cp bench-libGC.sh eval_maude.sh \
   newbench.sh Main.hs Makefile README testLangs.sh scomp-tests.sh \
   run_lar.sh compile-link-mods.sh compile-module.sh \
   $TARGET_DIR
cp SLIC/*.hs $TARGET_DIR/SLIC
cp SLIC/Distr/*.hs $TARGET_DIR/SLIC/Distr
cp SLIC/Front/*.hs $TARGET_DIR/SLIC/Front
cp SLIC/ITrans/*.hs $TARGET_DIR/SLIC/ITrans
cp SLIC/LAR/*.hs $TARGET_DIR/SLIC/LAR
cp SLIC/Maude/*.hs $TARGET_DIR/SLIC/Maude
cp SLIC/TTD/*.hs $TARGET_DIR/SLIC/TTD
cp SLIC/Front/LLifter/*.hs $TARGET_DIR/SLIC/Front/LLifter
cp c/gc.h c/gc.c c/cat.c c/lar.h c/lar_opt.h c/lar_compact.h c/gic_builtins.h \
   $TARGET_DIR/c
cp maude/full-maude26.maude maude/eduction.maude \
   $TARGET_DIR/maude
cp Examples/NewBench/*.hs \
   $TARGET_DIR/Examples/NewBench
cp Examples/Num/exmh*.hs Examples/Num/myex*.hs Examples/Num/memoize.hs \
   Examples/Num/bigints.hs \
   $TARGET_DIR/Examples/Num
cp Examples/Data/*.hs $TARGET_DIR/Examples/Data
cp Examples/Polymorphic/*.hs $TARGET_DIR/Examples/Polymorphic
cp Examples/GADT/*.hs $TARGET_DIR/Examples/GADT
cp Examples/Modules/Example1/ModuleA.hs Examples/Modules/Example1/Main.hs \
   $TARGET_DIR/Examples/Modules/Example1
cp Examples/Modules/Example2/Lib.hs Examples/Modules/Example2/Main.hs \
   $TARGET_DIR/Examples/Modules/Example2
cp Examples/Modules/Example3/Lib.hs Examples/Modules/Example3/Main.hs \
   $TARGET_DIR/Examples/Modules/Example3
cp Examples/Modules/Example4/Lib.hs Examples/Modules/Example4/Main.hs \
   $TARGET_DIR/Examples/Modules/Example4
tar -czf gic.tar.gz $TARGET_DIR

