# GHC profiling options
# HPROFILE=-prof -fprof-auto
HPROFILE=

# C profiling options
#CPROFILE=-pg -fprofile-arcs
CPROFILE=

#CC=gcc-3.0
CC=gcc
CFLAGS=-O3 -std=c99 -pedantic -Wall -g $(CPROFILE)

EXTRA=-package ghc -fwarn-unused-binds -Wall
#EXTRA=-Wall
#EXTRA=
#EXTRA=-fhpc
#EXTRA=-auto -prof

# Use of the GHC API for type checking.
USE_GHC=-DUSE_GHC -XRankNTypes
# USE_GHC=

HADDOCK=haddock

.PHONY: gic

gic:
	ghc $(EXTRA) $(HPROFILE) $(USE_GHC) --make -o $@ Main.hs

erl-gic: gic
	gcc $(CFLAGS) -fpic -shared -DERLANG_NIF_C c/cat.c -o libcat.so

clean:
	$(RM) *.hi *.o SLIC/*.hi SLIC/*.o SLIC/*/*.hi SLIC/*/*.o SLIC/*/*/*.hi SLIC/*/*/*.o libcat.so main.c dfmod.c main-link.c main.erl main.beam gc.beam warehouse.beam warehouse_redis.beam test.maude a.out gmon.out *~ *.da graph.dot doc/haddock/* Examples/Modules/Example*/*.dfi Examples/Modules/Example*/*.ii Examples/Modules/Example*/*.o
	$(RM) -r doc/doxygen/* 

distclean: clean
	$(RM) gic

haddock:
	$(HADDOCK) Main.hs --optghc='-package ghc' -h -o doc/haddock
