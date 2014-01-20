#!/bin/bash

# Compiler sanity check

GHCI_FLAGS="-v0 -w -XGADTs"

############# Test tags

echo Testing -tag...
TAG_EXAMPLE=Examples/Data/example2.hs
CFLAGS2="-DUSE_TAGS" ./run_lar.sh -tag ${TAG_EXAMPLE}
echo "result" | ghci ${GHCI_FLAGS} ${TAG_EXAMPLE}

############# Test -enum type transformation

echo Testing -enum...
TAG_EXAMPLE=Examples/Data/example2.hs
CC=gcc ./run_lar.sh -enum ${TAG_EXAMPLE}
echo "result" | ghci ${GHCI_FLAGS} ${TAG_EXAMPLE}

############# Test arbitrary precision integers

echo Testing Integer...
INTEGER_EXAMPLE=Examples/Num/bigints.hs
INTEGER_EXAMPLE2=${INTEGER_EXAMPLE}.x.hs
echo ${INTEGER_EXAMPLE}, LAR: 
CC=gcc ./run_lar.sh -gic-tc-nsig ${INTEGER_EXAMPLE}
echo -n ${INTEGER_EXAMPLE}, GHCi: 
# replace custom multiplication operator with * for GHC
cat ${INTEGER_EXAMPLE} | sed -e "s/\`mulI\`/*/" > ${INTEGER_EXAMPLE2}
echo "result"  | ghci ${GHCI_FLAGS} ${INTEGER_EXAMPLE2}
rm ${INTEGER_EXAMPLE2}

############# Tests the call-by-name eduction interpreter

echo -- Call-by-name eduction --
for file in Examples/Num/memoize.hs Examples/Data/example1.hs Examples/Data/addsx.hs Examples/Data/biglist.hs Examples/Data/records.hs Examples/Data/unit.hs
do
  echo -n ${file}, call-by-name eduction:\ 
  ./gic -ecbn ${file}
  echo -n ${file}, GHCi:\ 
  echo "result" | ghci ${GHCI_FLAGS} ${file}
done

############# Tests the non-strict FL interpreter

echo -- Non-strict FL interpreter --
for file in Examples/Num/memoize.hs Examples/Data/example1.hs Examples/Data/addsx.hs Examples/Data/biglist.hs Examples/Data/records.hs Examples/Data/unit.hs Examples/Data/addsx.hs
do
  echo -n ${file}, non-strict FL interpreter:\ 
  ./gic -fl ${file}
  echo -n ${file}, GHCi:\ 
  echo "result" | ghci ${GHCI_FLAGS} ${file}
done

############# Tests the lazy eduction interpreter

echo -- Lazy eduction --
for file in Examples/Num/memoize.hs Examples/Data/example1.hs Examples/Data/addsx.hs Examples/Data/biglist.hs Examples/Data/nested.hs Examples/Data/records.hs Examples/Data/unit.hs
do
  echo -n ${file}, lazy eduction:\ 
  ./gic -e ${file}
  echo -n ${file}, GHCi:\ 
  echo "result" | ghci ${GHCI_FLAGS} ${file}
done

############# Compares the results from the LAR back-end against those from GHCi

ulimit -s 262143

function testLAR {
  #echo Testing file ${file}...

  echo -n $1, GHCi:\ 
  echo "result" | ghci ${GHCI_FLAGS} $1

  echo -n $1, LAR:\ 
  ./run_libgc.sh $1
}

unset OMP

echo -- LAR --

echo -- 1. Simple types --
export GICFLAGS=-gic-tc-nsig
for file in Examples/Num/exmh*.hs Examples/Num/myex*.hs Examples/Data/*.hs
do
    testLAR ${file}
done

echo -- 2. Polymorphic --
# use the type checker of the GHC API and explicit type signatures
export GICFLAGS=-ghc-tc
for file in Examples/Polymorphic/*.hs
do
    testLAR ${file}
done

echo -- 3. GADTs --
export GICFLAGS=-ghc-tc
for file in Examples/GADT/*.hs
do
    testLAR ${file}
done

echo -- 4. Parallel --
export GICFLAGS=-gic-tc-nsig
export OMP=1
GHCIFLAGS="${GHCIFLAGS} -threaded"
for file in Examples/Parallel/*.hs
do
    testLAR ${file}
done
