#!/bin/bash
if [ "$HADDOCK" = "" ];
then
HADDOCK=/usr/local/stow/ghc-7.4.1/bin/haddock
fi
$HADDOCK Main.hs --optghc='-package ghc' -h -o doc/haddock
#haddock *.hs -h -o doc/haddock

