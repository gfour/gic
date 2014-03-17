-- | Helper definitions for the LAR back-end.

module SLIC.LAR.LARAux (ConfigLAR(..), enumNames, mkAct, wrapIfMacro,
                        wrapIfNotMacro,
                        wrapIfARGTAGS, wrapIfGMP, wrapIfOMP, wrapIfGC) where

import SLIC.Constants
import SLIC.LAR.LARGraph
import SLIC.LAR.SyntaxLAR
import SLIC.State
import SLIC.Types

-- | The configuration needed for compilation.
data ConfigLAR =
  ConfigLAR
  { getCBNVars   :: CBNVars    -- ^ table of call-by-name formals
  , getStricts   :: Stricts    -- ^ table of strict variables
  , getCIDs      :: CIDs       -- ^ numeric constructor id information
  , getArities   :: Arities    -- ^ table of function arities    
  , getOptions   :: Options    -- ^ the user options  
  , getPMDepths  :: PMDepths   -- ^ the mapping between variables
                               --   and pattern matching depth
  , getCAFnmsids :: CAFDct     -- ^ dictionary from CAF names to indices
  , getModName   :: MName      -- ^ the name of the module being compiled
  }
  
-- | Label each argument with a number (index in the LAR).
enumNames :: [QName] -> [(Int, QName)]
enumNames names = zip [0..] names

-- | Generates C code for an intensional operator (/ACTUAL/).
mkAct :: IsActuals -> Options -> ShowS
mkAct b opts =
  if b then
    ("ACTUAL;"++).nl
  else
    -- for functions, output a graphviz entry for the LAR entered,
    -- if graphviz trace mode is enabled
    logPrev opts

-- | Wraps a piece of code in @\#ifdef USE_OMP ... \#else ... \#endif@.
wrapIfOMP :: ShowS -> ShowS -> ShowS
wrapIfOMP = wrapIfMacro "USE_OMP"

-- | Wraps a piece of code in @\#ifdef HAS_GMP ... \#else ... \#endif@.
wrapIfGMP :: ShowS -> ShowS -> ShowS
wrapIfGMP = wrapIfMacro "HAS_GMP"

-- | Wraps a piece of code in @\#ifdef ARGTAGS ... \#else ... \#endif@.
wrapIfARGTAGS :: ShowS -> ShowS -> ShowS
wrapIfARGTAGS = wrapIfMacro "ARGTAGS"

-- | Wraps a piece of code in @\#ifdef GC ... \#else ... \#endif@.
wrapIfGC :: ShowS -> ShowS -> ShowS
wrapIfGC = wrapIfMacro "GC"

-- | Wraps a piece of code in @\#ifdef macro ... \#else .. \#endif@.
wrapIfMacro :: String -> ShowS -> ShowS -> ShowS
wrapIfMacro macroName s1 s2 =
  ("#ifdef "++).(macroName++).nl.
  s1.
  ("#else"++).nl.
  s2.
  ("#endif /* "++).(macroName++).(" */"++).nl

-- | Wraps a piece of code in @\#ifndef macro ... \#endif@.
wrapIfNotMacro :: String -> ShowS -> ShowS
wrapIfNotMacro macroName s =
  ("#ifndef "++).(macroName++).nl.
  s.
  ("#endif /* "++).(macroName++).(" */"++).nl
