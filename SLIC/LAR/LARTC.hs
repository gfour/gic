-- | Code generator for type classes.
-- 

module SLIC.LAR.LARTC where

import Data.List (intersperse)
import SLIC.AuxFun (foldDot)
import SLIC.Constants
import SLIC.Front.Typeclass
import SLIC.State
import SLIC.SyntaxAux
import SLIC.Tags
import SLIC.Types

{-
builtinTCs :: Options -> ShowS
builtinTCs opts =
  if optTag opts then
    mkC_builtinTcInsts "Show"
  else id
-}

-- | Generates the C name for the implementation of type class tc for dt.
tcNameC :: TcName -> DTName -> ShowS
tcNameC tc dt = (tc++).("_"++).pprint dt      

{-
-- | Generates the code for all built-in implementations of a type class.
mkC_builtinTcInsts :: TcName -> ShowS
mkC_builtinTcInsts tcName =
  let nmTcInst (TcInst (tcN, dtN) _) = tcNameC tcN dtN
  in  mkC_TcInst btc_Show_Int.
      ("void * tcImpl$"++).(tcName++).("[] = { "++).
      nmTcInst btc_Show_Int.
      (" };"++).nl

-- | Generates the C dictionary for a type class implementation.
mkC_TcInst :: TcInst -> ShowS
mkC_TcInst (TcInst (tc, dt) dict) =
  let mkMember (_, ifun) = pprint ifun
  in  ("void * "++).tcNameC tc dt.("[] = { "++).
      foldDot id (intersperse (", "++) (map mkMember dict)).
      (" };"++).nl
-}
