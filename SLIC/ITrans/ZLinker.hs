-- | The linker for the 0-order language of the intensional transformation.
-- 
--   Some functions take a single DFI; this is the result of merging all
--   input module DFIs using "DFI.mergeDFIs".
-- 

module SLIC.ITrans.ZLinker (mergeAndLinkZ) where

import Data.Map (lookup)
import SLIC.Front.Defunc (DfFlags, genDfModFinal)
import SLIC.DFI (DFI)
import SLIC.ITrans.ITrans (itransM)
import SLIC.ITrans.HFtoHI (fromHFtoHI)
import SLIC.ITrans.HItoZI (fromHItoZI)
import SLIC.ITrans.Syntax (ModZ, ProgZ, DefZ(DefZ), ExprZ(ConstrZ))
import SLIC.SyntaxAux
import SLIC.Types

-- | Merges together a number of 0-order modules, adding the required closure
--   constructor and dispatching functions.
mergeAndLinkZ :: DfFlags -> DFI -> [ModZ] -> ProgZ
mergeAndLinkZ flags@(_, str, _, _, _) dfi mods =
  concatProgs [concatCode mods, genDfCodeZ flags dfi, bProgZ str]

-- | Generates the 0-order module needed for linking defunctionalized modules.
--   Also generates the signatures table of the module.
genDfModZ :: DfFlags -> DFI -> ModZ
genDfModZ flags dfi =
  let (dfModFL, _) = genDfModFinal flags dfi
  in  fromHItoZI $ itransM $ fromHFtoHI dfModFL

-- | Generates the 0-order code needed for linking defunctionalized modules.
genDfCodeZ :: DfFlags -> DFI -> ProgZ
genDfCodeZ flags dfi = modProg $ genDfModZ flags dfi

-- | The built-in data types and constructors.
bProgZ :: Strictness -> ProgZ
bProgZ s =
  let Just (t_cons_0, _) = Data.Map.lookup bf_cons_0 builtinTEnv
      Just (t_cons_1, _) = Data.Map.lookup bf_cons_1 builtinTEnv
  in  Prog [ Data dtList [v_a]
             [ DConstr bf_Cons 
               [DT t_cons_0 s Nothing, DT t_cons_1 s Nothing] Nothing
             , DConstr bf_Nil  [] Nothing ]
           , Data dtUnit [] [ DConstr bf_Unit [] Nothing ]
           ]
           [ DefZ bf_Cons (ConstrZ bf_Cons)
           , DefZ bf_Nil  (ConstrZ bf_Nil )
           , DefZ bf_Unit (ConstrZ bf_Unit)
           ]