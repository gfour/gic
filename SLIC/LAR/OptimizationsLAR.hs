-- | LAR Optimizations.
-- 
-- * Actuals inlining.

module SLIC.LAR.OptimizationsLAR (inlineActs) where

import SLIC.LAR.SyntaxLAR
import SLIC.SyntaxAux (Mod(Mod), Prog(Prog))

-- * Inline actuals

-- | Removes the actuals() operator from constant formals.
inlineActs :: ModL -> ModL
inlineActs (Mod m es is (Prog dts blocks) an tcs) =
  let inlineActsB b@(DefL _ _ _) = b
      inlineActsB   (ActualL v True e@(LARC _ [])) =
        ActualL v False e
      inlineActsB b@(ActualL _ _ _) = b
  in  Mod m es is (Prog dts (map inlineActsB blocks)) an tcs
