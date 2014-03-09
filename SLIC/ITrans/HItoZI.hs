-- | Translates the HIL intermediate language to the zero-order intensional
--   language (ZOIL). 
-- 
--   Used in the Yaghi-style intensional transformation.

module SLIC.ITrans.HItoZI (fromHItoZI) where

import SLIC.ITrans.Syntax
import SLIC.SyntaxAux
import SLIC.Types

-- | HIL -> ZOIL, program translation.
fromHItoZIp :: ProgH -> ProgZ
fromHItoZIp (Prog ds defs) =
  let fromHItoZId (DefH vn _ expr)   = DefZ vn (fromHItoZIe expr)
      fromHItoZId (ActualsH vn m exprs) =
        ActualsZ vn m (map fromHItoZIe exprs)
      fromHItoZIe (XH v) = XZ v
      fromHItoZIe (ConH cn exprs) = ConZ cn (map fromHItoZIe exprs)
      fromHItoZIe (FH NOp vn [])  = XZ (V vn)
      fromHItoZIe (ConstrH c)     = ConstrZ c
      fromHItoZIe (FH qOp vn exprs)
        | vn == constV =
          let [XH (V qn)] = exprs
          in  FZ qOp qn
        | otherwise    = FZ qOp vn
      fromHItoZIe (CaseH d e pats) =
        let pats' = map fromHItoZIpat pats
        in  CaseZ d (fromHItoZIe e) pats'
      fromHItoZIpat (PatB c e) = PatB c (fromHItoZIe e)
  in  Prog ds (map fromHItoZId defs)

-- | HIL -> ZOIL, module translation.
fromHItoZI :: ModH -> ModZ
fromHItoZI m = m{modProg=(fromHItoZIp $ modProg m)}
