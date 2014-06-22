-- | Simple translation from FL to the intermediate language HIL, used for
--   the transformation.

module SLIC.ITrans.HFtoHI (fromHFtoHI) where

import SLIC.AuxFun (ierr)
import SLIC.SyntaxFL
import SLIC.ITrans.Syntax
import SLIC.SyntaxAux
import SLIC.Types

-- | FL to HIL, expression translation.
fromHFtoHIe :: ExprF -> ExprH
fromHFtoHIe (XF vn) = XH vn
fromHFtoHIe (ConF cn exprs) = ConH cn (map fromHFtoHIe exprs)
fromHFtoHIe (FF (V vn) exprs ci) = FH NOp vn (map fromHFtoHIe exprs) ci
fromHFtoHIe (ConstrF c _) = ConstrH c
fromHFtoHIe app@(FF (BV _ _) _ _) =  
  ierr $ "fromHFtoHIe: the intensional transformation cannot process higher-order bound variables, as in "++(pprint app "")
fromHFtoHIe (CaseF cloc e _ pats) =
  let fromHFtoHIpat (PatB (SPat c bvs, _) eP) =
        let bvars = zip bvs (repeat cloc)
        in  PatB (c, PatInfo (areBound bvars eP)) (fromHFtoHIe eP)
  in  CaseH cloc (fromHFtoHIe e) (map fromHFtoHIpat pats)
fromHFtoHIe (LetF {}) = ierr "let found when translating from FL to HIL"
fromHFtoHIe (LamF {}) = ierr "lambda found when translating from FL to HIL"

-- | FL to HIL, definition translation.
fromHFtoHId :: DefF -> DefH
fromHFtoHId (DefF vn fs expr) = DefH vn (frmsToNames fs) (fromHFtoHIe expr)

-- | FL to HIL, program translation.
fromHFtoHIp :: ProgF -> ProgH
fromHFtoHIp (Prog ds defs) = Prog ds (map fromHFtoHId defs)

-- | FL to HIL, module translation.
fromHFtoHI :: ModF -> ModH
fromHFtoHI m = m{modProg=(fromHFtoHIp $ modProg m)}
