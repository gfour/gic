-- | Translates the zero-order intensional language (ZOIL) of the
--   intensional transformation to the LAR language needed for code
--   generation.
-- 
--   During translation, constructors are paired with assigned numerical IDs
--   and arity information, to be used during code generation.
-- 

module SLIC.LAR.ZItoLAR (fromZOILtoLAR) where

import SLIC.ITrans.Syntax
import SLIC.LAR.SyntaxLAR
import SLIC.SyntaxAux(Mod(..), Prog(Prog), PatB(PatB))
import SLIC.Types

-- | Translates ZOIL to LAR syntax.
fromZOILtoLAR :: FuncSigs -> CIDs -> ModZ -> ModL
fromZOILtoLAR sigs cids m =
  let Prog dt ds = modProg m
      pL = Prog dt (concat (map (mkBlocks sigs cids) ds))
  in  m{modProg=pL}

-- | Translates a 0-order definition to LAR blocks. If the definition is a
--   function, it is translated to a LAR function block. If it is a list of N
--   actuals, it is translated to a list of N LAR variable blocks.
mkBlocks :: FuncSigs -> CIDs -> DefZ -> [BlockL]
mkBlocks sigs cids (DefZ x e) =
  [DefL x (mkExp sigs cids e) (frmsOf x sigs)]
mkBlocks sigs cids (ActualsZ qn m exps) =
  let aux :: (Int, ExprZ) -> BlockL
      aux (i, e) = 
        let qn' = procLName (larIIndex (m, i)) qn
        in  ActualL qn' True (mkExp sigs cids e)
  in  map aux (zip [0..] exps)          

mkExp :: FuncSigs -> CIDs -> ExprZ -> ExprL
mkExp _ _ (XZ (V n))    = LARCall n []
mkExp _ _ (XZ (BV n d)) = BVL n d
mkExp sigs cids (ConZ c exprs) = LARC c (map (mkExp sigs cids) exprs)
mkExp _ _ (FZ NOp v) = LARCall v []
mkExp sigs _ (FZ (Call idx) v) =
  let args z = [ procLName (larIIndex idx) x | x <- (frmsOf z sigs) ]
  in  LARCall v (args v)
-- constructors are accompanied by their numbers and arities (for compilation)
mkExp _ cids (ConstrZ c) =
  ConstrL (mkCC c cids)
mkExp sigs cids (CaseZ d e ps) =
  let mkPat (PatB (cP, pI) eP) = PatB (mkCC cP cids, pI) (mkExp sigs cids eP)
  in  CaseL d (mkExp sigs cids e) (map mkPat ps)

-- | Translates an intensional index of a variable to a name suitable for C.
larIIndex :: IIndex -> SName -> String
larIIndex (m, i) sn = sn++"__"++m++"_"++(show i)
