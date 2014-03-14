-- | A pattern compiler from FL with full patterns (containing arbitrary
--   expressions), to simple patterns (one head constructor and a list of
--   pattern-bound formals).
-- 
--   Based on the pattern compiler described in "Efficient Compilation of
--   Pattern-Matching", Philip Wadler, Chapter 5, in "The Implementation
--   of Functional Programming Languages", Simon L. Peyton Jones, 1987.
-- 

module SLIC.Front.PatternCompiler (patComp, patCompMatches) where

import SLIC.AuxFun (ierr)
import SLIC.Front.Typeclass
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- | The pattern compiler entry point.
patComp :: ProgFH -> [TcInstFH] -> (ProgF, [TcInstF])
patComp (Prog dtsFL defsFH) tcInsts =
  let pcE :: ExprFH -> ExprF
      pcE (XF var)  = XF var
      pcE (ConF c el) = ConF c (map pcE el)
      pcE (FF v el)   = FF v (map pcE el)
      pcE (ConstrF c el) = ConstrF c (map pcE el)
      pcE (CaseF d e s pats) =
        if allSimple pats then
          CaseF d (pcE e) s
          (map (\(PatB (fP, pI) eP)->PatB (pcP fP, pI) (pcE eP)) pats)
        else
          let mkMatch (PatB (fPat, _) eP) = ([fPat], Nothing, eP)
          in  patCompMatches [pcE e] (map mkMatch pats)
      pcE (LetF d defs e) = LetF d (map pcD defs) (pcE e)
      pcE (LamF d v e) = LamF d v (pcE e)
      pcD :: DefFH -> DefF
      pcD (DefF f fs e) = DefF f fs (pcE e)
      pcP :: FullPat -> SimplePat
      pcP (FPatC c vars) = SPat c (map (\(FPatV v)->v) vars)
      pcP (FPatI i)      = SPat (intConstrQN i) []
      pcP _ = ierr "pcP: pattern not simple"
      pcTcI (TcInst tcn tv methods) = TcInst tcn tv (map pcD methods)
  in  (Prog dtsFL (map pcD defsFH), map pcTcI tcInsts)

-- | Checks if all patterns are simple.
allSimple :: [PatFH] -> Bool
allSimple pats =
  let isSimple (FPatC _ ps) = all isFPV ps
      isSimple (FPatI _)    = True
      isSimple _            = False
  in  all (\(PatB (fp, _) _)->isSimple fp) pats

-- | A pattern guard is just an expression (well-formedness checked by the parser).
type GuardFH = ExprFH
-- | A match is a list of full patterns, maybe a guard, and an expression body.
type Match = ([FullPat], Maybe GuardFH, ExprFH)

-- | The core function of the pattern compiler, takes a list of expression
--   scrutinees, a list of matches and generates the resulting case expression.
patCompMatches :: PPrint a => [ExprFL a] -> [Match] -> ExprFL a
patCompMatches es _ =
    error $ "Found expressions that need the pattern compiler: "++(pprint es "")
