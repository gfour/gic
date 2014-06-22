-- | The syntax of the intermediate language used by the LAR back-end.
-- 

module SLIC.LAR.SyntaxLAR (ProgL, BlockL(..), CCstrName(..), ExprL(..),
                           IsActuals, ModL, calcFuncArities,
                           countPMDepthL, countPMDepthsL, getBlockName,
                           isFun, mkCC, printLAR) where

import Data.Map (empty, insert, insertWithKey)
import SLIC.AuxFun (ierr, showStrings, spaces)
import SLIC.Constants (nl)
import SLIC.SyntaxAux
import SLIC.Types

-- * The LAR language

-- | A LAR program.
type ProgL  = Prog BlockL

-- | A LAR block.
data BlockL =
    DefL QName ExprL [QName]       -- ^ a function is an expression and bindings
  | ActualL QName IsActuals ExprL  -- ^ an actual variable is an expression

-- | A flag to indicate that a LAR statement is an /actuals()/. It is set to
--   False by constant inlining (see "OptimizationsLAR").
type IsActuals = Bool

-- | Compiled LAR constructor (accompanied by numerical ID and arity).
data CCstrName = CC CstrName CID Arity

-- | A LAR module.
type ModL = Mod ProgL

-- | Returns a compiled constructor using a CIDs table.
mkCC :: CstrName -> CIDs -> CCstrName
mkCC c cids =
  let (cArity, cId) = findArID c cids
  in CC c cId cArity 

-- | A LAR expression.
data ExprL  = LARCall QName [QName] CI -- ^ call variable with a LAR of variables
            | LARC Const [ExprL]       -- ^ built-in constant application
            | ConstrL CCstrName        -- ^ constructor call
            | BVL QName CaseLoc        -- ^ bound variable (constructor projection)
            | CaseL CaseLoc ExprL [PatL] -- ^ pattern matching expression

-- | A LAR pattern.
type PatL = PatB CCstrName ExprL

instance PPrint CCstrName where
  pprint (CC c cid ar) = pprintTH c.("{"++).shows cid.("}/"++).shows ar

instance PPrint BlockL where
  pprint (DefL v e bind) =
    pprint v.spaces 1.showStrings " " (map qName bind).(" = "++).pprint e
  pprint (ActualL v act e) =
    pprint v.(" = "++).(if act then ("ACTUAL."++) else id).pprint e

instance PPrint ExprL where
  pprint (LARCall v vs _) = pprint v.spaces 1.showStrings " " (map qName vs)
  pprint (LARC cn el)     = prettyConst 0 cn el
  pprint (ConstrL c)      = pprint c
  pprint (BVL v cn)       = pprintBVC v cn
  pprint (CaseL cl@(cn, _) e pats) =
    let dep = tabIdxOf cn
        pprintPats []             = id
        pprintPats (pat : ps)     = nl.spaces dep.pprint pat.pprintPats ps
    in  ("case "++).pprint e.(" of{"++).pprintCaseLoc cl.("}"++).pprintPats pats

-- | Pretty printer for LAR modules. The typing environment is also given.
printLAR :: TEnv -> ModL -> IO ()
printLAR eLAR modL =
  putStrLn "== Environment ==" >>
  putStrLn (pprintE eLAR "") >>
  putStrLn "== Module ==" >>
  putStr   (pprint modL "")

-- | Filter for function definition blocks.
isFun :: BlockL -> Bool
isFun (DefL {}) = True
isFun (ActualL {}) = False

-- | Returns the name of a block.
getBlockName :: BlockL -> QName
getBlockName (DefL vnm _ _) = vnm
getBlockName (ActualL vnm _ _) = vnm

-- | Counts the maximum depth of nested pattern matching expressions
--   in a LAR expression.
countPMDepthL :: ExprL -> Int
countPMDepthL (LARCall _ _ _) = 0
countPMDepthL (ConstrL _) = 0
countPMDepthL (BVL _ _) = 0
countPMDepthL (LARC _ []) = 0
countPMDepthL (LARC _ args) = maximum (map countPMDepthL args)
-- addition of dict arrays, to reuse/do any better we need some analysis
countPMDepthL (CaseL (cn, _) e pats) = 
  let patE (PatB _ eP) = eP
      maxPatDepth [] = 0
      maxPatDepth ps = maximum $ map (countPMDepthL.patE) ps
      cDep = case cn of CLoc _ -> 1 ; CFrm _ -> 0
  in  cDep + (countPMDepthL e) + (maxPatDepth pats)

-- | Counts the maximum depths of nested pattern matching expressions
--   in a LAR program, for every definition.
countPMDepthsL :: ModL -> PMDepths
countPMDepthsL mL = 
  let blocks = progDefs $ modProg mL
      mUpd m (DefL v e _) = insert v (countPMDepthL e) m
      mUpd m (ActualL v _ e) = insert v (countPMDepthL e) m
      aux m [] = m
      aux m (b : bs) = aux (mUpd m b) bs
  in  aux empty blocks

-- | Calculates a table of all function arities.
calcFuncArities :: ModL -> Arities
calcFuncArities mL =
  let blocks = progDefs $ modProg mL
      aux m [] = m
      aux m ((DefL v _ vs) : bs) =
        aux (insertWithKey checker v (length vs) m) bs
      aux m ((ActualL {}) : bs) = aux m bs
      checker v a a' =
        if a==a' then a
        else ierr $ "Duplicate function "++(qName v)++" of different arities "++
             (show a)++", "++(show a')
  in  aux empty blocks
