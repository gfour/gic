-- | The syntax of the intermediate language used by the LAR back-end.
-- 

module SLIC.LAR.SyntaxLAR (ProgL, BlockL(..), CCstrName(..), ExprL(..), IsActuals, ModL, PatL(..), calcFuncArities, countPMDepth, countPMDepths, getBlockName, isFun, mkCC, printLAR) where

import Data.Map (empty, insert, insertWithKey)
import SLIC.AuxFun (ierr, showStrings, spaces)
import SLIC.Constants (nl)
import SLIC.ITrans.Syntax (pprintBinds)
import SLIC.SyntaxAux
import SLIC.SyntaxFL
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
  let (cArity, cId) = findArId c cids
  in CC c cId cArity 

-- | A LAR expression.
data ExprL  = LARCall QName [QName]    -- ^ call variable with a LAR of variables
            | LARC Const [ExprL]       -- ^ built-in constant application
            | ConstrL CCstrName        -- ^ constructor call
            | BVL QName CaseLoc        -- ^ bound variable (constructor projection)
            | CaseL CaseLoc ExprL [PatL] -- ^ pattern matching expression

-- | A LAR pattern.
data PatL   = PatL CCstrName ExprL Bool

instance PPrint CCstrName where
  pprintPrec _ (CC c cid ar) = pprintTH c.("{"++).shows cid.("}/"++).shows ar

instance PPrint BlockL where
    pprintPrec _ (DefL v e bind) =
        pprint v.spaces 1.showStrings " " (map qName bind).
        (" = "++).pprint e
    pprintPrec _ (ActualL v act e) =
        pprint v.(" = "++).(if act then ("ACTUAL."++) else id).pprint e
        
instance PPrint ExprL where
    pprintPrec _ (LARCall v vs) = pprint v.spaces 1.showStrings " " (map qName vs)
    pprintPrec p (LARC cn el) = prettyConst p cn el
    pprintPrec _ (ConstrL c) = pprint c
    pprintPrec _ (BVL v (d, f)) = pprintBVD v d.("{"++).pprint f.("}"++)
    pprintPrec _ (CaseL (d, _) e pats) =
        let pprintPats []             = id
            pprintPats (pat : ps)     = showPat pat.pprintPats ps
            showPat (PatL c0 e0 b)= 
                nl.(" "++).
                (if b then ("#"++) else spaces 1).
                spacing d.("| "++).pprint c0.(" -> "++).pprint e0.
                pprintBinds b
        in  ("case "++).pprint e.(" of{"++).showsDep d.("}"++).
            pprintPats pats
            
-- | Pretty printer for LAR modules. The typing environment is also given.
printLAR :: TEnv -> ModL -> IO ()
printLAR eLAR modL =
  putStrLn "== Environment ==" >>
  putStrLn (pprintE eLAR "") >>
  putStrLn "== Module ==" >>
  putStr   (pprint modL "")

-- | Filter for function definition blocks.
isFun :: BlockL -> Bool
isFun (DefL _ _ _) = True
isFun (ActualL _ _ _) = False

-- | Returns the name of a block.
getBlockName :: BlockL -> QName
getBlockName (DefL vnm _ _) = vnm
getBlockName (ActualL vnm _ _) = vnm

-- | Counts the maximum depth of nested pattern matching expressions
--   in a LAR expression.
countPMDepth :: ExprL -> Int
countPMDepth (LARCall _ _) = 0
countPMDepth (ConstrL _) = 0
countPMDepth (BVL _ _) = 0
countPMDepth (LARC _ []) = 0
countPMDepth (LARC _ args) = maximum (map countPMDepth args)
-- addition of dict arrays, to reuse/do any better we need some analysis
countPMDepth (CaseL _ e pats) = 
  let patE (PatL _ eP _) = eP
      maxPatDepth [] = 0
      maxPatDepth ps = maximum (map countPMDepth (map patE ps))
  in  1 + (countPMDepth e) + (maxPatDepth pats)

-- | Counts the maximum depths of nested pattern matching expressions
--   in a LAR program, for every definition.
countPMDepths :: ModL -> PMDepths
countPMDepths mL = 
  let blocks = progDefs $ modProg mL
      mUpd m (DefL v e _) = insert v (countPMDepth e) m
      mUpd m (ActualL v _ e) = insert v (countPMDepth e) m
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
      aux m ((ActualL _ _ _) : bs) = aux m bs
      checker v a a' =
        if a==a' then a
        else ierr $ "Duplicate function "++(qName v)++" of different arities "++
             (show a)++", "++(show a')
  in  aux empty blocks
