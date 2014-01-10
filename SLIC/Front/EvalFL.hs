-- | A non-strict interpreter for FL.
-- 
--   The interpreter is based on activation records that contain variable bindings. Evaluation uses a 
--   context stack and pattern matching uses a list of nested contexts. The evaluation model is very close
--   to eduction after the intensional transformation is applied to the source program.
-- 

module SLIC.Front.EvalFL (evalFL) where

import qualified Data.Map (lookup)
import Data.Maybe (fromJust)
import Data.Sequence (elemIndexR, fromList)
import SLIC.AuxFun (ierr)
import qualified SLIC.SyntaxAux as SA
import qualified SLIC.SyntaxFL as SFL
import qualified SLIC.Types as T

type CN     = String                -- constructor name
type VN     = String                -- variable name
type BN     = String                -- built-in constant
type CaseID = Int                   -- pattern matching identifier
type CPos   = Int                   -- constructor component position

type Prog  = [FDef]                 -- program
type FDef  = (VN, ([VN], Expr))     -- function definition
data Expr  = V VN                   -- local
           | CProj CaseID CPos      -- constructor projection
           | BApp BN [Expr]         -- built-in application
           | FApp VN [Expr]         -- function application
           | CApp CN [Expr]         -- constructor application
           | Case CaseID Expr [Br]  -- pattern matching clause
           | LitI Int               -- integer literal
           | LitB Bool              -- boolean literal
           deriving (Show)
type Br = (CN, Expr)                -- pattern branch

type AR = [(VN, Expr)]              -- activation record

data Value = GI Int | GB Bool       -- ground values
           | SC Susp                -- suspended constructor

type Context = (AR, [(CaseID, Susp)]) -- context
type Stack = [Context]                -- stack of contexts

data Susp = Susp (CN, [Expr]) Stack

eval :: Prog -> Expr -> Stack -> Value
eval p (V vn) ((ar, _):st) =
  case lookup vn ar of
    Just eBinding -> eval p eBinding st
    Nothing -> error $ "no binding found for "++vn++" in "++(show ar)

{-  
  let Just eBinding = lookup vn ar                     -- see note 1.
  in  eval p eBinding st
-}
eval p (BApp "+" [e1, e2]) st =
  let GI i1 = eval p e1 st
      GI i2 = eval p e2 st
  in  GI (i1 + i2)
eval p (BApp "-" [e1, e2]) st =
  let GI i1 = eval p e1 st
      GI i2 = eval p e2 st
  in  GI (i1 - i2)
eval p (BApp "*" [e1, e2]) st =
  let GI i1 = eval p e1 st
      GI i2 = eval p e2 st
  in  GI (i1 * i2)
eval p (BApp "==" [e1, e2]) st =
  let v1 = eval p e1 st
      v2 = eval p e2 st
      equ (GI i1) (GI i2) = i1 == i2
      equ (GB b1) (GB b2) = b1 == b2
      equ _ _ = error "equ: malformed types"
  in  GB (v1 `equ` v2)
eval p (BApp "<=" [e1, e2]) st =
  let GI i1 = eval p e1 st
      GI i2 = eval p e2 st
  in  GB (i1 <= i2)
eval p (BApp "<" [e1, e2]) st =
  let GI i1 = eval p e1 st
      GI i2 = eval p e2 st
  in  GB (i1 < i2)
eval p (BApp ">" [e1, e2]) st =
  let GI i1 = eval p e1 st
      GI i2 = eval p e2 st
  in  GB (i1 > i2)
eval p (BApp ">=" [e1, e2]) st =
  let GI i1 = eval p e1 st
      GI i2 = eval p e2 st
  in  GB (i1 >= i2)
eval p (BApp "if" [e0, e1, e2]) st =
  let GB b = eval p e0 st
  in  if b then
        eval p e1 st
      else
        eval p e2 st
eval p (FApp f el) st =
  let st' = (mkAR p f el, []) : st
  in  -- trace2 ("entering "++f++":") $
      eval p (lookupBody p f) st'
eval _ (CApp cn el) st = SC (Susp (cn, el) st)
eval p (Case cid e bl) st@((ar, susps) : stPrev) =
  let SC susp@(Susp (cn, _) _) = eval p e st
      eBranch = findBranch cn bl
      st' = (ar, ((cid, susp) : susps)) : stPrev
  in  eval p eBranch st'
eval p (CProj cid cpos) ((_, susps) : _) =
  let Just (Susp (_, el) stSusp) = lookup cid susps    -- see note 2.
  in  eval p (el !! cpos) stSusp
eval _ (LitI i) _ = GI i
eval _ (LitB b) _ = GB b
eval _ e _ = error $ "evaluation stuck for: "++(show e)

findFDef :: [FDef] -> VN -> ([VN], Expr)
findFDef fdefs vn = case lookup vn fdefs of Just x -> x ; Nothing -> error $ "unknown function "++vn

findBranch :: CN -> [Br] -> Expr
findBranch cn bl = fromJust $ lookup cn bl   -- patterns are assumed exhaustive

lookupBody :: Prog -> VN -> Expr
lookupBody fdefs vn = snd $ findFDef fdefs vn

mkAR :: Prog -> VN -> [Expr] -> AR
mkAR fdefs vn el =
  let frms = fst $ findFDef fdefs vn
  in  -- trace2 ("formals="++(show frms)++" --> actuals="++(show el)) $
      zip frms el

transFL :: SFL.ProgF -> Prog
transFL p =
  let fdefs = SA.progDefs p
      transD (SFL.DefF qn frms e) =
        let vs = map (T.lName . SFL.fstFrm) frms
        in  (T.lName qn, (vs, transE vs e))
      transE vs (SFL.XF (T.V v)) =
        let vn = (T.lName v)
        in  if vn `elem` vs then (V vn) else (FApp vn [])
      transE _ (SFL.XF (T.BV v (Just cId, _))) =
        let bvName = T.lName v
        in  -- ugly hack to find the position of a constructor component
            case elemIndexR '_' (Data.Sequence.fromList bvName) of
              Nothing  -> ierr $ "No component position found for "++(T.pprint v "")
              Just idx ->
                case (reads (drop (idx+1) bvName) :: [(Int, String)]) of
                  [(cPos, _)] -> CProj cId cPos
                  _ -> ierr "error converting to constructor projection"
      transE vs (SFL.FF (T.V f) el) = FApp (T.lName f) (map (transE vs) el)
      transE _  (SFL.ConF (SA.LitInt i) []) = LitI i
      transE _  (SFL.ConF (SA.CN SA.CTrue) []) = LitB True
      transE _  (SFL.ConF (SA.CN SA.CFalse) []) = LitB False
      transE vs (SFL.ConF (SA.CN cn) el) = 
        let el' = map (transE vs) el
        in  case Data.Map.lookup cn SA.cOps of
              Just cStr -> BApp cStr el'
              Nothing   -> error $ "missing bop "++(T.pprint cn "")
      transE vs (SFL.ConstrF qn el) = CApp (T.lName qn) (map (transE vs) el)
      transE vs (SFL.CaseF (Just cid, _) e _ brs) = Case cid (transE vs e) (map (transB vs) brs)
      transE _ _ = error "transE: unhandled construct"
      transB vs (SFL.PatF (SFL.SPat c _) e) = (T.lName c, transE vs e)
  in  (map transD fdefs)++builtinDefsFL

-- | Built-in function definitions. This includes constructor wrapper functions (they are not that
--   special in this interpreter).
builtinDefsFL :: [FDef]
builtinDefsFL =
  let c_cons = T.lName T.bf_Cons
      c_cons_frms = [T.lName T.bf_cons_0, T.lName T.bf_cons_1]
      c_nil = T.lName T.bf_Nil
      c_unit = T.lName T.bf_Unit
  in  [ (c_cons, (c_cons_frms, CApp c_cons (map V c_cons_frms)))
      , (c_nil, ([], CApp c_nil []))
      , (c_unit, ([], CApp c_unit []))
      ]

-- | Evaluates an FL program.
evalFL :: SFL.ProgF -> IO ()      
evalFL pF =
  let p   = transFL pF
      val = lookupBody p "result"
      st  = [(error "dummy top-level context reached", [])]
  in  if False then error (show p) else   -- TODO
      case eval p val st of
        GI i -> putStrLn $ "int result = "++(show i)
        GB b -> putStrLn $ "bool result = "++(show b)
        SC _ -> putStrLn "suspended constructor"
