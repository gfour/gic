-- | The Maude back-end.
-- 
--   Translates ZOIL (without data types and modules) to Maude. The resulting
--   Maude term can be subject to concurrent term rewriting.
-- 
--   The translation is described in:
--   /G. Fourtounis, P. C. Ölveczky, N. Papaspyrou. Formally Specifying /
--   /and Analyzing a Parallel Virtual Machine for Lazy Functional Languages /
--   /Using Maude. In Proc. of the Fifth International Workshop on High-level /
--   /Parallel Programming and Applications, 2011./
-- 

module SLIC.Maude.ZItoMaude (callMaudeBackend) where

import Data.List
import SLIC.AuxFun (ierr)
import SLIC.ITrans.Syntax
import SLIC.State
import SLIC.SyntaxAux
import SLIC.Types

-- | Generates the Maude term for a ZOIL program.
makeMaude :: ProgZ -> Int -> String
makeMaude (Prog _ defs) numWarehouses =
    let defsMaude = concatMap makeMaudeDef defs
        whouse j = "whouse(" ++ (show j) ++ ") "
        whouseNames = (concatMap whouse [0..(numWarehouses-1)]) ++ " none"
        warehouse j = "< "++ (whouse j) ++ ": WHouse | slots : none, pend : none >\n"
        warehouses = concatMap warehouse [0..(numWarehouses-1)]
    in  "(omod TESTS is\n" ++
        "  protecting EXECUTION .\n" ++
        "  op defs : -> List{Def} .\n" ++
        "  eq defs = " ++ defsMaude ++ " .\n\n" ++
        "  op user : -> Oid .\n" ++
        "  op init : -> Configuration .\n" ++
        "  eq init = \n" ++
        "   < root : Node | expr : < $ \""++(qName $ mainDefQName "Main")++"\" ; snil >, status : running, wp : none, whs : " ++ whouseNames ++ 
        ", prog : defs >\n   " ++ warehouses ++ " .\n\n" ++
        "  eq choose(ID:String, CTXT:Stack, WHS:Oids) = whouse(sz(CTXT:Stack) rem "++ (show numWarehouses) ++ ") .\n" ++
        "endom)\n\n" ++
        "(frew in TESTS : init .)\n"

-- | Generates the Maude term for a ZOIL definition.
makeMaudeDef :: DefZ -> String
makeMaudeDef (DefZ v e) = "def(\""++(qName v)++"\", "++(makeMaudeE e)++")\n"
makeMaudeDef (ActualsZ _ _ []) = ""
makeMaudeDef (ActualsZ v _ el) =
    let el' = map (\x -> "("++(makeMaudeE x)++")") el
        mEl = concat (intersperse " : " el')
    in  "def(\""++(qName v)++"\", actuals("++mEl++"))\n"

makeMaudeVar :: QName -> String
makeMaudeVar v = "$ \""++(qName v)++"\""

-- | Generates the Maude term for a ZOIL expression.
makeMaudeE :: ExprZ -> String
makeMaudeE (CaseZ _ _ _) = error "Maude backend doesn't support pattern matching"
makeMaudeE (ConstrZ _) = error "Maude backend doesn't support thunks"
makeMaudeE (XZ (V v)) = makeMaudeVar v
makeMaudeE (XZ (BV _ _)) = error "Maude backend doesn't support bound variables"
makeMaudeE (FZ NOp v) = makeMaudeVar v
makeMaudeE (FZ (Call (_, i)) v) = "call("++(show i)++", "++(makeMaudeVar v)++")"
-- all builtins with two arguments are strict
makeMaudeE (ConZ (CN bOp) [e1, e2]) =
    let m1 = makeMaudeE e1
        m2 = makeMaudeE e2
    in  "cOp(\""++(pprint bOp "")++"\", ("++m1++") : ("++m2++"))"
-- if-then-else
makeMaudeE (ConZ (CN CIf) [e0, e1, e2]) =
    let m0 = makeMaudeE e0
        m1 = makeMaudeE e1
        m2 = makeMaudeE e2
    in  "cOp(\"if\", ("++m0++") : ("++m1++") : ("++m2++"))"
-- numbers
makeMaudeE (ConZ (LitInt val) []) = "# "++(show val)
makeMaudeE e =
  error $ "Found unknown expression to translate into Maude: "++(pprint e "")

callMaudeBackend :: ProgZ -> Options -> IO ()
callMaudeBackend p options =
  case optAction options of
    ACompileMaude -> putStrLn $ makeMaude p $ optWhNum options
    a -> ierr $ "The Maude back-end cannot handle action "++(show a)
