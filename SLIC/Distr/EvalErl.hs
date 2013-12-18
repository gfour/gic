-- | Distributed back-end, prototype Erlang interpreter.
-- 
--   This is the Erlang representation generator for use
--   with the 'erlang/par_eduction.erl' interpreter.
--   To use it, see the 'eval_erl.sh' script.
-- 
--   The execution model of distributed eduction is described in:
--   /G. Fourtounis, P. C. Olveczky, N. Papaspyrou. Formally Specifying /
--   /and Analyzing a Parallel Virtual Machine for Lazy Functional Languages /
--   /Using Maude. In Proc. of the Fifth International Workshop on High-level /
--   /Parallel Programming and Applications, 2011./
-- 
--   This interpreter supports only first-order programs without data types.
-- 

module SLIC.Distr.EvalErl (makeErlRepr) where

import Data.List
import SLIC.AuxFun
import SLIC.ITrans.Syntax
import SLIC.SyntaxAux
import SLIC.Types

-- | Produces the Erlang representation of a ZOIL program.
makeErlRepr :: MName -> ProgZ -> String
makeErlRepr m (Prog _ defs) =
    let defsZ = filter isDefZ defs
        actualsZ = filter (not.isDefZ) defs
        defsErl = concat (intersperse ";\n" 
                          ((map makeErlDef defsZ)++(makeErlActuals actualsZ)))
    in  "-module(main).\n" ++ 
        "-export([init/0, run/6]).\n\n" ++
        "p(Id) -> case Id of\n" ++ defsErl ++ "\nend.\n" ++
        makeErlR m

-- | Produces the Erlang representation for a ZOIL definition.
makeErlDef :: DefZ -> String
makeErlDef (DefZ v e) =
    "\'"++(qName v)++"\' -> {def, "++(makeErlE e)++"}"    
makeErlDef (ActualsZ _ _ _) =
  ierr "makeErlDef was called for actuals()"  
  
makeErlActuals :: [DefZ] -> [String]
makeErlActuals actualsZ =
  let actNames = map defVarZ actualsZ
      filtAct _ (DefZ _ _) = ierr "filtAct was called for function definition"
      filtAct a (ActualsZ v _ _) = v==a
      actGroups = map (\a -> (a, filter (filtAct a) actualsZ)) actNames
      makeAct (v, acts) =
        let aux (ActualsZ _ m el) =
              let el' = map makeErlE el
                  mEl = concat (intersperse ", " el')
              in  "'"++m++"' -> ["++mEl++"]"
            aux (DefZ _ _) = ierr "makeErlActuals found function definition"
        in  "\'"++(qName v)++"' -> fun (Module) -> case Module of "++
            (concat $ intersperse "; " $ map aux acts)++" end end"
  in  map makeAct actGroups

{-
-- | Prepends a ''c'' to constructor names to make them Erlang atoms.
makeErlConstr :: CstrName -> String
makeErlConstr cName = "c"++cName
-}

makeErlVar :: QName -> String
makeErlVar v = "{id, '"++(qName v)++"'}"

-- | Produces the Erlang representation for a ZOIL expression.
makeErlE :: ExprZ -> String
makeErlE (CaseZ _ _ _) = 
  error "Pattern matching clauses are not supported yet in the Erlang interpreter"
{-
  let erlE = makeErlE e
      makeErlPat (PatZ c' e') = "("++(makeErlConstr c')++") -> "++(makeErlE e')
      patsE = concat (intersperse "; " (map makeErlPat pats))
  in  "{match, "++(show d)++", "++erlE++", fun "++patsE++" end}"
-}
makeErlE (ConstrZ _) =
  error "Thunks are not supported yet in the Erlang interpreter"
--  "{thunk, '"++(makeErlConstr c)++"'}"
makeErlE (XZ (V v)) = makeErlVar v
makeErlE (XZ (BV _ _)) =
  error "Bound variables are not supported yet in the Erlang interpreter"
-- "{bid, '"++bv++"', "++(show d)++"}"
makeErlE (FZ NOp v) = makeErlVar v
makeErlE (FZ (Call m) v) = "{call, "++(makeErlIdx m)++", "++(makeErlVar v)++"}"
-- all builtins with two arguments are strict
makeErlE (ConZ (CN bOp) [e1, e2]) =
    "{'"++(pprint bOp "")++"', "++(makeErlE e1)++", "++(makeErlE e2)++"}"
makeErlE (ConZ (CN CTrue) []) = "{val, 1}"
makeErlE (ConZ (CN CFalse) []) = "{val, 0}"
-- numbers
makeErlE (ConZ (LitInt val) []) = "{val, "++(show val)++"}"
-- the "if" clause
makeErlE (ConZ (CN CIf) [e0, e1, e2]) =
    "{iff, "++(makeErlE e0)++", "++(makeErlE e1)++", "++(makeErlE e2)++"}"
makeErlE e = ierr ("Erlang back-end -- missing case: "++(pprint e ""))

makeErlIdx :: IIndex -> String
makeErlIdx (m, i) = "{'"++m++"', "++(show i)++"}"

-- | Generates the function that returns the 'result' definition name.
makeErlR :: MName -> String
makeErlR m = "resultName() -> '"++(qName $ mainDefQName m)++"' .\n"
