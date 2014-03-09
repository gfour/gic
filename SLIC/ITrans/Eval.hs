-- | The ZOIL interpreter that corresponds to the \"dictionary\"-based semantics
--   of the intensional language.
--   It is call-by-name, /including the built-in IO functions/. Nested
--   pattern matching is not supported.
-- 
--   For more details, read:
--   Fourtounis, N. Papaspyrou, and P. Rondogiannis. ''The Intensional
--   Transformation for Functional Languages with User-Defined Data Types.'' 
--   In Proceedings of the 8th Panhellenic Logic Symposium, 2011.
-- 

module SLIC.ITrans.Eval (evalZOILCBN) where

import Data.List (intersperse)
import SLIC.AuxFun (ierr, spaces)
import SLIC.DFI (DFI)
import SLIC.ITrans.Syntax
import SLIC.ITrans.ZLinker (mergeAndLinkZ)
import SLIC.State
import SLIC.SyntaxAux
import SLIC.SyntaxFL (cArgsC)
import SLIC.Types

-- | Contexts in the intensional transformation.
type Ctxt = [IIndex]

-- | A dictionary from contexts to contexts.
type Dict = [(Ctxt, Ctxt)]

-- | Lazy constructors embed context dictionaries.
type DValue = Value Dict

-- | Evaluates an expression.
evalZ :: Bool -> ProgZ -> Int -> ExprZ -> Ctxt -> Dict -> Ctxt -> IO DValue
evalZ _ _ _ (ConZ (CN CTrue) []) _ _ _ =
    return (VB True)
evalZ _ _ _ (ConZ (CN CFalse) []) _ _ _ =
    return (VB False)
-- the "par" and "pseq" combinators are ignored, only the second argument
-- is evaluated
evalZ _ _ _ (ConZ (LitInt i) []) _ _ _ = return (VI (fromIntegral i))
evalZ _ _ _ (ConZ cn []) _ _ _ = ierr $ "unknown nullary constant: "++(show cn)
evalZ trace prog d (ConZ (CN CIf) [e, e1, e2]) ls dict pc = 
    evalZ trace prog (d+1) e ls dict pc >>= \test -> 
    if boolFrom test then
        evalZ trace prog (d+1) e1 ls dict pc
    else
        evalZ trace prog (d+1) e2 ls dict pc
evalZ trace prog d (ConZ (CN CPlus) [e1, e2]) ls dict pc = 
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VI (intFrom i1 + intFrom i2))
evalZ trace prog d (ConZ (CN CMinus) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VI (intFrom i1 - intFrom i2))
evalZ trace prog d (ConZ (CN CMult) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VI (intFrom i1 * intFrom i2))
evalZ trace prog d (ConZ (CN CDivide) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VI (intFrom i1 `div` intFrom i2))
evalZ trace prog d (ConZ (CN CDiv) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VI (intFrom i1 `div` intFrom i2))
evalZ trace prog d (ConZ (CN CMod) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VI (intFrom i1 `mod` intFrom i2))
evalZ trace prog d (ConZ (CN CEqu) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VB (intFrom i1 == intFrom i2))
evalZ trace prog d (ConZ (CN CNEq) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VB (intFrom i1 /= intFrom i2))
evalZ trace prog d (ConZ (CN CLt) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VB (intFrom i1 < intFrom i2))
evalZ trace prog d (ConZ (CN CGt) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VB (intFrom i1 > intFrom i2))
evalZ trace prog d (ConZ (CN CLe) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VB (intFrom i1 <= intFrom i2))
evalZ trace prog d (ConZ (CN CGe) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \i1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \i2 ->
    return (VB (intFrom i1 >= intFrom i2))
evalZ trace prog d (ConZ (CN CAnd) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \b1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \b2 ->
    return (VB (boolFrom b1 && boolFrom b2))
evalZ trace prog d (ConZ (CN COr) [e1, e2]) ls dict pc =
    evalZ trace prog (d+1) e1 ls dict pc >>= \b1 ->
    evalZ trace prog (d+1) e2 ls dict pc >>= \b2 ->
    return (VB (boolFrom b1 || boolFrom b2))
evalZ trace prog d (ConZ (CN CNeg) [e]) ls dict pc =
    evalZ trace prog (d+1) e ls dict pc >>= \i ->
    return (VI (-(intFrom i)))
-- built-in readIntIO() function
evalZ _ _ _ (XZ (V r)) _ _ _ | r == bf_readIntIO =
    getLine >>= \val ->
    case reads val :: [(Int, String)] of
      [(v, "")] -> return (VI v)
      _ -> ierr $ "Found something that is not a number: "++val
evalZ trace prog d (XZ (V vn)) ls dict pc =
  let (e0, ls0) =
        case searchActuals vn prog of
          [] -> -- this is not an actual variable, find its definition
            let Just def                  = searchZD vn prog
                tkExpr (DefZ _ e)         = e
                tkExpr (ActualsZ _ _ _)   = ierr "tkExpr: ActualsZ..."
            in  if isActualsZ def then ierr "searchActuals missed an actuals()"
                else (tkExpr def, ls)
          actuals ->
            let (m, indx):ls' = ls
                [ActualsZ _ _ es] = filter (\(ActualsZ _ m' _)->m==m') actuals
            in  (lookupAct indx es, ls')
  in  (if trace then
         spc d >> putStr "BEGIN\t" >>
         putStr (qName vn) >>
         putStr " @ " >>
         printWorld ls >>
         putStr "\n"
       else
         return ()) >>
      evalZ trace prog d e0 ls0 dict pc >>= \val ->
      (if trace then
         spc d >> putStr "END\t" >>
         putStr (qName vn) >>
         putStr " @ " >>
         printWorld ls >>
         putStr " = " >>
         print (pprint val "")
       else
         return ()) >>
      return val
evalZ trace prog d (FZ NOp v) ls dict pc =
    evalZ trace prog d (XZ (V v)) ls dict pc    
evalZ trace prog d (FZ (Call i) v) ls dict pc =
  evalZ trace prog d (XZ (V v)) (i:ls) dict pc
evalZ trace prog d (CaseZ (CLoc (Just (0, 0)), _) e pats) ls dict pc =
    evalZ trace prog (d+1) e ls dict ls >>= \e' ->
        let VT (c, dict') = e'
            checkPat c0 (PatB (c1, _) _) = c0==c1
            PatB _ e'' = head (filter (checkPat c) pats)
        in  evalZ trace prog (d+1) e'' ls dict' pc
evalZ _ _ _ eM@(CaseZ (CLoc loc, _) _ _) _ _ _ =
  ierr $ "Eval: nested pattern matching is not supported, found depth "++(pprintLoc loc "")++"!=0, in: "++(pprint eM "")
evalZ trace _ d (ConstrZ c) ls dict pc =
    (if trace then
       spc d >> putStr "Dictionary update: " >>
       printWorld pc >>
       putStr " -> " >>
       printWorld ls >>
       putStr "\n"
     else
       return ()) >>
    return (VT (c, (pc, ls) : dict))
evalZ trace prog d (XZ (BV bv (CLoc (Just (0, 0)), _))) ls dict pc =
    let findH ctxt [] =
            ierr $ "Context " ++ (show ctxt) ++ " not found in dictionary " ++ (show dict)
        findH ctxt ((c0, pc0) : tl) =
            if (ctxt == c0) then pc0 else findH ctxt tl
    in  (if trace then
           spc d >> putStr "Bound variable @" >>
           putStr (qName bv) >>
           putStr " with dict " >>
           print dict >>
           putStr "\n"
         else
           return ()) >>
        evalZ trace prog d (XZ (V bv)) (findH ls dict) dict pc
evalZ _ _ _ bv@(XZ (BV _ _)) _ _ _ =
  ierr $ "The ZOIL interpreter does not support bound variable: " ++(pprint bv "")
evalZ _ _ _ e _ _ _ = error ("ZOIL interpreter can't handle expression "++(pprint e ""))

-- | Prints out a world (stack).
printWorld :: Ctxt -> IO ()
printWorld ctxt = putStr $ "[" ++ (concat (intersperse ", " (map show ctxt))) ++ "]"
-- printWorld []             = return ()
-- printWorld (l : ls)       = putStr (show l) >> printWorld ls

-- | The main evaluator function.
evaluatorZ :: MName -> Bool -> ProgZ -> IO DValue
evaluatorZ m trace prog =
    let Just (DefZ _ expr) = searchZD (mainDefQName m) prog
        emptyLs            = []
        emptyDict          = []
        nullPC             = []
    in  evalZ trace prog 0 expr emptyLs emptyDict nullPC

-- | Forces a value in order to be printed: if it is ground (such as Int and
--   Bool), it is displayed, otherwise its constructor is displayed and its
--   components are recursively evaluated through the phi() meta-interpreter
--   of the semantics. It forces a suspended constructor using the args()
--   component name generator.
forceVal :: Bool -> ProgZ -> DValue -> Ctxt -> CIDs -> IO ()
forceVal trace prog val ctxt cids =
  case val of
    VI _ -> putStr (pprint val "")
    VB _ -> putStr (pprint val "")
    VT (c, h) -> 
      let cArity = findArity c cids
          noFunc = ierr "no enclosing function in interpreter mode"
          evalVar v =
            evalZ trace prog 0 (XZ (BV v (CLoc (Just (0, 0)), noFunc))) ctxt h ctxt >>= \val0 ->
            forceVal trace prog val0 ctxt cids
          evalVars [] = return ()
          evalVars [v] = evalVar v
          evalVars (v : vs) = evalVar v >> putStr ", " >> evalVars vs
          notNullaryConstr = (findArity c cids) > 0
      in  putStr (qName c) >>
          (if notNullaryConstr then
            putStr "(" >>
            evalVars (cArgsC c cArity) >>
            putStr ")"
          else return ())

-- | Prints d spaces.
spc :: Int -> IO ()
spc d = putStr $ spaces d ""

-- | The interpreter function called by the compiler driver. If the result is
--   a constructor, it is fully forced. Takes a single DFI that is all module 
--   DFIs merged (using "DFI.mergeDFIs") and a list of 0-order modules to run.
evalZOILCBN :: Options -> DFI -> ModZ -> IO ()
evalZOILCBN opts dfi p3 =
  let trFlag    = optVerbose opts
      emptyCtxt = []
      p3' = mergeAndLinkZ opts dfi [p3]
      Prog dts' _ = modProg p3
      m = fst $ modNameF p3
  in  evaluatorZ m trFlag p3' >>= \val -> 
      forceVal trFlag p3' val emptyCtxt (calcCIDs dts') >>
      putStrLn ""
