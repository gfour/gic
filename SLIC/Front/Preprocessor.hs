-- | A collection of preprocessing utilites.
-- 
--   Contains:
-- 
--   (1) the /constructors-as-functions/ transformation,
-- 
--   (2) a static analysis to identify pattern-bound variables,
-- 
--   (3) case-lifting to fix the syntactic restriction of no pattern-matching
--   parameters,
-- 
--   (4) bound variables discovery and enumeration,
-- 
--   (5) a preprocessor from code translated from GHC Core to FL,
-- 
--   (6) a scanner that qualifies unqualified names in a program.
-- 
--   For the lambda lifter, see the "Lifter".
--   For the unique names generator, see the "Renamer" (or the mini-renamer
--   for lifted arguments inside the "Lifter").

module SLIC.Front.Preprocessor (checkMod, cNested, constrToFuncs, convertFromGHC,
                                dummyCName, fixSyntax, genProjSelTEnv,
                                markStrict, procBV, projCName, 
                                qual, updCName) where

import Data.List (elemIndex)
import qualified Data.Map as Map (Map, fromList, keys, lookup, map, mapKeys, union)
import Data.Maybe (catMaybes)
import SLIC.AuxFun (errM, ierr, toLowerFirst)
import SLIC.Constants (tcMod)
import SLIC.Front.Typeclass
import SLIC.State
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- | A bound variable aliased to a constructor variable of some counter/depth.
type Alias = (QName, (QName, CaseLoc))

-- | Aliased variables of the program become bound variables of a specific name,
--   depth, and enclosing function.
type BVInfo = [Alias]

-- * The constructors-as-functions transformation

-- | Replaces constructor calls with function calls that lead to thunks.
--   (The /constructors-as-functions/ transformation.)
constrToFuncs :: (TypeChecker, ScrutOpt) -> ModF -> ModF
constrToFuncs (tc, scrutOpt) modF =
  let Prog ds defs = modProg modF
      newDefs = concatMap (newDataDefs scrutOpt) ds
      defs'   = map replaceConstrD defs
      prog'   = Prog ds (defs' ++ newDefs)
      tAnnot' = if tc == GICTypeInf False then
                  modTAnnot modF
                else
                  (modTAnnot modF) `Map.union` (genProjSelTEnv ds)
  in  modF{modProg=prog'}{modTAnnot=tAnnot'}

-- | Replaces constructor calls in definitions.
replaceConstrD :: DefF -> DefF
replaceConstrD (DefF v vl e) = DefF v vl (replaceConstrE e)

-- | Replaces constructor calls with calls to constructor functions.
replaceConstrE :: ExprF -> ExprF
replaceConstrE e@(XF _) = e
replaceConstrE (ConF c el) = ConF c (map replaceConstrE el)
replaceConstrE (FF f el) = FF f (map replaceConstrE el)
replaceConstrE (CaseF d e b pats) =
  let replaceConstrP (PatB pat0 e0) = PatB pat0 (replaceConstrE e0)
  in  CaseF d (replaceConstrE e) b (map replaceConstrP pats)
-- thunks become applications
replaceConstrE (ConstrF c el) = FF (V c) (map replaceConstrE el)
replaceConstrE (LetF dep defs e) =
  LetF dep (map replaceConstrD defs) (replaceConstrE e)
replaceConstrE (LamF dep v e) =
  LamF dep v (replaceConstrE e)

-- | Creates a wrapper function for each constructor of a data type. Also
--   generates the constructor projection functions.
newDataDefs :: ScrutOpt -> Data -> [DefF]
newDataDefs scrOpt (Data _ _ dts) = concatMap (newDataDefsDC scrOpt) dts

-- | Creates a wrapper function for a given constructor. Also generates
--   the constructor projection/update/dummy-creation functions.
newDataDefsDC :: ScrutOpt -> DConstr -> [DefF]
newDataDefsDC scrOpt (DConstr c dTypes _) =
    let -- for a Constr(a0, ..., an) creates constr_0, ..., constr_n
        -- formals (keeping their strictness annotations) - the counter
        -- should be initialized to 0
        formals :: [DT] -> Int -> [Frm]
        formals [] _ = []
        formals ((DT _ s _) : dts) counter =
            (Frm (ithFrmOfCstr counter c) s) : (formals dts (counter+1))
        frms = formals dTypes 0
        wrapper = DefF c frms (ConstrF c (map (\(Frm v _) -> XF (V v)) frms))
        -- generate prjection functions for every i-th component
        bvs   = cArgsC c (length dTypes)
        mkProj (DT _ s sel, i) =
          let (pn, [px]) = projCSig c i
              cPat = (SPat c bvs, PatInfo True)
              selBody func arg =
                CaseF (cNested scrOpt 0 func) (XF (V arg)) underscoreVar
                [ PatB cPat (XF (V (bvs !! i))) ]
              recFields =
                case sel of
                  Nothing  -> []
                  Just selName ->
                    let sx = procLName (++"$_0") selName
                        selector = DefF selName [Frm sx s] (selBody selName sx)
                        updName = updCName selName
                        ux0 = procLName (++"$_0") updName
                        ux1 = procLName (++"$_1") updName
                        updFields = (take i bvs) ++ [ux1] ++ (drop (i+1) bvs)
                        updater  = DefF updName [Frm ux0 s, Frm ux1 s]
                                   (CaseF (cNested scrOpt 0 updName)
                                    (XF (V ux0)) underscoreVar
                                    [PatB cPat
                                     (FF (V c) (map (\v->XF(V v)) updFields))]
                                   )
                    in  [selector, updater]
          in  (DefF pn [Frm px s] (selBody pn px)) : recFields
        dummyConstr =
          if containSelectors dTypes then
            let errorField i = FF (V bf_error)
                               [mkStrList $ (lName c)++
                                ": acess to uninitialized field "++(show i)]
                dc = dummyCName c
            in  [DefF dc [] (FF (V c) (map errorField [0..(length dTypes-1)]))]
          else []
    in  [wrapper] ++ (concatMap mkProj $ zip dTypes [0..]) ++
        dummyConstr

-- | The name of the dummy constructor function. Used to generate partially
--   initialized records.
dummyCName :: CstrName -> QName
dummyCName cn = procLName ("dummy$"++) cn

-- | The name of the projection function for field i of a constructor.
projCName :: String -> Int -> String
projCName constr i = "sel$"++constr++"$"++(show i)

-- | Generates the signature for the i-th projection of a constructor.
projCSig :: CstrName -> Int -> FSig
projCSig c i = 
  let pn = procLName (\cName -> projCName cName i) c
      px = procLName (\n -> toLowerFirst n ++ "_0") pn
  in  (pn, [px])
              
-- | Generates the typing environment for constructor unpacking and record
--   field selection. To be used as a starting point for type inference.
genProjSelTEnv :: [Data] -> TEnv
genProjSelTEnv dts =
  let genProjData (Data dt _ dcs) = concatMap (genProjDC dt) dcs
      genProjDC dt dc@(DConstr c dcs _) =
        (concatMap (genProjDCi dt c) (zip dcs [0..])) ++ (genDummy dt dc)
      genProjDCi dt c (DT argType _ sel, i) =
        let (pF, [pFrm])   = projCSig c i
            pFrm_t = Tg (T dt)
            pF_t = Tf pFrm_t argType
            projTE = [ (pF,   (pF_t, Just 1))
                     , (pFrm, (pFrm_t, Nothing)) ]
            selTE  =
              case sel of
                Nothing -> []
                Just qn ->
                  let (sF, [sFrm]) = selCSig qn
                  in  [ (sF,   (pF_t, Just 1))    -- same types as the projection
                      , (sFrm, (pFrm_t, Nothing)) ]
        in  projTE ++ selTE
      genDummy dt (DConstr c dcs rt) =
        let -- if ADT, its type is already known, if GADT, read it from last field
            dt' = case rt of Nothing -> Tg (T dt) ; Just t' -> t'
        in  if containSelectors dcs then
              [ (dummyCName c, (dt', Just 0)) ]
            else []
  in  Map.fromList $ concatMap genProjData dts

-- | Given a record field name, generates a name for its field updating function.
updCName :: QName -> QName
updCName sel = procLName (++"$upd") sel

-- | Generates the signature for a record field selector.
selCSig :: QName -> FSig
selCSig selName =
  let sx = procLName (++"$_0") selName
  in  (selName, [sx])

-- * Case expressions and bound variables enumeration

-- | Enumerates case expressions and bound variables according to their 
--   depth in the program text.
procBV :: Options -> MName -> ProgF -> ProgF
procBV opts m (Prog dts defs) = 
  let -- Enumerates the bound variables of a definition.
      procBVD :: DefF -> DefF
      procBVD def@(DefF v frms e) =
        -- the initial depth is -1: local vars outside of case..of should
        -- compile to illegal code
        DefF v frms (procBVE (optScrut opts) m (defSig def) [] (-1) e)
  in  Prog dts (map procBVD defs)

-- | Processes all local bound variables to standard variable names that refer
--   to an enclosing pattern-matching clause.
--   The first two arguments are the names of the enclosing module and function
--   signature for the clause. The third argument is the aliases encountered so far
--   (created by patterns). The fourth argument is the curent depth.
--   If a correctly enumerated case expression or bound variable is found, it
--   is left unchanged (it is assumed to have been created by defunctionalization).
--   Enumeration happens across every branch path and the same depth may be used
--   by different case expressions that are at the same level but under different
--   branches (and are therefore mutually exclusive).
procBVE :: ScrutOpt -> MName -> FSig -> BVInfo -> Int -> ExprF -> ExprF
procBVE scrOpt m fsig@(func, frms) al d (CaseF cloc@(cn, ef) e b pats) =
  let procBVPat dNext cloc' (PatB (SPat c vs, pI) eP) = 
        let al' = getBVAliasesPat cloc' c vs
            vs' = cArgsC c (length vs)
        in  PatB (SPat c vs', pI) (procBVE scrOpt m fsig (al++al') dNext eP)
      pats' dNext cloc' = map (procBVPat dNext cloc') pats
      ierrFunc = ierr $ "procBVE: pattern matching has different enclosing function already set: "++(qName ef)++" /= "++(qName func)++" for "++(pprint cn "")
  in  case (cn, e) of
        (CFrm i, XF (V v)) -> 
          -- Accept already set CFrm annotations (but do sanity check).
          if ef /= func then    ierrFunc
          else if v /= frms !! i then ierr "CFrm annotation mismatch"
               else CaseF cloc (procBVE scrOpt m fsig al d e) b (pats' d cloc)
        (CFrm _, _) -> ierr "CFrm set for non-formal scrutinee"
        (CLoc Nothing, XF (V v)) | scrOpt && (v `elem` frms) ->
          case elemIndex v frms of
            Just i ->
              let cloc' = (CFrm i, func)
              in  CaseF cloc' (procBVE scrOpt m fsig al d e) b (pats' d cloc')
            Nothing -> ierr "procBVE: error handling function formal"
        (CLoc loc, _) ->
          let dN   = d+1
              cloc' = (CLoc (Just (dN, dN)), func)
              case' = CaseF cloc' (procBVE scrOpt m fsig al d e) b (pats' dN cloc')
          in  case loc of
                Nothing -> case'
                Just (_, d') ->
                  if (d' == d+1) || (ef == noEFunc) || (ef == func) then 
                    case'
                  else
                    if d' /= d+1 then
                      ierr $ "preprocessor: pattern matching has depth already set, value="++(show d')++", but it was going to be "++(show (d+1))
                    else if ef /= noEFunc && ef /= func then
                           ierrFunc
                         else
                           ierr "preprocessor: malformed case expression"
procBVE so m func al d (ConF c el) = ConF c (map (procBVE so m func al d) el)
procBVE so m func al d (ConstrF c el) = ConstrF c (map (procBVE so m func al d) el)
procBVE so m func al d (FF f el) =
  case f of
    V fName -> FF (procBVEV al fName) (map (procBVE so m func al d) el)
    BV _ _  -> ierr "Bound variable found by procBVE, should not appear here"
procBVE _ _ _ _ _ bv@(XF (BV _ _)) = bv
procBVE _ _ _ al _ (XF (V v)) = XF (procBVEV al v)
procBVE _ _ _ _ _ e@(LetF {}) =
  ierr $ "procBVE(): let binding not lifted: " ++ (pprint e "")
procBVE _ _ _ _ _ e@(LamF {}) =
  ierr $ "procBVE(): lambda should have already been lifted: " ++ (pprint e "")

-- | The actual function that spots and renames a variable.
procBVEV :: BVInfo -> QName -> V
procBVEV al v =
  case (filter (\(v', _) -> (v'==v)) al) of
    [] -> V v
    [(_, (cVar, loc))] -> BV cVar loc
    _ -> error $ "found more than one pattern bindings for bound variable: "++(pprint v "")

-- | In a pattern that binds variables, it returns a set of aliases
--   for the bound variables. Underscore binders are ignored.
--   For example, pattern 'Cons x y' of depth 1 produces the following mapping:
--   [(x, (cons_0, 1)), (y, (cons_1, 1))]
getBVAliasesPat :: CaseLoc -> CstrName -> [QName] -> [Alias]
getBVAliasesPat loc c vs = 
  let cVars = map (\v -> (v, loc)) (cArgsC c (length vs))
      aliases = zip vs cVars
  in  filter (\(v, _)->v/=underscoreVar) aliases

-- * Preprocessor for code converted from GHC Core

-- | Preprocesses a program that has resulted from conversion from GHC Core:
-- 
--   * GHC.Types.I#(n) becomes the number constant n
-- 
--   * case e of True -> e1 ; False -> e2 becomes if e then e1 else e2
-- 
convertFromGHC :: ProgF -> ProgF
convertFromGHC (Prog ds defs) =
  let elimPrim var@(XF (V _)) = var
      elimPrim (XF (BV _ _)) = error "Cannot feed bound variables to GHC"
      elimPrim (ConF c el) = ConF c (map elimPrim el)
      elimPrim (FF f@(V _) el) = FF f (map elimPrim el)
      elimPrim (FF (BV _ _) _) = ierr "elimPrim: bound variable application"
      elimPrim (ConstrF (QN (Just "GHC.Types") "I#") [e]) = e
      elimPrim (ConstrF c el) = ConstrF c (map elimPrim el)
      
      elimPrim (CaseF _ e _ [
                   PatB (SPat (QN (Just "GHC.Types") "True" ) [], _) e1,
                   PatB (SPat (QN (Just "GHC.Types") "False") [], _) e2]) =
        ConF (CN CIf) [elimPrim e, elimPrim e1, elimPrim e2]
      elimPrim (CaseF _ e _ [
                   PatB (SPat (QN (Just "GHC.Types") "False") [], _) e1,
                   PatB (SPat (QN (Just "GHC.Types") "True" ) [], _) e2]) =
        ConF (CN CIf) [elimPrim e, elimPrim e2, elimPrim e1]
      elimPrim (CaseF d e b pats) =
        let elimPrimP (PatB pat eP) = PatB pat (elimPrim eP)
        in  CaseF d (elimPrim e) b (map elimPrimP pats)
      elimPrim (LetF d bs e) =
        LetF d (map elimPrimD bs) (elimPrim e)
      elimPrim (LamF d v e) = LamF d v (elimPrim e)
      
      -- rename the :Main.main = runMainIO ... function of GHC to something
      -- that will probably not clash with Main.main when the prefix is eliminated
      -- elimPrimD (DefF (QN (Just "Main") "main") [] eD) = DefF (QN Nothing "Main_GHC_main") [] (elimPrim eD)
      elimPrimD (DefF f fs e) = DefF f fs (elimPrim e)
      
      -- TODO: delete this code
      -- omit the :Main.main = runMainIO ... function of GHC to something
      -- omitMain (DefF (QN (Just ":Main") "main") [] _) = False
      omitMain (DefF {}) = True
  in  Prog ds (map elimPrimD (filter omitMain defs))
      
-- * Syntactic restrictions preprocessor

-- | Information for naming the lifted pattern matching: (depth, position).
type CInfo = (Int, Int)

-- | Fixes the FL syntax restrictions (to admit intensional transformation).
--     
--   * Lifts pattern matching from inside function/built-in applications.
--     Abstracts 'case .. of' parameters as let-bindings. This is a syntactic
--     restriction of the extended intensional transformation: pattern matching
--     cannot be a parameter of a function call or a built-in call.
fixSyntax :: MName -> ProgF -> ProgF
fixSyntax m (Prog ds defs) =  
  Prog ds (map (liftParamCaseD m (0, 0)) defs)

-- | Case-lifting a definition.
liftParamCaseD :: MName -> CInfo -> DefF -> DefF
liftParamCaseD m ci (DefF f fs e) = DefF f fs (liftParamCase m ci e)
  
-- | Case-lifting an expression.
liftParamCase :: MName -> CInfo -> ExprF -> ExprF
liftParamCase _ _ e@(XF _) = e
liftParamCase m (d, i) (ConF c el) =
  liftParamExprL m (d+1, i) el (\el' -> ConF c el')
liftParamCase m (d, i) (ConstrF c el) =
  liftParamExprL m (d+1, i) el (\el' -> ConstrF c el')
liftParamCase m (d, i) (FF f el) =
  liftParamExprL m (d+1, i) el (\el' -> FF f el')
liftParamCase m ci (CaseF dep e bnd pats) =
  let aux (PatB pat eP) = PatB pat (liftParamCase m ci eP)
  in  CaseF dep (liftParamCase m ci e) bnd (map aux pats)
liftParamCase m ci (LetF dep binds e) =
  LetF dep (map (liftParamCaseD m ci) binds) (liftParamCase m ci e)
liftParamCase m ci (LamF dep v e) = LamF dep v (liftParamCase m ci e)

-- | Applies the case-lifting transformation to a construct of shape
--   a(b_0, ..., b_n), where no case expressions should appear inside the
--   b_i. The provided function does the wrap-up of the result.
liftParamExprL :: MName -> CInfo -> [ExprF] -> ([ExprF] -> ExprF) -> ExprF 
liftParamExprL m ci el f =
  let -- (maybe) lift each expression using its position
      aux _ [] = []
      aux n (e:es) = (liftCase m (fst ci, n) e) : (aux (n+1) es)
      (el', maybeNewBinds) = unzip (aux 0 el)
  in  case catMaybes maybeNewBinds of
        []   -> f el'
        bnds -> LetF Nothing bnds (f el')

-- | If its argument is a case expression, it is transformed to a new variable,
--   accompanied by a definition containing the case expresion. If it is not
--   an expression, case-lifting proceeds to process it.        
liftCase :: MName -> CInfo -> ExprF -> (ExprF, Maybe DefF) 
liftCase m ci@(d, i) (CaseF dC eC bC patsC) =
  let -- names the i-th expression that is a pattern matching
      dn    = QN (Just m) ("lifted_case_"++show d++"__"++show i)
      pats' = map (liftParamCaseP ci) patsC
      liftParamCaseP ci0 (PatB (SPat c bs, pI) eP) =
        PatB (SPat c bs, pI) (liftParamCase m ci0 eP)
      cf'   = DefF dn [] $ CaseF dC (liftParamCase m ci eC) bC pats'
  in  (XF (V dn), Just cf')
liftCase m ci e = (liftParamCase m ci e, Nothing)

-- * Other preprocessing stages

-- | If the second flag is set, all constructors and functions are set to
--   accept strict formals.
markStrict :: Bool -> ProgF -> ProgF
markStrict s p@(Prog dts defs) =
    if not s then
      p
    else
      let markStrictDT (DT d _ sel) = DT d True sel
          markStrictDC (DConstr c dcts rt) =
            DConstr c (map markStrictDT dcts) rt
          markStrictData (Data dtName as dcs) =
            Data dtName as (map markStrictDC dcs)
          markStrictDef (DefF f frms e) = DefF f (map markStrictFrm frms) e
          markStrictFrm (Frm f _) = Frm f True
      in  Prog (map markStrictData dts) (map markStrictDef defs)

-- * Name qualifying scanner

-- | A mapping from unqualified names to modules.
type NmToMod = Map.Map SName (Maybe MName)

-- | The information used to reason about name qualification.
type QInfo = (NmToMod, MNameF)

-- | The main entry point to name qualification.
qual :: (ModF, [TcInstF]) -> (ModF, [TcInstF])
qual (modF, tcInsts) =
  let qInfo = genQInfo modF
  in  (qualM qInfo modF, map (qualTcInst qInfo) tcInsts)

-- | Reads a module and generates a 'QInfo' structure.
genQInfo :: ModF -> QInfo
genQInfo modF =
  let fImports = modImports modF
      tcs = tcIDecls $ modTCs modF
      builtin_nm = map (\(QN mn vn)->(vn, mn)) (cBuiltinFuncs++builtinDTypes)
      imported_nm =
        concatMap (\(IDecl mN is _)->
                    zip (map lName $ Map.keys is) (repeat (Just mN)))
                  fImports
      tcMethods_nm = 
        concatMap (\(TcDecl _ _ methods)->
                    concatMap (\((f, fs), _)-> zip (f:fs) (repeat $ Just tcMod))
                    methods) tcs
      nm = Map.fromList $ builtin_nm ++ imported_nm ++ tcMethods_nm
  in  (nm, modNameF modF)

-- | Given a module, it qualifies all names with the modules they come from.
qualM :: QInfo -> ModF -> ModF 
qualM info modF =
  let Prog dts defs = modProg modF
      TcInfo tcs tcis = modTCs modF
      progQ = Prog (map (qualData info) dts) (map (qualDef info) defs)
      tAnnotQ = Map.mapKeys (qualQName info) $
                Map.map (\(t, ar)->(qualT info t, ar)) $ modTAnnot modF
      tcsQ = TcInfo (map (qualTcDecl info) tcs) (map (qualTcISig info) tcis)
  in  modF{modProg=progQ}{modTAnnot=tAnnotQ}{modTCs=tcsQ}

qualData :: QInfo -> Data -> Data
qualData info@(_, fm@(m, _)) (Data (QN Nothing dt) as dcs) =
  let qualDC (DConstr (QN Nothing dc) cs rt) =
        DConstr (QN (Just m) dc) (map qualDT cs) (fmap (qualT info) rt)
      qualDC (DConstr dc@(QN (Just _) _) _ _) =
        errM fm $"found already qualified constructor declaration: "++(qName dc)
      qualDT (DT t s sel) =
        let sel' =
              case sel of
                Nothing              -> Nothing
                Just (QN Nothing n)  -> Just (QN (Just m) n)
                Just (QN (Just m') n) ->
                  if m==m' then Just (QN (Just m) n)
                  else 
                    errM fm $ "found record selector declaration for module "++m'
        in  DT (qualT info t) s sel'
  in  Data (QN (Just m) dt) as (map qualDC dcs)
qualData (_, fm) (Data (QN (Just _) _) _ _) =
  errM fm "found already qualified data declaration"

qualDef :: QInfo -> DefF -> DefF
qualDef info@(_, (m, _)) (DefF (QN Nothing f) fs e) =
  let aux (Frm frm s) = Frm (qualQName info frm) s
  in  DefF (QN (Just m) f) (map aux fs) (qualE info e)
qualDef (_, fm) (DefF f@(QN (Just _) _) _ _) =
  errM fm $ "Found declaration of already qualified name: "++(qName f)

qualE :: QInfo -> ExprF -> ExprF
qualE info (XF (V v)) = XF (V (qualQName info v))
qualE info (ConF c el) = ConF c (map (qualE info) el)
qualE info (FF (V f) el) = FF (V (qualQName info f)) (map (qualE info) el)
qualE info (ConstrF c el) = ConstrF (qualQName info c) (map (qualE info) el)
qualE info (CaseF cl e sc pats) =
  let qualPat (PatB (SPat c bvs, pI) eP) =
        PatB (SPat (qualQName info c) (map (qualQName info) bvs), pI)
             (qualE info eP)
  in  CaseF cl (qualE info e) (qualU info sc) (map qualPat pats)
qualE info (LetF d defs e) =            
  LetF d (map (qualDef info) defs) (qualE info e)
qualE info@(_, (m, _)) (LamF d (QN Nothing v) e) =
  LamF d (QN (Just m) v) (qualE info e)
-- sanity checks  
qualE (_, fm) (FF (BV _ _) _) =
  errM fm "found bound variable application while parsing"
qualE (_, fm) (XF (BV _ _)) = errM fm "found bound variable while parsing"
qualE (_, fm) (LamF _ (QN (Just _) _) _) =
  errM fm "found qualified lambda-bound variable while parsing"

qualTcDecl :: QInfo -> TcDecl -> TcDecl
qualTcDecl info (TcDecl tcn tv methods) =
  TcDecl tcn tv (map (\(sig, t)->(sig, qualT info t)) methods)

qualTcISig :: QInfo -> TcInstSig -> TcInstSig
qualTcISig info ((tcn, t), mn) = ((tcn, qualT info t), mn)
  
qualTcInst :: QInfo -> TcInstF -> TcInstF
qualTcInst info (TcInst tcn t methods) =
  TcInst tcn (qualT info t) (map (qualDef info) methods)

-- | Qualifies a name that may be an underscore.
qualU :: QInfo -> QName -> QName
qualU info qn
  | qn == underscoreVar = qn
  | otherwise = qualQName info qn

-- | The basic function that qualifies names. Takes a simple name; if the name
--   is found in the imported names table, then it is qualified with the
--   imported module name, otherwise the current module name is used.
qualName :: QInfo -> SName -> QName
qualName (nm, (m, _)) name =
  case Map.lookup name nm of
    Just m' -> QN m'       name
    Nothing -> QN (Just m) name

-- | Given a 'QName', if it is found to be unqualified, process it.
qualQName :: QInfo -> QName -> QName
qualQName _ qn@(QN (Just _) _) = qn
qualQName info (QN Nothing n) = qualName info n

-- | Qualifies a ground type.
qualG :: QInfo -> Ground -> Ground
qualG info (T dt) = T (qualQName info dt)
qualG info (TDF tdf dt) = TDF tdf (qualT info dt)

-- | Qualifies a type.
qualT :: QInfo -> Type -> Type
qualT info (Tg g) = Tg (qualG info g)
qualT _  t@(Tv _) = t
qualT info (Tf t1 t2) = Tf (qualT info t1) (qualT info t2)
qualT info (Ta t1 t2) = Ta (qualT info t1) (qualT info t2)

-- | Checks a module for uses of undefined names. Fails with an error 
--   if such a name is found, otherwise the original module is returned.
checkMod :: ModF -> ModF
checkMod modF = if checkNames modF then modF else error "Found unknown name."

-- | Checks that all the variable names used in a program exist as
--   definitions, formals, pattern-bound variables, or are imported.
checkNames :: ModF -> Bool
checkNames modF =
  let fm = modNameF modF
      importedNames = Map.keys $ mergeINames $ modImports modF
      Prog dts defs = modProg modF
      tcs = tcIDecls $ modTCs modF
      -- TODO: add here the imported type class declarations
      tcMethods = concatMap (\(TcDecl _ _ methods)->map (fst.fst) methods) tcs
      definedConstrs = concatMap (\(Data _ _ dcs)-> map dcName dcs) dts
      definedFuncs  = defVNames defs
      globalNames = importedNames ++ definedConstrs ++ definedFuncs ++
                    cBuiltinFuncs
      checkD names (DefF _ fs e) = checkE (names++(frmsToNames fs)) e
      checkE names (XF v) =
        let vName = nameOfV v
        in  if vName `elem` names then
              True
            else
              errM fm $ "Unknown variable "++(qName vName)++", known names: "++(pprintList 0 (", "++) names "")
      checkE names (FF f el) =
        let fName = nameOfV f
        in  if (fName `elem` names) || ((lName fName) `elem` tcMethods) then
              all (checkE names) el
            else
              errM fm $ "Found call to unknown function "++(qName fName)++", known names: "++(pprintList 0 (", "++) names "")
      checkE names (ConF _ el) = all (checkE names) el
      checkE names (ConstrF c el) =
        if c `elem` names then
          all (checkE names) el
        else
          errM fm $ "Found call to unknown constructor: "++(qName c)
      checkE names (CaseF _ e scr pats) =  
        let names' = scr:names             -- add the scrutinee name
        in  (checkE names e) && (all (checkPat names') pats)
      checkE names (LetF _ ldefs e) =
        let names' = names++(defVNames ldefs)
        in  (checkE names' e) && (all (checkD names') ldefs)
      checkE names (LamF _ v e) =
        let names' = v:names               -- add bound variable
        in  checkE names' e
      checkPat names (PatB (SPat _ bvs, _) e) = checkE (names++bvs) e
  in  all (checkD globalNames) defs

-- | If the first argument is true, then it returns the second as a LAR slot
--   for a formal that is a scrutinee of the third function. Otherwise it
--   returns the pattern matching location 0 of the third function.
cNested :: ScrutOpt -> Int -> QName -> CaseLoc
cNested True  i func = (CFrm i, func)
cNested False _ func = (CLoc (Just (0, 0)), func)
