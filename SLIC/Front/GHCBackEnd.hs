-- | The GHC back-end that translates GHC Core to FL.
-- 

{-# LANGUAGE CPP #-}
module SLIC.Front.GHCBackEnd (coreGHC, transfCore) where

import BasicTypes (isBanged)
import CorePrep (corePrepPgm)
import CoreSyn (Alt, Bind(..), CoreBind, CoreBndr, CoreExpr, CoreProgram, 
                AltCon(..), Expr(..))
import DataCon (dataConName)
import FastString (unpackFS)
import GHC 
import Id (idName)
import Literal
import Name (nameOccName)
import TyCon (tyConName)
import Type (isTyVar, pprType)
import Var (varName, varType)
import System.Exit (ExitCode(ExitSuccess), exitWith)

import Data.Map (empty, elems, fromList, mapKeys)
import SLIC.AuxFun (ierr, trace2)
import SLIC.Constants
import SLIC.Driver (processFL)
import SLIC.Front.GHCFrontEnd (showPPr, showSDoc', transT)
import SLIC.State (Options(..), ScrutOpt, defaultOptions)
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- * GHC Core transformation

-- | The main entry point of the GHC connector, takes a GHC Core program
--   (as a set of binders and type constructors) and feeds it to the compiler.
transfCore :: Options -> DynFlags -> CoreProgram -> [TyCon] -> IO ()
transfCore opts dfs binds tyCons = 
  let scr       = optScrut opts
      defs      = processPatMatch scr datatypes (concatMap (transBind dfs) binds)
      datatypes = map (transTyCon dfs) tyCons
      fm        = (defaultMod, modNoPath)
      exports   = empty
      tAnnot    = getVTypesCore dfs defaultMod binds
      tcs       = TcInfo [] []     -- TODO: handle type classes in this back-end
      moduleFL  = Mod fm exports [] (Prog datatypes defs) tAnnot tcs      
  in  -- trace2 (showPPr dfs binds) $
      -- trace2 "========================" $
      trace2 (pprint moduleFL "") $
      processFL defaultOptions [] moduleFL >>
      exitWith ExitSuccess

-- | Reads the type annotations from a GHC Core program. Takes the
--   compilation flags, the current module name, and the Core program.
--   Returns the typing environment.
getVTypesCore :: DynFlags -> MName -> CoreProgram -> TEnv
getVTypesCore dfs mn binds =
  let v2qn f = mapGHCtoGICName $ stringToQName $ showPPr dfs f
      vtBinder f ar = (v2qn f, (transT dfs (varType f), ar))
      vtBindExpr (f, e) = (vtBinder f (Just $ lamArity e)):(vtExpr e)
      vtBind (NonRec b e) = vtBindExpr (b, e)
      vtBind (Rec bs) = concatMap vtBindExpr bs
      vtExpr (Var _) = []
      vtExpr (Lit _) = []
      vtExpr (App e a) = (vtExpr e)++(vtExpr a)
      vtExpr (Lam b e) = (vtBinder b Nothing):(vtExpr e)
      vtExpr (Let bind e) = (vtBind bind)++(vtExpr e)
      vtExpr (Case e bnd _ alts) =
        (vtExpr e)++[vtBinder bnd Nothing]++(concatMap vtAlt alts)
      vtExpr (Cast _ _) = error "getVTypesCore: Casts are not supported."
      vtExpr (Coercion _) = error "getVTypesCore: Coercions are not supported."
      vtExpr (Tick _ e) = vtExpr e
      vtExpr (Type _) = [] -- error "'Type's are not supported by the Core interface."
      vtAlt (_, bs, eAlt) = (map (\b->vtBinder b Nothing) bs)++(vtExpr eAlt)
      lamArity (Lam _ e) = 1 + (lamArity e)
      lamArity _ = 0
      tEnv = fromList $ concatMap vtBind binds
      fillMName qn@(QN (Just _) _) = qn
      fillMName (QN Nothing  f) = QN (Just mn) f
  in  mapKeys fillMName tEnv

-- | Translates GHC Core bindings to FL definitions.
transBind :: DynFlags -> CoreBind -> [DefF]
transBind dfs bind =
  let frms b (Lam bL eL) =
        if isTyVar bL then
           error $ "Type variable '"++(showPPr dfs bL)++"' detected in function "++(showPPr dfs b)++", did you forget to put a type annotation?"
        else (stringToQName $ showPPr dfs bL) : (frms b eL)
      frms _ _ = []
      body (Lam _ eL) = body eL
      body e = e
      transBind' (b, e) =
        let bQN = stringToQName $ showPPr dfs b
            fs  = (namesToFrms (frms b e) (defaultEvOrder False))
        in  DefF bQN fs (transExpr dfs (body e))
  in  case bind of
        NonRec b e -> [transBind' (b, e)]
        Rec binds -> map transBind' binds

-- | Translates a GHC Core expression to an FL expression.
transExpr :: DynFlags -> CoreExpr -> ExprF
transExpr dfs (Var vId) = 
  case showPPr dfs vId of
    "True"  -> ConF (CN CTrue) []
    "False" -> ConF (CN CFalse) []
    iName   ->
      let iNameQN = stringToQName iName
      in  --    "GHC.Show.$fShowInt" -> error "$fShowInt in var"
          if isConstr iName then ConstrF iNameQN []
          else XF (V iNameQN)
transExpr _ (Lit literal) = 
  let mInt i = ConF (LitInt i) []
  in  case literal of
        MachStr str    -> mkStrList $ unpackFS str
        MachInt i      -> mInt $ fromInteger i
        MachInt64 i    -> mInt $ fromInteger i
        MachWord i     -> mInt $ fromInteger i
        MachWord64 i   -> mInt $ fromInteger i
        LitInteger i _ -> mInt $ fromInteger i
        MachChar _     -> error "MachChar!"
        -- MachStr ms  -> error $ "found MachStr "++(show ms)
        MachNullAddr   -> error "MachNullAddr!"
        MachFloat _    -> error "MachFloat!"
        MachDouble _   -> error "MachDouble!"
        _              -> error "Unknown Lit!"
transExpr dfs eApp@(App _ _) = flatten dfs eApp []
transExpr dfs (Lam b e) =
  LamF Nothing (stringToQName $ showPPr dfs b) (transExpr dfs e)
transExpr dfs (Let bind e) = LetF Nothing (transBind dfs bind) (transExpr dfs e)
transExpr dfs (Case e bnd _ alts) =
  let e' = transExpr dfs e
      notDEFAULT (DEFAULT, _, _) = False
      notDEFAULT _ = True
      pats = map (transPat dfs) (filter notDEFAULT alts)
      -- keepPat (PatB _ _ (FF (V (QN Nothing "patError")) _)) = False
      keepPat _ = True
      bndName = stringToQName $ showPPr dfs bnd
  in  -- if take 4 bndName == noBinder then 
        CaseF noCaseLoc e' bndName (filter keepPat pats)
      -- else
      --  error $ "non-wildcard binder '"++bndName++"' is not yet supported in GHC Core pattern matching"
transExpr _ (Cast _ _) = error "Cast!"
transExpr _ (Tick _ _) = error "Tick!"
transExpr dfs (Type t) = 
  error $ "Types are not supported: "++(showSDoc' dfs (pprType t))
transExpr _ (Coercion _) = error "Coercion!"

-- | Translates a GHC Core pattern to an FL pattern.
transPat :: DynFlags -> Alt CoreBndr -> PatF
transPat dfs (DataAlt altCon, bvs, e) =
  let cstr' = stringToQName $ showPPr dfs (dataConName altCon)
      bvs'  = map (stringToQName.showPPr dfs) bvs      
  in  PatB (SPat cstr' bvs', PatInfo True) (transExpr dfs e)
transPat _ (LitAlt _, _, _) = error "GHC Core literal patterns not yet supported"
transPat _ (DEFAULT, _, _) = error "GHC Core DEFAULT branches not yet supported"
  
-- | Returns the name of an Id.
nm :: DynFlags -> Id -> String
nm dfs id0 = showPPr dfs (nameOccName (idName id0))

-- | Identifies the constructor of an expression, used for debugging.
ident :: forall a. CoreExpr -> a
ident (Var {})      = error "@@Var@@"
ident (Lit {})      = error "@@Lit@@"
ident (App {})      = error "@@App@@"
ident (Lam {})      = error "@@Lam@@"
ident (Let {})      = error "@@Let@@"
ident (Case {})     = error "@@Case@@"
ident (Cast {})     = error "@@Cast@@"
ident (Tick {})     = error "@@Tick@@"
ident (Type {})     = error "@@Type@@"
ident (Coercion {}) = error "@@Coercion@@"

-- | Flattens nested function applications. Translates special GHC Core
--   functions and operators.
flatten :: DynFlags -> CoreExpr -> [CoreExpr] -> ExprF
flatten dfs (App f (Type _)) args = flatten dfs f args
flatten dfs (App f e) args = flatten dfs f (e:args)
flatten dfs (Var f) args = 
  let transArgs = map (transExpr dfs) args
      appNameS = nm dfs f                 -- simple name
      appNameF = showPPr dfs f            -- full name
      appNameQN = mapGHCtoGICName $ stringToQName appNameF
      ff f' args' = FF (V f') args' NoCI
  in  if appNameS `elem` cBuiltinOps then
        if appNameS `elem` (elems cOps) then
          case (length args) of
            2 -> ConF (CN $ cOpForStr appNameS) transArgs
            3 -> -- Constant operator taking dictionary, examine.
              case args !! 0 of
                Var arg0 ->
                  let arg0Name = showPPr dfs arg0
                      intClasses = ["GHC.Num.$fNumInt", "GHC.Classes.$fOrdInt"]
                  in  if arg0Name `elem` intClasses then
                        ConF (CN $ cOpForStr appNameS) (drop 1 transArgs)
                      else
                        ierr $ "Unknown first parameter: "++arg0Name
                _ -> ierr $ "Cannot handle first parameter to constant "++appNameS
            _ -> ierr $ "Unknown GHC built-in: "++appNameS
        else
          error $ "missing implementation of builtin op "++appNameS++
                  " with arity: "++(show (length transArgs))
      else if appNameS `elem` (map lName cBuiltinFuncs) then
             case appNameS of
               "I#" ->
                 case (args, transArgs) of
                   ([_], [transArg0]) -> transArg0
                   _                  -> ierr "I# error"
               -- "unpackCString#" -> 
               --   case args of {
               --     [Lit (MachStr str)] -> mkStrList $ unpackFS str ;
               --     _ -> error "unpackCString# should be applied to one string literal" ;
               --     }
               "putStrLn" -> ff appNameQN transArgs
               -- omit the extra arguments of show, keep the last one
               "show" -> ff appNameQN [last transArgs]
               "runMainIO" -> ff appNameQN transArgs
               _   ->
                 error $ "GHCBackend: builtin function not supported: "++appNameS
           else if isConstr appNameS then
                  ConstrF appNameQN transArgs
                else
                  ff appNameQN transArgs
flatten _ x _ = ierr $ "error flattening -- "++ ident x

-- | Maps functions of the GHC API to functions implemented by GIC.
mapGHCtoGICName :: QName -> QName
mapGHCtoGICName (QN (Just "GHC.Show") "show") = bf_show
mapGHCtoGICName (QN (Just "System.IO") "putStrLn") = bf_putStrLn
mapGHCtoGICName (QN (Just "GHC.TopHandler") "runMainIO") = bf_runMainIO
mapGHCtoGICName qn = qn

-- | Translates a GHC TyCon to a data definition.
transTyCon :: DynFlags -> TyCon -> Data
transTyCon dfs tyCon =
  let dtName = stringToQName $ showPPr dfs (tyConName tyCon)
      dcons = map dataConToDC (tyConDataCons tyCon)
      as = map (\tv->showPPr dfs (varName tv)) $ tyConTyVars tyCon
      dataConToDC con =
        let (_, _, typeArgs, _) = dataConSig con
            evOrderInfo = map bang2eo (dataConStrictMarks con)
            bang2eo x = defaultEvOrder $ isBanged x
            -- dtName = showPPr dfs resultType
            dCName = stringToQName $ showPPr dfs $ dataConName con
            dCArgs =
              let mkDT (fs, s) = DT fs s Nothing
              in  map mkDT $ zip (map (transT dfs) typeArgs) evOrderInfo
        in  if ByValue `elem` evOrderInfo then 
              ierr $ "TODO: found strict components for data type: " ++ (qName dCName)
            else DConstr dCName dCArgs Nothing
  in  Data dtName as dcons

-- | Processes pattern matching expressions to make them more FL-like:
--   removes scrutinee binders, translates boolean pattern matching 
--   to if-then-else, processes calls to built-in tuples.
processPatMatch :: ScrutOpt -> [Data] -> [DefF] -> [DefF]
processPatMatch scrOpt datatypes defs =
  let isTupleConstr s =
        (length s > 2) && (head s == '(') && (last s == ')') && 
        (all (==',') $ tail $ init s)
      procPMD (DefF f fs e) = DefF f fs (procPME (scrOpt, frmsToNames fs) e)
      procPME _ e@(XF _) = e
      procPME si (ConF c el) = ConF c (map (procPME si) el)
      procPME si (FF f el ci) = FF f (map (procPME si) el) ci
      procPME si (ConstrF (QN Nothing c) el@[_, _]) | isTupleConstr c =
          let tupleC = bf_Tuple (length el)
          in  if constrExists datatypes tupleC then
                ConstrF tupleC (map (procPME si) el)
              else
                error $ "GHC tuple found, you should define a " ++
                        (show tupleC) ++ " constructor"
      procPME si (ConstrF c el) = ConstrF c (map (procPME si) el)
      procPME si (CaseF d e bnd pats) =
        case e of
          ConF (CN c) _ ->
            let testForConstr cT (PatB (SPat c' _, _) _) = cT==c'
                tPats = filter (testForConstr const_GHC_Types_True ) pats
                fPats = filter (testForConstr const_GHC_Types_False) pats
            in  if c `elem` cOpsBool then
                  case (tPats, fPats) of
                    ([PatB _ tExpr], [PatB _ fExpr]) ->
                      if bnd /= underscoreVar &&
                         countVarUses si (V bnd) tExpr == 0 &&
                         countVarUses si (V bnd) fExpr == 0 then
                        ConF (CN CIf) [e, tExpr, fExpr]
                      else
                        error "boolean pattern matching uses binder"
                    _ -> error "boolean pattern matching without 2 branches"
                else
                  error $ "strange builtin scrutinee: "++(show e)
          _ ->
            let aux orig scrut v = if v==scrut then orig else v
                renVar :: QName -> QName -> ExprF -> ExprF
                renVar orig scrut (XF (V v)) =
                  XF (V (aux orig scrut v))
                renVar _ _ (XF (BV _ _)) =
                  ierr "renVar: found bound variable"
                renVar orig scrut (ConF c el) =
                  ConF c (map (renVar orig scrut) el)
                renVar orig scrut (FF (V v) el ci) =
                  FF (V (aux orig scrut v)) (map (renVar orig scrut) el) ci
                renVar _ _ (FF (BV _ _) _ _) =
                  ierr "renVar: found bound variable application"
                renVar orig scrut (ConstrF c el) =
                  ConstrF c (map (renVar orig scrut) el)
                renVar orig scrut (CaseF dC eC bndC patsC) =
                  let renVarP (PatB sPat eP) =
                        PatB sPat (renVar orig scrut eP)                  
                  in  CaseF dC (renVar orig scrut eC) bndC
                            (map renVarP patsC)
                renVar orig scrut (LetF dL bindsL eL) =
                  let renVarD (DefF f fs eD) =
                        DefF f fs (renVar orig scrut eD)
                  in  LetF dL (map renVarD bindsL) (renVar orig scrut eL)
                renVar orig scrut (LamF dL vL eL) =
                  LamF dL vL (renVar orig scrut eL)
                renPat orig scrut (PatB sPat ePat) =
                  PatB sPat (renVar orig scrut ePat)
            in case e of
                 -- if 'case v of v2 pats' then 'case v of pats[v2/v]'
                 XF (V var) -> 
                   let renPats = map (renPat var bnd) pats
                   in  CaseF d (procPME si e) underscoreVar
                       (map (procPMP si) renPats)
                 -- in the general case, transform 'case e of v2 pats'  
                 -- to let v2 = e in case e of pats'
                 -- this may lead to loss of efficiency
                 _ ->
                   LetF Nothing [DefF bnd [] e] (
                     CaseF d (XF (V bnd)) underscoreVar (map (procPMP si) pats)
                   )
      procPME _ (LetF {}) =
        error "TODO: procPME/si for let"
      -- procPME si (LetF d binds e) =
        -- LetF d (map procPMD binds) (procPME si e)
      procPME _ (LamF {}) =
        error "TODO: procPME/si for lambda"
      -- procPME si (LamF d v e) =
        -- LamF d v (procPME e)
      procPMP si (PatB (SPat (QN Nothing tc) bvs@[_, _], pI) eP)
        | isTupleConstr tc =
          PatB (SPat (dtTuple (length bvs)) bvs, pI) (procPME si eP)
      procPMP si (PatB sPat eP) = PatB sPat (procPME si eP)
  in  map procPMD defs
      
-- * GHC built-in constructors

const_GHC_Types_True :: QName
const_GHC_Types_True = QN (Just "GHC.Types") "True"
const_GHC_Types_False :: QName
const_GHC_Types_False = QN (Just "GHC.Types") "False"

-- | Runs a source file through GHC, until GHC Core is emitted.
coreGHC :: GhcMonad m => DynFlags -> FPath -> [MName] -> [MName] ->
           m (DynFlags, IO CoreProgram)
coreGHC dflags file _ _ =
    do coreMod <- compileToCoreModule file
       -- coreMod <- compileToCoreSimplified file
#if __GLASGOW_HASKELL__ >= 706
       hscEnv <- getSession
       let corePrep = corePrepPgm dflags hscEnv (cm_binds coreMod) []
#else
       let corePrep = corePrepPgm dflags (cm_binds coreMod) []
#endif       
       dflagsFinal <- getSessionDynFlags
       return (dflagsFinal, corePrep)
