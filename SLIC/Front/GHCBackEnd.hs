-- | The GHC back-end connector. To be used together with a source tree of GHC.
-- 

{-# LANGUAGE CPP #-}
module SLIC.Front.GHCBackEnd (coreGHC, getVTypes, showPPr, tcGHC, transfCore) where

import Bag (bagToList)
import BasicTypes (isBanged)
import CorePrep (corePrepPgm)
import CoreSyn (Alt, Bind(..), CoreBind, CoreBndr, CoreExpr, CoreProgram, 
                AltCon(..), Expr(..))
import DataCon (dataConName)
import FastString (unpackFS)
import GHC 
import Id (idName)
import Literal
import Name (nameOccName, occNameString)
import Outputable (Outputable, SDoc, ppr, showSDoc)
import TyCon (tyConName)
import Type (Type, isTyVar, pprType, splitFunTys)
import Var (varName, varType)
import System.Exit (ExitCode(ExitSuccess), exitWith)

import Data.List ((\\))
import Data.Map (empty, elems, fromList)
import SLIC.AuxFun (ierr, pathOf, trace2)
import SLIC.Constants
import SLIC.Driver (processFL)
import SLIC.State (defaultOptions)
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- * GHC Core transformation

-- | The main entry point of the GHC connector, takes a GHC Core program
--   (as a set of binders and type constructors) and feeds it to the compiler.
transfCore :: DynFlags -> CoreProgram -> [TyCon] -> IO ()
transfCore dfs binds tyCons = 
  let defs      = processPatMatch datatypes (concatMap (transBind dfs) binds)
      datatypes = map (transTyCon dfs) tyCons
      fm        = (defaultMod, modNoPath)
      exports   = empty
      tAnnot    = empty
      tcs       = TcInfo [] []     -- TODO: handle type classes in this back-end
      moduleFL  = (Mod fm exports [] (Prog datatypes defs)) tAnnot tcs
  in  trace2 (pprint moduleFL "") (
      processFL defaultOptions [] moduleFL >>
      exitWith ExitSuccess
      )
      
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
        let bQN = (stringToQName $ showPPr dfs b)
        in  DefF bQN (namesToFrms (frms b e) False) (transExpr dfs (body e))
  in  case bind of
        NonRec b e -> [transBind' (b, e)]
        Rec binds -> map transBind' binds

-- | Translates a GHC Core expression to an FL expression.
transExpr :: DynFlags -> CoreExpr -> ExprF
transExpr dfs (Var vId) = 
  case nm dfs vId of
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
      -- keepPat (PatF _ _ (FF (V (QN Nothing "patError")) _)) = False
      keepPat _ = True
      bndName = stringToQName $ nm dfs bnd
  in  -- if take 4 bndName == noBinder then 
        CaseF (Nothing, noEFunc) e' bndName (filter keepPat pats)
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
      bvs'  = map (stringToQName.nm dfs) bvs      
  in  PatF (SPat cstr' bvs') (transExpr dfs e)
transPat _ (LitAlt _, _, _) = error "GHC Core literal patterns not yet supported"
transPat _ (DEFAULT, _, _) = error "GHC Core DEFAULT branches not yet supported"
  
-- | Returns the name of an Id.
nm :: DynFlags -> Id -> String
nm dfs id0 = showPPr dfs (nameOccName (idName id0))

-- | Calls the GHC pretty printer for an Outputable argument.
showPPr :: DynFlags -> (Outputable a) => a -> String
showPPr dfs s = showSDoc' dfs (ppr s)

showSDoc' :: DynFlags -> Outputable.SDoc -> String
-- showSDoc' :: DynFlags -> (Outputable a) => a -> String
#if __GLASGOW_HASKELL__ >= 706
showSDoc' dfs s = showSDoc dfs s
#else
showSDoc' _ s = showSDoc s
#endif

-- | Identifies the constructor of an expression, used for debugging.
ident :: forall a. CoreExpr -> a
ident (Var _) = error "@@Var@@"
ident (Lit _) = error "@@Lit@@"
ident (App _ _) = error "@@App@@"
ident (Lam _ _) = error "@@Lam@@"
ident (Let _ _) = error "@@Let@@"
ident (Case _ _ _ _) = error "@@Case@@"
ident (Cast _ _) = error "@@Cast@@"
ident (Tick _ _) = error "@@Tick@@"
ident (Type _) = error "@@Type@@"
ident (Coercion _) = error "@@Coercion@@"

-- | Flattens nested function applications. Translates special GHC Core
--   functions and operators.
flatten :: DynFlags -> CoreExpr -> [CoreExpr] -> ExprF
flatten dfs (App f (Type _)) args = flatten dfs f args
flatten dfs (App f e) args = flatten dfs f (e:args)
flatten dfs (Var f) args = 
  let transArgs = map (transExpr dfs) args
      appName = nm dfs f
      appNameQN = stringToQName appName
      ff f' args' = FF (V f') args'
  in  if appName `elem` cBuiltinOps then
        -- TODO: drop dictionary arguments
        if appName `elem` (elems cOps) then
          ConF (CN $ cOpForStr appName) transArgs
        else
          error $ "missing implementation of builtin op "++appName++" with arity: "++(show (length transArgs))
      else if appName `elem` (map lName cBuiltinFuncs) then
             case appName of
               "I#" -> if length args /= 1 then 
                         error "I# should be applied to one integer"
                       else transArgs !! 0 -- ConF (pprint (transArgs !! 0) "") []
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
                 error $ "GHCBackend: builtin function not supported: "++appName
           else if isConstr appName then
                  ConstrF appNameQN transArgs
                else
                  ff appNameQN transArgs
flatten _ x _ = ierr $ "error flattening -- "++ ident x

-- | Translates a GHC TyCon to a data definition.
transTyCon :: DynFlags -> TyCon -> Data
transTyCon dfs tyCon =
  let dtName = stringToQName $ showPPr dfs (tyConName tyCon)
      dcons = map dataConToDC (tyConDataCons tyCon)
      as = map (\tv->showPPr dfs (varName tv)) $ tyConTyVars tyCon
      dataConToDC con =
        let (_, _, typeArgs, _) = dataConSig con
            stricts= map isBanged (dataConStrictMarks con)
            -- dtName = showPPr dfs resultType
            dCName = stringToQName $ showPPr dfs $ dataConName con
            dCArgs =
              map (\(fs, s)->DT fs s Nothing) $ zip (map (transT dfs) typeArgs) stricts
        in  if or stricts then 
              ierr $ "TODO: found strict components for data type: " ++ (qName dCName)
            else DConstr dCName dCArgs Nothing
  in  Data dtName as dcons

-- | Translates a GHC type to a GIC type.
transT :: DynFlags -> Type.Type -> SLIC.Types.Type
transT dfs ty =
  let (tArgs, tRes) = splitFunTys ty
      ftRes = Tg (T (stringToQName $ showPPr dfs tRes))     -- TODO: does this work with h.o. types?
  in  case tArgs of
        [] -> ftRes
        _  -> let aux [] = ftRes
                  aux (t:ts) = Tf (transT dfs t) (aux ts)
              in  aux tArgs

      -- case (\a-> FTypeS (showPPr dfs a)) (\a-> FTypeS (showPPr dfs a))

-- | Processes pattern matching expressions to make them more FL-like:
--   removes scrutinee binders, translates boolean pattern matching 
--   to if-then-else, processes calls to built-in tuples.
processPatMatch :: [Data] -> [DefF] -> [DefF]
processPatMatch datatypes defs =
  let isTupleConstr s =
        (length s > 2) && (head s == '(') && (last s == ')') && 
        (all (==',') $ tail $ init s)
      procPMD (DefF f fs e) = DefF f fs (procPME e)
      procPME e@(XF _) = e
      procPME (ConF c el) = ConF c (map procPME el)
      procPME (FF f el) = FF f (map procPME el)
      procPME (ConstrF (QN Nothing c) el@[_, _]) | isTupleConstr c =
          let tupleC = bf_Tuple (length el)
          in  if constrExists datatypes tupleC then
                ConstrF tupleC (map procPME el)
              else
                error $ "GHC tuple found, you should define a " ++
                        (show tupleC) ++ " constructor"
      procPME (ConstrF c el) = ConstrF c (map procPME el)
      procPME (CaseF d e bnd pats) =
        case e of
          ConF (CN c) _ ->
            let testForConstr cT (PatF (SPat c' _) _) = cT==c'
                tPats = filter (testForConstr const_GHC_Types_True ) pats
                fPats = filter (testForConstr const_GHC_Types_False) pats
            in  if c `elem` cOpsBool then
                  if length tPats == 1 && length fPats == 1 then
                    let (PatF _ tExpr) = tPats !! 0
                        (PatF _ fExpr) = fPats !! 0
                    in  if bnd /= underscoreVar &&
                           countVarUses (V bnd) tExpr == 0 &&
                           countVarUses (V bnd) fExpr == 0 then
                          ConF (CN CIf) [e, tExpr, fExpr]
                        else
                          error "boolean pattern matching uses binder"
                  else
                    error "boolean pattern matching without 2 branches"
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
                renVar orig scrut (FF (V v) el) =
                  FF (V (aux orig scrut v)) (map (renVar orig scrut) el)
                renVar _ _ (FF (BV _ _) _) =
                  ierr "renVar: found bound variable application"
                renVar orig scrut (ConstrF c el) =
                  ConstrF c (map (renVar orig scrut) el)
                renVar orig scrut (CaseF dC eC bndC patsC) =
                  let renVarP (PatF sPat eP) =
                        PatF sPat (renVar orig scrut eP)                  
                  in  CaseF dC (renVar orig scrut eC) bndC
                            (map renVarP patsC)
                renVar orig scrut (LetF dL bindsL eL) =
                  let renVarD (DefF f fs eD) =
                        DefF f fs (renVar orig scrut eD)
                  in  LetF dL (map renVarD bindsL) (renVar orig scrut eL)
                renVar orig scrut (LamF dL vL eL) =
                  LamF dL vL (renVar orig scrut eL)
                renPat orig scrut (PatF sPat ePat) =
                  PatF sPat (renVar orig scrut ePat)
            in case e of
                 -- if 'case v of v2 pats' then 'case v of pats[v2/v]'
                 XF (V var) -> 
                   let renPats = map (renPat var bnd) pats
                   in  CaseF d (procPME e) underscoreVar (map procPMP renPats)
                 -- in the general case, transform 'case e of v2 pats'  
                 -- to let v2 = e in case e of pats'
                 -- this may lead to loss of efficiency
                 _ ->
                   LetF Nothing [DefF bnd [] e] (
                     CaseF d (XF (V bnd)) underscoreVar (map procPMP pats)
                   )
      procPME (LetF d binds e) = LetF d (map procPMD binds) (procPME e)
      procPME (LamF d v e) = LamF d v (procPME e)
      procPMP (PatF (SPat (QN Nothing tc) bvs@[_, _]) eP) | isTupleConstr tc =
          PatF (SPat (dtTuple (length bvs)) bvs) (procPME eP)
      procPMP (PatF sPat eP) = PatF sPat (procPME eP)
  in  map procPMD defs
      
-- * GHC built-in constructors

const_GHC_Types_True :: QName
const_GHC_Types_True = QN (Just "GHC.Types") "True"
const_GHC_Types_False :: QName
const_GHC_Types_False = QN (Just "GHC.Types") "False"

-- * GHC interfaces

-- | Runs a source file through the GHC parser/type inference engine.
tcGHC :: GhcMonad m => DynFlags -> FPath -> [MName] -> [MName] ->
         m (DynFlags, TypecheckedModule)
tcGHC _ file mNames mg =
  do let mn = if length mNames == 1 then (mNames!!0) else "Main"
     let fPath = pathOf file
     let fNames = map (\m->fPath++[dirSeparator]++m++".hs") mg
     -- TODO: handle whole program compilation here
     targets <- mapM (\f -> guessTarget f Nothing) fNames
     setTargets targets
     dflags <- getSessionDynFlags
     -- TODO: is this needed?
     let inclPaths' = (includePaths dflags)++[fPath]
     _ <- setSessionDynFlags dflags{includePaths=inclPaths'}{hscOutName="/dev/null"}
     let ctxtMods = map (\x -> IIDecl $ (simpleImportDecl . mkModuleName) x)
                    ((\\) mg [mn])
     setContext ctxtMods
     Succeeded <- load LoadAllTargets
     modSum <- getModSummary $ mkModuleName mn
     p <- GHC.parseModule modSum
     tMod <- typecheckModule p
     -- d <- desugarModule t
     -- l <- loadModule d
     -- n <- getNamesInScope
     -- c <- return $ coreModule d
     dflagsFinal <- getSessionDynFlags
     return $ (dflagsFinal, tMod)

-- | Runs a source file through GHC, until GHC Core is emitted.
coreGHC :: GhcMonad m => DynFlags -> FPath -> [MName] -> [MName] ->
           m (DynFlags, IO CoreProgram)
coreGHC dflags file _ _ =
    do coreMod <- compileToCoreModule file
       -- coreMod <- compileToCoreSimplified file
#if __GLASGOW_HASKELL__ >= 706
       hscEnv <- getSession
       corePrep <- return $ corePrepPgm dflags hscEnv (cm_binds coreMod) []
#else
       corePrep <- return $ corePrepPgm dflags (cm_binds coreMod) []
#endif       
       dflagsFinal <- getSessionDynFlags
       return $ (dflagsFinal, corePrep)

-- | Reads type information contained in source typechecked by GHC.
getVTypes :: DynFlags -> TypecheckedSource -> TEnv
getVTypes dflags prog =
  let progBinds = map unLoc $ bagToList prog
      vt ab@(AbsBinds {}) =
        let binds = map unLoc $ bagToList $ abs_binds ab
        in  concatMap vtBind binds
      vt fb@(FunBind {}) = vtBind fb
      vt _ = error "vt: unknown binding"          
      vtBind fb@(FunBind {}) =
        let -- function name
            f    = unLoc $ fun_id fb
            f_vn = varName f
            f_qn = QN Nothing (occNameString (nameOccName f_vn))
            f_t  = transT dflags (varType f)
            fI   = (f_qn, (f_t, Just 0))
        in  [fI]
      vtBind _ = ierr "vtBind: unknown binding"
  in  fromList $ concatMap vt progBinds
