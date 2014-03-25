-- | The modular polymorphic defunctionalization algorithm.
--   
--   Modular polymorphic defunctionalization can separately defunctionalize 
--   modules written in a higher-order language, FLm. Each module
--   can be separately transformed to list of first-order functions and a
--   defunctionalization interface (DFI). The list of functions can be used
--   for separate compilation. The DFIs of all modules can be combined with
--   the defunctionalized functions at linking time to produce the final
--   first-order program. For input programs containing polymorphism or
--   GADTs, the code generated by defunctionalization also uses GADTs.
-- 
--   For a formal account of polymorphic defunctionalization, see:
--   François Pottier and Nadji Gauthier. \"Polymorphic typed
--   defunctionalization and concretization.\" Higher-Order and Symbolic
--   Computation, 19:125-162, March 2006. For the modular defunctionalization
--   technique, see: G. Fourtounis, N. Papaspyrou, P. Rondogiannis.
--   \"The Generalized Intensional Transformation for Implementing Lazy
--   Functional Languages.\" In Proc. of the Fifteenth International Symposium
--   on Practical Aspects of Declarative Languages, 2013.
-- 
--   The syntax and types used for the input and output languages can be
--   found in "SLIC.SyntaxFL".
-- 
--   Also contains the FL linker that adds the missing FL code for closure 
--   construction and application to separately defunctionalized FL code.
-- 

module SLIC.Front.Defunc (DfFlags, ModD(ModD), defuncMod, dfDT, dfFlags,
                          dfiAppSigs, dfMod, dfName, genDfCode, genDfModFinal,
                          genModDFI, genNApp, linkF, optEnums) where

import Data.List as L (map)
import qualified Data.Map as M (empty, filterWithKey, fromList, keys, lookup,
                                map, mapWithKey, toList, union, unions)
import Data.Maybe (mapMaybe)
import Data.Set as S (Set, difference, filter, fromList, map, size, toList, union)
import SLIC.AuxFun (ierr)
import SLIC.Constants
import SLIC.DFI (DfConstrs, DfInfo(..), DFI(..), DfInfo(DfInfo), DFC(DFC, dfcA),
                 ExtAppFuns, LARInfo(LARInfo, liCIDs, liPMDs), addApp,
                 emptyDfInfo, mergeDfInfo, noApps)
import SLIC.Front.Preprocessor (cNested, constrToFuncs)
import SLIC.ITrans.Optimizations (optEnumsKernel)
import SLIC.State
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- * Defunctionalization data structures

type LVars     = TEnv            -- ^ local variables in a function
type VFuns     = TEnv            -- ^ functions visible from inside a module

-- | A defunctionalized module is an FL module containing the defunctionalized
--   version of the input higher-order FL module, accompanied by a DFI.
data ModD      = ModD ModF DFI

instance PPrint ModD where
  pprint (ModD m dfi) =
    ("* Defunctionalized module:"++).nl.pprint m.
    pprint dfi

-- | The flags used by defunctionalization.
type DfFlags = (DoNullDf, Strictness, ScrutOpt, TypeChecker)

-- | Extract defunctionalization flags from the full set of flags.
dfFlags :: Options -> DfFlags
dfFlags opts = (optNullDf opts, optStrict opts, optScrut opts, optTC opts)

-- | Make 0-order data types equivalent to integers if the 'enum' mode is enabled.
--   This optimization breaks encapsulation (it may know if an imported data type 
--   has hidden nullary constructors), so it is used only in whole program mode.
optEnums :: ModD -> ModD
optEnums (ModD modF dfi) = 
  let Prog dts defs = modProg modF
      (dts', env') = optEnumsKernel (dts, dfiTEnv dfi)
  in  ModD modF{modProg = Prog dts' defs} dfi{dfiTEnv=env'}

-- * State threaded by defunctionalization

type DfState a = (a, DfInfo)

unzipI :: [(a, DfInfo)] -> ([a], DfInfo)
unzipI aInfos =
  (L.map fst aInfos, mergeDfInfo (L.map snd aInfos))

-- * Transformation

-- | Defunctionalize a module.
--   Takes two environments: the constructor types and the function types.
defuncMod :: Options -> TEnv -> ModF -> ModD
defuncMod opts ve' m'@(Mod fm@(mN, _) exportsR importsR (Prog dts defs) an tcs) =
  let vFuns     = visibleFuns ve' m'
      vFunNames = M.keys vFuns
      -- Read the required flags (nullary defunctionalization, strictness).
      flags@(ndf, _, _, _) = dfFlags opts
      -- 1. Transform the function definitions using defunctionalization.
      -- 2. Gather the constructors generated by defunctionalization
      --    and the closure dispatcher calls inserted.
      (dfDefs, dfI) = unzipI $ L.map (defuncD ndf ve' vFuns) defs
      dfcs = diDfcs dfI
      -- 3. Replace higher-order components in data types with closure data types.
      dfDataC (DConstr c ts rt) =
        DConstr c (L.map (\(DT t s sel)->DT (lowT t) s sel) ts) (fmap lowT rt)
      dfData    = L.map (\(Data dt as cs) ->
                             Data dt as (L.map dfDataC cs)) dts
      -- Update the environment with the types of the closure constructors.
      (dfConstrsVE, appFrmsVE, clSigs, appSigs) = makeAppInfo ve' dfcs
      -- Defunctionalize types, arities stay the same.
      dfConstrsVE' = M.map (\(t, ar)->(defuncT t, ar)) dfConstrsVE
      appSigs'     = M.unions [clSigs, appSigs, genAllAppSigs ve' defs]
      appFrmsVE'   = {- M.union -} appFrmsVE {- (genAllAppTypes ve' defs) -} 
      -- Defunctionalize types in the environment.
      prettyPrinters = L.map (\(Data dt _ _)->QN (Just mN) (pprint dt "")) dts
      funcs = vFunNames ++ cBuiltinFuncs ++ prettyPrinters
      dfAppsVE' = mkDfAppsTEnv $ diEApps dfI
      ve'' = M.unions [ dfConstrsVE', dfAppsVE'
                      , defuncEnv funcs $ M.union ve' appFrmsVE' ]
      tcs' = L.map defuncTcDecl $ tcIDecls tcs
      -- Calculate the constructor ids (to put them in the DFI).
      cids = calcCIDs dfData
      -- Generate the module graph info.
      mg = (mN, filterRealMods importsR)
      -- Generate the DFI for the current module (CAFs/PMDepths are empty here).
      larInfo = LARInfo [] cids M.empty Nothing
      tcInfo = TcInfo tcs' (tcISigs tcs)
      dfi = DFI mg ve'' (getSigs defs) dfI larInfo tcInfo
      -- Make the closure GADT.
      dfClosDT = mkClosureGADT flags dfcs
      importsR' = importsR ++ [dfImports ve'' dfClosDT (M.union clSigs appSigs')]
  in  ModD (Mod fm exportsR importsR' (Prog dfData dfDefs) an tcInfo) dfi

-- | Takes a list of closure types and signatures of /apply()/ functions and
--   creates the corresponding ''import'' declarations.
dfImports :: TEnv -> Data -> FuncSigs -> IDecl
dfImports dfEnv dfClosDT dfSigs = 
  let mkIInfoApp (f, fs) =
        let fType = (Just $ findType f dfEnv)
        in  (f, IInfo fType (Just $ length fs) NFunc Nothing (Just 1))
      mkIInfoCC :: Data -> [(QName, IInfo)]
      mkIInfoCC (Data _ _ dcs) = 
        let aux (DConstr cc dts _) =
              (cc, IInfo (Just $ findType cc dfEnv) (Just $ length dts)
                   NConstr Nothing Nothing)
        in  L.map aux dcs      
      iDfNames = M.fromList ((mkIInfoCC dfClosDT)++(L.map mkIInfoApp $ M.toList dfSigs))
      -- the CIDs table is empty; closure constructors are compiled during linking, 
      -- so this information is useless
      dfCids = M.empty   
  in  IDecl dfMod iDfNames (Just (dfSigs, dfCids))

defuncTcDecl :: TcDecl -> TcDecl
defuncTcDecl (TcDecl tcn tv methods) =
  TcDecl tcn tv (L.map (\(sig, t) -> (sig, defuncT t)) methods)

-- | Calculate the visible functions from inside this module.
visibleFuns :: TEnv -> ModF -> TEnv
visibleFuns ve modF = 
  let imports = modImports modF
      defs = progDefs $ modProg modF
      localFuns :: TEnv
      localFuns   = M.fromList $ L.map (\(DefF f fs _)->(f, (findType f ve, Just (length fs)))) defs
      -- ignore imported data type names
      iEnvToDfEnv (_, iinfo) | (impC iinfo == NDType) = Nothing
      iEnvToDfEnv (v, IInfo (Just t ) ar _ _ _) = Just (v, (t, ar))
      iEnvToDfEnv (_, IInfo (Nothing) _  _ _ _) =
        ierr "defunctionalization found untyped import"
      vfsImported = M.fromList $ mapMaybe iEnvToDfEnv 
                    (concatMap M.toList $ L.map ideclINames imports)
  in  M.union localFuns vfsImported

-- | Generates information about the closure dispatchers: their types, the 
--   types of their formals and the signatures of the closure constructors
--   and dispatchers (for the intensional transformation).
makeAppInfo :: TEnv -> DfConstrs -> (TEnv, TEnv, FuncSigs, FuncSigs)
makeAppInfo ve dfcs =
  let dfcTs :: TEnv
      dfcTs = M.fromList $ L.map typeDFC $ S.toList dfcs
      appTs = concatMap (appDFC ve) $ S.toList dfcs
      -- the types of closure dispatchers
      appTsVE :: TEnv
      appTsVE   = M.fromList $ L.map (\(f, _, ei)->(f, ei)) appTs
      -- the types of closure dispatcher formals
      appFrmsVE :: TEnv
      appFrmsVE = M.fromList $ concatMap (\(_, fs, _)->fs) appTs
      -- the signatures of function dispatchers
      appFSigs  = M.fromList $ L.map (\(f,fs,_)->(f, L.map fst fs)) appTs 
      -- the signatures of closure constructors
      clSigs = M.fromList
               (L.map (\(DFC c a _ _)->(c, cArgsC c a)) $ S.toList dfcs)
      -- the types of the closure constructors' parameters
      clArgsVE= M.fromList $ concatMap 
                (\(DFC c a _ _)->zip (cArgsC c a) 
                                 (L.map (\(t, ar)->(lowT t, ar)) $ 
                                  getTypesOf c dfcTs)) (S.toList dfcs)
      -- from a DFC definition, creates a constructor and its type/arity info
      typeDFC :: DFC -> (CstrName, EInfo)
      typeDFC (DFC c cAr _ (f, _)) =
        case M.lookup f ve of
          Just (t, _)  ->
            let cT = constrT (take cAr $ types t) (Tg (TDF dfDT (residualT cAr t)))
            in  (c, (cT, Just cAr))
          Nothing -> ierr "typeDFC: don't know where to find type"
  in  (M.unions [dfcTs, appTsVE, clArgsVE], appFrmsVE, clSigs, appFSigs)

-- | From a DFC definition, creates the closure dispatcher name, the type 
--   information of its formals, and the dispatcher's information.
appDFC :: TEnv -> DFC -> [(QName, TEnvL, EInfo)]
appDFC ve (DFC _ ar rT (f, _)) =
  case M.lookup f ve of
    Just (t, Just fullAppArity) ->
      let argTs i = take' i (types rT)
          clo typ = Tg (TDF dfDT typ)
          appFrms :: Int -> TEnvL
          appFrms n =
            (genCl n, (clo rT, Nothing)):
            (L.map (\(j, ty)->
                        (genCla n j, (ty, Nothing))) (zip [0..(n-1)] (argTs n)))
          appPartT n =
            let partT = constrT ((clo rT):(argTs n)) (clo (dropT n rT))
            in  (genNApp n, appFrms n, (partT, Just n))
          appFullT n =
            (genNApp n, appFrms n, (Tf (clo rT) (dropT ar t), Just n))
          argsLeft = fullAppArity - ar
      in  (L.map appPartT [1..argsLeft-1])++[appFullT argsLeft]
    Just (_, Nothing) ->
      ierr $ "appDFC: closure of "++(qName f)++" has no arity"
    Nothing -> ierr "appDFC: don't know where to find type"

{-
pprintAppInfo :: [(QName, TEnvL, EInfo)] -> ShowS
pprintAppInfo apps =
  pprFSigs $ M.fromList $ L.map (\(qn, params, _)->(qn, L.map fst params)) apps
-}

genAllAppSigs :: TEnv -> [DefF] -> FuncSigs
genAllAppSigs ve defs =
  let genAppSigsD lvs (DefF _ frms e) = genAppSigsE ((frmsToNames frms)++lvs) e
      genAppSigsE _ (XF _) = M.empty
      genAppSigsE lvs (ConF _ el) = genAppSigsL lvs el
      genAppSigsE lvs (ConstrF _ el) = genAppSigsL lvs el
      genAppSigsE lvs (CaseF _ e _ pats) =
        M.unions $ (genAppSigsE lvs e) : (L.map (genAppSigsP lvs) pats)
      genAppSigsE lvs (LetF _ ldefs e) =
        M.unions $ (genAppSigsE lvs e) : (L.map (genAppSigsD lvs) ldefs)
      genAppSigsE lvs (LamF _ _ e) = genAppSigsE lvs e
      genAppSigsE lvs (FF f el) =        
        let fName = nameOfV f
        in  if fName `elem` lvs then
              case M.lookup fName ve of
                Nothing -> ierr $ "genAllAppSigs: no type for "++(qName fName)
                Just _ ->
                  let appName = genNApp argsLen
                      appFrms = (genCl argsLen) : (L.map (genCla argsLen) [0..(argsLen-1)])
                      argsLen = length el
                  in  M.union (M.fromList [(appName, appFrms)]) (genAppSigsL lvs el)
            else
              genAppSigsL lvs el
      genAppSigsP lvs (PatB (SPat _ bvs, _) e) = genAppSigsE (bvs++lvs) e
      genAppSigsL lvs el = M.unions $ L.map (genAppSigsE lvs) el
  in  M.unions $ L.map (genAppSigsD []) defs

-- | Defunctionalizes a definition by replacing partial applications with
--   closure constructors and inserting apply_N() calls for applications of
--   closures to N arguments.
defuncD :: DoNullDf -> TEnv -> VFuns -> DefF -> DfState DefF
defuncD _ _ _ def@(DefF _ _ (ConstrF c el)) =
  let isVar (XF (V _)) = True
      isVar _          = False
  in  if all isVar el then
        (def, emptyDfInfo)
      else
        ierr $ "defuncD: malformed constructor function for "++(qName c)
defuncD ndf ve vfs (DefF f vs e) =
  let frmNames = frmsToNames vs
      lvs :: LVars
      lvs      = M.filterWithKey (\v _ ->v `elem` frmNames) ve
      (e', info') = defuncE ndf ve vfs lvs e
  in  (DefF f vs e', info')

-- | Defunctionalize a list of expressions.
defuncEL :: DoNullDf -> TEnv -> VFuns -> LVars -> [ExprF] -> DfState [ExprF]
defuncEL _ _ _ _ [] = ([], emptyDfInfo)
defuncEL ndf ve vfs lvs el =
  let (el', infoL) = unzip $ L.map (defuncE ndf ve vfs lvs) el
  in  (el', mergeDfInfo infoL)

-- | Defunctionalize an expression.
defuncE :: DoNullDf -> TEnv -> VFuns -> LVars -> ExprF -> DfState ExprF
defuncE _ _ vfs _ e@(XF (V vName)) =
  case M.lookup vName vfs of
    Just (vT, Just vArity)
      | vArity>0 ->        -- top-level function as a higher-order name
        let dfcs = allClosuresFor vName vT [0..(vArity-1)]
        in  (FF (V (genNC vName 0)) [], (DfInfo dfcs noApps))
      | otherwise ->
        (e, emptyDfInfo)   -- top-level variable
    Just (_, Nothing) -> ierr $ "Top-level without arity: "++(qName vName)
    Nothing -> (e, emptyDfInfo)  -- local variable
defuncE _ _ _ _ bv@(XF (BV _ _)) = (bv, emptyDfInfo)
defuncE ndf ve vfs lvs (ConF b el) =
  let (el', dfInfo) = defuncEL ndf ve vfs lvs el
  in  (ConF b el', dfInfo)
-- we assume that all constructors are inside special functions and have
-- already been filtered out from the code to be defunctionalized
defuncE _ _ _ _ (ConstrF _ _) =
  ierr "defuncE: non-top-level constructor encountered"
defuncE ndf ve vfs lvs (FF f@(V fName) el) =
  let (el', dfInfoL)  = defuncEL ndf ve vfs lvs el
  in  let argsLen = length el
      in  case M.lookup fName vfs of
            Just (fT, Just fArity)  ->       ----- top-level function -------
              if fArity == argsLen then      ----- saturated application ----
                (FF f el', dfInfoL)
              else if fArity > argsLen then  ----- partial application ------
                     pappToConstr ndf fName fT fArity argsLen el' dfInfoL
                   else
                     let innerEL = take fArity el' -- over-saturated call ----
                         innerCall = if fArity == 0 then XF f
                                     else FF f innerEL
                         aux e [] = e
                         aux e (p:ps) = aux (FF (V (genNApp 1)) [e, p]) ps
                     in  (aux innerCall (drop fArity el), addApp 1 dfInfoL)
            Just (_, Nothing) ->
              ierr $ "No arity information found for "++(pprint f "")
            Nothing ->                       ----- variable -----------------
              case M.lookup fName lvs of
                Just _  ->
                  (FF (V (genNApp argsLen)) ((XF f):el'), addApp argsLen dfInfoL)
                Nothing  -> 
                  case M.lookup fName ve of
                    Just (fT@(Tf _ _), Just fArity) ->
                      if fName `elem` cBuiltinFuncsC then
                        (FF f el', dfInfoL) -- built-in, leave untransformed
                      else                  ----- h.o. function -------------
                        pappToConstr ndf fName fT fArity argsLen el' dfInfoL
                    Just _        ->        -- global constructor function --
                      (FF f el', dfInfoL)
                    Nothing -> ierr $ "defunceE: no type for "++(qName fName)
defuncE _ _ _ _ (FF (BV _ _) _) =                        
  ierr "defuncE: bound variable application encountered"
defuncE ndf ve vfs lvs (CaseF d e v pats) =
  let defuncPat (PatB sPat@(SPat c bvs, _) eP) =
        let lvs' = M.union lvs (makeVEnv bvs (getTypesOf c ve))
            (eP', dfInfo) = defuncE ndf ve vfs lvs' eP
        in  (PatB sPat eP', dfInfo)
      (e'   , (DfInfo dfcsE appsE)) = defuncE ndf ve vfs lvs e
      (pats', (DfInfo dfcsL appsL)) = unzipI $ L.map defuncPat pats
  in  (CaseF d e' v pats', DfInfo (union dfcsE dfcsL) (union appsE appsL))
defuncE _ _ _ _ (LetF {}) = ierr "cannot defunctionalize let-binding"
defuncE _ _ _ _ (LamF {}) = ierr "cannot defunctionalize lambda"

-- | For a given partial application of a function of some original type
--   and arity, to a lesser number of arguments, return the defunctionalized
--   application. The defunctionalized arguments and information is also
--   threaded.
pappToConstr :: DoNullDf -> QName -> Type -> Arity -> Int -> [ExprF] ->
                DfInfo -> DfState ExprF
pappToConstr doNullDf fName _ _ n _ _ | doNullDf && n>0 =
  error $ "Nullary defunctionalization failed, for closure of "++
          (qName fName)++" binding "++(show n)++" arguments"
pappToConstr _ fName fT fArity argsLen el' dfInfo =
  let dfcs' = allClosuresFor fName fT [argsLen..(fArity-1)]
      dfcsEl' = S.union (diDfcs dfInfo) dfcs'
  in  (FF (V (genNC fName argsLen)) el', dfInfo{diDfcs=dfcsEl'})

-- | Make a environment from a list of variables and their types.
makeVEnv :: [QName] -> [EInfo] -> TEnv
makeVEnv vs ts =
  if   length vs == length ts then M.fromList $ zip vs ts
  else ierr "variables and types have different lengths"

-- | Returns the types of the components of a name in an environment.
getTypesOf :: QName -> TEnv -> [EInfo]
getTypesOf name ve =
  case M.lookup name ve of
    Just (t, _)  -> zip (takeParams t) (repeat Nothing)
    Nothing -> ierr $ "Name "++(qName name)++": unknown type"

-- * Name generators

-- | Generates a name for the closure dispatching function for closures of
--   type t, applied to n arguments.
genNApp :: Arity -> QName
genNApp n = dfName ("apply__"++(show n))

-- | For the closure of a function f with n arguments, create a closure constructor.
genNC :: QName -> Arity -> QName
genNC f n = dfName $ "Clos_"++(qName f)++"_"++(show n)

-- | Generates a name for a closure of arity n.
genCl :: Arity -> QName
genCl n = dfName $ "_"++(show n)

-- | Generates a name for the j-th argument of closure dispatcher of arity n.
genCla :: Arity -> Int -> QName
genCla n j = dfName $ (show n)++"_"++(show j)
                        
-- * Closure constructor generators

-- | For a higher-order variable 'v' of type 't', generate all closures for a
--   list of arities.
allClosuresFor :: QName -> Type -> [Arity] -> DfConstrs
allClosuresFor v t arities =
  S.fromList $ L.map (\j->(DFC (genNC v j) j (residualT j t) (v, t))) arities

-- | Gather all different residual type arities of closures from a list of 
--   closure constructors.
clArities :: DfConstrs -> Set Arity
clArities dfcs = S.map (\(DFC _ _ t _)->order t) dfcs

-- * Type defunctionalization

-- | Changes higher-order function types to their equivalent defunctionalized ones.
defuncT :: Type -> Type
defuncT typ = constrT (L.map lowT $ takeParams typ) (lowT $ last $ types typ)

-- | Maps a non-ground type to its equivalent defunctionalized type.
lowT :: Type -> Type
lowT t@(Tg _)   = t
lowT t@(Tv _)   = t
lowT t@(Ta _ _) = t
lowT t@(Tf _ _) = Tg (TDF dfDT t)

-- * Misc

-- | Defunctionalize a typing environment. Functions are assigned first-order types
--   with defunctionalized argument types. All other variables (locals, global
--   CAFs, pattern-bound variables) get 0-order types.
defuncEnv :: [QName] -> TEnv -> TEnv
defuncEnv funcs ve =
  let dfEnvT v t = if v `elem` funcs then defuncT t else lowT t
  in  M.mapWithKey (\v (t,ar)->(dfEnvT v t,ar)) ve

-- * Defunctionalization auxiliaries.

-- | The name of the new data type created by defunctionalization.
dfDT :: DTName
dfDT = QN (Just dfMod) "Closure"

-- | Generates fully qualified names for variables/functions/constructors 
--   generated by defunctionalization.
dfName :: String -> QName
dfName c = QN (Just dfMod) c

-- * Auxiliary DFI functions

-- | Generates the signatures of all apply() functions used in a set of modules, 
--   as indicated in their defunctionalization interfaces.
dfiAppSigs :: DFI -> FuncSigs
dfiAppSigs dfi =
  let DfInfo _ dfAppArs = dfiDfInfo dfi
      appSig ar = (genNApp ar, (genCl ar):(L.map (genCla ar) [0..(ar-1)]))
      appSigs   = S.map appSig dfAppArs
  in  M.fromList $ S.toList appSigs

-- | Given a module's DFI, the module code and CAF/pattern-depth metadata, it
--   generates the final DFI for the module.
genModDFI :: DFI -> ProgF -> CAFDct -> PMDepths -> DFI
genModDFI dfi code@(Prog _ modDefs) cafInfo pmDepths =
  let DFI mg@(m, _) env _ dfInfo larInfo tcInfo = dfi
      env' = restrictVEnvToProg env code
      mainDep = M.lookup (mainDefQName m) pmDepths
      cids = liCIDs larInfo
      larInfo' = LARInfo cafInfo cids pmDepths mainDep
  in  DFI mg env' (getSigs modDefs) dfInfo larInfo' tcInfo

-- * Defunctionalization linker
     
-- | Generates the closure constructor data types and closure dispatchers from the
--   closure constructor information. Also takes a set of all closure
--   dispatchers needed, to cater for dead code.
-- 
--   Note about code correctness: the DFI of each module is contains all
--   closure constructors used in this module; therefore, all the DFIs contain
--   all closure constructors of the program, together with their types
--   (binding types and closure residual type).
--   Assuming that the program is closed and receives no closures from
--   anywhere else, these constructors are the only constructors that can be
--   consumed by defunctionalization's closure dispatchers. Therefore, it is
--   sufficient to create dispatchers just for these closure constructors.
-- 
genDfCode :: DfFlags -> DfConstrs -> ExtAppFuns -> ProgF
genDfCode flags dfcs extApps =
  Prog [mkClosureGADT flags dfcs] (mkApplyFuns flags dfcs extApps)

-- | Given a set of closure constructors, generates the pseudo-module of
--   defunctionalization. The function signatures table contains the
--   signatures of the external functions used, to generate correct 'import'
--   declarations.
genDfMod :: DfFlags -> DFI -> ModF
genDfMod flags (DFI _ env fsigs (DfInfo dfcs extApps) larInfo _) =
  let dfCode@(Prog _ appDefs) = genDfCode flags dfcs extApps
      dfExps = M.fromList $ L.map defSig appDefs
      -- returns all closure constructors of imported functions from m
      filtM m (DFC _ _ _ (QN (Just m') _, _)) = m==m'
      filtM _ (DFC _ _ _ (QN (Nothing) _, _)) = ierr "Unqualified DFC found."
      dfcsOf m = S.filter (filtM m) dfcs
      -- module names to import
      takeIModName (DFC _ _ _ (QN (Just m) _, _)) = m
      takeIModName (DFC _ _ _ (QN Nothing _, _))  =
        ierr "Unqualified import found."
      iModNames = S.map takeIModName dfcs
      -- all the imported names from module m
      allINames m = S.toList $ S.map (\(DFC _ _ _ (f, _))->f) (dfcsOf m)
      -- the imports have some dummy fields
      dummyIInfo f =
        let Just (fT, fArity) = M.lookup f env
            fPMDepth = M.lookup f (liPMDs larInfo)
        in  IInfo (Just fT) fArity NFunc (ierr "no caf") fPMDepth
      -- the imported functions table for module m
      iNames m = M.fromList $ L.map (\n->(n, dummyIInfo n)) (allINames m)
      -- the signatures of imported functions
      iSigs m = M.fromList $ L.map (\n->(n, paramsOf n fsigs)) (allINames m)
      -- the full 'import' declaration for module m (no CIDs are used, since no
      -- constructors are imported)
      iMod m = IDecl m (iNames m) (Just (iSigs m, M.empty))
      -- all imports
      dfImps = L.map iMod (S.toList iModNames)
  in  Mod (dfMod, modNoPath) dfExps dfImps dfCode M.empty (TcInfo [] [])

-- | Generates information to be used by later stages of compilation.
genDfInfo :: DfFlags -> [DefF] -> ProgInfo
genDfInfo (_, str, scr, _) defs =
  let defSigs = M.fromList $ L.map defSig defs
      appCBNs = genAppCBNs scr defs
  in  (defSigs, (appCBNs, genAppStricts str defs), genAppPMDepths scr defs)

-- | Same as 'genDfMod', but the resulting module is now preprocessed (e.g.
--   constructor calls have been converted to function calls).
genDfModFinal :: DfFlags -> DFI -> (ModF, ProgInfo)
genDfModFinal flags@(_, _, scr, tc) dfi =
  let dfModF = constrToFuncs (tc, scr) $ genDfMod flags dfi
      Prog _ defs = modProg dfModF
      -- -- do the -enum transformation in the generated code
      -- (dts', env') = error "(if canOptEnums opts then optEnumsKernel else id) (dts, env)"
      -- -- finalProgLAR' = Prog dts' blocks 
      -- dfModF' = dfModF{modProg=(Prog dts' defs)}
  in  (dfModF, genDfInfo flags defs)

-- | Generate call-by-name information for defunctionalization code. Its is 
--   empty for closure constructors, all formals for closure dispatchers
--   (unless formal scrutinees are used, in which case the closure argument of
--   the dispatcher is call-by-need).
genAppCBNs :: ScrutOpt -> [DefF] -> CBNVars
genAppCBNs scrOpt defs =
  let cbnDef (DefF f _ (ConstrF _ _)) = (f, [])
      cbnDef (DefF f fs@((Frm cl _):args) (CaseF _ (XF (V cl')) _ _)) | cl == cl' =
        if scrOpt then (f, frmsToNames args) else (f, frmsToNames fs)
      cbnDef def@(DefF {}) =
        ierr $ "cbnDef: strange defunctionalization code: "++(pprint def "")
  in  M.fromList $ L.map cbnDef defs
  
-- | Generate strictness information for defunctionalization code. If full
--   strictness is on, assume all formals are strict, else no formals are strict.
genAppStricts :: Strictness -> [DefF] -> Stricts
genAppStricts str defs =
  let onlyIfStrict (DefF f fs _) =
        if str then (f, [0..(length fs)-1]) else (f, [])
  in  M.fromList $ L.map onlyIfStrict defs
  
-- | Generates the pattern matching depth information for defunctionalization 
--   code. Dispatchers have depth 0 if formal scrutinees are used, otherwise 0.
--   Closure constructors have depth 0.
genAppPMDepths :: ScrutOpt -> [DefF] -> PMDepths
genAppPMDepths scrutOpt defs =
  let pmdDef (DefF f _ (ConstrF _ _)) = (f, 0)
      pmdDef (DefF f _ (CaseF _ (XF (V _)) _ _)) = (f, if scrutOpt then 0 else 1)
      pmdDef def@(DefF {}) = ierr $ "pmdDef: this doesn't look like defunctionalization code: "++(pprint def "")
  in  M.fromList $ L.map pmdDef defs

-- | Uses the list of closure constructors to generate the closure dispatching
--   functions needed by the final program. Takes a list of all closure
--   dispatchers mentioned in the original code, in order to also create
--   dummy ones for non-inhabited closure types (called by dead code).
mkApplyFuns :: DfFlags -> DfConstrs -> ExtAppFuns -> [DefF]
mkApplyFuns (ndf, strictness, scrOpt, _) dfcs extApps =
  let -- If doing nullary defunctionalization, keep only 0-arg closures.
      dfcs' = if ndf then S.filter (\dfc->(dfcA dfc)==0) dfcs else dfcs
      usedArities = clArities dfcs'
      -- Calculate the parameters given to the closure.
      nParams ar = L.map (genCla ar) [0..(ar-1)]
      appBody ar params pats =
        let -- Calculate the function formals.
            frmNames = (genCl ar):params
            frms     = L.map (\v->Frm v strictness) frmNames
            appFunc  = genNApp ar
            cn       = cNested scrOpt 0 appFunc
        in  DefF appFunc frms
            (CaseF cn (XF $ V $ genCl ar) underscoreVar pats)
      -- Create dispatchers for inhabited closure residual types.
      aux 0  = 
        ierr "mkApplyFuns: cannot create a dispatcher for 0 parameters"
      aux ar =
        let params = nParams ar
            -- find all closure constructors that take exactly 'ar' parameters
            dfcsF = S.filter (\(DFC _ _ t _)->order t==ar) dfcs'
            -- create full application branches
            appFull = mkApply scrOpt mkPatFull dfcsF params
            -- find all closure constructors that take more than 'ar' parameters
            dfcsP = S.filter (\(DFC _ _ t _)->order t>ar) dfcs'
            -- create partial application branches
            appPartial = mkApply scrOpt mkPatPartial dfcsP params
        in  if size (union dfcsF dfcsP) == 0 then
              Nothing             -- no closures of this arity, no function needed
            else
              Just (appBody ar params (appFull++appPartial))
      -- create dummy dispatchers for unused code
      missingApps = S.toList $ difference extApps usedArities
      missingAppDefs = L.map (\ar -> appBody ar (nParams ar) []) missingApps
  in  (mapMaybe aux $ S.toList usedArities) ++ missingAppDefs

-- | Generates an apply function for a given list of parameters. Takes the pattern 
--   branch generator (that does full application or closure construction), the 
--   closure constructors, and the formals of the dispatching function.
mkApply :: ScrutOpt -> PatGen -> DfConstrs -> [QName] -> [PatF]
mkApply _ _ _ [] =
  ierr "Cannot create closure dispatcher without parameters."
mkApply scrOpt mkPat dfcs frms = 
  let frmsLen  = length frms
      app      = genNApp frmsLen
  in  L.map (mkPat scrOpt app frms) (S.toList dfcs)

-- | The type of the generator of the dispatching function branch.
type PatGen = ScrutOpt -> QName -> [QName] -> DFC -> PatF

-- | Take a list of apply()-formals and a closure constructor and generate
--   a pattern branch for a closure dispatcher that calls a function.
mkPatFull :: PatGen
mkPatFull scrOpt app frms (DFC c ar _ (f, _)) =
  let bns = cArgsC c ar
      bvs = L.map (\v->XF $ BV v (cNested scrOpt 0 app)) bns
      pI  = PatInfo (ar/=0)
  in  PatB (SPat c bns, pI) (FF (V f) (bvs ++ (L.map (\v->XF $ V v) frms)))

-- | Take a list of apply()-formals and a closure constructor and generate
--   a pattern branch for a closure partial application function.
mkPatPartial :: PatGen
mkPatPartial scrOpt app frms (DFC c ar _ (f, _)) =
  let bns  = cArgsC c ar
      bvs  = L.map (\v->XF $ BV v (cNested scrOpt 0 app)) bns
      pI  = PatInfo (ar/=0)
  in  PatB (SPat c bns, pI) $
      ConstrF (genNC f (ar + length frms)) (bvs ++ (L.map (\v->XF $ V v) frms))

-- | Given an FL module and its DFI, generates the missing code of
--   defunctionalization and returns the FL program with all merged code.
linkF :: DfFlags -> ModF -> DFI -> ProgF
linkF flags modF dfi =
  let (modDF, _) = genDfModFinal flags dfi
  in  concatCode [modDF, modF]

-- * Typing with GADTs

-- | The GADT that contains the closure constructors of polymorphic
--   defunctionalization.
mkClosureGADT :: DfFlags -> DfConstrs -> Data
mkClosureGADT (ndf, s, _, _) dfcs =
  let mkGCConstr dfc@(DFC _ ar _ _) | ndf && ar>0 =
        error $ "Nullary defunctionalization failed, found closure constructor: "++
                (pprint dfc "")
      mkGCConstr (DFC c ar _ (_, t)) =
        let ts = types t
            comps = L.map (\t0->DT t0 s Nothing) $ take ar ts
            retT  =
              case drop ar ts of
                [retT'] -> retT'     -- result is ground
                retTs   ->           -- result is closure
                  let -- generate a tuple type for the arguments
                      argT =
                        if length retTs == 2 then    -- 1 arg, no tuple
                          head retTs
                        else                         -- many args, tuple
                          applyTs (init retTs) (mkTupleTg ((length retTs)-1))
                  in  Ta (Ta (Tg (T dfDT)) argT) (last retTs)
        in  DConstr c comps (Just retT)
      allConstrs = L.map mkGCConstr $ S.toList dfcs
  in  Data dfDT ["d$0", "d$1"] allConstrs

-- | Generate the types of the closure dispatching functions.
mkDfAppsTEnv :: ExtAppFuns -> TEnv
mkDfAppsTEnv es =
  let eApps = S.toList es
      resTv = Tv "$res"
      parTv i = Tv ("$par"++(show i))
      mkAppT n =
        let parTvs =
              case n of
                1 -> parTv (0::Int)
                _ -> applyTs (L.map parTv [0..n-1]) (mkTupleTg n)
            appType = Tf (Tf (Ta (Ta (Tg (T dfDT)) parTvs) resTv) parTvs) resTv
        in  (genNApp n, (appType, Just n))
  in  M.fromList $ L.map mkAppT eApps

-- | Apply the second type to a list of types.
applyTs :: [Type] -> Type -> Type
applyTs [] t1 = t1
applyTs (t0:ts0) t1 = applyTs ts0 (Ta t1 t0)
