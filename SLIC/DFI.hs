-- | The defunctionalization interfaces needed to combine defunctionalization with
--   support for partial compilation.
-- 
--   DFI serialization is done with auto-deriving Read/Show for the required
--   data types.
-- 

module SLIC.DFI (DfConstrs, DFI(..), DfInfo(..), DFC(..), ExtAppFuns,
                 LARInfo(..), addApp, calcExtDFInfo, dfiFile, dfiFor,
                 dfiSuffix, emptyDfInfo, getMainDepth, mergeDFIs, mergeLARInfo,
                 mergeDfInfo, mergeEnvs, mergeExtAppArs, mergeSigs, noApps,
                 parseDFI, parseDFIs, restrictVEnvToProg, updExtTypesDFI,
                 updPMDepths) where

import Data.Char (isUpper)
import Data.List as List (map)
import Data.Map (Map, fromList, lookup, member, toList, unions)
import Data.Maybe (mapMaybe)
import Data.Set (Set, empty, insert, toList, unions)

import SLIC.AuxFun (errM, foldDot, ierr, nameOf, showStrings)
import SLIC.Constants (dfiSuffix, dirSeparator, nl)
import SLIC.Front.Preprocessor (projCName)
import SLIC.State (Verb, opt)
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- * Defunctionalization interfaces

-- | A closure constructor.
data DFC       = DFC { dfcN :: QName         -- ^ closure constructor name
                     , dfcA :: Arity         -- ^ binding arity
                     , dfcT :: Type          -- ^ type
                     , dfcF :: (QName, Type) -- ^ original function (and its type)
                     }
               deriving (Eq, Ord, Read, Show)

instance PPrint DFC where
  pprint (DFC c a t v) =
    pprint c.(":"++).shows a.("::"++).pprint t.(" of "++).pprint v.nl

-- | A set of closure constructors.
type DfConstrs = Set DFC

-- | A list of external closure dispatching functions needed by the module.
--   Only the required arities are stored.
type ExtAppFuns = Set Arity

pprintExtAppFuns :: ExtAppFuns -> ShowS
pprintExtAppFuns extAppFuns =
  let funs = Data.Set.toList extAppFuns
  in  (case funs of [] -> ("none"++) ; _  -> shows funs).nl

-- * State used by defunctionalization

-- | The information collected during defunctionalization.
data DfInfo = DfInfo { diDfcs :: DfConstrs, diEApps :: ExtAppFuns }
              deriving (Read, Show)

instance PPrint DfInfo where
  pprint (DfInfo dfcs extApps) =
    ("** Closure constructors:"++).nl.
    foldDot pprint (Data.Set.toList dfcs).
    ("** Uses closure dispatchers of arities: "++).pprintExtAppFuns extApps

-- | Empty set of closure constructors.
noDfcs :: DfConstrs
noDfcs = Data.Set.empty

-- | Empty set of used closure dispatchers.
noApps :: ExtAppFuns
noApps = Data.Set.empty

-- | Empty defunctionalization information, to be used as initial state.
emptyDfInfo :: DfInfo
emptyDfInfo = DfInfo noDfcs noApps

-- | Merges a number of defunctionalization information structures to a single one.
mergeDfInfo :: [DfInfo] -> DfInfo
mergeDfInfo info =
  let dfcs    = map diDfcs info
      extApps = map diEApps info
  in  DfInfo (Data.Set.unions dfcs) (Data.Set.unions extApps)

-- | Add the closure dispatcher of some arity to the set of used dispatchers
--   in a defunctionalizaton information structure.
addApp :: Arity -> DfInfo -> DfInfo
addApp i dfInfo = dfInfo{diEApps=(Data.Set.insert i (diEApps dfInfo))}

-- * Defunctionalization interfaces

-- | LAR-specific information for separate module compilation.
data LARInfo =
  LARInfo { liCAFD :: CAFDct     -- ^ indexes of CAF tables
          , liCIDs :: CIDs       -- ^ compiled constructors
          , liPMDs :: PMDepths   -- ^ depths of exported functions
          , liDepth :: Depth -- ^ depth of main function (if defined in the module)
          }
  deriving (Read, Show)

instance PPrint LARInfo where
  pprint (LARInfo cafs cids pmDepths rdep) =
    ("** CAFs:"++).nl.pprintCAFs cafs.
    ("** Compiled constructors:"++).nl.pprintCIDs cids.
    ("** Pattern matching depths:"++).nl.pprintPD pmDepths.
    (case rdep of
        Nothing -> id
        Just d  ->
          ("** Defines ["++).(mainDefName++).("] of depth: "++).
          shows d.nl)

-- | Finds the depth of the main function in a set of DFIs. Returns 0 if
--   nothing is found.
getMainDepth :: [DFI] -> PMDepth
getMainDepth dfis =
  case liDepth (mergeLARInfo dfis) of
    Just d  -> d
    Nothing -> 0

-- | A node in the module graph (module name, imported module names).
type MGInfo = (MName, [MName])

noMGInfo :: MGInfo
noMGInfo = error "No module graph information."

-- | The interface of a defunctionalized module, contains: a typed symbol table,
--   the closure constructors that may be used, the function signatures, the
--   closure dispatchers that may be used, information about the module CAFs 
--   and function nestings, and type class information.
data DFI  = DFI { dfiMInfo :: MGInfo     -- ^ module graph information
                , dfiTEnv :: TEnv        -- ^ typed exported symbol table
                , dfiSigs :: FuncSigs    -- ^ exported function signatures
                , dfiDfInfo :: DfInfo    -- ^ defunctionalization information
                , dfiLARInfo :: LARInfo  -- ^ LAR-specific information
                , dfiTcInfo :: TcInfo    -- ^ type class information
                }
          deriving (Read, Show)

instance PPrint DFI where
  pprint (DFI (m, ms) fts fsigs dfInfo larInfo tcInfo) =
    ("--------------"++).nl.
    ("* Defunctionalization interface of module "++).(m++).nl.
    ("** Imports modules: "++).
    (case ms of [] -> ("-"++) ; _ -> showStrings ", " ms).nl.
    ("** Name table:"++).nl.
    pprintE fts.
    ("** Function signatures:"++).nl.(pprFSigs fsigs).
    pprint dfInfo.
    pprint larInfo.
    pprint tcInfo.
    ("--------------"++).nl

{-
-- | Show the gathered defunctionalized constructors (grouped by arity).
pprintDfcs :: TEnv -> DfConstrs -> ShowS
pprintDfcs ve dfcs =
  let showCC (DFC c  0 _ _) = pprint c
      showCC (DFC c ar _ f) = pprint c.(" "++).pprintList space (take ar $ typs f)
      typs :: QName -> [Type]
      typs f = List.map (\(t, _)->defuncT t) (getTypesOf f ve)
      aux :: Arity -> ShowS
      aux n = 
        let dfcs' = Set.filter (\(DFC _ _ t' _)->n==order t') dfcs
        in  (foldDot (\cc->("       | "++).showCC cc.nl) $ Set.toList dfcs')
  in  ("data "++).((qName dfDT)++).(" = "++).nl.
      foldDot aux (toList $ clArities dfcs)
-}

-- * DFI reader

-- | Returns the file name for a module's DFI file.
dfiFile :: MName -> String        
dfiFile m = m++dfiSuffix

-- | Parses a DFI file.
parseDFI :: FileName -> IO DFI
parseDFI dfiF = 
  (if isUpper ((nameOf dfiF) !! 0) then 
     return ()
   else error $ "DFI file name not a module name: "++( nameOf dfiF)) >>
  readFile dfiF >>= \dfiText ->
  case reads dfiText :: [(DFI, String)] of
    [(dfi, "")] -> return dfi
    _ -> error $ "Failed to read defunctionalization interface from file "++dfiF

-- | Parses a list of files containing defunctionalization interfaces.
parseDFIs :: Verb -> [FileName] -> IO [DFI]
parseDFIs _ []  = ierr "No module names were given."
parseDFIs v [f] = do (opt v $ putStrLn $ "Reading: "++f)
                     dfi <- parseDFI f
                     return [dfi]
parseDFIs v (f:fs) = do (opt v $ putStrLn $ "Reading: "++f)
                        dfi  <- parseDFI  f
                        dfis <- parseDFIs v fs 
                        return (dfi : dfis)

-- | Updates an import declaration using the defunctionalization interface file
--   of the imported module.
updExtTypesDFI :: Verb -> MNameF -> FPath -> [DFI] -> [IDecl] -> IO [IDecl]
updExtTypesDFI _ _ _ _ [] = return []
-- For import declarations that have Empty information, load it.
updExtTypesDFI v mThis fPath dfis ((IDecl mn imns Nothing):imps) =
  do rest <- updExtTypesDFI v mThis fPath dfis imps
     (case Data.Map.lookup mn builtinModules of
         -- If built-in module, use the built-in information.
         Just builtinIDecl -> return (builtinIDecl : rest)
         -- Otherwise, load the DFI file.
         Nothing ->
           do (case filter (\d->(fst.dfiMInfo) d==mn) dfis of
                  [] -> return ()
                  (dfi':_) -> putStrLn "Found an interface..."
                )
              let f = dfiFor fPath mn
              (opt v $ putStrLn $ "Reading: "++f)
              (DFI _ fTypes fSigs _ larInfo _) <- parseDFI f
              let mn' = concatMap (updIInfo mThis larInfo fSigs mn fTypes)
                        (Data.Map.toList imns)
              let cids = liCIDs larInfo
              let import' = IDecl mn (Data.Map.fromList mn') (Just (fSigs, cids))
              return (import' : rest))
-- Don't touch import declarations that have filled-in information.
updExtTypesDFI v mThis fPath dfis (i:is) = 
  do rest <- updExtTypesDFI v mThis fPath dfis is
     return (i : rest)

dfiFor :: FPath -> MName -> FileName
dfiFor fPath m = fPath++[dirSeparator]++(dfiFile m)

-- | Update the CAF/nesting/arity interface information of an imported function.
--   If it is an imported constructor, also import its projection functions.
updIInfo :: MNameF -> LARInfo -> FuncSigs -> MName -> TEnv -> (QName, IInfo) -> [(QName, IInfo)]
-- data type names are left unchanged
updIInfo _ _ _ _ _ iinfo@(_, IInfo _ _ NDType _ _) = [iinfo]
updIInfo mThis lInfo sigs m ve (qn, IInfo Nothing Nothing ni Nothing Nothing) = 
  case Data.Map.lookup qn ve of 
    Just (t, Just ar) ->
      case Data.Map.lookup qn sigs of
        Just frms ->
          if length frms == ar then
            let cafs = liCAFD lInfo
                nestings = liPMDs lInfo    
                vInfo = (qn, IInfo (Just t) (Just ar) ni (Prelude.lookup qn cafs) (Data.Map.lookup qn nestings))
                constrFuncs = map (projC_i qn t) [0..(ar-1)]
            in  if (ni == NConstr && ar > 0) then 
                  vInfo : constrFuncs
                else
                  [vInfo]
          else
            errM mThis $ "Different arities in the environment and in the import for "++(qName qn)
        Nothing -> errM mThis $ "The interface of "++m++" does not define the formals of "++(qName qn)
    Just (_, Nothing) -> errM mThis $ "No arity for "++(qName qn)
    Nothing ->
      errM mThis $ "The interface of "++m++" does not define "++(qName qn)
updIInfo mThis _ _ _ _ (qn, IInfo (Just _) _ _ _ _) =
  errM mThis $ "external type is already updated in module "++(show mThis)++" for "++(qName qn)
updIInfo mThis _ _ _ _ (qn, IInfo Nothing (Just _) _ _ _) =
  errM mThis $ "external arity is already updated in module "++(show mThis)++" for "++(qName qn)
updIInfo mThis _ _ _ _ (qn, IInfo Nothing Nothing _ (Just _) _) =
  errM mThis $ "external caf idx is already updated in module "++(show mThis)++" for "++(qName qn)
updIInfo mThis _ _ _ _ (qn, IInfo Nothing Nothing _ Nothing (Just _)) =
  errM mThis $ "external nesting is already updated in module "++(show mThis)++" for "++(qName qn)

-- | Given a constructor name/type, generates the part of the 'import' declaration 
--   that names its i-th projection function.
projC_i :: CstrName -> Type -> Int -> (QName, IInfo)
projC_i constr cT i =
  let qn = procLName (\n -> projCName n i) constr
      t = Tf (takeRes cT) ((takeParams cT) !! i)
  in  (qn, IInfo (Just t) (Just 1) NFunc Nothing (Just 1))

-- * DFI helper functions

-- | Merges a set of DFIs to a single DFI.
--   We assume that the types of functions and constructors in different DFIs 
--   are not in conflict.
mergeDFIs :: [DFI] -> DFI
mergeDFIs dfis =
  let ve     = mergeEnvs dfis
      dfcs   = mergeDfcs dfis
      sigs   = mergeSigs dfis
      apps   = mergeExtAppArs dfis
      info   = mergeLARInfo dfis
      tcInfo = mergeTcInfo dfis
  in  DFI noMGInfo ve sigs (DfInfo dfcs apps) info tcInfo

-- | Returns the union of all typing environments from a list of DFIs.
mergeEnvs :: [DFI] -> TEnv
mergeEnvs dfis = Data.Map.unions $ List.map dfiTEnv dfis

-- | Returns the union of all function signatures from a list of DFIs.
mergeSigs :: [DFI] -> FuncSigs
mergeSigs dfis = Data.Map.unions $ List.map dfiSigs dfis

-- | Returns the union of all closure constructors from a list of DFIs.
mergeDfcs :: [DFI] -> DfConstrs
mergeDfcs dfis = Data.Set.unions $ List.map (diDfcs.dfiDfInfo) dfis

-- | Gathers all external apply() arities appearing in a list of DFIs.
mergeExtAppArs :: [DFI] -> ExtAppFuns
mergeExtAppArs dfis = Data.Set.unions $ List.map (diEApps.dfiDfInfo) dfis

-- | Merges all LAR info appearing in a list of DFIs.
mergeLARInfo :: [DFI] -> LARInfo
mergeLARInfo dfis =
  let cafsL = List.map (liCAFD.dfiLARInfo) dfis
      cidsL = List.map (liCIDs.dfiLARInfo) dfis
      pmdsL = List.map (liPMDs.dfiLARInfo) dfis
      rDep  =
        case mapMaybe (liDepth.dfiLARInfo) dfis of
          []  -> Nothing
          [r] -> Just r
          (_:_) -> error $ "More than one DFIs define a depth for "++mainDefName
  in  LARInfo (concat cafsL) (Data.Map.unions cidsL) (Data.Map.unions pmdsL) rDep

mergeTcInfo :: [DFI] -> TcInfo
mergeTcInfo dfis = mergeTcInfos $ List.map dfiTcInfo dfis
  
-- | Returns the arities of all closure constructor functions and closure
--   dispatchers generated by defunctionalization.
calcExtDFInfo :: ProgF -> (Map CstrName Arity, Map QName Arity)
calcExtDFInfo (Prog [Data _ _ dcs] defs) =
  let clcs           = fromList $ List.map 
                       (\(DConstr c dts _) -> (c, length dts)) dcs
      isNotCC (f, _) = not $ member f clcs 
      apps = fromList $ filter isNotCC $ List.map (\(DefF f vs _) -> (f, length vs)) defs
  in  (clcs, apps)
calcExtDFInfo p = ierr $ "wrong generated defunctionalized code: "++(pprint p "")

{-
-- | A fake DFI for modules that use the Prelude.
preludeDFI :: DFI
preludeDFI =
  let dfcs   = Data.Set.empty
      cids   = Data.Map.empty
      eApps  = Data.Set.empty
      larInfo= ([], cids, builtinPmDepths, Nothing)
  in  DFI builtinTEnv builtinFuncSigs (dfcs, eApps) larInfo
-}

-- | Updates the pattern-matching depths field in a DFI, given its FL module.
updPMDepths :: DFI -> ModF -> DFI
updPMDepths dfi modF =
  let pmds = countPMDepths $ progDefs $ modProg modF
      (mn, _) = modNameF modF
      rDep = Data.Map.lookup (mainDefQName mn) pmds
      newLARInfo = (dfiLARInfo dfi){liPMDs=pmds}{liDepth=rDep}
  in  dfi{dfiLARInfo=newLARInfo}
