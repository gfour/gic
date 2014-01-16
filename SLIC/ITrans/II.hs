-- | The intensional interfaces used for separate instensional transformation of
--   modules.
module SLIC.ITrans.II (updExtTypesII, parseII, parseIIs, writeIIForMod) where

import Data.Map (fromList, toList)
import SLIC.AuxFun (ierr)
import SLIC.Constants (iiSuffix, nl)
import SLIC.DFI (DfInfo)
import SLIC.ITrans.Syntax
import SLIC.State (Verb, opt)
import SLIC.SyntaxAux
import SLIC.Types

-- | The intensional interface of a module lists the types and signatures of
--   its exported functions and the compiled constructor ids.
data II = II TEnv FuncSigs DfInfo CIDs deriving (Read, Show)

instance PPrint II where
  pprint (II fts fsigs dfInfo cids) =
    ("--------------"++).nl.
    ("* Intensional interface:"++).nl.
    ("** Name table:"++).nl.
    pprintE fts.
    pprint dfInfo.
    ("** Function signatures:"++).nl.(pprFSigs fsigs).    
    ("** Compiled constructors:"++).nl.pprintCIDs cids.
    ("--------------"++).nl

-- | Writes the intensional interface file for a module.
writeIIForMod :: TEnv -> DfInfo -> ModZ -> IO ()
writeIIForMod mEnv dfInfo modZ =
  do let (m, _) = modNameF modZ
     let exports = modExports modZ
     let dts = progData $ modProg modZ
     writeFile (iiFile m) (show (II mEnv exports dfInfo (calcCIDs dts)))

-- | Parses an II file.
parseII :: FileName -> IO II
parseII iiF = 
  readFile iiF >>= \iiText ->
  case reads iiText :: [(II, String)] of
    [(ii, "")] -> return ii
    _ -> error $ "Failed to read intensional interface from file "++iiF

-- | Parses a list of files containing intensional interfaces.
parseIIs :: Verb -> [FileName] -> IO [II]
parseIIs _ []  = ierr "No module names were given."
parseIIs v [f] = do (opt v $ putStrLn $ "Reading: "++f)
                    ii <- parseII f
                    return [ii]
parseIIs v (f:fs) = do (opt v $ putStrLn $ "Reading: "++f)
                       ii  <- parseII  f
                       iis <- parseIIs v fs 
                       return (ii : iis)

{-
-- | Merges a set of IIs to a single II.
--   We assume that the types of functions and constructors in different IIs 
--   are not in conflict.
mergeIIs :: [II] -> II
mergeIIs dfis =
  let ve   = mergeEnvsI dfis
      dfcs = mergeDfcsI dfis
      sigs = mergeSigsI dfis
      apps = mergeExtAppArs dfis
      info = mergeLARInfo dfis
  in  II ve sigs (dfcs, apps) info
-}

-- | Returns the file name for a module's II file.
iiFile :: MName -> String
iiFile m = m++iiSuffix

-- | Updates an import declaration using the intensional interface file of the
--   imported module.
updExtTypesII :: [IDecl] -> IO [IDecl]
updExtTypesII _ = error "TODO: remove the II interfaces"
{-
updExtTypesII [] = return []
updExtTypesII (imp@(IDecl m _ _):is) =
  do rest <- updExtTypesII is
     if m == mControlParallel then
       return (importControlParallel : rest)
     else
       return (imp : rest)
updExtTypesII ((IDecl imn@(IMN m) _ Nothing):is) = 
  do (II mEnv fsigs _ cids) <- parseII $ iiFile m
     let iinfos ve = Data.Map.fromList $ map 
                     (\(v, (t, ar))-> (v, IInfo (Just t) ar (ierr "imported constructor status in II is missing") Nothing Nothing)) $
                     Data.Map.toList ve
     let import' = IDecl imn (iinfos mEnv) (Just (fsigs, cids))
     rest <- updExtTypesII is
     return (import' : rest)
updExtTypesII _ = ierr "updExtTypesII called with wrong argument"
-}
