-- | Generates the top-level C source that will link a set of modules.
-- 
--   The modules must have already been separately compiled and their
--   DFI interface files generated.
--   The linker will then read the DFI information and generate the
--   required closure constructors and /apply()/ dispatching functions
--   needed by the defunctionalized compiled code.
-- 
--   Some functions take a single DFI; this is the result of merging all
--   input module DFIs using 'mergeDFIs' from "SLIC.DFI".
-- 

module SLIC.LAR.LARLinker (compileWholeL, linkLAR) where

import Data.List (map)
import qualified Data.Map as Map (empty, fromList, insert, map, union)
import Data.Set as Set (toList)

import SLIC.AuxFun (foldDot, nameOf)
import SLIC.Constants
import SLIC.DFI
import SLIC.Front.Defunc
import SLIC.ITrans.HFtoHI (fromHFtoHI)
import SLIC.ITrans.HItoZI (fromHItoZI)
import SLIC.ITrans.ITrans (itransM)
import SLIC.ITrans.Optimizations (canOptEnums, optEnumsKernel)
import SLIC.LAR.ZItoLAR
import SLIC.LAR.LAR
import SLIC.LAR.LARAux
import SLIC.LAR.LARBuiltins
import SLIC.LAR.SMacrosAux
import SLIC.LAR.SyntaxLAR
import SLIC.State
import SLIC.SyntaxAux
import SLIC.Types

-- | Links a given program with its missing defunctionalization code. Used
--   in whole-program mode. Returns the extra usage information needed for
--   later stages of compilation. Takes the merged DFI of all
--   the DFIs to link.
linkWithDf :: DfFlags -> DFI -> ProgL -> (ModL, ProgInfo)
linkWithDf flags dfi p =
  let (modDF, pInfo) = genDfModLAR flags dfi
  in  (modDF{modProg = concatProgs [p, modProg modDF]}, pInfo)

-- | Generates defunctionalization's LAR module. Also returns information
--   required by later stages of compilation. Takes the merged DFI of all
--   the DFIs to link.
genDfModLAR :: DfFlags -> DFI -> (ModL, ProgInfo)
genDfModLAR flags dfi@(DFI _ _ sigs dfInfo _ _) =
  let dfCArities = map (\(DFC c ar _ _)->(c, ar)) $ toList $ diDfcs dfInfo
      (dfModuleF, progInfo@(defSigs, _, _)) = genDfModFinal flags dfi
      dfModuleZ  = fromHItoZI $ itransM $ fromHFtoHI dfModuleF
      cids = Map.fromList $ map (\((a, b), c)->(a, (b, c))) $ zip dfCArities [0..]
      sigs' = Map.union sigs defSigs
      dfModuleL = fromZOILtoLAR sigs' cids dfModuleZ
  in  (dfModuleL, progInfo)
  
-- | Generates the C code for defunctionalization's module. Takes the
--   user options and the merged defunctionalization interfaces.
makeDfModC :: Options -> DFI -> ShowS
makeDfModC opts dfi =
  let env = dfiTEnv dfi
      (mL, (_, (cbns, stricts), pmds)) = genDfModLAR (dfFlags opts) dfi
      is = modImports mL
      pL@(Prog dts _) = modProg mL
      config   = ConfigLAR { getCBNVars   = cbns
                           , getStricts   = stricts
                           , getCIDs      = calcCIDs dts
                           , getArities   = calcFuncArities mL
                           , getOptions   = opts{optCMode=CompileModule}
                           , getPMDepths  = pmds
                           , getCAFnmsids = []
                           , getModName   = dfMod
                           }
      imports = mergeINames is
  in  makeC pL env config (dfi, imports, Map.empty)

-- | Generates the linking C code.
makeCLinker :: Options -> [DFI] -> [MName] -> ShowS
makeCLinker opts dfis modNames =
  let env = mergeEnvs dfis
      extInitMod m = ("void "++).genInitMod m.("(TP_ T0);"++).nl
      mainDepth = getMainDepth dfis
      arities   = builtinArities
      modName   = "Main$Linker"
      mainDef   = mainDefQName "Main"
      pmDepths  = Map.insert mainDef mainDepth builtinPmDepths 
      cafs      = []
      arityCAF  = length cafs
      gc        = optGC opts
      compact   = optCompact opts
  in  headersC opts.
      macrosC opts modName arities pmDepths arityCAF.
      prologue opts modName arityCAF.
      (case gc of
          SemiGC ->
            createSemiGCARInfra modName gc compact arities pmDepths arityCAF
          LibGC  ->
            mkLARMacroOpt opts (qName mainDef) 0 0 mainDepth.nl).
      ("extern "++).protoFunc mainDef.
      foldDot extInitMod modNames.             -- linked module initializers
      declarationsBuiltins opts.
      mainFunc env opts mainDepth modNames.
      prettyPrintersC compact.nl.
      epilogue opts.nl

-- | Whole program compilation: generates the defunctionalization module,
--   links it with the current LAR code, and gives it to the LAR back-end.
compileWholeL :: DFI -> ProgL -> ConfigLAR -> ImportedNames -> IO ()
compileWholeL dfi finalProgLAR config allImports =
  let eLAR = dfiTEnv dfi
      opts = getOptions config
      (modFinal, (fsigs', (cbns', strs'), pmdepths')) =
        linkWithDf (dfFlags opts) dfi finalProgLAR
      Prog dts blocks = modProg modFinal
      -- update the configuration with defunctionalization's usage information
      cbns    = Map.union (getCBNVars config) cbns'
      stricts = Map.union (getStricts config) strs'
      pmds    = Map.union (getPMDepths config) pmdepths'
      ars     = Map.union (getArities config) (Map.map length fsigs')
      config' = config{getCBNVars = cbns}{getStricts = stricts}{getPMDepths=pmds}{getArities=ars}
      -- Do the enumeration transformation in the generated code.
      (dts', eLAR') = (if canOptEnums opts then optEnumsKernel else id) (dts, eLAR)
      finalProgLAR' = Prog dts' blocks 
  in  putOutput
      (makeC finalProgLAR' eLAR' config' (dfi, allImports, Map.empty) "")

-- | Prints a string to a file.
putOutput :: String -> IO ()
putOutput s = do
  let fName = "./main.c"
  putStrLn $ "Output written to " ++ fName
  writeFile fName s

-- | Links a list of separately compiled modules using the LAR back-end.
linkLAR :: [String] -> Options -> IO ()
linkLAR files opts =  
  do dfis <- parseDFIs (optVerbose opts) (Data.List.map dfiFile files)
      -- disable verbosity, or the LAR back-end will generate code for graphviz
     let opts' = opts{optVerbose = False}{optCMode=Whole}
     -- let cMain = makeCLinker opts' (Data.Map.fromList [(mainDefName, (tInt,Just 0))]) dfis modNames ""
     let dfModC = makeDfModC opts (mergeDFIs dfis) ""
     writeFile "dfmod.c" dfModC
     let modNames = map nameOf files
     let cMain = makeCLinker opts' dfis modNames ""
     writeFile "main-link.c" cMain  
