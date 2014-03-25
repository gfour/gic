-- | The compilation manager.
-- 

module SLIC.CompManager (ModFPre, contProcFL, prepareFL, rearrangeMods,
                         readMGraphForL) where

import Data.Graph (SCC(..), stronglyConnComp)
import Data.List ((\\))
import Data.Map (Map, fromList, insert, keys, lookup, member, singleton, 
                 toList, union, unions)
import Data.Maybe (mapMaybe)
import SLIC.AuxFun (pathOf, showStrings)
import SLIC.DFI
import SLIC.Driver (processFL)
import SLIC.Front.Preprocessor (qual)
import SLIC.Front.Typeclass
import SLIC.State
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- * Module dependencies

-- | A module graph node is a module importing other modules.
type MGNodes = Map MName [MName]

-- | Given a module, generates the full graph of all imported modules.
createMGraph :: (MNameF, [IDecl]) -> IO MGNodes
createMGraph ((mn, file), imports) =
  do let imods = filterRealMods imports
     readMGraphForL (mn, pathOf file) imods (singleton mn imods)

-- | For a given module/file, read the interface of some other module and
--   proceed recursively to the modules imported there. In the end, return
--   the list of all visited modules. The last argument is the accumulator
--   of the visited modules.
readMGraphFor :: (MName, FPath) -> MName -> MGNodes -> IO MGNodes
readMGraphFor si@(startM, fPath) m visited =
  if m==startM then
    error $ "Cyclic dependency found while following the imports of module "++m
  else
    do dfi <- parseDFI $ dfiFor fPath m
       let (_, ms) = dfiMInfo dfi
       readMGraphForL si (ms \\ (keys visited)) (Data.Map.insert m ms visited)

-- | Version of 'readMGraphFor' for module lists.
readMGraphForL :: (MName, FPath) -> [MName] -> MGNodes -> IO MGNodes
readMGraphForL _ [] visited  = return visited
readMGraphForL si (m:ms) visited =
  if m `member` visited then
    readMGraphForL si ms visited
  else
    do visited1 <- readMGraphFor si m visited
       visited2 <- readMGraphForL si ms visited1
       return $ union visited1 visited2

findCompOrder :: [ModFPre] -> IO [MName]
findCompOrder modsFL =
  let getMName (AcyclicSCC mn) = mn
      getMName (CyclicSCC mns) =
        error $ "Modules form cycle: "++(showList mns "")
  in  do mGraphs <- mapM createMGraph $
                    map (\(m, _)->(modNameF m, modImports m)) modsFL
         let mGraph = Data.Map.toList $ Data.Map.unions mGraphs
         let topolSorted = stronglyConnComp $ map (\(m, ms)->(m, m, ms)) mGraph
         return $ map getMName topolSorted
         
type ModFPre = (ModF, [TcInstF])

-- | Analyzes the dependencies needed to build a set of modules. Returns the
--   modules in the order that they should be built, together with the full
--   module list (which may contain names of external modules) in topologically
--   sorted order.
rearrangeMods :: Bool -> [ModFPre] -> IO ([ModFPre], [MName])
rearrangeMods showMsg modsFL =
  let modTable = fromList $ map (\modFL->((fst.modNameF.fst) modFL, modFL)) modsFL
  in  do mNames <- findCompOrder modsFL
         (if showMsg then
            putStrLn $ "Module graph: "++(showStrings ", " mNames "")
          else
            return ())
         return (mapMaybe (`Data.Map.lookup` modTable) mNames, mNames)

-- * Compilation manager

-- | Processes each parsed input module: it prepares it using 'prepareFL', then
--   it feeds it to the driver ("SLIC.Driver") and finally uses the generated
--   DFI to continue (if more than one files were given).
contProcFL :: Options -> FPath -> [ModFPre] -> [DFI] -> IO ()
contProcFL opts path (modF:modsF) dfis =
  do modF' <- prepareFL (optVerbose opts) path dfis modF
     mDfi  <- processFL opts dfis modF'
     case mDfi of
       Just dfi -> contProcFL opts path modsF (dfi:dfis)
       Nothing -> return ()
contProcFL _ _ [] _ = return ()

-- | Prepares FL modules after parsing: reads interface files to fill in
--   imported symbol information, does name qualification and adds the
--   type class instance methods as top-level definitions.
prepareFL :: Bool -> FPath -> [DFI] -> ModFPre -> IO ModF
prepareFL verb path dfis (modF, tcInsts) =
  let mThis = modNameF modF
      imports = modImports modF
  in  do -- read interface files to update imported symbol information
         imports' <- updExtTypes verb mThis path dfis imports
         let modFI = modF{modImports=imports'}
         -- qualify all names
         let (modFQ, tcInstsQ) = qual (modFI, tcInsts)
         -- add the type class instance methods defined in the code
         let modFTcI = addTcInsts tcInstsQ modFQ
         return modFTcI

-- | Read referenced interfaces to type the imported functions and find the
--   imported names information.
updExtTypes :: Verb -> MNameF -> FPath -> [DFI] -> [IDecl] -> 
               IO [IDecl]
updExtTypes _ _ _ _ [] = return []
updExtTypes v mThis fPath dfis idecl = updExtTypesDFI v mThis fPath dfis idecl
