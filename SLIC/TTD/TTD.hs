-- | The top-level module of the TTD back-end.

module SLIC.TTD.TTD (callTTDBackend) where

import SLIC.AuxFun (ierr)
import SLIC.Constants (dfMod)
import SLIC.ITrans.Syntax
import SLIC.State
import SLIC.SyntaxAux (Prog(..), Mod(..))
import SLIC.TTD.DFG (generateDFG)
import SLIC.TTD.EvalTTD (evalTTD)
import SLIC.TTD.ZItoTTD (fromZOILtoTTD, idOf)
import SLIC.Types (mainDefQName, mName, pprint)

-- | The entry point to the TTD back-end.
callTTDBackend :: ModZ -> Options -> IO ()
callTTDBackend m opts =
  let mn = fst $ modNameF m
      Prog _ defsZ = modProg m
      (pTTD, defIDs) = fromZOILtoTTD (filter isLocal defsZ)
      isLocal def =
        let Just mn' = mName $ defVarZ def
        in  (mn'==mn) || (mn'==dfMod)
  in  case optAction opts of
        APrintTTD ->
          putStrLn "== Dataflow program ==" >>
          putStrLn (pprint pTTD "")
        AGenerateDFG ->
          let file = "./dfg.dot"
          in  putStrLn ("Writing graph to file: "++file) >>
              writeFile file (generateDFG defIDs pTTD "")
        AEvalTTD ->
          let resultID = idOf defIDs (mainDefQName mn)
          in  evalTTD resultID pTTD
        a -> ierr $ "The TTD back-end cannot handle action "++(show a)
