-- | The top-level module of the TTD back-end.

module SLIC.TTD.TTD (callTTDBackend) where

import SLIC.AuxFun (ierr)
import SLIC.Constants (dfMod)
import SLIC.ITrans.Syntax
import SLIC.State
import SLIC.SyntaxAux (Prog(..), Mod(..))
import SLIC.TTD.DFG (generateDFG)
import SLIC.TTD.ZItoTTD (fromZOILtoTTD)
import SLIC.Types (mName, pprint)

-- | The entry point to the TTD back-end.
callTTDBackend :: ModZ -> Options -> IO ()
callTTDBackend m opts =
  let mn = fst $ modNameF m
      Prog _ defsZ = modProg m
      pTTD = fromZOILtoTTD (filter isLocal defsZ)
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
              writeFile file (generateDFG pTTD "")
        ACompileTTD ->
          let ttdCode = error "TODO: makeTTD pTTD"
          in  putStrLn (ttdCode "")
        a -> ierr $ "The TTD back-end cannot handle action "++(show a)
