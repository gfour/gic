-- | The top-level module of the TTD back-end.

module SLIC.TTD.TTD (callTTDBackend) where

import SLIC.AuxFun (ierr)
import SLIC.DFI (DFI)
import SLIC.Front.Defunc (dfFlags)
import SLIC.ITrans.Syntax
import SLIC.ITrans.ZLinker (mergeAndLinkZ)
import SLIC.State
import SLIC.SyntaxAux (Prog(..), Mod(..))
import SLIC.TTD.DFG (generateDFG)
import SLIC.TTD.EvalTTD (evalTTD)
import SLIC.TTD.ZItoTTD (fromZOILtoTTD, idOf)
import SLIC.Types (mainDefQName, pprint)

-- | The entry point to the TTD back-end.
callTTDBackend :: Options -> DFI -> ModZ -> IO ()
callTTDBackend opts dfi m =
  let mn = fst $ modNameF m
      Prog _ defsZ = mergeAndLinkZ (dfFlags opts) dfi [m]
      (pTTD, defIDs) = fromZOILtoTTD defsZ
      resultID = idOf defIDs (mainDefQName mn)
  in  case optAction opts of
        APrintTTD ->
          putStrLn "== Dataflow program ==" >>
          putStrLn (pprint pTTD "")
        AGenerateDFG ->
          let file = "./dfg.dot"
          in  putStrLn ("Writing graph to file: "++file) >>
              writeFile file (generateDFG (True, False) defIDs resultID pTTD "")
        AEvalTTD ->
          if optLink opts then
            error "The TTD emulator does not support linking."
          else
            case optCMode opts of
              CompileModule ->
                error "The TTD emulator does not support separate compilation."
              Whole -> evalTTD dfi (optNWorkers opts) resultID pTTD
        a -> ierr $ "The TTD back-end cannot handle action "++(show a)
