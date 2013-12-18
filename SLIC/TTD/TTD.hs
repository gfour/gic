-- | The top-level module of the TTD back-end.

module SLIC.TTD.TTD (callTTDBackend) where

import Data.List (sort)
import SLIC.AuxFun (ierr)
import SLIC.ITrans.Syntax
import SLIC.State
import SLIC.TTD.DFG
import SLIC.TTD.SyntaxTTD
import SLIC.Types

-- | The entry point to the TTD back-end.
callTTDBackend :: ProgZ -> Options -> IO ()
callTTDBackend p opts =
  let pMergedOps  = mergeOpsZI p
      pTTD        = fromZItoTTD (fst pMergedOps)
      pTTDOps     = snd pMergedOps
      pTTDPorts   = enumPorts pTTD pTTDOps
      -- let pTTDSeq   = enumPorts $ compact $ sequenceProg pTTD
  in  case optAction opts of
        ACompileTTD ->
          let ttdCode = error "TODO: makeTTD pTTD"
          in  putStrLn (ttdCode "")
        APrintTTD ->
          let -- file = "./test.dot"
              putOps :: [MOpDef] -> IO ()
              putOps [] = putStrLn ""
              putOps ((opN, op):ops) = putStrLn ("Operator "++(qName opN)++": "++(pprint op "")) >> putOps ops
          in  putStrLn "== ZOIL program with merged ops ==" >>
              putStrLn (pprint (fst pMergedOps) "") >>
              putOps (sort (snd pMergedOps)) >>
              putStrLn "== Tagged-Token Dataflow ==" >>
              putStrLn (pprint pTTDPorts "")
              -- putStrLn "== Sequenced Dataflow ==" >>
              -- putStrLn (show pTTDSeq) >>
              -- putStrLn "== Flow fragments ==" >>
              -- putStrLn (fragments pTTDSeq "") >>
              -- putStr   "Dataflow Graph saved in: " >>
              -- putStrLn file >>
              -- writeFile file ((dotGraph pTTDPorts) "")                  
              -- writeFile file ((dotGraph $ analyzeTTD pTTDPorts) "")
        AGenerateDFG ->
          let file = "./dfg.dot"
          in  putStr (pprint pTTDPorts "") >>
              -- return () -- 
              writeFile file (generateDFG pTTDPorts "")
        a -> ierr $ "The TTD back-end cannot handle action "++(show a)
