-- | Transforms a ZOIL program to a program that can be executed by a dataflow
--   engine.
-- 

module SLIC.TTD.ZItoTTD (fromZOILtoTTD) where

import Data.Map (Map, elems, fromList, lookup)
import SLIC.AuxFun (ierr, threadfunc_l)
import SLIC.ITrans.Syntax
import SLIC.SyntaxAux
import SLIC.TTD.SyntaxTTD
import SLIC.Types

type NodeIDs = Map QName NodeID

-- | Translates an intensional program to a TTD program containing labelled
--   TTD instructions.
fromZOILtoTTD :: [DefZ] -> ProgT
fromZOILtoTTD defsZ =
  let -- Generate the node IDs corresponding to the defined variables.
      vIDs :: NodeIDs
      vIDs = fromList $ zip (map defVarZ defsZ) [0..]
      maximumVarID = maximum $ elems vIDs
      (entries, _) = threadfunc_l maximumVarID defsZ (transD vIDs)
  in  ProgT ((map fst entries)++(concatMap snd entries))

-- | Translate an intensional expression to a TTD expression containing node IDs.
--   Subexpressions are broken off as separate TTD instructions with their own
--   IDs. Takes the variable IDs table, the last used node ID and the expression
--   to translate.
--   Returns the new TTD instruction corresponding to the expression, any other
--   TTD sub-instructions generated, and the last used node ID.
transE :: NodeIDs -> NodeID -> ExprZ -> ((EntryT, [EntryT]), NodeID)
transE _ n (ConZ i@(LitInt _) []) =
  let n' = n+1
  in (((n', ConT i []), []), n')
transE vIDs n (ConZ cBinOp@(CN _) [e1, e2]) =
  let ((entry1, others1), n1) = transE vIDs n  e1
      ((entry2, others2), n2) = transE vIDs n1 e2
      n' = n2+1
  in  (((n', ConT cBinOp [n1, n2]), entry1:entry2:(others1++others2)), n')
transE _ _ (ConZ _ _) = ierr "TODO: fromZOILtoTTD: unknown built-in constant"
transE vIDs n (FZ qOp f) = 
  let n' = n+1
  in  case qOp of
        Call iidx -> (((n', CallT iidx (idOf vIDs f)), []), n')
        NOp       -> transE vIDs n (XZ (V f))
transE vIDs n (XZ (V qn)) =
  let n' = n+1
  in  (((n', VarT (idOf vIDs qn)), []), n')
transE _ _ e = ierr $ "The ZItoTTD translator does not understand: "++(pprint e "")
  
-- | Translate an intensional definition to a TTD instruction containing node IDs.
--   Takes the variable IDs table, the last used node ID and the definition to
--   translate.
--   Returns the new TTD instruction corresponding to the definition, any other
--   TTD sub-instructions generated, and the last used node ID.
transD :: NodeIDs -> NodeID -> DefZ -> ((EntryT, [EntryT]), NodeID)
transD vIDs n (DefZ qn eD) = 
  let ((entry, others), nD) = transE vIDs n eD
  in  (((idOf vIDs qn, snd entry), others), nD)
transD vIDs n (ActualsZ qn _ el) =
  let (elActs, nActs) = threadfunc_l n el (transE vIDs)
      entries :: [EntryT]
      entries = map fst elActs
      entriesIDs :: [NodeID]
      entriesIDs = map fst entries
      others :: [EntryT]
      others = concatMap snd elActs
      actT :: EntryT
      actT = (idOf vIDs qn, ActualsT entriesIDs)
  in  ((actT, entries++others), nActs)
      
-- | Returns the node ID assigned to one of the variables in the intensional      
--   program.
idOf :: NodeIDs -> QName -> NodeID
idOf vIDs qn =
  case Data.Map.lookup qn vIDs of
    Just nID -> nID
    Nothing  -> ierr $ "idOf: no node ID for "++(pprint qn "")
