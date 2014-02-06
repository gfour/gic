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
transE vIDs n (ConZ cOp@(CN _) el) =
  let (topEntriesIDs, entries, n') = transL vIDs n el
      n'' = n' + 1
  in  (((n'', ConT cOp topEntriesIDs), entries), n'')
transE _ _ (ConZ c _) = ierr $ "TODO: fromZOILtoTTD: unknown built-in constant"++(pprint c "")
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
  let (topEntriesIDs, entries, n') = transL vIDs n el
  in  (((idOf vIDs qn, ActualsT topEntriesIDs), entries), n')

-- | Translates a list of expressions. Returns the list of top-level IDs,
--   all the generated instruction entries, and the last used ID.
transL :: NodeIDs -> NodeID -> [ExprZ] -> ([NodeID], [EntryT], NodeID)
transL vIDs n el =
  let (el', n') = threadfunc_l n el (transE vIDs)
      topEntries = map fst el'
      topEntriesIDs = map fst topEntries
      others = concatMap snd el'
  in  (topEntriesIDs, topEntries++others, n')

-- | Returns the node ID assigned to one of the variables in the intensional      
--   program.
idOf :: NodeIDs -> QName -> NodeID
idOf vIDs qn =
  case Data.Map.lookup qn vIDs of
    Just nID -> nID
    Nothing  -> ierr $ "idOf: no node ID for "++(pprint qn "")
