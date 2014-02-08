-- | Transforms a 0-order program to a program that can be executed by a
--   demand-driven dataflow engine.
-- 

module SLIC.TTD.ZItoTTD (InstrIDs, builtinInstrIDs, fromZOILtoTTD,
                         idOf, maxBuiltinInstrID) where

import Data.Map (Map, elems, fromList, keys, lookup, union)
import SLIC.AuxFun (ierr, threadfunc_l)
import SLIC.ITrans.Syntax
import SLIC.SyntaxAux
import SLIC.TTD.SyntaxTTD
import SLIC.Types

-- | A mapping from qualified names to instruction IDs, used to replace
--   variables with their instruction entry.
type InstrIDs = Map QName InstrID

-- | Translates an intensional program to a TTD program containing labelled
--   TTD instructions. Also returns the IDs assigned to the definitions.
fromZOILtoTTD :: [DefZ] -> (ProgT, InstrIDs)
fromZOILtoTTD defsZ =
  let -- Generate the instruction IDs corresponding to the defined variables.
      defIDs :: InstrIDs
      defIDs = fromList $ zip (map defVarZ defsZ) [(maxBuiltinInstrID+1)..]
      maximumVarID = maximum $ elems defIDs
      vIDs = union builtinInstrIDs defIDs
      (entries, _) = threadfunc_l maximumVarID defsZ (transD vIDs)
  in  (ProgT ((map fst entries)++(concatMap snd entries)), defIDs)

-- | Translate an intensional expression to an instruction entry. There is no
--   nesting: subexpressions of the original expression are broken off as
--   separate TTD instructions with their with their own IDs; these IDs are then
--   filled in the parent expression's "Plug" ports.
--   This function takes the variable IDs table, the last used instruction ID and 
--   the expression to translate. It returns the new TTD instruction corresponding
--   to the expression, any other TTD sub-instructions generated, and the last
--   used instruction ID.
transE :: InstrIDs -> InstrID -> ExprZ -> ((IEntry, [IEntry]), InstrID)
transE _ n (ConZ i@(LitInt _) []) =
  let n' = n+1
  in (((n', ConT i []), []), n')
transE vIDs n (ConZ cOp@(CN _) el) =
  let (topEntriesIDs, entries, n') = transL vIDs n el
      plugs = zip topEntriesIDs [0..]
      n'' = n' + 1
  in  (((n'', ConT cOp plugs), entries), n'')
transE _ _ (ConZ c _) = ierr $ "TODO: fromZOILtoTTD: unknown built-in constant"++(pprint c "")
transE vIDs n (FZ qOp f) = 
  let n' = n+1
  in  (((n', CallT qOp (idOf vIDs f, 0)), []), n')
transE vIDs n (XZ (V qn)) =
  let n' = n+1
  in  (((n', VarT (idOf vIDs qn, 0)), []), n')
transE _ _ e = ierr $ "The ZItoTTD translator does not understand: "++(pprint e "")
  
-- | Translate an intensional definition to an instruction entry. See "transE"
--   for details. This function takes the variable IDs table, the last used 
--   instruction ID, and the definition to translate.
--   Returns the new TTD instruction corresponding to the definition, any other
--   TTD sub-instructions generated, and the last used instruction ID.
transD :: InstrIDs -> InstrID -> DefZ -> ((IEntry, [IEntry]), InstrID)
transD vIDs n (DefZ qn eD) = 
  let ((entry, others), nD) = transE vIDs n eD
  in  (((idOf vIDs qn, snd entry), others), nD)
transD vIDs n (ActualsZ qn _ el) =
  let (topEntriesIDs, entries, n') = transL vIDs n el
      plugs = zip topEntriesIDs [0..]
  in  (((idOf vIDs qn, ActualsT plugs), entries), n')

-- | Translates a list of expressions. Returns the list of top-level IDs,
--   all the generated instruction entries, and the last used ID.
transL :: InstrIDs -> InstrID -> [ExprZ] -> ([InstrID], [IEntry], InstrID)
transL vIDs n el =
  let (el', n') = threadfunc_l n el (transE vIDs)
      topEntries = map fst el'
      topEntriesIDs = map fst topEntries
      others = concatMap snd el'
  in  (topEntriesIDs, topEntries++others, n')

-- | Returns the instruction ID assigned to one of the variables in the
--   intensional program.
idOf :: InstrIDs -> QName -> InstrID
idOf vIDs qn =
  case Data.Map.lookup qn vIDs of
    Just nID -> nID
    Nothing  -> ierr $ "idOf: no instruction ID for "++(pprint qn "")

-- | Returns the maximum instruction ID assigned to a built-in function.
maxBuiltinInstrID :: InstrID
maxBuiltinInstrID = maximum $ elems builtinInstrIDs

-- | Contains the instruction IDs of the built-in functions.
builtinInstrIDs :: InstrIDs
builtinInstrIDs = 
  let builtinNames = keys builtinFuncSigs -- ++ (concat $ elems builtinFuncSigs)
  in  fromList $ zip builtinNames [0..]
