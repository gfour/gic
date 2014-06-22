-- | Transforms a 0-order program to a program that can be executed by a
--   demand-driven dataflow engine.
-- 

module SLIC.TTD.ZItoTTD (InstrIDs, VarIDs, builtinInstrIDs, builtinVarIDs,
                         fromZOILtoTTD, idOf, maxBuiltinInstrID, mkVarIDs) where

import Data.List (nub)
import Data.Map (Map, elems, filterWithKey, fromList, keys, lookup,
                 mapWithKey, notMember, toList, union)
import Data.Tuple (swap)
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
  let -- Filter out the actuals of built-in formals.
      -- The call to 'nub' is needed to handle multiple "actuals" definitions.
      newDefNames =
        filter (\qn->notMember qn builtinInstrIDs) $ nub $ map defVarZ defsZ
      -- Generate the instruction IDs corresponding to the defined variables.
      defIDs = fromList $ zip newDefNames [(maxBuiltinInstrID+1)..]
      maximumVarID = maximum $ elems defIDs
      vIDs = union builtinInstrIDs defIDs
      FProg fDefsZ aDefsZ_used = fProg defsZ
      (fEntries, maximumFuncID) =
        threadfunc_l maximumVarID (toList fDefsZ) (transDef vIDs)
      (aEntries, _) =
        let actNames  = nub $ map fst $ keys aDefsZ_used
            actsOf v  = filterWithKey (\(k, _) _->k==v) aDefsZ_used
            actuals v = map snd $ toList $ 
                        mapWithKey (\(_, iidx) e->(iidx, e)) $ actsOf v
            allActs = [ (act, actuals act) | act<-actNames ] 
        in  threadfunc_l maximumFuncID allActs (transAct vIDs)
      entries = fEntries ++ aEntries
  in  (ProgT ((map fst entries)++(concatMap snd entries)), defIDs)

-- | Translate an intensional expression to an instruction entry. There is no
--   nesting: subexpressions of the original expression are broken off as
--   separate TTD instructions with their with their own IDs; these IDs are then
--   filled in the parent expression's depending instructions' ports.
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
      n'' = n' + 1
  in  (((n'', ConT cOp topEntriesIDs), entries), n'')
transE _ _ (ConZ c _) = ierr $ "TODO: fromZOILtoTTD: unknown built-in constant"++(pprint c "")
transE vIDs n (FZ qOp f _) = 
  let n' = n+1
  in  (((n', CallT qOp (idOf vIDs f)), []), n')
transE vIDs n (XZ var) =
  let n' = n+1
      varT = case var of
               V qn     -> VarT (idOf vIDs qn)
               BV qn cl -> BVarT (idOf vIDs qn) cl
  in  (((n', varT), []), n')
transE vIDs n (CaseZ loc e patsZ) =
  let patE (PatB _ eP) = eP
      patC (PatB (cP, _) _) = cP
      ((eID:patIDs), entries, n') = transL vIDs n (e:(map patE patsZ))
      n'' = n' + 1
      patsT = map (\(cP, eP)->PatT cP eP) $ zip (map patC patsZ) patIDs
  in (((n'', CaseT loc eID patsT), entries), n'')  
transE _ n (ConstrZ c) =
  let n' = n+1
  in  (((n', ConstrT c), []), n')
  
-- | Translate an intensional definition to an instruction entry. See "transE"
--   for details. This function takes the instruction IDs table, the last used 
--   instruction ID, and the definition to translate.
--   Returns the new TTD instruction corresponding to the definition, any other
--   TTD sub-instructions generated, and the last used instruction ID.
transDef :: InstrIDs -> InstrID -> (QName, ExprZ) -> ((IEntry, [IEntry]), InstrID)
transDef vIDs n (qn, eD) = 
  let ((entry, others), nD) = transE vIDs n eD
  in  (((idOf vIDs qn, snd entry), others), nD)
    
-- | Translates an actuals definition to an instruction entry. See "transE" for
--   for details. This function takes the instruction IDs table, the last used
--   instruction ID, and the actuals definition, which is a pair of the actual
--   name and a list of all intensional indexes and their actual expression for
--   this variable.
--   Returns the new TTD instruction corresponding to the definition, any other
--   TTD sub-instructions generated, and the last used instruction ID.
transAct :: InstrIDs -> InstrID -> (QName, [(IIndex, ExprZ)]) ->
            ((IEntry, [IEntry]), InstrID)
transAct vIDs n (qn, acts) =
  let (iidxs, el) = unzip acts
      (topEntriesIDs, entries, n') = transL vIDs n el
  in  (((idOf vIDs qn, ActualsT (zip iidxs topEntriesIDs)), entries), n')

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
  case Data.Map.lookup qn builtinInstrIDs of
    Just iID -> iID
    Nothing  ->
      case Data.Map.lookup qn vIDs of
        Just iID -> iID
        Nothing  -> ierr $ "idOf: no instruction ID for "++(pprint qn "")

-- | Returns the maximum instruction ID assigned to a built-in function.
maxBuiltinInstrID :: InstrID
maxBuiltinInstrID = maximum $ elems builtinInstrIDs

-- | Contains the instruction IDs of the built-in functions.
builtinInstrIDs :: InstrIDs
builtinInstrIDs = 
  let builtinNames = keys builtinFuncSigs ++ (concat $ elems builtinFuncSigs)
  in  fromList $ zip builtinNames [0..]

-- | The inverse of InstrIDs, used to map instruction IDs to built-in variables.
type VarIDs = Map InstrID QName

-- | Transforms an 'InstrIDs' table to a 'VarIDs'.
mkVarIDs :: InstrIDs -> VarIDs
mkVarIDs iIDs = fromList $ map swap $ toList iIDs

-- | The mapping from instruction IDs to built-in variable names.
builtinVarIDs :: VarIDs
builtinVarIDs = mkVarIDs builtinInstrIDs
