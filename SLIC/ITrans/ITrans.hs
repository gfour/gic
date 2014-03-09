-- | The first-order intensional transformation, Yaghi-style.

module SLIC.ITrans.ITrans (itransM, itransP) where

import Data.Map (Map, elems, empty, fromList, keys, lookup, union, unions, update)
import Data.List (sortBy)
import SLIC.AuxFun (ierr)
import SLIC.ITrans.Syntax
import SLIC.SyntaxAux
import SLIC.Types

-- | A map from variables to indices.
type Inds = Map QName IIndex

-- | Transforms a HIL module.
itransM :: ModH -> ModH
itransM modH = --(Mod fm@(m, _) exports imports p an) =
  let (m, _) = modNameF modH
      imports = modImports modH
      aux idecl =
        case ideclInfo idecl of
          Just (fsigs, _) -> fsigs
          Nothing         -> empty
      importsI = unions $ map aux imports
      allSigs = union importsI builtinFuncSigs
  in  modH{modProg=(itransP m allSigs (modProg modH))}

-- | Transforms a HIL prog, the output is another HIL program.
itransP :: MName -> FuncSigs -> ProgH -> ProgH
itransP m extSigs (Prog cs defs) =
  let takeDefName (DefH f _ _)     = f
      takeDefName (ActualsH _ _ _) =
          ierr "actuals() exists in program before the intensional transformation"
      fNames = (map takeDefName defs) ++ (keys extSigs)
      initInds = fromList (zip fNames (repeat (m, 0)))
      -- insert calls
      defs1 = insertCalls initInds defs
      -- create newDefs
      defsActuals = createActuals m extSigs defs1
      allDefs = defs1 ++ defsActuals
      -- remove function arguments
      procDefs = map removeFHArgs allDefs
  in  Prog cs procDefs

-- | Finds the formals of every function and creates an actuals(...)
--   definition for it.
createActuals :: MName -> FuncSigs -> [DefH] -> [DefH]
createActuals m extSigs defs =
  let -- make a table of every (function name, its formals)
      takeDefSig (DefH f vs _) = (f, vs)
      takeDefSig (ActualsH _ _ _) =
          ierr "createActuals: found a definition that is already an actuals()"
      allSigs = union (fromList (map takeDefSig defs)) extSigs  -- add external sigs
      -- take all the formals
      allFrms = concat (Data.Map.elems allSigs)
      -- find all actuals of all formals
      defActs (DefH _ _ e) = getActs allSigs e
      defActs (ActualsH _ _ _) =
          ierr "can't create new actuals() definitions for existing actuals() definitions"
      allActuals = concatMap defActs defs      
      actsOf v = map snd $ sortBy cmpActs $ map snd $ (filter (\(v', _) -> v==v') allActuals)
      cmpActs (i1, _) (i2, _) = if i1>i2 then GT else LT
      -- make a table of every formal and its list of actuals
      acts = zip allFrms (map actsOf allFrms)
  in  map (\(v, as)->ActualsH v m as) acts

-- | Finds all the actuals used in an expression. The result is a list of
--   triplets (variable, call-index, expression) for all the actuals encountered.
getActs :: FuncSigs -> ExprH -> [(QName, (IIndex, ExprH))]
getActs _ (XH _) = []   
getActs _ (ConstrH _) = []
getActs allSigs (ConH _ el) = concatMap (getActs allSigs) el
getActs allSigs (FH (Call i) f el) = 
  let fNames =
        case Data.Map.lookup f allSigs of
          Just fn -> fn
          Nothing -> ierr $ "getActs: definition lookup failed for "++(qName f)++" in signatures:\n"++(pprFSigs allSigs "")
      acts = map (\e->(i, e)) el
  in  (zip fNames acts) ++ (concatMap (getActs allSigs) el)         
getActs _ fc@(FH NOp _ _) =
  ierr $ "getActs: this function call should have a call() before it: "++(pprint fc "")
getActs allSigs (CaseH _ e pats) = 
  (getActs allSigs e) ++ (concatMap (\(PatB _ e0) -> getActs allSigs e0) pats)
  
-- | Removes the arguments from all function applications.
removeFHArgs :: DefH -> DefH
removeFHArgs def =
  let remFHArgsE (XH v) = XH v
      remFHArgsE (ConH c el) = ConH c (map remFHArgsE el)
      remFHArgsE (FH call@(Call _) f _) = FH call f []
      remFHArgsE fc@(FH NOp _ _) =
        ierr $ "remFHArgsE: this function call should have a call() before it: "++(pprint fc "")
      remFHArgsE (CaseH d e pats) =
        CaseH d (remFHArgsE e)
        (map (\(PatB c0 e0) -> PatB c0 (remFHArgsE e0)) pats)
      remFHArgsE (ConstrH c) = ConstrH c
  in  case def of
        DefH f vs e   -> DefH f vs (remFHArgsE e)
        ActualsH f m el -> ActualsH f m (map remFHArgsE el)

-- | Inserts the call() operators in the program, sequentially.
--   This is the main call of the function.           
insertCalls :: Inds -> [DefH] -> [DefH]
insertCalls inds ((DefH f vs e) : defs) =
  let (e', inds') = insertCallsE inds e
  in  (DefH f vs e') : (insertCalls inds' defs)
insertCalls _ (def@(ActualsH _ _ _) : _) =
  ierr $ "this definition is already an actuals, the program to be transformed should not have actuals definitions: "++(pprint def "")
insertCalls _ [] = []
    
-- | Helper of 'insertCalls' for expressions.
insertCallsE :: Inds -> ExprH -> (ExprH, Inds)
insertCallsE inds (XH v) = (XH v, inds)
insertCallsE inds (ConH c el) =
  let (el', inds') = insertCalls_List inds insertCallsE el
  in  (ConH c el', inds')
insertCallsE inds (ConstrH c) = (ConstrH c, inds)  
insertCallsE inds (FH NOp f el) =
  let iindex =
        case Data.Map.lookup f inds of
          Just iidx -> iidx
          Nothing   -> ierr $ "insertCallsE: call to "++(qName f)++" has no index"
      addOne (m, i) = (m, i+1)
      callId  = Call iindex
      inds1   = Data.Map.update (\_ -> Just (addOne iindex)) f inds
      (el', inds') = insertCalls_List inds1 insertCallsE el
  in  (FH callId f el', inds')
insertCallsE _ fc@(FH _ _ _) =
  ierr $ "Cannot insert call(), there is already one in: "++(pprint fc "")
insertCallsE inds (CaseH d e pats) =
  let (e', inds') = insertCallsE inds e
      (pats', inds'') = insertCalls_List inds' insertCallsPat pats
  in  (CaseH d e' pats', inds'')
      
-- | Helper of 'insertCalls' for patterns.
insertCallsPat :: Inds -> PatH -> (PatH, Inds)
insertCallsPat inds (PatB c e) =
  let (e', inds') = insertCallsE inds e
  in  (PatB c e', inds')

-- | Helper of 'insertCalls', sequentializes the effect
--   of a processing function on a list.
insertCalls_List :: Inds -> (Inds -> a -> (a, Inds)) -> [a] -> ([a], Inds)
insertCalls_List inds _ [] = ([], inds)
insertCalls_List inds f (e : es) =
  let (e', inds')   = f inds e
      (el', inds'') = insertCalls_List inds' f es
  in  (e' : el', inds'')
