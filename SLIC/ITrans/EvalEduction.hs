-- | The call-by-need interpreter for ZOIL programs.
-- 
--   This is based on the classic 'eduction' technique of evaluating intensional
--   programs using a 'warehouse' to store memoized results of evaluating
--   (variable, context) pairs.
--   Features:
-- 
--   * Modules (via the modular intensional transformation). No linking
--     stage is necessary.
-- 
--   * Non-strict data types (implemented with context nesting). This uses a
--     context allocator that works as a free-list (context allocation table, CAT)
--     to assign ids to contexts.
-- 
--   * Supports garbage collection of the warehouse.
-- 

{-# LANGUAGE CPP #-}
module SLIC.ITrans.EvalEduction (evalZOILLazy) where

import Data.Map (Map, empty, elems, filter, filterWithKey,
                 insert, keys, lookup, size, toList, update)
import Data.Maybe (catMaybes)
import SLIC.AuxFun (foldDot, ierr, showStrings, trace2)
import SLIC.Constants
import SLIC.DFI (DFI)
import SLIC.ITrans.Syntax
import SLIC.ITrans.ZLinker
import SLIC.State
import SLIC.SyntaxAux
import SLIC.Types

-- * Auxiliary types

-- | The options passed to the interpreter: verbosity and maximum warehouse
--   size.
type EOpts = (Verb, Int)

-- * Contexts and their allocator

-- | A context ID is just an integer that points to the table of allocated
--   contexts.
type CtxtID = Integer

-- | The flag used during garbage collection to mark live contexts.
type GCLive = Bool

-- | A context is a triple of a "head" (intensional index), a "tail" (context
--   ID of the tail context), and an optional list of nested contexts.
type ECtxt = (IIndex, CtxtID, [Maybe CtxtID], GCLive)

pprintECtxt :: ECtxt -> ShowS
pprintECtxt (iidx, cid, n, gcm) =
  ("{"++).pprintIdx iidx.(", "++).shows cid.
  (let nctxts = Prelude.map pprNested n
       pprNested Nothing  = "?"
       pprNested (Just c) = show c
   in  (", nested: { "++).showStrings ", " nctxts.(" }"++)).
  (", GC: "++).shows gcm.nl

type CTMap = Map CtxtID ECtxt

pprintCTMap :: CTMap -> ShowS
pprintCTMap cmap =
  let aux (ci, ectxt) = (" * "++).shows ci.(" | "++).pprintECtxt ectxt
  in  foldDot aux (toList cmap)

-- | A table that contains the contexts allocated so far and the next free
--   context ID.
type CtxtsTable = (CTMap, CtxtID)

{-
pprintCtxtsTable :: CtxtsTable -> ShowS
pprintCtxtsTable (cmap, cid) =
  ("Contexts table:"++).nl.
  pprintCTMap cmap.
  ("* Next free context ID: "++).shows cid.nl
-}

-- | Allocate a new context from a "head" intensional index and a "tail"
--   context. Takes and returns the full state of evaluation, as it is here
--   that garbage collection may be called.
allocCtxt :: EOpts -> IIndex -> CtxtID -> EState -> (CtxtID, EState)
allocCtxt (t, maxWhSize) idx ctxt st@((_, nextID), wh) =
  let ((tblGC, nextIDGC), whGC) =
        if size wh > maxWhSize then
          let stGC'@((_, _), whGC') =
                (if t then trace2 "Starting garbage collection..." else id)
                gc t ctxt st
          in  if size whGC' > maxWhSize then
                error $ "More memory needed, adjust with -maxwh, current value="++
                        (show $ maxWhSize)
              else
                stGC'
        else
          st
  in  let tbl' = (insert nextID (idx, ctxt, [], False) tblGC, nextID+1)
      in  (if t then
              trace2 $ "Allocated context ["++(pprintIdx idx "")++
                       ":"++(show ctxt)++"] := "++(show nextIDGC) 
           else id)
          (nextIDGC, (tbl', whGC))

-- | Returns the context pointed to by a context ID.
getCtxt :: CtxtID -> CTMap -> ECtxt
getCtxt ctxtID cmap =
  if ctxtID == initCtxtID then
    ierr $ "Cannot look up the top-level context "++(show ctxtID)++".\n"++
           (pprintCTMap cmap "")
  else
    case Data.Map.lookup ctxtID cmap of
      Just ctxt -> ctxt
      Nothing   ->
        ierr $ "Could not find context "++(show ctxtID)++" in the contexts table"

-- | Nests a context under another context in the ctxts table (given
--   a nesting position/"depth").    
nestCtxtUnder :: CtxtID -> CtxtID -> CtxtsTable -> Int -> CtxtsTable
nestCtxtUnder ctxt ctxtTarget (cmap, cn) i =
  let (idx, prevCtxt, nested, gcm) = getCtxt ctxtTarget cmap
      nLen = length nested
  in  if nLen /= i then
        ierr $ "nestCtxtUnder: tried to nest at last position "++
        (show i)++" but context has size "++(show nLen)
      else let nested' = nested++[Just ctxt]
               newCtxt = (idx, prevCtxt, nested', gcm)
           in  (Data.Map.update (\_->Just newCtxt) ctxtTarget cmap, cn)

-- | The initial contexts table.
initCtxtsTable :: CtxtsTable
initCtxtsTable = (Data.Map.empty, initCtxtID+1)

-- | The context ID of the top-level context.
initCtxtID :: Integer
initCtxtID = 0

-- | Returns the i-th context already nested under another context.
nestedOf :: Int -> CtxtID -> CtxtsTable -> CtxtID
nestedOf d ctxt (cmap, _) =
  let (_, _, nested, _) = getCtxt ctxt cmap
  in  if d < length nested then
        case nested !! d of
          Nothing -> ierr "nestedOf: nested context is empty"
          Just nc -> nc
      else
        ierr "nestedOf: Nested field is smaller than requested index"

-- * Evaluation functions

-- | The state contains a contexts table for context allocation and a
--   warehouse for value memoization.
type EState = (CtxtsTable, Warehouse)

-- | Lazy constructors embed their enclosing context.
type IValue = Value CtxtID

-- | The main entry point of evaluation.
evalEduction :: EOpts -> MName -> FProg -> IValue
evalEduction eOpts@(t, _) m p =
  let mainDef = mainDefQName m
  in  (if t then trace2 ("Evaluating "++(qName mainDef)) else id)
      (case searchFunc mainDef p of
          Just e ->
            let initState = (initCtxtsTable, initWH)
                (ctxt, st) = allocCtxt eOpts (m, 0) initCtxtID initState
                (val, _) = evalE eOpts p e ctxt st
            in  val
          Nothing -> error $ "No suitable starting definition found for "++mainDefName++", maybe you want to do separate module compilation?"
      )

-- | Evaluates a 0-order expression.
evalE :: EOpts -> FProg -> ExprZ -> CtxtID -> EState -> (IValue, EState)
evalE _ _ (ConZ (LitInt i) _) _ st = (VI i, st)
evalE _ _ (ConZ (CN CTrue) []) _ st = (VB True, st)
evalE _ _ (ConZ (CN CFalse) []) _ st = (VB False, st)
evalE _ _ (ConZ (CN cn) []) _ _ = ierr $ "unknown nullary constant: "++(show cn)
evalE eOpts p (ConZ (CN CPlus) [e1, e2]) ctxt st =            
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VI (intFrom val1 + intFrom val2), st2)
evalE eOpts p (ConZ (CN CMinus) [e1, e2]) ctxt st =            
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VI (intFrom val1 - intFrom val2), st2)
evalE eOpts p (ConZ (CN CMult) [e1, e2]) ctxt st =            
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VI (intFrom val1 * intFrom val2), st2)
evalE eOpts p (ConZ (CN CEqu) [e1, e2]) ctxt st =            
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VB (val1 == val2), st2)    -- polymorphic equality
evalE eOpts p (ConZ (CN CLt) [e1, e2]) ctxt st =            
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VB ((intFrom val1) < (intFrom val2)), st2)
evalE eOpts p (ConZ (CN CLe) [e1, e2]) ctxt st =            
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VB ((intFrom val1) <= (intFrom val2)), st2)
evalE eOpts p (ConZ (CN CGt) [e1, e2]) ctxt st =            
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VB ((intFrom val1) > (intFrom val2)), st2)
evalE eOpts p (ConZ (CN CGe) [e1, e2]) ctxt st =            
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VB ((intFrom val1) >= (intFrom val2)), st2)
evalE eOpts p (ConZ (CN CAnd) [e1, e2]) ctxt st =            
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VB ((boolFrom val1) && (boolFrom val2)), st2)
evalE eOpts p (ConZ (CN COr) [e1, e2]) ctxt st =            
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VB ((boolFrom val1) || (boolFrom val2)), st2)
evalE eOpts p (ConZ (CN CDiv) [e1, e2]) ctxt st =
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VI ((intFrom val1) `div` (intFrom val2)), st2)
evalE eOpts p (ConZ (CN CMod) [e1, e2]) ctxt st =
  let (val1, st1) = evalE eOpts p e1 ctxt st
      (val2, st2) = evalE eOpts p e2 ctxt st1
  in  (VI ((intFrom val1) `mod` (intFrom val2)), st2)
evalE _ _ (ConZ (CN c) [_, _]) _ _ =
  ierr $ "lazy eduction: unhandled binary operator "++(show c)
evalE eOpts p (ConZ (CN CIf) [e0, e1, e2]) ctxt st =
  let (val0, st0) = evalE eOpts p e0 ctxt st
  in  if boolFrom val0 then
        evalE eOpts p e1 ctxt st0
      else
        evalE eOpts p e2 ctxt st0
evalE eOpts p (FZ NOp v) ctxt st = evalV eOpts p v ctxt st
evalE eOpts p (FZ (Call ii) v) ctxt st =
  let (ctxt', st') = allocCtxt eOpts ii ctxt st
  in  evalV eOpts p v ctxt' st'
evalE eOpts p (XZ (V v)) ctxt st =  
  evalV eOpts p v ctxt st
evalE eOpts p (XZ (BV v (Just (_, d), _))) ctxt st@(ctbl, _) =
  let ctxt' = nestedOf d ctxt ctbl
  in  evalV eOpts p v ctxt' st
evalE eOpts p (CaseZ (Just (_, d), _) e0 pats) ctxt st =
  let (val0, (ctbl0, wh0)) = evalE eOpts p e0 ctxt st
  in  case val0 of
        VT (c, ctxt') -> 
          case Prelude.filter (\(PatZ cP _ _)->(cP==c)) pats of
            [] -> ierr $ "case: no branch for constructor "++(qName c)
            [PatZ _ eP _] ->
              let ctbl' = nestCtxtUnder ctxt' ctxt ctbl0 d
              in  evalE eOpts p eP ctxt (ctbl', wh0)
            (_:_) -> ierr $ "case: multiple branches for constructor "++(qName c)
        _ -> ierr "case expression is not a constructor"
evalE _ _ (ConstrZ c) ctxt st = (VT (c, ctxt), st)
evalE _ _ e _ _ = ierr $ "Unhandled expression: ("++(pprint e ")")

evalV :: EOpts -> FProg -> QName -> CtxtID -> EState -> (IValue, EState)
evalV eOpts@(t, _) p v ctxt st =
  case searchFunc v p of
    Nothing ->
      (if t then
        trace2 $ "Looking up formal "++(pprint v "")++" in context "++(show ctxt)
      else id)
      lookupWH eOpts p v ctxt st
    Just e ->
      (if t then
          trace2 ("Entering function "++(qName v)++"...")
       else id)
      evalE eOpts p e ctxt st

-- * The warehouse

-- | A slot in th warehouse. 
data WHSlot = Pending           -- ^ pending computation
            | Memo !IValue      -- ^ memoized value
            deriving (Eq)
                       
instance PPrint WHSlot where
  pprint Pending    = ("{pending}"++)
  pprint (Memo val) = ("{value="++).shows val.("}"++)

-- | The warehouse is a pair of a size counter and a table from contexts to
--   computed values.
type Warehouse = Map (QName, CtxtID) WHSlot

-- | The initial empty warehouse.
initWH :: Warehouse
initWH = Data.Map.empty

-- | Looks up the value of a variable in a context, in the warehouse.
lookupWH :: EOpts -> FProg -> QName -> CtxtID -> EState -> (IValue, EState)
lookupWH eOpts@(t, _) p v ctxt state@((cmap, cid), whtbl) =
  case Data.Map.lookup (v, ctxt) whtbl of
    Just (Memo val) ->
      (if t then
         trace2("found memoized ("++(qName v)++","++(show ctxt)++")="++(pprint val ""))
       else id)
      (val, state)
    Nothing  ->
      let whtblP = insert (v, ctxt) Pending whtbl -- insert 'pending' slot
          stateP = ((cmap, cid), whtblP)
      in  case searchFunc v p of
            Just e -> evalE eOpts p e ctxt stateP
            Nothing  ->
              let (iidx, prevCtxt, _, _) = getCtxt ctxt cmap
              in  case searchFormal (v, iidx) p of
                    Just e -> 
                      let (valR, stR) = evalE eOpts p e prevCtxt stateP
                          (ctblR, whtblR) = stR   -- unfold returned state
                          whtbl' = insert (v, ctxt) (Memo valR) whtblR -- memoize
                      in  (valR, (ctblR, whtbl'))
                    Nothing -> 
                      ierr $ "No actuals definition for formal "++(qName v)
    Just Pending -> ierr "Tried to evaluate pending intensional expression."
      
-- | Takes a single DFI that is all module DFIs merged (using "DFI.mergeDFIs"),
--   the typing environment of the current module, and a list of 0-order modules
--   to run.
evalZOILLazy :: Options -> DFI -> ModZ -> IO ()
evalZOILLazy opts dfi p3 =
  if optLink opts then
    error "The lazy eduction interpreter does not support linking."
  else
    case optCMode opts of
      CompileModule ->
        error"The lazy eduction interpreter does not support separate compilation."
      Whole ->
        let eOpts  = (optVerbose opts, optWhSize opts)
            p3'    = mergeAndLinkZ opts dfi [p3]
            val    = evalEduction eOpts (fst $ modNameF p3) (fProg $ progDefs p3')
        in  putStrLn (pprint val "")

-- * Garbage collection

-- | Garbage collection entry point.
gc :: Verb -> CtxtID -> EState -> EState
gc t ctxt ((cmap, cid), wh) =
  let -- traverse the contexts table to mark live contexts
      cmap' = markLive wh ctxt cmap      
      -- find all pending computations in the warehouse and mark their
      -- contexts pending as well
      pendingCtxts = Prelude.map snd $ Data.Map.keys $ 
                     Data.Map.filter (\x->x==Pending) wh
      cmap'' = foldr (markLive wh) cmap' pendingCtxts
      -- find all contexts that have 'prev' pointers pointing to this context
      nextCtxts = Data.Map.keys $ Data.Map.filterWithKey
                  (\_ (_, prevCtxt', _, _)->prevCtxt'==ctxt) cmap''
      cmap''' = foldr (markLive wh) cmap'' nextCtxts
      cmapMarked = cmap'''
      -- naive double traversal, should be single (use 'break' or 'span')
      -- keep only those contexts marked as live
      -- cmapLive = Data.Map.map (\(idx, prev, ns, _) ->
      --                           (idx, prev, ns, False)) $
      --            Data.Map.filter (\(_, _, _, gcm)->gcm) cmapMarked
      liveCtxts = Data.Map.keys $ 
                  Data.Map.filter (\(_, _, _, gcm)->gcm) cmapMarked
      -- deadCtxts = Data.Map.keys $ 
      --             Data.Map.filter (\(_, _, _, gcm)->not gcm) cmapMarked
      whLive = Data.Map.filterWithKey (\(_, ctx0) _-> ctx0 `elem` liveCtxts) wh
  in  (if t then
          trace2 ("Found "++(show $ length liveCtxts)++" live contexts.") 
       else id)
      -- error (shows pendingCtxts "") -- pprintCTMap cmapLive "")
      ((cmap, cid), whLive)

-- | Marks a context as \'live\', proceeding recursively to its linked contexts.
markLive :: Warehouse -> CtxtID -> CTMap -> CTMap
markLive wh ctxt cmap =
  let -- find the live context
      (idx, prevCtxt, ns, gcm) = getCtxt ctxt cmap
  in  if gcm then
        -- already taken care of by GC or initial ctxt, just return
        cmap
      else
        let -- set GC flag
            cmap1 = Data.Map.update (\_->Just (idx, prevCtxt, ns, True)) ctxt cmap
            -- mark live its previous context and its nested contexts
            -- TODO: maybe we don't need marking the nested since we mark formals
            liveCtxts =
              let nsctxts = catMaybes ns
              in  if prevCtxt == initCtxtID then
                    nsctxts             -- ignore initial dymmy context
                  else
                    prevCtxt : nsctxts
            -- mark live the contexts lazy constructors (memoized in this
            -- context) point to
            constrCtxts = findCCtxtsIn ctxt wh
        in  foldr (markLive wh) cmap1 (liveCtxts++constrCtxts)

-- | Given a context and a warehouse, finds all the lazy constructors memoized
--   in this context and shows 
findCCtxtsIn :: CtxtID -> Warehouse -> [CtxtID]
findCCtxtsIn ctxt wh =
  let lConstrCtxt (Memo (VT (_, ctxtC))) = Just ctxtC
      lConstrCtxt _ = Nothing
  in  catMaybes $ Prelude.map lConstrCtxt $ Data.Map.elems $
      Data.Map.filterWithKey (\(_, ctxt') _ -> ctxt==ctxt') wh 
