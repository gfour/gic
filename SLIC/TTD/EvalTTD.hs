-- | The tagged-token dataflow interpreter.
-- 

module SLIC.TTD.EvalTTD (evalTTD) where

import qualified Data.Map as M
import SLIC.AuxFun (ierr)
import SLIC.Constants (comma, modNoPath)
import SLIC.DFI (DFI, getMainDepth)
import SLIC.ITrans.Syntax (QOp(..))
import SLIC.SyntaxAux
import SLIC.TTD.SyntaxTTD
import SLIC.Types (Counter, CstrName, IIndex, PPrint(..), Value(..),
                   pprintIdx, pprintList)

-- | Values.
type ValueT = Value Token

-- | A token (also a LAR ID).
type Token = [(IIndex, Nested)]

-- | The nested tokens inside another token.
data Nested = N [Maybe Token]
            deriving (Eq, Ord, Show)

-- | Creates /n/ nested fields, not yet initialized.
dummyNested :: Int -> Nested
dummyNested n = N $ take n $ repeat Nothing

-- | Branches are used by instructions that depend on other instructions.
data Branch = Branch Int
            deriving (Eq, Ord, Show)

-- | A demand chain.
type DChain = [(InstrID, Branch, Maybe Nested)]

-- | A message color is a token and a demand-chain for propagating demands and
--   responses both ways.
type Color = (Token, DChain)

-- | A message is the tuple (sender, receiver, token, demand chain, payload).
data Msg = Msg InstrID InstrID Color Payload
           deriving (Eq)

instance PPrint Msg where
  pprint (Msg src dest (token, dchain) pl) =
    ("{ "++).shows src.("->"++).shows dest.(", "++).shows (token, dchain).
    (", ["++).(case pl of Demand -> ("D"++) ; Response val -> shows val).
    ("] }"++)

-- | The data carried by an instruction activation message.
data Payload = Demand             -- ^ a demand carries no data
             | Response ValueT    -- ^ a response contains a value
             deriving (Eq)

-- | A join structure for collecting results from fork-join operations.
data JoinData = JD InstrID Color Branch ValueT

-- | After sending a message to a dataflow instruction, either one or more
--   messages come out, or a forked computation sends a join update to notify
--   its parent for a value.
data MsgResult = Msgs [Msg] | JoinUpdate JoinData

-- | A blocked operation of an instruction, for a color.
type BlockedOp = (InstrID, Color)

-- | The join table records all results from children spawned in fork-join style.
--   It is used to activate instructions waiting for more than one other
--   instruction to finish.
type JoinTable = M.Map InstrID (M.Map BlockedOp (M.Map Branch ValueT))

-- | Evaluates a dataflow program. Takes the ID of the top instruction of the
--   \"result\" definition, and the actual program.
evalTTD :: DFI -> Int -> InstrID -> ProgT -> IO ()
evalTTD dfi nWorkers resultID p =
  let entriesTable = mkProgT' p      
      mainDepth = getMainDepth [dfi]
      color0 = ([((modNoPath, 0), dummyNested mainDepth)], [])
      initMsg = Msg (-1) resultID color0 Demand
      joinTable = M.fromList (zip (M.keys entriesTable) (repeat M.empty))
      (resVal, cycles) = runLoop nWorkers 1 entriesTable joinTable [initMsg]
  in  do -- putStrLn "Running..."
         putStrLn ("Result = "++(pprint resVal "")++
                   ", Cycles: "++(show cycles)++
                   ", Workers: "++(show nWorkers))

-- | Processes the messages until a value response to the root node is found.
--   Returns the value and the cycles needed to compute it.
runLoop :: Int -> Integer -> ProgT' -> JoinTable -> [Msg] -> (ValueT, Integer)
runLoop nWorkers counter entriesTable joins msgQ =
  let msgsToRun = take nWorkers msgQ
      msgsNext  = drop nWorkers msgQ
      -- Run the workers and gather the results.
      runResults = map (sendMsg entriesTable) msgsToRun
      -- Extract all messages and join updates.
      (newMessages, newJUpdates) = unzipR runResults
      -- Update the joins structure and reduce.
      joins' = addJoins newJUpdates joins
      -- Gather messages to root.
      msgsToRoot = filter (\(Msg _ dest _ _)->dest==(-1)) newMessages
  in  -- trace2 "-------------------------------" $ 
      case msgsToRoot of
        [] ->
          -- trace2 ("= Running next cycle, workers="++(show nWorkers)++"... =") $
          -- trace2 (pprintList (", "++) msgsToRun "") $
          -- trace2 ("Joins are now:\n"++(pprintJoinT joins' "")) $
          let (msgsR, joinsR) = reduceJoins nWorkers entriesTable joins' []
              pendingMsgs = msgsR++msgsNext++newMessages
          in  case pendingMsgs of
                [] -> ierr "Program ended with no result."
                _  -> runLoop nWorkers (counter+1) entriesTable joinsR pendingMsgs
        [Msg _ _ _ (Response valT)] -> (valT, counter)
        _ -> ierr $ "Cannot handle root msg:"++(pprintList comma msgsToRoot "")

-- | Runs a cycle, i.e. sends a message to a dataflow instruction in the program. 
--   The result is one or more messages to be processed in the next cycle.
--   A join update may also be produced, to update a join structure before the
--   next cycle.
sendMsg :: ProgT' -> Msg -> MsgResult
sendMsg p (Msg src dest color@(token, dchain) Demand) =
  Msgs
  (case M.lookup dest p of
      Just (CallT (Call iidx) iID) ->
        let token'   = (iidx, dummyNested 5):token
        in  -- TODO: hardcoded 5
            [ Msg dest iID (token', (src, Branch 0, Nothing):dchain) Demand ]
      Just (VarT iID) ->
        [ Msg dest iID (token, (src, Branch 0, Nothing):dchain) Demand ]
      Just (BVarT iID (Just (c, _), _)) ->
        let (_, nested):_ = token
        in  [ Msg dest iID (getN nested c, (src, Branch 0, Nothing):dchain) Demand]
      Just (ActualsT acts) ->
        let (i, nested):token' = token
            b = calcIdxBranch i acts
        in  case lookup i acts of
              Just iID ->
                [Msg dest iID (token', (src, Branch b, Just nested):dchain) Demand]
              Nothing  -> ierr $ "No actual for idx="++(pprintIdx i "")
      Just (ConT (CN _) [iID0, iID1]) ->
        [ Msg dest iID0 (token, (src, Branch 0, Nothing):dchain) Demand
        , Msg dest iID1 (token, (src, Branch 1, Nothing):dchain) Demand ]
      Just (ConT (LitInt i) []) ->
        [ Msg dest src color (Response (VI i)) ]
      Just (ConT (CN CIf) [iID, _, _]) ->
        [ Msg dest iID (token, (src, Branch 0, Nothing):dchain) Demand ]
      Just (CaseT _ iID _) ->
        [ Msg dest iID (token, (src, Branch 0, Nothing):dchain) Demand ]
      Just (ConstrT c) ->
        [ Msg dest src color (Response (VT (c, token))) ]
      Just instrT -> ierr $ "no dispatch for Demand: "++(pprint instrT "")
      Nothing     -> ierr $ "no instruction #"++(show dest)++" for Demand")
sendMsg p (Msg src dest (token, dchain) rVal@(Response val)) = 
  let (src', br@(Branch brn), nested):dchain' = dchain      -- pop demand chain
  in  case M.lookup dest p of
        Just (CallT (Call _) iID) | (brn==0) ->
          let _:token' = token
          in  (checkResponseIDs iID src)              
              Msgs [ Msg dest src' (token', dchain') rVal ]
        Just (ActualsT acts) ->
          let (i, _)   = acts !! brn
              Just nn  = nested
              token'   = (i, nn):token
          in  (checkResponseActuals i acts)
              Msgs [ Msg dest src' (token', dchain') rVal ]
        Just (VarT iID) | (brn==0) ->
          (checkResponseIDs iID src)
          Msgs [ Msg dest src' (token, dchain') rVal ]
        Just (BVarT iID _) ->
          (checkResponseIDs iID src)
          Msgs [ Msg dest src' (token, dchain') rVal ]          
        Just (ConT (CN _) [_, _]) | (brn==0) || (brn==1) ->
          -- Add 'join' entry for pending value.
          JoinUpdate (JD dest (token, dchain) br val)
        Just (ConT (CN CIf) [_, iID0, iID1]) | (brn==0) ->
          let VB cond = val
              (cID, b) = if cond then (iID0, 1) else (iID1, 2)
          in  Msgs [Msg dest cID (token, (src', Branch b, Nothing):dchain') Demand]
        Just (ConT (CN CIf) [_, _, _]) | (brn==1) || (brn==2) ->
          Msgs [ Msg dest src' (token, dchain') (Response val) ]
        Just (CaseT (Just (cID, _), _) _ pats) | (brn==0) ->
          let VT (c, cToken) = val
              (brn', patID) = findPat c 1 pats
              (i, nested'):token' = token
              updNested = setN nested' cID cToken
              color' = ((i, updNested):token',
                        (src', Branch brn', Just updNested):dchain')
          in  Msgs [ Msg dest patID color' Demand ]
        Just (CaseT _ _ _) | (brn/=0) ->
          Msgs [ Msg dest src' (token, dchain') (Response val) ]
        Just instrT -> ierr $ "no dispatch for Response: "++(pprint instrT "")
        Nothing     -> ierr $ "no instruction #"++(show dest)++" for Response"

findPat :: CstrName -> Int -> [PatT] -> (Int, InstrID)
findPat c _ [] = ierr $ "Pattern matching has no branch for "++(pprint c "")
findPat c i ((PatT c' iID):pats) = if c==c' then (i, iID) else findPat c (i+1) pats

-- | Evaluates a binary constant operator.
cBinOp :: COp -> ValueT -> ValueT -> ValueT
cBinOp CPlus  (VI x) (VI y) = VI (x+y)
cBinOp CMinus (VI x) (VI y) = VI (x-y)
cBinOp CMult  (VI x) (VI y) = VI (x*y)
cBinOp CDivide(VI x) (VI y) = VI (x `div` y)    -- we only do integer division
cBinOp CMod   (VI x) (VI y) = VI (x `mod` y)
cBinOp CDiv   (VI x) (VI y) = VI (x `div` y)
cBinOp CAnd   (VB x) (VB y) = VB (x && y)
cBinOp COr    (VB x) (VB y) = VB (x || y)
cBinOp CEqu   (VI x) (VI y) = VB (x==y)
cBinOp CNEq   (VI x) (VI y) = VB (x/=y)
cBinOp CLt    (VI x) (VI y) = VB (x<y)
cBinOp CGt    (VI x) (VI y) = VB (x>y)
cBinOp CLe    (VI x) (VI y) = VB (x<=y)
cBinOp CGe    (VI x) (VI y) = VB (x>=y)
cBinOp CMulI  _ _ = error "The TTD emulator does not support big integers."
cBinOp c _ _ = ierr $ "Unhandled cBinOp: "++(pprint c "")

-- | Takes the results from sending messages to a number of instructions and
--   splits them into two lists: the new messages generated and the join
--   update operations.
unzipR :: [MsgResult] -> ([Msg], [JoinData])
unzipR msgResults =
  let aux [] msgs jds                    = (msgs, jds)
      aux ((Msgs ms):mrs) msgs jds       = aux mrs (ms++msgs) jds
      aux ((JoinUpdate jd):mrs) msgs jds = aux mrs msgs (jd:jds)
  in  aux msgResults [] []

-- * Join table functions

-- | Adds a set of join result updates in the join table.
addJoins :: [JoinData] -> JoinTable -> JoinTable
addJoins [] jt = jt
addJoins ((JD iID (token, (iID', _, _):dchain) br val):jds) jt =
  -- TODO: we ignore nesting here, is it ok?
  case M.lookup iID jt of
    Just iM ->
      let clr' = (token, dchain)
          blockedOp = (iID', clr')
          iM' =
            M.insert blockedOp (
              case M.lookup blockedOp iM of
                -- No join for this color found, create it.
                Nothing -> M.fromList [(br, val)]
                Just jM -> M.insertWithKey check br val jM) iM
          check br' _ _ = ierr $ "Duplicate branch in join: "++(show br')
      in  addJoins jds (M.insert iID iM' jt)
    Nothing -> ierr $ "addJoins found no slot for iID="++(show iID)
addJoins _ _ = ierr "addJoins: found malformed join data"

-- | Scans once the whole table for fork-join operations that are complete and
--   runs their instructions. Takes the maximum number of join operations to 
--   perform, the list of all instruction IDs, and the join table.
--   Returns the new message results that may be generated and the new
--   join table (without the completed joins).
reduceJoins :: Int -> ProgT' -> JoinTable -> [Msg] -> ([Msg], JoinTable)
reduceJoins 0 _ jt msgAccum =
  -- trace2 ("reduceJoins returns: "++(pprintList (", "++) msgAccum "")) $ 
  (msgAccum, jt)
reduceJoins jCounter entriesTable jt msgAccum =
  case extractNextJoin (M.keys entriesTable) jt of
    Nothing -> -- trace2 "No joins to reduce." $
               reduceJoins 0 entriesTable jt msgAccum
    Just (iID, blockedOp@(iID', clr), valL, valR) ->
      case M.lookup iID entriesTable of
        Just (ConT (CN c) [_, _]) ->
          let val = cBinOp c valL valR
              msgVal = Msg iID iID' clr (Response val)
              jt' = removeJoin iID blockedOp jt
          in  -- trace2 ("A join is waiting, result="++(pprint val "")) $
              -- trace2 ("Sending message: "++(pprint msgVal "")) $
              -- trace2 ("Joins difference:") $
              -- trace2 ("["++(pprintJoinT jt  "]")) $
              -- trace2 ("["++(pprintJoinT jt' "]")) $
              reduceJoins (jCounter-1) entriesTable jt' (msgVal:msgAccum)
        Just i  -> ierr $ "Not a joinable instruction: "++(pprint i "")
        Nothing -> ierr $ "Join could not find instruction #"++(show iID)

extractNextJoin :: [InstrID] -> JoinTable ->
                   Maybe (InstrID, BlockedOp, ValueT, ValueT)
extractNextJoin [] _ = Nothing
extractNextJoin (iID:iIDs) jt =
  case M.lookup iID jt of
    Just cm ->
      let mJoins = M.filter (\m->(M.size m)==2) cm
      in  if M.null mJoins then
            extractNextJoin iIDs jt     -- Nothing found, check next instruction.
          else
            let (bOp, bjs):_ = M.toList mJoins
                [(br0, val0), (br1, val1)] = M.toList bjs
            in  case (br0, br1) of
                  (Branch 0, Branch 1) -> Just (iID, bOp, val0, val1)
                  (Branch 1, Branch 0) -> Just (iID, bOp, val1, val0)
                  brs             ->
                    ierr $ "These are not Left/Right branches: "++(show brs)
    Nothing -> ierr $ "extractNextJoin: no join table for "++(show iID)

-- | Delete a complete join entry from the map.
removeJoin :: InstrID -> BlockedOp -> JoinTable -> JoinTable
removeJoin iID blockedOp jt =
  let delCheck bm =
        case M.lookup blockedOp bm of
          Nothing -> ierr $ "Could not find complete join to delete: "++
                     (show (iID, blockedOp))
          Just jm -> if M.size jm /= 2 then
                       ierr $ "Cannot delete non-complete join: "++
                       (show (iID, blockedOp))
                     else
                       M.delete blockedOp bm
  in  M.adjust delCheck iID jt

{-
pprintJoinT :: JoinTable -> ShowS
pprintJoinT jt = 
  let pprintCMap (iID, cm) = shows iID.(":"++).nl.foldDot pprintBMap (M.toList cm)
      pprintBMap (clr, bvs) | M.size bvs <=2 =
        shows clr.(": "++).foldDot pprintBVal (M.toList bvs).nl
                            | otherwise =
        ierr "printBMap: more than two elements in join"
      pprintBVal (br, val) = shows br.("="++).pprint val.(" "++)
  in  foldDot pprintCMap (M.toList $ M.filter (not . M.null) jt)
-}

-- * Invariant checkers

-- | Takes a dependent instruction ID and the ID of an instruction that sent
--   a response. If the two IDs are the same, it is a real response and the
--   result is the identity function, otherwise it produces an error. This is
--   used as a wrapper for instruction responses.
checkResponseIDs :: InstrID -> InstrID -> (a->a)
checkResponseIDs iID src =
  if src /= iID then
    ierr $ "Response sanity check failed: src="++(show src)++"!="++(show iID)
  else id

checkResponseActuals :: IIndex -> Acts -> (a->a)
checkResponseActuals i acts =
  case lookup i acts of
    Just _  -> id
    Nothing -> ierr $ "Response, Actuals, i="++(show i)++", not in "++(show acts)

-- * Auxiliary functions

-- | Get token \#d from a nested field. The location must already be filled in.
getN :: Nested -> Counter -> Token
getN (N nested) d =
  let nL = length nested
  in  if d<nL then
        case nested !! d of
          Just nT -> nT
          Nothing -> ierr $ "getN: nested["++(show d)++"] is empty"
      else
        ierr $ "getN: no position d="++(show d)++", nested has length "++(show nL)

-- | Sets the token at position \#d in a nested field. The location must be empty.
setN :: Nested -> Counter -> Token -> Nested
setN (N nested) d cToken =
  let nL = length nested
  in  if d<nL then
        case nested !! d of
          Nothing -> N $ (take d nested)++[Just cToken]++(drop (d+1) nested)
          Just nT -> ierr $ "setN: nested["++(show d)++"] exists: "++(show nT)
      else
        ierr $ "setN: no position d="++(show d)++", nested has length "++(show nL)
