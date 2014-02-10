-- | The tagged-token dataflow interpreter.
-- 

module SLIC.TTD.EvalTTD (evalTTD) where

import qualified Data.Map as M
import SLIC.AuxFun (ierr)
import SLIC.Constants (comma, nWorkers)
import SLIC.ITrans.Syntax (QOp(..))
import SLIC.SyntaxAux
import SLIC.TTD.SyntaxTTD
import SLIC.Types (PPrint(pprint), pprintList)

-- | Base values are just integers.
data ValueT = VI Int | VB Bool
            deriving (Eq, Show)

instance PPrint ValueT where
  pprint (VI i) = shows i
  pprint (VB b) = shows b

-- | A token (also a LAR ID).
type Token = [Int]

-- | Branches are used by instructions that depend on two instructions.
data Branch = LeftB | RightB | Single
            deriving (Eq, Ord, Show)

-- | A demand chain.
type DChain = [(Plug, Branch)]

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

-- | Program representation with a map for fast instruction lookup.
type ProgT' = M.Map InstrID InstrT

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
evalTTD :: InstrID -> ProgT -> IO ()
evalTTD resultID (ProgT entries) =
  let entriesTable = M.fromList entries
      initMsg = Msg (-1) resultID ([], []) Demand
      joinTable = M.fromList (zip (M.keys entriesTable) (repeat M.empty))
      (resVal, cycles) = runLoop 1 entriesTable joinTable [initMsg]
  in  do putStrLn "Running..."
         putStrLn ("Result = "++(pprint resVal "")++", Cycles: "++(show cycles))

-- | Processes the messages until a value response to the root node is found.
--   Returns the value and the cycles needed to compute it.
runLoop :: Integer -> ProgT' -> JoinTable -> [Msg] -> (ValueT, Integer)
runLoop counter entriesTable joins msgQ =
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
                _  -> runLoop (counter+1) entriesTable joinsR pendingMsgs
        [Msg _ _ _ (Response valT)] -> (valT, counter)
        _ -> ierr $ "Cannot handle root msg:"++(pprintList comma msgsToRoot "")

-- | Runs a cycle, i.e. sends a message to a dataflow instruction in the program. 
--   The result is one or more messages to be processed in the next cycle.
--   A join update may also be produced, to update a join structure before the
--   next cycle.
sendMsg :: ProgT' -> Msg -> MsgResult
sendMsg p (Msg src dest (token, dchain) Demand) =
  Msgs
  (case M.lookup dest p of
      Just (CallT (Call (_, i)) (iID, pID)) ->
        [ Msg dest iID (i:token, ((src, pID), Single):dchain) Demand ]
      Just (VarT (iID, pID)) ->
        [ Msg dest iID (token, ((src, pID), Single):dchain) Demand ]
      Just (ActualsT plugs) ->
        let i:token' = token
            (iID, pID) = plugs !! i
        in  [ Msg dest iID (token', ((src, pID), Single):dchain) Demand ]
      Just (ConT (CN _) [(iID0, pID0), (iID1, pID1)]) ->
        [ Msg dest iID0 (token, ((src, pID0), LeftB ):dchain) Demand
        , Msg dest iID1 (token, ((src, pID1), RightB):dchain) Demand ]
      Just (ConT (LitInt i) []) ->
        [ Msg dest src (token, dchain) (Response (VI i)) ]
      Just (ConT (CN CIf) [(iID, pID), _, _]) ->
        [ Msg dest iID (token, ((src, pID), LeftB):dchain) Demand ]
      Just instrT -> error $ "TODO: node dispatch (Demand): "++(pprint instrT "")
      Nothing     -> ierr $ "no instruction #"++(show dest)++" in graph (Demand)")
sendMsg p msg2@(Msg src dest (token, dchain) rVal@(Response val)) = 
  let ((src', pID'), br):dchain' = dchain      -- pop demand chain
      -- isPending (iID, color, _, _) =
      --   (iID==dest) && (cTail color == cTail (token, dchain))
  in  case M.lookup dest p of
        Just (CallT (Call (_, i)) (iID, pID)) | br==Single ->
          let i':token' = token
          in  (checkResponseCall (i, i') (iID, pID) (src, pID'))
              Msgs [ Msg dest src' (token', dchain') rVal ]
        Just (ActualsT plugs) ->
          let token' = pID':token      -- this assumes pID'==intensional index
          in  (checkResponseActuals pID' (length plugs))
              Msgs [ Msg dest src' (token', dchain') rVal ]
        Just (VarT (iID, pID)) | br==Single ->
          (checkResponseVar (iID, pID) (src, pID'))
          Msgs [ Msg dest src' (token, dchain') rVal ]
        Just (ConT (CN c) [_, _]) ->
           case br of
             Single -> ierr$(pprint c ": no single dependency ")++(pprint msg2 "")
             -- Add 'join' entry for pending value.
             _ -> JoinUpdate (JD dest (token, dchain) br val)
        Just (ConT (CN CIf) [_, (iID0, pID0), (iID1, pID1)]) ->
          case br of
            Single -> ierr "'if' has no single dependency"
            LeftB  ->
              let VB cond = val
                  m = if cond then
                        Msg dest iID0 (token, ((src', pID0), RightB):dchain') Demand
                      else
                        Msg dest iID1 (token, ((src', pID1), RightB):dchain') Demand
              in  Msgs [ m ]
            RightB ->
              let msg = Msg dest src' (token, dchain') (Response val)
              in  Msgs [ msg ]
        Just instrT ->
          error $ "TODO: node dispatch (Response): "++(pprint instrT "")
        Nothing -> ierr $ "no instruction "++(show dest)++" in graph (Response)"

-- | Evaluates a binary constant operator.
cBinOp :: COp -> ValueT -> ValueT -> ValueT
cBinOp CPlus  (VI x) (VI y) = VI (x+y)
cBinOp CMinus (VI x) (VI y) = VI (x-y)
cBinOp CLt    (VI x) (VI y) = VB (x<y)
cBinOp CGt    (VI x) (VI y) = VB (x>y)
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
addJoins ((JD iID (token, ((iID', _), _):dchain) br val):jds) jt =
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
                  (LeftB, RightB) -> Just (iID, bOp, val0, val1)
                  (RightB, LeftB) -> Just (iID, bOp, val1, val0)
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

checkResponseVar :: Plug -> Plug -> (a->a)
checkResponseVar (iID, pID) (src, pID') =
  (if pID /= pID' then
     ierr $ "port="++(show pID')++"!="++(show pID)
   else if src /= iID then
          ierr $ "src="++(show src)++"!="++(show iID)
        else id)

checkResponseCall :: (Int, Int) -> Plug -> Plug -> (a->a)
checkResponseCall (i, i') (iID, pID) (src, pID') =
  if pID /= pID' then
    ierr $ "port="++(show pID')++"!="++(show pID)
  else if i /= i' then
         ierr $ "idx="++(show i')++"!="++(show i)
       else if src /= iID then
              ierr $ "src="++(show src)++"!="++(show iID)
            else id

checkResponseActuals :: PortID -> Int -> (a->a)
checkResponseActuals pID' ls =
  if pID' > ls then
    ierr $ "pID'="++(show pID')++">"++(show ls)
  else id
