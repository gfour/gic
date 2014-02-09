-- | The tagged-token dataflow interpreter.
-- 

module SLIC.TTD.EvalTTD (evalTTD) where

import Prelude hiding (Either(..))
import Data.List (delete)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (catMaybes)
import SLIC.AuxFun (ierr, trace2)
import SLIC.Constants (comma)
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
data Branch = Left | Right | Single
            deriving (Eq, Show)

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
type ProgT' = Map InstrID InstrT

-- | A join structure for collecting results from fork-join operations.
type JoinData = (InstrID, Color, Branch, ValueT)
data JoinUpdate = AddJoin JoinData | RemoveJoin JoinData

-- | Evaluates a dataflow program. Takes the ID of the top instruction of the
--   \"result\" definition, and the actual program.
evalTTD :: InstrID -> ProgT -> IO ()
evalTTD resultID (ProgT entries) =
  let entriesTable = Data.Map.fromList entries
      initMsg = Msg (-1) resultID ([], []) Demand
  in  do putStrLn "Running..."
         putStrLn ("Result = "++(pprint (runLoop entriesTable [] [initMsg])) "")

-- | Processes the messages until a value response to the root node is found.
runLoop :: ProgT' -> [JoinData] -> [Msg] -> ValueT
runLoop entriesTable joins msgQ@(_:_) =
  let msgsToRun = take numWorkers msgQ
      msgsNext = drop numWorkers msgQ
      numWorkers = 1
      -- Run the workers and gather the results.
      runResults = map (sendMsg entriesTable joins) msgsToRun
      -- Extract all messages and join updates.
      (mMessages, mJUpdates) = unzip runResults
      newMessages = concat $ catMaybes mMessages
      newJUpdates = catMaybes $ mJUpdates
      -- Update the joins structure.
      joins' = updJoins newJUpdates joins
      -- Gather messages to root.
      msgsToRoot = filter (\(Msg _ dest _ _)->dest==(-1)) newMessages
  in  -- trace2 "-------------------------------" $ 
      case msgsToRoot of
        [] ->
          -- trace2 "==== Running next cycle... ====" $
          -- trace2 (pprintList (", "++) msgsToRun "") $
          -- trace2 ("Joins are now: "++(show joins')) $
          runLoop entriesTable joins' (msgsNext++newMessages)
        [Msg _ _ _ (Response valT)] ->
          -- trace2 "==== Program end ====" $
          -- trace2 (show valT) $
          valT
        _ -> ierr $ "Cannot handle root msg:"++(pprintList comma msgsToRoot "")
runLoop _ _ [] = ierr "The program ended without a result."

-- | Updates the joins information between cycles. Addition and removal of joins
--   can happen in any order, since there is no dependency between the join
--   updates generated in a single cycle.
updJoins :: [JoinUpdate] -> [JoinData] -> [JoinData]
updJoins [] joins = joins
updJoins ((AddJoin jd):jUpds) joins =
  -- trace2 ("Adding join: "++(show jd)) $
  updJoins jUpds (jd:joins)
updJoins ((RemoveJoin jd):jUpds) joins =
  if jd `elem` joins then
    updJoins jUpds (delete jd joins)
  else
    ierr $ (show jd)++" is not in "++(show joins)

-- | Runs a cycle, i.e. sends a message to a dataflow instruction in the program. 
--   The result is one or more messages to be processed in the next cycle.
--   A join update may also be produced, to update a join structure before the
--   next cycle.
sendMsg :: ProgT' -> [JoinData] -> Msg -> (Maybe [Msg], Maybe JoinUpdate)
sendMsg p _ (Msg src dest (token, dchain) Demand) =
  (Just
  (case Data.Map.lookup dest p of
      Just (CallT (Call (_, i)) (iID, pID)) ->
        [ Msg dest iID (i:token, ((src, pID), Single):dchain) Demand ]
      Just (VarT (iID, pID)) ->
        [ Msg dest iID (token, ((src, pID), Single):dchain) Demand ]
      Just (ActualsT plugs) ->
        let i:token' = token
            (iID, pID) = plugs !! i
        in  [ Msg dest iID (token', ((src, pID), Single):dchain) Demand ]
      Just (ConT (CN _) [(iID0, pID0), (iID1, pID1)]) ->
        [ Msg dest iID0 (token, ((src, pID0), Left ):dchain) Demand
        , Msg dest iID1 (token, ((src, pID1), Right):dchain) Demand ]
      Just (ConT (LitInt i) []) ->
        [ Msg dest src (token, dchain) (Response (VI i)) ]
      Just (ConT (CN CIf) [(iID, pID), _, _]) ->
        [ Msg dest iID (token, ((src, pID), Left):dchain) Demand ]
      Just instrT -> error $ "TODO: node dispatch (Demand): "++(pprint instrT "")
      Nothing     -> ierr $ "no instruction #"++(show dest)++" in graph (Demand)"
   ), Nothing)
sendMsg p joins msg2@(Msg src dest (token, dchain) rVal@(Response val)) = 
  let ((src', pID'), br):dchain' = dchain      -- pop demand chain
      updJoin =
        case br of Single -> Nothing
                   _      -> ierr "Unhandled branch in value response."
      isPending (iID, color, _, _) =
        (iID==dest) && (cHead color == cHead (token, dchain))
  in  case Data.Map.lookup dest p of
        Just (CallT (Call (_, i)) (iID, pID)) ->
          let i':token' = token
          in  (checkResponseCall (i, i') (iID, pID) (src, pID'))
              (Just [ Msg dest src' (token', dchain') rVal ], updJoin)
        Just (ActualsT plugs) ->
          let token' = pID':token      -- this assumes pID'==intensional index
          in  (checkResponseActuals pID' (length plugs))
              (Just [ Msg dest src' (token', dchain') rVal ], updJoin)
        Just (VarT (iID, pID)) ->
          (checkResponseVar (iID, pID) (src, pID'))
          (Just [ Msg dest src' (token, dchain') rVal ], updJoin)
        Just (ConT (CN c) [_, _]) ->
          case br of
            Single -> ierr $(pprint c " has no single dependency ")++(pprint msg2 "")
            _ ->
              case filter isPending joins of
                -- No other results, the instruction is still blocked.
                [] -> (Nothing, Just (AddJoin (dest, (token, dchain), br, val)))
                -- Single (other) result, the instruction can continue.
                [jd'@(_, color', br', val')] ->
                  let rVal' = case (br, br') of
                                (Left, Right) -> cBinOp c val  val'
                                (Right, Left) -> cBinOp c val' val
                                _ -> ierr $ "Malformed complete join structure: "++(show (jd, jd'))++"\nThe joins table is: "++(show joins)
                      jd = (dest, (token, dchain), br, val)
                      msg' = Msg dest src' (token, dchain') (Response rVal')
                  in  (checkJColors color' (token, dchain))
                      (Just [msg'], Just (RemoveJoin jd'))
                js -> ierr $ "Found too much join data: "++(show $ length js)
        Just (ConT (CN CIf) [_, (iID0, pID0), (iID1, pID1)]) ->
          case br of
            Single -> ierr "'if' has no single dependency"
            Left ->
              let VB cond = val
                  m = if cond then
                        Msg dest iID0 (token, ((src', pID0), Right):dchain') Demand
                      else
                        Msg dest iID1 (token, ((src', pID1), Right):dchain') Demand
              in  (Just [ m ], Nothing)
            Right ->
              let msg = Msg dest src' (token, dchain') (Response val)
              in  (Just [ msg ], Nothing)
        Just instrT ->
          error $ "TODO: node dispatch (Response): "++(pprint instrT "")
        Nothing -> ierr $ "no instruction "++(show dest)++" in graph (Response)"

-- | Evaluates a binary constant operator.
cBinOp :: COp -> ValueT -> ValueT -> ValueT
cBinOp CPlus  (VI x) (VI y) = VI (x+y)
cBinOp CMinus (VI x) (VI y) = VI (x-y)
cBinOp CLt    (VI x) (VI y) = VB (x<y)
cBinOp c' _ _ = ierr $ "Unhandled cBinOp: "++(pprint c' "")

cHead :: Color -> Color
cHead (tl, (_:dl)) = (tl, dl)
cHead (_, []) = ierr "No color head for empty demand chain."
                      
-- | Check for mismatched colors in join.
checkJColors :: Color -> Color -> (a->a)
checkJColors c1 c2 = 
  let ch1 = cHead c1
      ch2 = cHead c2
  in  if ch1 /= ch2 then
        ierr $ "Mismatched colors in join:\n"++
        (show ch1)++"\n"++(show ch2)
      else id

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
