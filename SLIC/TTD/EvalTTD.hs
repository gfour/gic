-- | The tagged-token dataflow interpreter.
-- 

module SLIC.TTD.EvalTTD (evalTTD) where

import Prelude hiding (Either(..))
import Data.List (delete)
import Data.Map (Map, fromList, lookup)
import SLIC.AuxFun (ierr, trace2)
import SLIC.Constants (comma)
import SLIC.ITrans.Syntax (QOp(..))
import SLIC.SyntaxAux
import SLIC.TTD.SyntaxTTD
import SLIC.Types (PPrint(pprint), pprintList)

-- | Base values are just integers.
type ValueT = Int

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
data JoinUpdate = AddJoin JoinData | RemoveJoin JoinData JoinData

-- | Evaluates a dataflow program. Takes the ID of the top instruction of the
--   \"result\" definition, and the actual program.
evalTTD :: InstrID -> ProgT -> IO ()
evalTTD resultID (ProgT entries) =
  let entriesTable = Data.Map.fromList entries
      initMsg = Msg (-1) resultID ([], []) Demand
  in  do putStrLn "Running..."
         putStrLn ("Result = "++(show (runLoop entriesTable [] [initMsg])))

-- | Processes the messages until a value response to the root node is found.
runLoop :: ProgT' -> [JoinData] -> [Msg] -> ValueT
runLoop entriesTable joins msgQ@(_:_) =
  let [msg] = take 1 msgQ
      msgs = drop 1 msgQ
  in  case sendMsg entriesTable joins msg of
        (Nothing, jUpdate@(Just _)) ->
          -- The instruction was stuck, rechedule.
          trace2 "Rescheduling..." $
          runLoop entriesTable (updJoins jUpdate joins) (msgs++[msg])
        (Just msgs', jUpdate) ->
          trace2 "Running next cycle..." $
          -- The instruction ran and produced one or more messages.
          let msgsToRoot = filter (\(Msg _ dest _ _)->dest==(-1)) msgs'
          in  case msgsToRoot of
                [] -> runLoop entriesTable (updJoins jUpdate joins) (msgs'++msgs)
                [Msg _ _ _ (Response valT)] -> valT
                _ ->
                  ierr$"Cannot handle root msg:"++(pprintList comma msgsToRoot "")
        (Nothing, Nothing) -> ierr "Stuck instruction generated no activity."
runLoop _ _ [] = ierr "The program ended without a result."

updJoins :: Maybe JoinUpdate -> [JoinData] -> [JoinData]
updJoins Nothing joins = joins
updJoins (Just (AddJoin jd)) joins = jd:joins
updJoins (Just (RemoveJoin jd0 jd1)) joins =
  if (jd0 `elem` joins) && (jd1 `elem` joins) then
    delete jd0 $ delete jd1 $ joins
  else
    ierr $ "One of "++(show [jd0, jd1])++" is not in "++(show joins)

-- | Runs a cycle, i.e. sends a message to a dataflow instruction in the program. 
--   The result is one or more messages to be processed in the next cycle.
--   A join update may also be produced, to update a join structure before the
--   next cycle.
sendMsg :: ProgT' -> [JoinData] -> Msg -> (Maybe [Msg], Maybe JoinUpdate)
sendMsg p _ (Msg src dest (token, dchain) Demand) =
  (Just
  (case Data.Map.lookup dest p of
      Just (CallT (Call (_, i)) (iID, pID)) ->
        [Msg dest iID (i:token, ((src, pID), Single):dchain) Demand]
      Just (VarT (iID, pID)) ->
        [Msg dest iID (token, ((src, pID), Single):dchain) Demand]
      Just (ActualsT plugs) ->
        let i:token' = token
            (iID, pID) = plugs !! i
        in  [Msg dest iID (token', ((src, pID), Single):dchain) Demand]
      Just (ConT (CN _) [(iID0, pID0), (iID1, pID1)]) ->
        [ Msg dest iID0 (token, ((src, pID0), Left):dchain) Demand
        , Msg dest iID1 (token, ((src, pID1), Right):dchain) Demand]
      Just (ConT (LitInt i) []) ->
        [ Msg dest src (token, dchain) (Response i) ]
      Just instrT -> error $ "TODO: node dispatch (Demand): "++(pprint instrT "")
      Nothing     -> ierr $ "no instruction #"++(show dest)++" in graph (Demand)"
   ), Nothing)
sendMsg p joins (Msg src dest (token, dchain) rVal@(Response val)) = 
  let ((src', pID'), br):dchain' = dchain
      updJoin =
        case br of Single -> Nothing
                   _      -> ierr "Unhandled branch in value response."
  in  case Data.Map.lookup dest p of
        Just (CallT (Call (_, i)) (iID, pID)) ->
          let i':token' = token
          in  (if pID /= pID' then
                 ierr $ "port="++(show pID')++"!="++(show pID)
               else if i /= i' then
                      ierr $ "idx="++(show i')++"!="++(show i)
                    else if src /= iID then
                           ierr $ "src="++(show src)++"!="++(show iID)
                         else id)
              (Just [Msg dest src' (token', dchain') rVal], updJoin)
        Just (ActualsT plugs) ->
          let token' = pID':token      -- this assumes pID'==intensional index
              ls = length plugs
          in  (if pID' > ls then
                 ierr $ "pID'="++(show pID')++">"++(show ls)
               else id)
              (Just [Msg dest src' (token', dchain') rVal], updJoin)
        Just (VarT (iID, pID)) ->
          (if pID /= pID' then
             ierr $ "port="++(show pID')++"!="++(show pID)
           else if src /= iID then
                  ierr $ "src="++(show src)++"!="++(show iID)
                else id)
          (Just [Msg dest src' (token, dchain') rVal], updJoin)
        Just (ConT (CN CPlus) [_, _]) ->
          case br of
            Single -> ierr "CPlus has no single dependency"
            _ ->
              case filter (\(iID, _, _, _) -> iID==dest) joins of
                -- No results, the instruction is still blocked.
                [] -> (Nothing, Just (AddJoin (dest, (token, dchain), br, val)))
                -- Single (other) result, the instruction can continue.
                [jd'@(_, color, br', val')] ->
                  let rVal' = case (br, br') of
                                (Left, Right) -> val  + val'
                                (Right, Left) -> val' + val
                                _ -> ierr "Malformed complete join structure."
                      jd = (dest, (token, dchain), br, val)
                      msg' = Msg dest src' (token, dchain') (Response rVal')
                      cHead (tl, (_:dl)) = (tl, dl)
                      cHead (_, []) = ierr "No color head for empty demand chain."
                  in  (if cHead color /= cHead (token, dchain) then
                        ierr $ "Mismatched colors in join: "++
                               (show [cHead color, cHead (token, dchain)])
                      else id)
                      (Just [msg'], Just (RemoveJoin jd jd'))
                js -> ierr $ "Found too much join data: "++(show $ length js)
        Just instrT ->
          error $ "TODO: node dispatch (Response): "++(pprint instrT "")
        Nothing -> ierr $ "no instruction "++(show dest)++" in graph (Response)"

