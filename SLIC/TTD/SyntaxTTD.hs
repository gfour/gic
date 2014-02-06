-- | The language of the Tagged-Token Dataflow back-end.
-- 
--   The TTD back-end compiles the intensional program to a dataflow graph.
--   The execution model follows closely the ideas of tagged-token dataflow
--   architectures (where contexts are tags), aided by a distributed warehouse 
--   to do value memoization.
-- 
--   The intensional program is broken down to dataflow instructions, each
--   being a labelled equivalent to a simple ZOIL expression.
-- 

module SLIC.TTD.SyntaxTTD where

import SLIC.AuxFun (foldDot, ierr, insCommIfMore)
import SLIC.Constants (nl)
import SLIC.SyntaxAux
import SLIC.Types

-- * The TTD language

-- | Base values are just integers.
type ValueT = Int

-- | A token (also a LAR ID).
type Token = Int

-- | The unique identifier characterizing every TTD node.
type NodeID = Int

-- | TODO: Is this needed?
type Loc = NodeID

-- | A demand chain.
type DChain = [Loc]

-- | A message is the tuple (sender, receiver, token, payload, demand chain).
type Message a = (NodeID, NodeID, Token, a, DChain)

-- | A demand is a message without a value.
type Demand = Message ()

-- | A response is a message carrying a value.
type Response = Message ValueT

-- | A TTD instruction.
data InstrT = CallT IIndex NodeID   -- ^ Call a node using an intensional index.
            | VarT NodeID            -- ^ Call a node using the same context.
            | ActualsT [NodeID]
            | ConT Const [NodeID]

-- | A node containing an instruction.
type EntryT = (NodeID, InstrT)

-- | A TTD program is a list of unique locations containing instructions.
data ProgT = ProgT [EntryT]

instance PPrint ProgT where
  pprint (ProgT entries) =
    foldDot (\(nID, instrT)->pprintNodeID nID.(" : "++).pprint instrT.nl) entries

instance PPrint InstrT where
  pprint (CallT iidx nID) = ("Call-"++).pprintIdx iidx.("->"++).pprintNodeID nID
  pprint (VarT nID) = ("->"++).pprintNodeID nID
  pprint (ActualsT nIDs) =
    ("Actuals["++).insCommIfMore (map pprintNodeID nIDs).("]"++)
  pprint (ConT (LitInt i) []) = shows i
  pprint (ConT (CN c) nIDs) =
    ("Op-"++).pprint c.("("++).insCommIfMore (map pprintNodeID nIDs).(")"++)
  pprint _ = ierr "Unknown instruction, no pretty printer available."

pprintNodeID :: NodeID -> ShowS
pprintNodeID nId = ("~"++).shows nId
