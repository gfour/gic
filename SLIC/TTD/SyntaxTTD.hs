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
import SLIC.ITrans.Syntax (QOp)
import SLIC.SyntaxAux
import SLIC.Types

-- * The TTD language

-- | Base values are just integers.
type ValueT = Int

-- | A token (also a LAR ID).
type Token = Int

-- | The unique identifier characterizing every TTD node.
type NodeID = Int

-- | The identifier of a TTD demand-receive port (unique per node).
type PortID = Int

-- | A plug is a pair of a node and a port.
type Plug = (NodeID, PortID)

-- | A demand chain.
type DChain = [Plug]

-- | A message is the tuple (sender, receiver, token, payload, demand chain).
type Message a = (NodeID, NodeID, Token, a, DChain)

-- | A demand is a message without a value.
type Demand = Message ()

-- | A response is a message carrying a value.
type Response = Message ValueT

-- | A TTD instruction.
data InstrT = CallT QOp Plug         -- ^ Call a node using an intensional index.
            | VarT Plug              -- ^ Call a node using the same context.
            | ActualsT [Plug]        -- ^ Intensional actuals() operator.
            | ConT Const [Plug]      -- ^ Built-in operator (including base values).

-- | A node containing an instruction.
type EntryT = (NodeID, InstrT)

-- | A TTD program is a list of unique locations containing instructions.
data ProgT = ProgT [EntryT]

instance PPrint ProgT where
  pprint (ProgT entries) =
    foldDot (\(nID, instrT)->shows nID.(" | "++).pprint instrT.nl) entries

instance PPrint InstrT where
  pprint (CallT qOp plug) = pprint qOp.pprintPlug plug
  pprint (VarT plug) = ("var"++).pprintPlug plug
  pprint (ActualsT plugs) =
    ("actuals("++).insCommIfMore (map pprintPlug plugs).(")"++)
  pprint (ConT (LitInt i) []) = shows i
  pprint (ConT (CN c) plugs) =
    ("["++).pprint c.("]("++).insCommIfMore (map pprintPlug plugs).(")"++)
  pprint _ = ierr "Unknown instruction, no pretty printer available."

pprintPlug :: Plug -> ShowS
pprintPlug (nId, pId) = ("~["++).shows nId.(":"++).shows pId.("]"++)

