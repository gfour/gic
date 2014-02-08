-- | The language of the Tagged-Token Dataflow back-end.
-- 
--   The TTD back-end compiles the intensional program to a dataflow graph.
--   The execution model follows closely the ideas of tagged-token dataflow
--   architectures (where contexts are tags), aided by a distributed warehouse 
--   to do value memoization.
-- 
--   The intensional program is broken down to dataflow instructions. No nesting
--   is allowed; for example, a 0-order expression that has two subexpressions
--   will be represented as three intructions, with the first depending on the
--   other two.
--   
--   An instruction is a node in the dataflow graph and is connected to other
--   instructions. If an instruction depends on N other instructions and M other
--   instructions depend on it, it has N+1 ports: N ports for the dependencies
--   ('Plug' outlets) and one \"firing\" port multiplexing input from the M
--   dependent instructions.
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

-- | The unique identifier characterizing every instruction.
type InstrID = Int

-- | The identifier of a demand-receive port (unique per instruction).
type PortID = Int

-- | A plug is a pair of an instruction ID and a port.
type Plug = (InstrID, PortID)

-- | A demand chain.
type DChain = [Plug]

-- | A message is the tuple (sender, receiver, token, payload, demand chain).
type Message a = (InstrID, InstrID, Token, a, DChain)

-- | A demand is a message without a value.
type Demand = Message ()

-- | A response is a message carrying a value.
type Response = Message ValueT

-- | A dataflow instruction.
data InstrT = CallT QOp Plug     -- ^ Call instruction using intensional index.
            | VarT Plug          -- ^ Call instruction using current context.
            | ActualsT [Plug]    -- ^ Intensional /actuals/ operator.
            | ConT Const [Plug]  -- ^ Built-in operator (including base values).

-- | An instruction entry is an instruction labelled by an ID.
type IEntryT = (InstrID, InstrT)

-- | A dataflow program is a list of instruction entries.
data ProgT = ProgT [IEntryT]

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

-- | Pretty printer for plugs.
pprintPlug :: Plug -> ShowS
pprintPlug (nId, pId) = ("~["++).shows nId.(":"++).shows pId.("]"++)
