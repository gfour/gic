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
--   and one \"firing\" port multiplexing input from the M dependent instructions.
-- 

module SLIC.TTD.SyntaxTTD where

import SLIC.AuxFun (foldDot, ierr, insCommIfMore)
import SLIC.Constants (nl)
import SLIC.ITrans.Syntax (QOp)
import SLIC.SyntaxAux
import SLIC.Types

-- * The TTD language

-- | The unique identifier characterizing every instruction.
type InstrID = Int

-- | A dataflow instruction.
data InstrT = CallT QOp InstrID     -- ^ Call instruction using intensional index.
            | VarT InstrID          -- ^ Call instruction using current context.
            | ActualsT [InstrID]    -- ^ Intensional /actuals/ operator.
            | ConT Const [InstrID]  -- ^ Built-in operator (including base values).

-- | An instruction entry is an instruction labelled by an ID.
type IEntry = (InstrID, InstrT)

-- | A dataflow program is a list of instruction entries.
data ProgT = ProgT [IEntry]

instance PPrint ProgT where
  pprint (ProgT entries) =
    foldDot (\(nID, instrT)->shows nID.(" | "++).pprint instrT.nl) entries

instance PPrint InstrT where
  pprint (CallT qOp iID) = pprint qOp.pprintInstrPtr iID
  pprint (VarT iID) = ("var"++).pprintInstrPtr iID
  pprint (ActualsT iIDs) =
    ("actuals("++).insCommIfMore (map pprintInstrPtr iIDs).(")"++)
  pprint (ConT (LitInt i) []) = shows i
  pprint (ConT (CN c) iIDs) =
    ("["++).pprint c.("]("++).insCommIfMore (map pprintInstrPtr iIDs).(")"++)
  pprint _ = ierr "Unknown instruction, no pretty printer available."

-- | Pretty printer for pointers to instructions.
pprintInstrPtr :: InstrID -> ShowS
pprintInstrPtr iId = ("~["++).shows iId.("]"++)
