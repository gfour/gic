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

import Data.List (elemIndex)
import Data.Map (Map, fromList)
import SLIC.AuxFun (foldDot, ierr, insCommIfMore)
import SLIC.Constants (nl, tab)
import SLIC.ITrans.Syntax (QOp)
import SLIC.SyntaxAux
import SLIC.Types

-- * The TTD language

-- | The unique identifier characterizing every instruction.
type InstrID = Int

-- | The actual values of formals in all program locations.
type Acts = [(IIndex, InstrID)]

-- | A dataflow instruction.
data InstrT = CallT QOp InstrID        -- ^ call instruction with intensional index
            | VarT InstrID             -- ^ call instruction using current context
            | BVarT InstrID CaseLoc    -- ^ call instruction using nested context
            | ActualsT Acts            -- ^ intensional /actuals/ operator
            | ConT Const [InstrID]     -- ^ built-in operator (including values)
            | CaseT CaseLoc InstrID [PatT] -- ^ pattern matching expression
            | ConstrT CstrName         -- ^ constructor

-- | A pattern branch contains a constructor and an instruction dependency.
data PatT = PatT CstrName InstrID

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
  pprint (BVarT iID (Just d, _)) =
    ("bvar"++).pprintInstrPtr iID.("{"++).shows d.("}"++)
  pprint (ActualsT acts) =
    let aux (iidx, iID) = ("({"++).pprintIdx iidx.("}: "++).
                          pprintInstrPtr iID.(")"++).nl
    in  ("actuals:"++).nl.foldDot aux acts
  pprint (ConT (LitInt i) []) = shows i
  pprint (ConT (CN c) iIDs) =
    ("["++).pprint c.("]("++).insCommIfMore (map pprintInstrPtr iIDs).(")"++)
  pprint (ConstrT c) = pprintTH c
  pprint (CaseT _ iID pats) =
    let pprintPat (PatT cP iIDP) = tab.pprintTH cP.(" -> "++).pprintInstrPtr iIDP.nl
    in  ("case "++).pprintInstrPtr iID.(" of"++).nl.
        foldDot pprintPat pats
  pprint _ = ierr "Unknown instruction, no pretty printer available."

-- | Pretty printer for pointers to instructions.
pprintInstrPtr :: InstrID -> ShowS
pprintInstrPtr iId = ("~["++).shows iId.("]"++)

-- | Returns the position in the actuals list pointed to by an index.
calcIdxBranch :: IIndex -> Acts -> Int
calcIdxBranch iidx acts =
  case elemIndex iidx (map fst acts) of
    Just i  -> i
    Nothing -> ierr $ "calcIdxBranch: no index "++(pprintIdx iidx "")

-- | Program representation with a map for fast instruction lookup.
type ProgT' = Map InstrID InstrT

-- | Generates a ProgT' for fast lookup.
mkProgT' :: ProgT -> ProgT'
mkProgT' (ProgT entries) = fromList entries
