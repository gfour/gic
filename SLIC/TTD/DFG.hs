-- | Generates a graph for the TTD program in Graphviz format.
-- 

module SLIC.TTD.DFG (generateDFG) where

import SLIC.AuxFun (foldDot)
import SLIC.Constants
import SLIC.TTD.SyntaxTTD
import SLIC.Types

-- | Generates a Graphviz graph for a TTD program.
generateDFG :: ProgT -> ShowS
generateDFG (ProgT entries) =
  ("digraph G {"++).nl.
  tab.("{"++).nl.
  foldDot genEntryBox entries.
  tab.("}"++).nl.
  foldDot connectEntries entries.
  ("}"++).nl.nl
  
-- | Generates a box for a TTD instruction entry.
genEntryBox :: EntryT -> ShowS
genEntryBox (nID, instrT) =
  tab.instrName nID.
  (" ["++).("shape=box, style=\"rounded,filled\","++).
  (" color=black, fillcolor=lightgrey,"++).
  (" label=\""++).shows nID.(": "++).pprint instrT.("\"]"++).nl

-- | Generates a name for a TTD instruction.
instrName :: NodeID -> ShowS
instrName nID = ("instr_"++).shows nID

-- | Connects instructions.
connectEntries :: EntryT -> ShowS
connectEntries (nID, CallT _ nID')    = createEdge nID nID'
connectEntries (nID, VarT nID')       = createEdge nID nID'
connectEntries (nID, ActualsT nIDs)   = foldDot (createEdge nID) nIDs
connectEntries (nID, ConT _ nIDs)     = foldDot (createEdge nID) nIDs

-- | Creates an edge between two instructions. The direction of the arrow is
--   that of the demand messages, the response messages are assumed to have
--   the opposite direction.
createEdge :: NodeID -> NodeID -> ShowS
createEdge n1 n2 = tab.tab.instrName n1.(" -> "++).instrName n2.semi.nl
