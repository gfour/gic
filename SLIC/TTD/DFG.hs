-- | The Graphviz-based dataflow graph generator.

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
  foldDot connectBoxes entries.
  ("}"++).nl.nl
  
genEntryBox :: EntryT -> ShowS
genEntryBox (nID, instrT) =
  tab.nodeName nID.(" ["++).nl.
  tab.tab.("style=filled,"++).nl.
  tab.tab.("color=lightgrey,"++).nl.
  -- tab.tab.("node [style=filled, color=white];"++).nl.
  tab.tab.("label=\""++).shows nID.(": "++).pprint instrT.("\""++).nl.
  tab.("]"++).nl

nodeName :: NodeID -> ShowS
nodeName nID = ("node_"++).shows nID

connectBoxes :: EntryT -> ShowS
connectBoxes (nID, CallT _ nID')    = createEdge nID nID'
connectBoxes (nID, VarT nID')       = createEdge nID nID'
connectBoxes (nID, ActualsT nIDs)   = foldDot (createEdge nID) nIDs
connectBoxes (nID, ConT _ nIDs)     = foldDot (createEdge nID) nIDs

createEdge :: NodeID -> NodeID -> ShowS
createEdge n1 n2 = tab.tab.nodeName n1.(" -> "++).nodeName n2.semi.nl
