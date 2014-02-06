-- | Generates a graph for the TTD program in Graphviz format.
-- 

module SLIC.TTD.DFG (generateDFG) where

import Data.Map (filter, toList)
import Data.Maybe (catMaybes)
import SLIC.AuxFun (foldDot)
import SLIC.Constants
import SLIC.TTD.SyntaxTTD
import SLIC.TTD.ZItoTTD (builtinNodeIDs, maxBuiltinNodeID)
import SLIC.Types

-- | Generates a Graphviz graph for a TTD program.
generateDFG :: ProgT -> ShowS
generateDFG (ProgT entries) =
  ("digraph G {"++).nl.
  tab.("{"++).nl.
  genBuiltinEntryBoxes entries.
  foldDot genEntryBox entries.
  tab.("}"++).nl.
  foldDot connectEntries entries.
  ("}"++).nl.nl

-- | Generates opaque boxes for the built-in functions used by the program.
genBuiltinEntryBoxes :: [EntryT] -> ShowS
genBuiltinEntryBoxes entries =
  let test nID = if nID > maxBuiltinNodeID then Nothing else Just nID
      findUsed (CallT _ (nID, _)) = [test nID]
      findUsed (VarT (nID, _))    = [test nID]
      findUsed (ActualsT plugs)   = map (test.fst) plugs
      findUsed (ConT _ plugs)     = map (test.fst) plugs
      usedBuiltinIDs = catMaybes $ concatMap (findUsed.snd) entries
      usedNodeMap = Data.Map.filter (\i->i `elem` usedBuiltinIDs) builtinNodeIDs
  in  foldDot genBuiltinEntryBox (toList usedNodeMap)

-- | Generates a box for a built-in block.
genBuiltinEntryBox :: (QName, NodeID) -> ShowS
genBuiltinEntryBox (qn, nID) =
  tab.instrName nID.
  (" ["++).("shape=record, style=\"rounded,filled\","++).
  (" color=black, fillcolor=white,"++).
  (" label=\""++).shows nID.(" | Built-in: "++).pprint qn.("\"]"++).nl
  
-- | Generates a box for a TTD instruction entry.
genEntryBox :: EntryT -> ShowS
genEntryBox (nID, instrT) =
  let -- Escape special characters that clash with the 'label' format of Graphviz.
      instrLabel = concatMap escapeChar (pprint instrT "")
      escapeChar '<' = "\\<"
      escapeChar '>' = "\\>"
      escapeChar '}' = "\\}"
      escapeChar '{' = "\\{"
      escapeChar '|' = "\\|"
      escapeChar c   = [c]
  in  tab.instrName nID.
      (" ["++).("shape=record, style=\"rounded,filled\","++).
      (" color=black, fillcolor=lightgrey,"++).
      (" label=\""++).shows nID.(" | "++).(instrLabel++).("\"]"++).nl

-- | Generates a name for a TTD instruction.
instrName :: NodeID -> ShowS
instrName nID = ("instr_"++).shows nID

-- | Connects instructions.
connectEntries :: EntryT -> ShowS
connectEntries (nID, CallT _ pID)   = createEdge nID pID
connectEntries (nID, VarT pID)      = createEdge nID pID
connectEntries (nID, ActualsT pIDs) = foldDot (createEdge nID) pIDs
connectEntries (nID, ConT _ pIDs)   = foldDot (createEdge nID) pIDs

-- | Creates an edge between two instructions. The direction of the arrow is
--   that of the demand messages, the response messages are assumed to have
--   the opposite direction.
createEdge :: NodeID -> Plug -> ShowS
createEdge n1 (n2, p2) =
  tab.instrName n1.(" -> "++).instrName n2.
  ("[label=\""++).shows p2.("\"]"++).semi.nl
