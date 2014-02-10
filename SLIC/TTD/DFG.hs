-- | Generates a graph for the TTD program in Graphviz format.
-- 

module SLIC.TTD.DFG (generateDFG) where

import Data.Map (filter, toList, union)
import Data.Maybe (catMaybes)
import SLIC.AuxFun (foldDot, ierr, showStrings)
import SLIC.Constants
import SLIC.SyntaxAux
import SLIC.TTD.SyntaxTTD
import SLIC.TTD.ZItoTTD (InstrIDs, builtinInstrIDs, maxBuiltinInstrID)
import SLIC.Types

-- | Generates a Graphviz graph for a TTD program.
generateDFG :: InstrIDs -> ProgT -> ShowS
generateDFG defIDs (ProgT entries) =
  let usedIDs = usedInstrMap entries
  in  ("digraph G {splines=true"++).nl.  -- Another option: splines=ortho
      tab.("{"++).nl.
      foldDot genBuiltinEntryBox (toList usedIDs).
      foldDot genEntryBox entries.
      tab.("}"++).nl.
      foldDot connectEntries entries.
      genLegend (union defIDs usedIDs).
      ("}"++).nl.nl

-- | Generates the legend showing which instruction ID is assigned to a name.
genLegend :: InstrIDs -> ShowS
genLegend nIDs =
  let (nIDs_qns, nIDs_ids) = unzip $ toList nIDs
      makeCells f sl = showStrings " | " $ map f sl
  in  tab.("legend [shape=record, label=\"{ID|"++).
      makeCells show nIDs_ids.("}| {Function|"++).
      makeCells (\x->pprint x "") nIDs_qns.("}\"];"++).nl

-- | Returns the part of the built-in IDs table only for the IDs actually used.
usedInstrMap :: [IEntry] -> InstrIDs
usedInstrMap entries =
  let test nID = if nID > maxBuiltinInstrID then Nothing else Just nID
      findUsed (CallT _ iID)   = [test iID]
      findUsed (VarT iID)      = [test iID]
      findUsed (ActualsT iIDs) = map test iIDs
      findUsed (ConT _ iIDs)   = map test iIDs
      usedBuiltinIDs = catMaybes $ concatMap (findUsed.snd) entries
  in  Data.Map.filter (\i->i `elem` usedBuiltinIDs) builtinInstrIDs

-- | Generates an opaque box for a built-in function.
genBuiltinEntryBox :: (QName, InstrID) -> ShowS
genBuiltinEntryBox (qn, nID) =
  tab.instrName nID.
  (" ["++).("shape=record, style=\"rounded,filled\","++).
  (" color=black, fillcolor=white,"++).
  (" label=\""++).shows nID.(" | Built-in: "++).pprint qn.("\"]"++).nl
  
-- | Generates a box for a TTD instruction entry.
genEntryBox :: IEntry -> ShowS
genEntryBox (nID, instrT) =
  let -- Escape special characters that clash with the 'label' format of Graphviz.
      escape c = ((concatMap escapeChar (pprint c ""))++)
      escapeChar '<' = "\\<"
      escapeChar '>' = "\\>"
      escapeChar '}' = "\\}"
      escapeChar '{' = "\\{"
      escapeChar '|' = "\\|"
      escapeChar c   = [c]
      ports ps = [0..(length ps)-1]
      pField p = (("<"++).portName p.("> ~"++).shows p) ""
      pFields ps = showStrings " | " $ map pField $ ports ps
  in  tab.instrName nID.
      (" ["++).("shape=record, style=\"rounded,filled\","++).
      (" color=black, fillcolor=lightgrey,"++).
      (" label=\"<"++).instrPortName.("> "++).shows nID.(" | "++).      
      (case instrT of
          ConT (CN c) iIDs  -> ("["++).escape c.("] | "++).pFields iIDs
          ConT (LitInt i) []-> shows i 
          ActualsT iIDs     -> (" actuals | "++).pFields iIDs
          CallT qOp iID     -> pprint qOp.(" | "++).pFields [iID]
          VarT iID          -> (" var | "++).pFields [iID]
          _                 -> ierr "genEntryBox: unknown instruction"
      ).("\"]"++).nl

-- | Generates the port name used in a Graphviz record.
portName :: InstrID -> ShowS
portName iID = ("p"++).shows iID

-- | The name of the default firing port in a Graphviz record.
instrPortName :: ShowS
instrPortName = ("p"++)

-- | Generates a name for a TTD instruction.
instrName :: InstrID -> ShowS
instrName iID = ("instr_"++).shows iID

-- | Connects instructions.
connectEntries :: IEntry -> ShowS
connectEntries (nID, CallT _ pID)   = createEdge nID (pID, 0)
connectEntries (nID, VarT pID)      = createEdge nID (pID, 0)
connectEntries (nID, ActualsT pIDs) = foldDot (createEdge nID) $ zip pIDs [0..]
connectEntries (nID, ConT _ pIDs)   = foldDot (createEdge nID) $ zip pIDs [0..]

-- | Creates an edge between two instructions. The direction of the arrow is
--   that of the demand messages, the response messages are assumed to have
--   the opposite direction.
createEdge :: InstrID -> (InstrID, Int) -> ShowS
createEdge n1 (n2, p2) =
  tab.instrName n1.(":"++).portName p2.(" -> "++).
  instrName n2.(":"++).instrPortName.semi.nl
