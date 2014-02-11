-- | Generates a graph for the TTD program in Graphviz format.
-- 

module SLIC.TTD.DFG (generateDFG) where

import Data.Map (filter, toList, union)
import Data.Maybe (catMaybes)
import SLIC.AuxFun (foldDot, ierr)
import SLIC.Constants
import SLIC.SyntaxAux
import SLIC.TTD.SyntaxTTD
import SLIC.TTD.ZItoTTD (InstrIDs, builtinInstrIDs, maxBuiltinInstrID)
import SLIC.Types

-- * Blue theme.
-- | Instruction background.
instrBG     :: ShowS ; instrBG = ("white"++)
instrPortBG :: ShowS ; instrPortBG = ("cyan2"++)
fieldBG     :: ShowS ; fieldBG = ("deepskyblue"++)

-- Grayscale theme.
-- instrBG = ("white"++)
-- instrPortBG = ("lightgrey"++)
-- fieldBG = ("grey"++)

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
  let makeRow (f, nID) =
        ("<TR>"++).
        ("<TD BGCOLOR=\""++).instrPortBG.("\">"++).shows nID.("</TD>"++).
        ("<TD>"++).pprint f.("</TD>"++).
        ("</TR>"++)
  in  tab.("legend [shape=\"plaintext\", label=<"++).nl.
      ("<TABLE BORDER=\"1\"><TR>"++).
      ("<TD BGCOLOR=\""++).instrPortBG.("\">Instruction ID</TD>"++).
      ("<TD>Variable</TD></TR>"++).nl.
      foldDot makeRow (toList nIDs).
      ("</TABLE>>];"++).nl

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
      escapeChar '<' = "&lt;"
      escapeChar '>' = "&gt;"
      -- The following escape codes are only valid for the "record" style.
      -- escapeChar '<' = "\\<"
      -- escapeChar '>' = "\\>"
      -- escapeChar '}' = "\\}"
      -- escapeChar '{' = "\\{"
      -- escapeChar '|' = "\\|"
      escapeChar c   = [c]
      ports ps = [0..(length ps)-1]
      pField p = ("<TD BGCOLOR=\""++).fieldBG.("\" PORT=\""++).portName p.("\">"++).shows p.("</TD>"++)
      pFields ps = foldDot pField $ ports ps
      td s = ("<TD BGCOLOR=\""++).instrBG.("\">"++).s.("</TD>"++)
  in  tab.instrName nID.
      (" ["++).("shape=\"plaintext\", style=\"rounded\","++).
      (" color=black, fillcolor=lightgrey,"++).
      (" label=<"++).nl.
      ("<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"2\" CELLSPACING=\"0\"><TR>"++).
      ("<TD BORDER=\"0\" BGCOLOR=\""++).instrPortBG.
      ("\" PORT=\""++).instrPortName.("\">"++).shows nID.(":</TD>"++).
      (case instrT of
          ConT (CN c) iIDs  -> td (("["++).escape c.("]"++)).pFields iIDs
          ConT (LitInt i) []-> td (shows i)
          ActualsT iIDs     -> td ("actuals"++).pFields iIDs
          CallT qOp iID     -> td (pprint qOp).pFields [iID]
          VarT iID          -> td ("var"++).pFields [iID]
          _                 -> ierr "genEntryBox: unknown instruction"
      ).
      ("</TR></TABLE>>];"++).nl

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
