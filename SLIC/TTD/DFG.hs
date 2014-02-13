-- | Generates a graph for the TTD program in Graphviz format.
-- 

module SLIC.TTD.DFG (generateDFG) where

import Data.List ((\\), sortBy)
import qualified Data.Map as M (filter, filterWithKey, lookup, toList, union)
import qualified Data.Set as S (Set, difference, filter, fromList, member, toList)
import SLIC.AuxFun (foldDot, ierr)
import SLIC.Constants
import SLIC.SyntaxAux
import SLIC.TTD.SyntaxTTD
import SLIC.TTD.ZItoTTD (InstrIDs, VarIDs, builtinInstrIDs, builtinVarIDs,
                         maxBuiltinInstrID, mkVarIDs)
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

-- | Generates a Graphviz graph for a TTD program. Takes the flags that control
--   the kind of graph to crate (display instruction IDs, show the warehouse),
--   the mapping of variable definitions to instruction IDs, and the dataflow
--   program to display. Generates the corresponding graph in Graphviz syntax.
generateDFG :: (Bool, Bool) -> InstrIDs -> InstrID -> ProgT -> ShowS
generateDFG (showInstrID, showWH) defIDs resultID p@(ProgT entries) =
  let p' = mkProgT' p
      usedIDs = S.fromList $ (reachableInstrs p' (mkVarIDs defIDs) []) resultID
      isReachableID iID = S.member iID usedIDs
      reachableIDs = M.filter isReachableID (M.union builtinInstrIDs defIDs)
      vIDs  = mkVarIDs reachableIDs
      builtins = filter (isReachableID.snd) $ M.toList $ 
                 M.filterWithKey (\k _->isBuiltinFunc k) builtinInstrIDs
      entries' = filter (isReachableID.fst) entries      
  in  ("digraph G {splines=true"++).nl.  -- Another option: splines=ortho
      tab.("{"++).nl.
      foldDot genBuiltinEntryBox builtins.
      foldDot (genEntryBox showInstrID vIDs) entries'.
      genUnreachableBoxes showInstrID vIDs usedIDs entries.
      tab.("}"++).nl.
      foldDot (connectEntries showWH) entries'.
      (if showWH then genWH else id).
      genLegend reachableIDs.
      ("}"++).nl.nl

-- | Generates the warehouse node.
genWH :: ShowS
genWH = ("warehouse [shape=house, style=\"filled\", color=black, fillcolor=\""++).
        instrPortBG.("\", label=\"Warehouse\"];"++).nl

-- | Generates the legend showing which instruction ID is assigned to a name.
genLegend :: InstrIDs -> ShowS
genLegend defIDs =
  let makeRow (f, iID) =
        ("<TR>"++).
        ("<TD BGCOLOR=\""++).instrPortBG.("\">"++).shows iID.("</TD>"++).
        ("<TD>"++).pprint f.("</TD>"++).
        ("</TR>"++)
      sorter (_, iID0) (_, iID1) = compare iID0 iID1
  in  tab.("legend [shape=\"plaintext\", label=<"++).nl.
      ("<TABLE BORDER=\"1\"><TR>"++).
      ("<TD BGCOLOR=\""++).instrPortBG.("\">Instruction ID</TD>"++).
      ("<TD>Variable</TD></TR>"++).nl.
      foldDot makeRow (sortBy sorter $ M.toList defIDs).
      ("</TABLE>>];"++).nl

-- | A reachability analysis is needed so that the generated graph only shows
--   the instructions reachable from the root node. Takes the dataflow program,
--   the list of already visited instructions, and the current instruction ID
--   that is checked. Returns the list of reachable instructions from this point.
reachableInstrs :: ProgT' -> VarIDs -> [InstrID] -> InstrID -> [InstrID]
reachableInstrs p vIDs visited instrID =
  let instr = M.lookup instrID p
      iIDs = case instr of
               Just (CallT _ iID)   -> [iID]
               Just (VarT iID)      -> [iID]
               Just (BVarT iID _)   -> [iID]
               Just (ActualsT acts) -> map snd acts
               Just (ConT _ iIDs')  -> iIDs'
               Just (CaseT _ iID ps)-> iID:(idsOfPats ps)
               Just (ConstrT _)     -> []
               Nothing ->
                 if instrID > maxBuiltinInstrID then
                   -- This is unreachable code, ignore. Example: empty
                   -- actuals() definitions that have been eliminated but
                   -- are referenced from other remaining actuals().
                   []
                 else
                   -- This is a built-in instruction, needs special treatment.
                   reachableIDsOfBuiltin instrID
      newIDs = (\\) iIDs visited
      visited' = visited ++ newIDs
  in  [instrID]++newIDs++(concatMap (reachableInstrs p vIDs visited') newIDs)

-- | Given the ID of a built-in instruction, finds the IDs of its formals, if it
--   is a function, otherwise returns the empty list.
reachableIDsOfBuiltin :: InstrID -> [InstrID]
reachableIDsOfBuiltin instrID =
  let qn = case M.lookup instrID builtinVarIDs of
             Just qn' -> qn'
             Nothing  -> ierr $ "Built-in not found: #"++(show instrID)
      fs = case M.lookup qn builtinFuncSigs of
             Just fs' -> fs'
             Nothing  -> []
      aux v = case M.lookup v builtinInstrIDs of
                Just iID -> iID
                Nothing  -> ierr $ "Unknown built-in: "++(pprint v "")
  in  map aux fs

-- | Generates an opaque box for a built-in function.
genBuiltinEntryBox :: (QName, InstrID) -> ShowS
genBuiltinEntryBox (qn, iID) =
  tab.instrName iID.
  (" ["++).("shape=record, style=\"rounded,filled\","++).
  (" color=black, fillcolor=white,"++).
  (" label=\"<"++).instrPortName.("> "++).shows iID.
  (" | Built-in: "++).pprint qn.("\"]"++).nl
  
-- | Generates a box for a TTD instruction entry. Takes a flag to control if the
--   instruction IDs are displayed, the mapping of variable names to IDs, and
--   the instruction entry for which it will create the box. Generates Graphviz
--   syntax for the instruction entry.
genEntryBox :: Bool -> VarIDs -> IEntry -> ShowS
genEntryBox showInstrID vIDs (nID, instrT) =
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
      -- Enumerate ports starting from c.
      ports c ps = [c..(length ps)-1+c]
      pField p = ("<TD BGCOLOR=\""++).fieldBG.("\" PORT=\""++).portName p.("\">"++).shows p.("</TD>"++)
      pFields c ps = foldDot pField $ ports c ps
      td s = ("<TD BGCOLOR=\""++).instrBG.("\">"++).s.("</TD>"++)
  in  tab.instrName nID.
      (" ["++).("shape=\"plaintext\", style=\"rounded\","++).
      (" color=black, fillcolor=lightgrey,"++).
      (" label=<"++).nl.
      (mkInstrLabel showInstrID vIDs nID
       (case instrT of
           ConT (CN c) iIDs      -> td (("["++).escape c.("]"++)).pFields 0 iIDs
           ConT (LitInt i) []    -> td (shows i)
           ActualsT acts         -> td ("actuals"++).pFields 0 (map snd acts)
           CallT qOp iID         -> td (pprint qOp).pFields 0 [iID]
           VarT iID              -> td ("var"++).pFields 0 [iID]
           BVarT iID (Just d, _) -> td (("bvar{"++).shows d.("}"++)).pFields 0 [iID]
           CaseT _ iID patsT     -> td ("case"++).pFields 0 [iID].td ("of"++).
                                    pFields 1 (idsOfPats patsT)
           ConstrT c             -> td (pprint c)          
           _                     -> ierr $ "genEntryBox: unknown instruction: "++
                                    (pprint instrT "")
       )).(">];"++).nl

-- | Generates the label of an instruction, using HTML table syntax for Graphviz.
mkInstrLabel :: Bool -> VarIDs -> InstrID -> ShowS -> ShowS
mkInstrLabel showInstrID vIDs nID s =
  ("<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"2\" CELLSPACING=\"0\">"++).
  ("<TR><TD BORDER=\"0\" BGCOLOR=\""++).instrPortBG.
  ("\" PORT=\""++).instrPortName.("\">"++). 
  (if showInstrID then shows nID else id).
  (case M.lookup nID vIDs of
      Just qn -> ("("++).pprint qn.(")"++) ; Nothing -> id).
  (if showInstrID then (":"++) else (" "++)).
  ("</TD>"++).
  s.
  ("</TR></TABLE>"++)

-- | Generates the port name of an instruction.
portName :: InstrID -> ShowS
portName iID = ("p"++).shows iID

-- | The name of the default firing port of an instruction.
instrPortName :: ShowS
instrPortName = ("p"++)

-- | Generates a name for a TTD instruction.
instrName :: InstrID -> ShowS
instrName iID = ("instr_"++).shows iID

-- | Connects instructions.
connectEntries :: Bool -> IEntry -> ShowS
connectEntries _ (nID, CallT _ iID)   = createEdge nID (iID, 0)
connectEntries wh (nID, VarT vID)     = createWHEdge wh nID.createEdge nID (vID, 0)
connectEntries _ (nID, BVarT vID _)   = createEdge nID (vID, 0)
connectEntries _ (nID, ActualsT acts) = createEdges nID (map snd acts)
connectEntries _ (nID, ConT _ iIDs)   = createEdges nID iIDs
connectEntries _ (nID, CaseT _ iID ps)= createEdges nID (iID:(idsOfPats ps))
connectEntries _ (_, ConstrT _)       = id

-- | Creates edges from an instruction to one or more other instructions.
createEdges :: InstrID -> [InstrID] -> ShowS
createEdges nID iIDs = foldDot (createEdge nID) $ zip iIDs [0..]
   
-- | Creates an edge between two instructions. The direction of the arrow is
--   that of the demand messages, the response messages are assumed to have
--   the opposite direction.
createEdge :: InstrID -> (InstrID, Int) -> ShowS
createEdge n1 (n2, p2) =
  tab.instrName n1.(":"++).portName p2.(" -> "++).
  instrName n2.(":"++).instrPortName.semi.nl

-- | Creates an edge between a variable instruction and the warehouse.
createWHEdge :: Bool -> InstrID -> ShowS
createWHEdge False _ = id
createWHEdge True iID =
  tab.instrName iID.(":"++).instrPortName.(" -> warehouse [style=dashed];"++).nl

-- | Returns the IDs of a list of pattern branches.
idsOfPats :: [PatT] -> [InstrID]
idsOfPats pats = map (\(PatT _ iID)->iID) pats

-- | Generates dummy nodes for unreachable code.
genUnreachableBoxes :: Bool -> VarIDs -> (S.Set InstrID) -> [IEntry] -> ShowS
genUnreachableBoxes showInstrID vIDs usedIDs entries =
  let usedUserIDs = S.filter (\x->(x>maxBuiltinInstrID)) usedIDs
      entriesIDs  = S.fromList $ map fst entries
      genUBox iID = 
        tab.instrName iID.(" [shape=\"plaintext\", style=\"rounded\", color=black, fillcolor=lightgrey, label=<"++).
        mkInstrLabel showInstrID vIDs iID id.(">];"++).nl
  in  foldDot genUBox $ S.toList $ S.difference usedUserIDs entriesIDs
