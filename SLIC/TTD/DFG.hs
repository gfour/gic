-- | The Graphviz-based dataflow graph generator.

module SLIC.TTD.DFG (generateDFG) where

import SLIC.AuxFun (foldDot, ierr)
import SLIC.Constants
import SLIC.SyntaxAux (Const(CN))
import SLIC.TTD.SyntaxTTD
import SLIC.Types

-- | Generates a Graphviz graph for a TTD program.
generateDFG :: ProgT -> ShowS
generateDFG (ProgT defs mops) =
  ("digraph G {"++).nl.
  foldDot genDef defs.
  foldDot genMOp mops.
  ("}"++).nl.nl
  
genLink :: QName -> NodeLink -> ShowS
genLink v (VarEdge v' _) =
  tab.tab.pprint v.(" -> "++).pprint v'.semi.nl
genLink v (VarCall v' _ i) =
  tab.tab.pprint v.(" -> "++).pprint v'.
  (" [ label=\""++).shows i.("\" ]"++).semi.nl

genNode :: QName -> NodeT -> ShowS
genNode v (OpT c nls) =
  tab.("subgraph cluster_"++).pprint v.(" {"++).nl.
  tab.tab.("style=filled;"++).nl.
  tab.tab.("color=lightgrey;"++).nl.
  tab.tab.("node [style=filled, color=white];"++).nl.
  tab.tab.("label=\""++).pprint v.("\";"++).nl.
  (if (c==CN cMOpId) then id
   else tab.tab.pprint v.(" -> \""++).pprint c.("\";"++).nl).
  foldDot (genLink v) nls.
  tab.("}"++).nl
genNode v (IfT b0 b1 b2) =
  let v0 = procLName (\ln->ln++"Cond") v
      v1 = procLName (\ln->ln++"True") v
      v2 = procLName (\ln->ln++"False") v
  in  tab.("subgraph cluster_"++).pprint v.(" {"++).nl.
      tab.tab.("style=filled;"++).nl.
      tab.tab.("color=green;"++).nl.
      tab.tab.("node [style=filled, color=coral3, shape=square];"++).nl.
      tab.tab.("label=\""++).pprint v.("\";"++).nl.
      genNode v0 b0.
      genNode v1 b1.
      genNode v2 b2.
      -- tab.tab.("\"cluster_"++).(v0++).("\";"++).nl.
      -- tab.tab.("\"cluster_"++).(v1++).("\";"++).nl.
      -- tab.tab.("\"cluster_"++).(v2++).("\";"++).nl.
      -- tab.tab.(v++).(" -> \""++).(v0++).("\";"++).nl.
      -- tab.tab.(v++).(" -> \""++).(v1++).("\";"++).nl.
      -- tab.tab.(v++).(" -> \""++).(v2++).("\";"++).nl.
      tab.("}"++).nl
   
genDef :: DefT -> ShowS
genDef (DefT v node) = genNode v node
genDef (ActualsT v m nodes) =
  let vName = qName v
      -- use procLName to create new variables with the same module info as v
      vs = map (\i -> procLName (\_->vName++(show i)) v) [0..(length nodes-1 )]
      aux [] [] = id
      aux (f:fs) (i:is) = f i . aux fs is
      aux _ _ = ierr "genDef error"
  in  tab.("subgraph cluster_"++).pprint v.("_"++).(m++).(" {"++).nl.
      tab.tab.("style=filled;"++).nl.
      tab.tab.("color=red;"++).nl.
      tab.tab.("node [style=filled, color=pink];"++).nl.
      tab.tab.("label=\""++).pprint v.("\";"++).nl.
      tab.("}"++).nl.
      aux (map genNode vs) nodes.
      foldDot (\v0->pprint v.(" -> "++).pprint v0.semi.nl) vs

genMOp :: MOpDef -> ShowS
genMOp (v, MOp c mops) =
  tab.("subgraph cluster_"++).pprint v.(" {"++).nl.
  tab.("label=\""++).pprint c.("\""++).nl.
  foldDot genMOp (zip (repeat v) mops).
  tab.("}"++).nl
genMOp (v, MNL link) = genLink v link
