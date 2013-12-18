-- | A Graphviz output module for the dataflow graphs in the TTD language.
-- 

{-
-- | Output the TTD graph as a GraphViz graph.
dotGraph :: ProgT -> ShowS
dotGraph (ProgT _ defs) = 
  let bName v = ("cluster_"++).(v++)
      makeGD [] = id
      makeGD (def:ds) = 
        (case def of
            DefT v (ports, b) -> vpre v.makeGPorts v ports.vpost
            ActualsT v bl     -> vpre v.makeGPorts v (concatMap fst bl).vpost
        ).makeGD ds
      makeGB :: BoxT -> ShowS
      makeGB (ports, e) = vpre ("b" ++ (concat $ intersperse "_" (map show ports))).vpost
      vpre v = ("subgraph "++).(bName v).(" { "++).nl.
               tab.("label = \""++).(v++).("\""++).nl
               -- tab.(v++).(" [ label = \""++).(v++).("\" ]"++).nl
      vpost  = ("}"++).nl
      outName v = (v++).("_out"++)
      makeGPorts v [] = tab.outName v.(" [ label = \"p:out\" color=\"blue\"]"++).nl
      makeGPorts v (p:ps) =
        let lbl = ("p:"++).(shows p)
        in  tab.nmPort (v, p).(" [ label = \""++).lbl.("\" color=\"blue\"]"++).nl.makeGPorts v ps
      nmPort (v, p) = (v++).("_"++).(shows p)
      graph = analyzeTTD defs
      -- [("fib_x",[("fib_x",1),("fib_x",2)]),("fib",[("fib_x",1),("fib",2),("fib",3)]),("result",[("fib",1)])]
      
      connect (n1, n2) color = n2++" -> "++n1++" [color="++color++"]"++(nl "")
      connectEdges :: TTDGraph -> ShowS
      connectEdges [] = id
      connectEdges ((n, ns):gs) =
        let vports = zip (repeat n) (map snd ns)
            vars  = map fst ns
            aux :: (VName, VPort) -> String
            aux (vo, vn) =
              let color = if (vo==fst vn) then "red" else "black"
              in  connect (outName vo "", nmPort vn "") color
        in  ((concatMap aux (zip vars vports))++).
            connectEdges gs
   in ("digraph G {"++).nl.
      ("fontname = \"Bitstream Vera Sans\""++).nl.
      ("fontsize = 8"++).nl.nl.
      ("node ["++).nl.
      tab.("fontname = \"Bitstream Vera Sans\""++).nl.
      tab.("fontsize = 8"++).nl.
      tab.("shape = \"record\""++).nl.
      ("]"++).nl.nl.
      ("edge ["++).nl.
      tab.("fontname = \"Bitstream Vera Sans\""++).nl.
      tab.("fontsize = 8"++).nl.
      ("]"++).nl.nl.
      makeGD defs.
      connectEdges graph.
      ("}"++).nl

dotGraph2 :: TTDGraph -> ShowS
dotGraph2 graph =
  let nodes :: TTDGraph -> ShowS
      nodes [] = id
      nodes ((n, _) : rns) = (n++).(" [ label = \""++).(n++).("\" ]"++).nl.nodes rns
      edges :: TTDGraph -> ShowS
      edges [] = id
      edges ((n, ns) : rns) = (foldDot edge (zip (repeat n) ns)) . edges rns
      edge (v, (v', port')) = if and [not showSelfEdges, v==v'] then id
                              else (v++).(" -> "++).(v'++).(" [label = \""++).shows port'.("\"]"++).nl
      showSelfEdges = True
  in  ("digraph G {"++).nl.
      ("fontname = \"Bitstream Vera Sans\""++).nl.
      ("fontsize = 8"++).nl.nl.
      ("node ["++).nl.
      tab.("fontname = \"Bitstream Vera Sans\""++).nl.
      tab.("fontsize = 8"++).nl.
      tab.("shape = \"record\""++).nl.
      ("]"++).nl.nl.
      ("edge ["++).nl.
      tab.("fontname = \"Bitstream Vera Sans\""++).nl.
      tab.("fontsize = 8"++).nl.
      ("]"++).nl.nl.
      nodes graph.nl.
      edges graph.nl.
      ("}"++).nl
-}
