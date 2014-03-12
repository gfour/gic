-- | Graphviz output mode: C code snippets that are inserted in the
--   output program to log memory allocation.
-- 
--   The resulting file can be converted to PNG from the command line as follows:
-- 
--   @dot -Tpng graph.dot -o graph.png@
-- 
--   Note: for programs that allocate a lot, this may take /a lot of time/.
-- 

module SLIC.LAR.LARGraph (logConstr, logDict, logGraphStart, logGraphEnd,
                          logPrev) where

import SLIC.Constants (nl, tab)
import SLIC.State (Options(optVerbose))
import SLIC.Types (Counter, CstrName, PPrint(pprint))

-- | Logs a dictionary connection.
logDict :: Options -> Counter -> ShowS
logDict opts counter =
  if optVerbose opts then
    tab.("fprintf(p, \"\\\"LAR_%p\\\" -> \\\"LAR_%p\\\" [style=dashed] ; \\n\", T0, CPTR(cl["++).shows counter.("])); "++)
  else id

-- | Logs a constructor evaluation hit.
logConstr :: Options -> CstrName -> ShowS
logConstr opts c =
  if optVerbose opts then
    tab.("fprintf(p, \"LAR_%p [style=dashed] [label=\\\"LAR_%p   [:"++).pprint c.("]\\\"];\\n\", T0, T0);"++).nl
  else id

-- | Opens the output file and creates its header.
logGraphStart :: Options -> ShowS
logGraphStart opts =
  if optVerbose opts then
    tab.("counter = 0;"++).nl. 
    tab.("p = fopen(\"graph.dot\", \"w\");"++).nl.
    tab.("if (p== NULL) { printf(\"Error in opening graph file.\"); exit(-1); } ; "++).nl.
    tab.("printf(\"Initial LAR:%p\\n\", T0);"++).nl.
    tab.("fprintf(p, \"digraph G {\\n\");"++).nl.
    tab.("fprintf(p, \"LAR_%p [shape=house];\\n\", T0);"++).nl
  else id

-- | Closes the graph file.
logGraphEnd :: Options -> ShowS
logGraphEnd opts =
  if optVerbose opts then
    tab.("fprintf(p, \"}\\n\");"++).nl.
    tab.("fclose(p);"++).nl
  else id

-- | Logs a link to the parent LAR.
logPrev :: Options -> ShowS
logPrev opts =
  (if optVerbose opts then
     ("fprintf(p, \"\\\"LAR_%p\\\" -> \\\"LAR_%p\\\" [color=black] [label=\\\"%d\\\"] ; \\n\", T0->prev, T0, counter++); "++).nl 
   else id)
  