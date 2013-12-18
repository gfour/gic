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

import SLIC.Constants
import SLIC.State
import SLIC.Types

-- | Logs a dictionary connection.
logDict :: Options -> Depth -> ShowS
logDict opts depth =
  if optVerbose opts then
    tab.("fprintf(p, \"LAR_%x -> LAR_%x [style=dashed] ; \\n\", T0, cl["++).
        shows depth.("].ctxt); "++)
  else id

-- | Logs a constructor evaluation hit.
logConstr :: Options -> CstrName -> ShowS
logConstr opts c =
  if optVerbose opts then
    tab.("fprintf(p, \"LAR_%x [style=dashed] [label=\\\"LAR_%x   [:"++).pprint c.("]\\\"];\\n\", T0, T0);"++).nl
  else id

-- | Opens the output file and creates its header.
logGraphStart :: Options -> ShowS
logGraphStart opts =
  if optVerbose opts then
    tab.("counter = 0;"++).nl. 
    tab.("p = fopen(\"graph.dot\", \"w\");"++).nl.
    tab.("if (p== NULL) { printf(\"Error in opening graph file.\"); exit(-1); } ; "++).nl.
    tab.("printf(\"Initial LAR:%x\\n\", t0);"++).nl.
    tab.("fprintf(p, \"digraph G {\\n\");"++).nl.
    tab.("fprintf(p, \"LAR_%x [shape=house];\\n\", t0);"++).nl
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
     ("fprintf(p, \"LAR_%x -> LAR_%x [color=black] [label=\\\"%d\\\"] ; \\n\", T0->prev, T0, counter++); "++).nl 
   else id)
  