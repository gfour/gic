#!/bin/sh
# Generates a dataflow graph from a Graphviz file (that is the output of
# the GIC TTD mode).

dot -Gnodesep=0.75 -Tpng dfg.dot -o dfg.png
