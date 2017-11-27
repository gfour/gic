#!/bin/bash

source find-gic.sh
echo "Using GIC=${GIC}"

(${GIC} -erl ${1} > main.erl) &&
(cat erlang/distr_eduction.erl >> main.erl) &&
(erlc main.erl erlang/warehouse2.erl) &&
# echo ${1} transformed to main.erl
# (echo "main:init()." | erl +sct L0-0c0-0 +P 1677216 main.beam)
(echo "main:init()." | erl +P 1677216 main.beam)
