#!/bin/bash
WH=20
MC=2000000

# Use the custom warehouse
REDIS=
WHERL=erlang/warehouse.erl
# Use the Redis-based warehouse
# REDIS=-redis
# WHERL=erlang/warehouse_redis.erl

echo Running ${1} with $WH warehouses and $MC maximum contexts per warehouse...
(./gic -cerl -wh $WH $REDIS -ctxts $MC ${1} ${2} ${3} ${4} ${5} > main.erl) &&
(erlc -v main.erl $WHERL erlang/gc.erl) &&
# echo ${1} transformed to main.erl
# (echo "main:init()." | erl +sct L0-0c0-0 +P 1677216 main.beam)
# (echo "c(main, [native]), main:init()." | erl +P 1677216 main.beam)
(echo "main:init()." | erl -pa ~/custom/redis/eredis/ebin +P 1677216 main.beam)
