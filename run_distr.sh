#!/bin/bash
for f in Examples/Num/fib.hs Examples/Num/cbn.hs Examples/Num/ack.hs Examples/Num/queens.hs
do
    echo "------ Testing $f..."
    (./gic -autopar -cerl -wh 20 $f > main.erl) && (erlc main.erl erlang/warehouse.erl > /dev/null)
    echo "------- gic "
    (echo "main:init()." | erl +P 1677216 main.beam | grep result)
    echo "------- GHCi "
    (echo "putStr \"result = \" >> putStrLn (show result)" | ghci $f | grep result)
done

