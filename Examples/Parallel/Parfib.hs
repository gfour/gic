-- | Parallel Fibonacci example, parallelism cutoff at parameter 14.

module Fibpar where

import Control.Parallel (par, pseq)

result :: Int
result = fib 24

fib :: Int -> Int
fib x = if x<2 then 1 else if x<14 then ((fib (x-1)) + (fib (x-2))) else addpar (fib (x-1)) (fib (x-2)) ;

addpar :: Int -> Int -> Int
addpar a b = (a `par` b) `pseq` (a+b)
