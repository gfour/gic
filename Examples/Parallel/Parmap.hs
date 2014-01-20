-- | Parallel map example.
module Parmap where

import Control.Parallel (par, pseq)

result :: Int
result = sum2 (parmap fib (ints 33))

ints :: Int -> [Int]
ints i = if i==0 then [] else i:(ints (i-1))

fib :: Int -> Int
fib x = if x<2 then 1 else ((fib (x-1)) + (fib (x-2)))

sum2 :: [Int] -> Int
sum2 l = case l of [] -> 0 ; x:xs -> x+(sum2 xs)

parmap :: (Int -> Int) -> [Int] -> [Int]
parmap f l =
  case l of
    []   -> []
    x:xs -> auxpar (f x) (parmap f xs)

auxpar :: Int -> [Int] -> [Int]
auxpar z fxs = (z `par` fxs) `pseq` (z:fxs)
