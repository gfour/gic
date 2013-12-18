module Main where

import Lib (Rec(RecC), fieldB, seventynine)

data TestData = Test Int

main :: IO ()
main = putStrLn (show result)

-- test the corner case of "result" doing pattern matching
result :: Int
result =
  case test of
    Test t -> g + f (seventynine + 10) + t ;

-- test imported constructor unpacking with let
g :: Int
g = let RecC x1 x2 = record
    in  x2

-- test if selectors and updaters work across modules
f :: Int -> Int
f x  = x + (fieldB record) -- (fieldB (record{fieldB=2}))

test :: TestData
test = Test seventynine

record :: Rec
record = RecC 10 8
