module Lib where

data Rec = RecC { fieldA :: Int, fieldB :: Int }

seventynine :: Int
seventynine = let f x = let g y = x*x + y in g 3 + g 4 in f 6

z1 :: Rec
z1 = RecC 6 6

z2 :: Int
z2 = fieldA z1
