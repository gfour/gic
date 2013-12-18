module Main where

import ModuleA (f, caf1, ThisOrThat(This, That))

main :: IO ()
main = putStrLn (show result)

result :: Int
result = f 10 + caf1 + caf2

test1 :: Int -> ThisOrThat
test1 x = This x

caf2 :: Int
caf2 = case (test1 10) of
         This a -> a
         That b -> 1000
