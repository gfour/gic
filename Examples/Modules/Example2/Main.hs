module Main where

import Lib (add3, high1)

main :: IO ()
main = putStrLn (show result)

result :: Int
result = high1 (add3 18) (add3 19 1) 1 2
-- result = high1 (add3 1 21) 3
