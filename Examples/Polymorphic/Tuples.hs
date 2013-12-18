module Tuples where

main :: IO ()
main = putStrLn (show result)

result :: Int
result = (sum2 (1, 2)) + (sum3 (3, 4, 5))

sum2 :: (Int, Int) -> Int
sum2 x =
  case x of (a, b) -> a+b

sum3 :: (Int, Int, Int) -> Int
sum3 x =
  case x of (a, b, c) -> a+b+c
