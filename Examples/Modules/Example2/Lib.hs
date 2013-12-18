module Lib where

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

high1 :: (Int -> Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
high1 f g x y = (f x y) + (g x)
-- dummy1 = high1 (add3 18) (add3 19 1) 1 2
