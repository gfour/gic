module Lib where

high :: (a -> b) -> a ->b
high g x = g x

h :: Int -> Int
h y = y + 1

test :: Int
test = high h 1

add :: Int -> Int -> Int
add a b = a + b

double :: (a -> a) -> a -> a
double f x = f (f x)
