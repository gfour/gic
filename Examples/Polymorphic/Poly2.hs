module Poly2 where

main :: IO ()
main = putStrLn (show result)

result :: Int
result = (double inc 1) + lengthZ (double ident [1, 2, 3])

double :: (a->a) -> a -> a
double f x = f (f x)

inc :: Int -> Int
inc z = z + 1

ident :: a -> a
ident x = x

lengthZ :: [Int] -> Int
lengthZ z =
  case z of
    []   -> 0
    x:xs -> 1 + (lengthZ xs)
