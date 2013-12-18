module Main where
import Lib (high, test, add, double)

main :: IO ()
main = putStrLn (show result)

result :: Int
result = f 10 + test  

f :: Int -> Int
f a = a + (high (double (add 1)) 1) + (double ident (high dec 2))

dec :: Int -> Int
dec x = x - 1

ident :: a -> a
ident z = z
