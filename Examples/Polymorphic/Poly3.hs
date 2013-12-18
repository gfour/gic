module Poly3 where

main :: IO ()
main = putStrLn (show result)

result :: Int
result = if h ident True then (h add 17) 25
         else h (add 2) 5
                                
h :: (a1 -> a2) -> a1 -> a2
h g x = g x

add :: Int -> Int -> Int
add a b = a + b

ident :: a1 -> a1
ident z = z
