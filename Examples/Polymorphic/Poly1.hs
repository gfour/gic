module Poly1 where

data List = Nil | Cons Int List

main :: IO ()
main = putStrLn (show result)

result :: Int
result = (length1 (ident a)) + (length2 (ident b))

length1 :: [Int] -> Int
length1 xl =
  case xl of
    []   -> 0
    x:xs -> 1 + length1 xs
a :: [Int]
a = [1,2,3]

length2 :: List -> Int
length2 xl =
  case xl of
    Nil       -> 0
    Cons x xs -> 1 + length2 xs
b :: List
b = Cons 1 (Cons 2 (Cons 3 Nil))

ident :: z -> z
ident z = z

