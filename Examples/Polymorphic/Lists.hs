-- Polymorphic lists.

module Lists where

result :: Int
result = (lengthP xP) + (lengthP yP) + (lengthL xL) + (lengthL yL)

-- user-defined

data List a = Nil | Cons a (List a)

lengthP :: List a -> Int
lengthP as =
  case as of
    Nil       -> 0
    Cons l ls -> 1 + (lengthP ls)

xP :: List Int
xP = Cons 1 (Cons 2 (Cons 3 Nil))

yP :: List Bool
yP = Cons True (Cons True (Cons False Nil))

-- built-in

lengthL :: [a] -> Int
lengthL as =
  case as of
    []   -> 0
    l:ls -> 1 + (lengthL ls)

xL :: [Int]
xL = [1, 2, 3]

yL :: [Bool]
yL = [True, True, False]
