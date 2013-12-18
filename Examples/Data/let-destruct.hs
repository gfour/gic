-- Test for destructuring an expression with 'let'.

data M = J Box | N
data Box = Box1 Int | Box2 Int Int | Box3 Box Int Box
  
result :: Int
result = (f (Box1 21)) + (g 1)

f :: Box -> Int
f x = let Box1 i = x
      in  i + i

g :: Int -> Int
g x = let J (Box3 (Box2 a b) c (Box2 _ d)) = bx x
      in  a + b + c + d

bx :: Int -> M
bx x = J (Box3 (Box2 x 2) 3 (Box2 4 5))
