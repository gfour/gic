-- Pattern compiler test.

result :: Int
result = length2 [1,2,3,4,5]

length2 :: [Int] -> Int
length2 [] = 0
length2 (a:b:xs) = 2 + length2 xs
length2 (x:xs) | (x > 0) = 1 + length2 xs
length2 _ = 0

-- length2 l | l == [] =
--   case l of
--     []       -> 0
--     (a:b:xs) -> 2 + length2 xs
--     (x:xs)   | (x > 0) -> 1 + length2 xs
--     _        -> 0
