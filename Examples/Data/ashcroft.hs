result :: Int
result = head2 (f (g 800000)) -- 8000000))

head2 :: [Int] -> Int
head2 hl = case hl of 
  cons_0 : cons_1 -> cons_0  
tail2 :: [Int] -> [Int]
tail2 tl = case tl of
  cons_0 : cons_1 -> cons_1
null2 :: [Int] -> Bool  
null2 ds = case ds of [] -> True
                      cons_0 : cons_1 -> False
g :: Int -> [Int]
g n  = if n == 0 then [] else n : (g (n-1))
f :: [Int] -> [Int]
f xs = if null2 xs then [] else f1 xs (tail2 xs)
f1 :: [Int] -> [Int] -> [Int]
f1 xs t = if null2 t then xs else f2 xs (f t)
f2 :: [Int] -> [Int] -> [Int]
f2 xs l = (head2 l) : (f ((head2 xs) : (f (tail2 l))))
