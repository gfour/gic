module Main where
main = putStrLn (show result) ;

result :: Int
result = head2 (tail2 (f 2)) ;
lst :: [Int]
lst = f 300 ;
head2 :: [Int] -> Int
head2 hl = case hl of cons_0 : cons_1 -> cons_0 ;
tail2 :: [Int] -> [Int]
tail2 tl = case tl of cons_0 : cons_1 -> cons_1 ;
f :: Int -> [Int]
f x = if (x <= 0) then [x+10] else (x*2) : (f (x-1))


