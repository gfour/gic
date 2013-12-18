result :: Int
result = select1 (qsort2 (nums 500)) 100
append2 :: [Int] -> [Int] -> [Int]
append2 l1 l2 = if (null2 l1) then l2 else (head2 l1) : (append2 (tail2 l1) l2);
filter2 :: (Int -> Int -> Bool) -> Int -> [Int] -> [Int]
filter2 f p l = if (null2 l) then [] else (if f (head2 l) p then (head2 l) : (filter2 f p (tail2 l)) else filter2 f p (tail2 l));
qsort2 :: [Int] -> [Int]
qsort2 l = if null2 l then [] else append2 (qsort2 (filter2 lt (head2 l) (tail2 l))) ((head2 l) : (qsort2 (filter2 ge (head2 l) (tail2 l))));
lt :: Int -> Int -> Bool
lt x y = if (x < y) then True else False;
ge :: Int -> Int -> Bool
ge x y = if (x >= y) then True else False;
null2 :: [Int] -> Bool
null2 ds = case ds of [] -> True
                      cons_0 : cons_1 -> False;
head2 :: [Int] -> Int
head2 hl = case hl of cons_0 : cons_1 -> cons_0;
tail2 :: [Int] -> [Int]
tail2 tl = case tl of cons_0 : cons_1 -> cons_1;
nums :: Int -> [Int]
nums n = if (n==0) then [] else n : (nums (n-1)) ;
select1 :: [Int] -> Int -> Int
select1 xs n = if (n==0) then (head2 xs) else (select1 (tail2 xs) (n-1));
