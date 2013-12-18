result :: [Int]
result = qsort2 [32, 7, 42, 8, 1] ;

append2 :: [Int] -> [Int] -> [Int]
append2 l1 l2 = if null2 l1 then l2 else (head2 l1) : (append2 (tail2 l1) l2);

filter2 :: (Int -> Int -> Bool) -> Int -> [Int] -> [Int]
filter2 f p l = if null2 l then [] else if f (head2 l) p then (head2 l) : (filter2 f p (tail2 l)) else filter2 f p (tail2 l);

qsort2 :: [Int] -> [Int]
qsort2 l = if null2 l then [] else append2 (qsort2 (filter2 lt (head2 l) (tail2 l))) ( (head2 l) : (qsort2 (filter2 ge (head2 l) (tail2 l))));

lt :: Int -> Int -> Bool
lt x y = x < y;
ge :: Int -> Int -> Bool
ge x y = x >= y;
null2 :: [Int] -> Bool
null2 ds = case ds of [] -> True
                      cons_0 : cons_1 -> False;
head2 :: [Int] -> Int
head2 hl = case hl of cons_0 : cons_1 -> cons_0;
tail2 :: [Int] -> [Int]
tail2 tl = case tl of cons_0 : cons_1 -> cons_1;
val1 :: Int
val1 = head2 result;
val2 :: Int
val2 = head2 (tail2 result);
val3 :: Int
val3 = head2 (tail2 (tail2 result));
val4 :: Int
val4 = head2 (tail2 (tail2 (tail2 result)));
val5 :: Int
val5 = head2 (tail2 (tail2 (tail2 (tail2 result))))
