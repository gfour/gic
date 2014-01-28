size :: Int
size = 2500

data List = Nil | Cons Int List;

result :: Int
result = (select1 (qsort2 (nums (size+10))) (size `div` 2)) +
         (select1 (qsort2 (nums (size-10))) (size `div` 2))

append2 :: List -> List -> List
append2 l1 l2 = case l1 of
  Nil -> l2
  Cons a b -> Cons a (append2 b l2) ;

filter2 :: (Int -> Int -> Bool) -> Int -> List -> List
filter2 f p l = 
  case l of
    Nil -> Nil
    Cons a b -> if f a p then Cons a (filter2 f p b) else filter2 f p b;
      
qsort2 :: List -> List
qsort2 l =
  case l of
    Nil -> Nil
    Cons a b -> append2 (qsort2 (filter2 lt a b)) 
                        (Cons a (qsort2 (filter2 ge a (b))));
      
lt :: Int -> Int -> Bool
lt x y = x < y;

ge :: Int -> Int -> Bool
ge x y = x >= y;

nums :: Int -> List
nums n = if (n==0) then Nil else (Cons n (nums (n-1)));

select1 :: List -> Int -> Int
select1 xs n =
  case xs of
    Cons a b -> if (n==0) then a else select1 b (n-1);
