data List = Nil | Cons Int List;

head2 :: List -> Int
head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
                      
{-
tail2 :: List -> List                      
tail2 tl = case tl of Cons cons_0 cons_1 -> cons_1 ;
                      
null2 :: List -> Bool
null2 ds = case ds of Nil -> True
                      Cons cons_0 cons_1 -> False ;
-}
                      
append1 :: List -> List -> List
-- append1 xs ys = if (null2 xs) then ys else (Cons (head2 xs) (append1 (tail2 xs) ys));
append1 xs ys =
  case xs of
    Nil -> ys
    Cons a b -> Cons a (append1 b ys) ;    

reverse1 :: List -> List
-- reverse1 xs = if (null2 xs) then xs else (append1 (reverse1 (tail2 xs)) (Cons (head2 xs) Nil));
reverse1 xs =
  case xs of
    Nil -> Nil
    Cons a b -> append1 (reverse1 b) (Cons a Nil) ;

createlist :: Int -> List
createlist n = if (n==0) then Nil else (Cons n (createlist (n-1)));  

result :: Int
result = head2 ( reverse1 (reverse1 (createlist 3000)))
