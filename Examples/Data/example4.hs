data List = Nil | Cons Pair List ;
data Pair = P Int Int ;

result :: Int
result = fst2 (head2 (tail2 lst)) ;

fst2 :: Pair -> Int
fst2 p = case p of P p_0 p_1 -> p_0 ;
                   
lst :: List                   
lst = Cons (P (10) (20)) (Cons (P (100) (200)) (Nil)) ;
-- lst = f 300 ;
-- f x = if (x <= 0) then Cons (P (x+1) (x+2)) (Nil) else Cons (P (x*2) 0) (f (x-1))

head2 :: List -> Pair
head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
tail2 :: List -> List
tail2 tl = case tl of Cons cons_0 cons_1 -> cons_1
