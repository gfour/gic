data List = Nil | Cons Int List;

head2 :: List -> Int
head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;

tail2 :: List -> List
tail2 tl = case tl of Cons cons_0 cons_1 -> cons_1 ;

append1 :: List -> List -> List
append1 xs ys =
  case xs of
    Nil -> ys
    Cons a b -> Cons a (append1 b ys) ;

reverse1 :: List -> List
reverse1 xs = rev2 xs Nil;

rev2 :: List -> List -> List
rev2 xs ys =
  case xs of
    Nil -> ys
    Cons cons_0 cons_1 -> rev2 cons_1 (Cons cons_0 ys)

createlist :: Int -> List
createlist n = if (n==0) then Nil else (Cons n (createlist (n-1)));

size :: Int
size = 5000000

result :: Int
result = head2 (reverse1 (reverse1 (createlist size)))
