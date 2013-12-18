data List = Nil | Cons Int List;

head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
tail2 tl = case tl of Cons cons_0 cons_1 -> cons_1 ;
null2 ds = case ds of Nil -> True
                      Cons cons_0 cons_1 -> False ;

createlist n = if (n==0) then Nil else (Cons 1 (createlist (n-1)));

len xs = if (null2 xs) then 0 else (1+(len (tail2 xs)));

result = (len (createlist 700000))

