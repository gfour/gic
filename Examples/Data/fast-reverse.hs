data List = Nil | Cons Int List;

head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
tail2 tl = case tl of Cons cons_0 cons_1 -> cons_1 ;
null2 ds = case ds of Nil -> True
                      Cons cons_0 cons_1 -> False ;
append1 xs ys = if (null2 xs) then ys else (Cons (head2 xs) (append1 (tail2 xs) ys));

reverse1 xs = rev2 xs Nil;

rev xs ys = if (null2 xs) then ys else (rev (tail2 xs) (Cons (head2 xs) ys));
rev2 xs ys =
  case xs of
    Nil -> ys
    Cons cons_0 cons_1 -> rev2 cons_1 (Cons cons_0 ys)
createlist n = if (n==0) then Nil else (Cons n (createlist (n-1)));

-- result = head2 ( reverse1 (reverse1 (createlist 5000000)))
result = head2 ( reverse1 (reverse1 (createlist 50000)))

