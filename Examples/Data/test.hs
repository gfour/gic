data List = Nil | Cons Int List ;
result = head2 (tail2 lst) ;
lst = Cons (10) (Cons (20) (Nil)) ;
head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
tail2 tl = case tl of Cons cons_0 cons_1 -> cons_1
