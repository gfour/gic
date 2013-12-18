data List = Nil | Cons Int List deriving Show ;
result = d ;
test x = (head2 x) + (head2inc x) ;
d = Cons (2) (Cons (9) (Nil)) ;
head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
head2inc hl = case hl of Cons cons_0 cons_1 -> cons_0 + 1
