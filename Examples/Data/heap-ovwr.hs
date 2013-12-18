data List = Nil | Cons Int List ;
result = (head1 (f 10)) + (head2 (f 20)) ;
head1 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
head2 hl = case hl of Cons cons_0 cons_1 -> (cons_0 + 1);
f x = if (x <= 0) then Cons (x+10) (Nil) else Cons (x*2) (f (x-1))
