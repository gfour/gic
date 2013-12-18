data List= Nil | Cons Int List ;
result   = head2 (tail2 (f(2))) ;
head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
tail2 ll = case ll of Cons cons_0 cons_1 -> cons_1 ;
f x      = if (x<=0) then Cons (x+10) Nil
           else Cons (2*x) (f (x-1))
