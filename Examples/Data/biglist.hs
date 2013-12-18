data List = Nil | Cons Int List ;
result = length2 (f 10) + length2 (f 10);
f x = if (x==0) then Nil else Cons x (f (x-1)) ;
inc l =
  case l of
    Nil -> Nil
    Cons h t -> Cons (h+1) (inc t) ;  
length2 d =
  case d of
    Nil -> 0
    Cons h t -> 1 + (length2 t)
