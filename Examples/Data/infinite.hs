data List = Nil | Cons Int List ;
result = head2 (take2 1 inf) ;
take2 n l =
    if n==0 then
        Nil
    else
        take2_aux n l ;
take2_aux n_aux l_aux =
    case l_aux of
      Cons cons_0 cons_1 -> Cons cons_0 (take2 (n_aux-1) cons_1) ;
head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
-- tail2 tl = case tl of Cons cons_0 cons_1 -> cons_1 ;
inf = Cons 42 inf

