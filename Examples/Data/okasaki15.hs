data List = Nil | Cons Int List ;
psucceed x1 sc1 fc1 ts1 = sc1 x1 fc1 ts1 ;
palt p2 q2 sc2 fc2 ts2 = p2 sc2 (q2 sc2 fc2 ts2) ts2 ;
pbind p3 f3 sc3 a3 b3 = p3 (pbind_aux f3 sc3) a3 b3 ;
pany sc4 fc4 ts4 = if null2 ts4 then fc4 else sc4 (head2 ts4) fc4 (tail2 ts4) ;
success0 x5 fc5 ts5 = if null2 ts5 then x5 else fc5 ;
ntdigit a6 b6 c6 = palt pany pany a6 b6 c6 ;
ntnumber n7 a7 b7 c7 = palt (pbind ntdigit ntnumber_aux1) (pbind ntdigit (ntnumber_aux2 n7)) a7 b7 c7 ;
result = ntnumber 0 success0 (0 - 999) (Cons 1 (Cons 0 (Nil))) ;
pbind_aux f8 sc8 x8 a8 b8 = f8 x8 sc8 a8 b8 ;
ntnumber_aux1 x9 a9 b9 c9 = ntnumber x9 a9 b9 c9 ;
ntnumber_aux2 n10 x10 a10 b10 c10 = psucceed n10 a10 b10 c10 ;
null2 ds = case ds of Nil -> True 
                      Cons cons_0 cons_1 -> False ;
head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
tail2 tl = case tl of Cons cons_0 cons_1 -> cons_1
