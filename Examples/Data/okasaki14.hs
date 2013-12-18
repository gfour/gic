data List2 = Nil | Cons Int List2 ;
pmax pmax_n1 pmax_n2 = if pmax_n1 > pmax_n2 then pmax_n1 else pmax_n2 ;
psucceed psucceed_x psucceed_sc psucceed_fc psucceed_ts psucceed_n = psucceed_sc psucceed_x psucceed_fc psucceed_ts psucceed_n ;
palt palt_p palt_q palt_sc palt_fc palt_ts palt_n = palt_p palt_sc (palt_1 palt_q palt_sc palt_fc palt_ts palt_n) palt_ts palt_n ;
pbind pbind_p pbind_f pbind_sc pbind__1 pbind__2 pbind__3 = pbind_p (pbind_1 pbind_f pbind_sc) pbind__1 pbind__2 pbind__3 ;
psatisfy psatisfy_p psatisfy_sc psatisfy_fc psatisfy_ts psatisfy_n = if null2 psatisfy_ts then psatisfy_fc psatisfy_n else if psatisfy_p (head2 psatisfy_ts) then psatisfy_sc (head2 psatisfy_ts) psatisfy_fc (tail2 psatisfy_ts) psatisfy_n else psatisfy_fc psatisfy_n ;
pliteral pliteral_t pliteral__1 pliteral__2 pliteral__3 pliteral__4 = psatisfy (equ pliteral_t) pliteral__1 pliteral__2 pliteral__3 pliteral__4 ;
fail0 fail0_n = 0 - 999 ;
success0 success0_x success0_fc success0_ts success0_n = if null2 success0_ts then success0_x else (0 - 999) ;
ntdigit ntdigit__1 ntdigit__2 ntdigit__3 ntdigit__4 = palt (pliteral 0) (pliteral 1) ntdigit__1 ntdigit__2 ntdigit__3 ntdigit__4 ;
ntnumber ntnumber_n ntnumber__1 ntnumber__2 ntnumber__3 ntnumber__4 = palt (pbind ntdigit (ntnumber_1 ntnumber_n)) (pbind ntdigit (ntnumber_2 ntnumber_n)) ntnumber__1 ntnumber__2 ntnumber__3 ntnumber__4 ;
result = ntnumber 0 success0 fail0 (Cons (1) (Cons (0) (Cons (1) (Cons (0) (Cons (1) (Cons (0) (Nil))))))) 0 ;
palt_1 palt_1_q palt_1_sc palt_1_fc palt_1_ts palt_1_n palt_1_np = palt_1_q palt_1_sc (palt_1_1 palt_1_fc palt_1_np) palt_1_ts palt_1_n ;
pbind_1 pbind_1_f pbind_1_sc pbind_1_x pbind_1__1 pbind_1__2 pbind_1__3 = pbind_1_f pbind_1_x pbind_1_sc pbind_1__1 pbind_1__2 pbind_1__3 ;
ntnumber_1 ntnumber_1_n ntnumber_1_x ntnumber_1__1 ntnumber_1__2 ntnumber_1__3 ntnumber_1__4 = ntnumber (2 * ntnumber_1_n + ntnumber_1_x) ntnumber_1__1 ntnumber_1__2 ntnumber_1__3 ntnumber_1__4 ;
ntnumber_2 ntnumber_2_n ntnumber_2_x ntnumber_2__1 ntnumber_2__2 ntnumber_2__3 ntnumber_2__4 = psucceed (2 * ntnumber_2_n + ntnumber_2_x) ntnumber_2__1 ntnumber_2__2 ntnumber_2__3 ntnumber_2__4 ;
palt_1_1 palt_1_1_fc palt_1_1_np palt_1_1_nq = palt_1_1_fc (pmax palt_1_1_np palt_1_1_nq) ;
null2 ds = case ds of Nil -> True
                      Cons cons_0 cons_1 -> False ;
head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
tail2 tl = case tl of Cons cons_0 cons_1 -> cons_1 ;
-- added for (==) aplication in pliteral
equ equ1 equ2 = equ1 == equ2
