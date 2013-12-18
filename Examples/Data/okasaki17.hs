psucceed x1 sc1 ts1 = sc1 x1 ts1 ;
pbind p2 f2 sc2 ts2 = p2 (pbind_aux f2 sc2 ts2) ts2 ;
pany sc3 ts3 = if ts3 == 0 then 999 else sc3 1 (ts3 - 1) ;
success0 x4 ts4 = x4 ;
ntnumber n5 sc5 ts5 = pbind pany (ntnumber_aux1 n5) sc5 ts5 + pbind pany (ntnumber_aux2 n5) sc5 ts5 ;
result = ntnumber 0 success0 5 ;
pbind_aux f6 sc6 ts6 x6 a6 = f6 ts6 sc6 a6 ;
ntnumber_aux1 n7 x7 a7 b7 = ntnumber (n7 + x7) a7 b7 ;
ntnumber_aux2 n8 x8 a8 b8 = psucceed n8 a8 b8
