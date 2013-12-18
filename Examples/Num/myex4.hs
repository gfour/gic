result = f ptwo 0;
f g y = g y + g y;
ptwo x = x + 2

{-
"result" = CALL (1,"f_g__0") CALL (0,"f_y__0")  ("f")
"f" = "+" [CALL (0,"f_g_0__0")  (ARG 1 0),CALL (0,"f_g_0__0")  (ARG 1 0)]
"ptwo" = "+" [ARG 0 0,"2" ]
"f_g__0" = SAVE (1,0) ACT_1 CALL (0,"ptwo_x__0")  ("ptwo")
"f_y__0" = SAVE (0,0) ACT_0 ACT_1 "0" 
"ptwo_x__0" = SAVE (0,0) ACT_0 CALL (1,"f_g__0")  (ARG 0 0)
"f_g_0__0" = SAVE (0,0) ACT_0 ARG 0 0
-}
