result = f inc 2;
f g y = g y * apply g y;
apply h x = h x;
inc a = a + 1


{-
"result" = CALL (1,"f_g__0") CALL (0,"f_y__0")  ("f")
"f" = "*" [CALL (0,"f_g_0__0")  (ARG 1 0),CALL (1,"apply_h__0") CALL (0,"apply_x__0")  ("apply")]
"apply" = CALL (0,"apply_h_0__0")  (ARG 1 0)
"inc" = "+" [ARG 0 0,"1" ]
"f_g__0" = SAVE (1,0) ACT_1 CALL (0,"inc_a__0")  ("inc")
"apply_h__0" = SAVE (1,0) ACT_1 CALL (0,"f_g_0__1")  (ARG 1 0)
"f_y__0" = SAVE (0,0) ACT_0 ACT_1 "2" 
"apply_x__0" = SAVE (0,0) ACT_0 ACT_1 ARG 0 0
"inc_a__0" = SAVE (0,0) ACT_0 CALL (1,"f_g__0")  (ARG 0 0)
"f_g_0__0" = SAVE (0,0) ACT_0 ARG 0 0
"f_g_0__1" = SAVE (0,0) ACT_0 CALL (1,"apply_h__0")  (ARG 0 0)
"apply_h_0__0" = SAVE (0,0) ACT_0 ARG 0 0
-}
