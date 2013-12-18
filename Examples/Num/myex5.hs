result = fib 5;
fib n = if n <= 1 then 1 else fib(n-1) + fib(n-2)

{-
"result" = CALL (0,"fib_n__0")  ("fib")
"fib" = "if" ["<=" [ARG 0 0,"1" ],"1" ,"+" [CALL (0,"fib_n__1")  ("fib"),CALL (0,"fib_n__2")  ("fib")]]
"fib_n__0" = SAVE (0,0) ACT_0 "2" 
"fib_n__1" = SAVE (0,0) ACT_0 "-" [ARG 0 0,"1" ]
"fib_n__2" = SAVE (0,0) ACT_0 "-" [ARG 0 0,"2" ]
-}
