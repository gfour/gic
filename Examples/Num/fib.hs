result = fib 22 ;
fib x = if x<2 then 1 else (fib (x-1)) + (fib (x-2))

-- fib x = if x<2 then 1 else paradd (fib (x-1)) (fib (x-2)) ;
-- paradd a b = (a `par` b) `pseq` (a+b)

