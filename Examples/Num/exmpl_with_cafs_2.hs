fib x = if x<2 then 1 else (fib (x-1)) + (fib (x-2))
f34 = fib 34
result = f34 + f34 + f34
