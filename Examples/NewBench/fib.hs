size :: Int
size = 37

result :: Int
result = fib size ;

fib :: Int -> Int
fib x = if x<2 then 1 else (fib (x-1)) + (fib (x-2))
