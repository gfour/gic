-- result = h 50000 (fib 25);

result :: Int
result = h 8 17;
-- h y a = if y <= 1 then 0 else a + (h (y-1) a);

h :: Int -> Int -> Int
h y a = if y <= 1 then 0 else (fib a) + (h (y-1) a);

fib :: Int -> Int
fib x = if x <= 1 then 1 else fib(x-1) + fib(x-2)
