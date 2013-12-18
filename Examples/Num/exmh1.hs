result :: Int
result = f 2 ;
-- result2 = f 4 + f 5;

f :: Int -> Int
f x = g (x + 1) 0;

g :: Int -> Int -> Int
g y a = y ;
