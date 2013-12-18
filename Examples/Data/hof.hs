-- Higher-order function bodies.

result :: Int
result = (y 10 20) + (z 30 1);

z :: Int -> Int -> Int
z = mult 20 ;

y :: Int -> Int -> Int
y c = mult c c

mult :: Int -> Int -> Int -> Int
mult a b c = a * b * c
