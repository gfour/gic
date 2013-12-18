result :: Int
result = f sq 10 ;

sq :: Int -> Int
sq c = c * c ;

add :: Int -> Int -> Int
add a b = a + b ;

f :: (Int -> Int) -> Int -> Int
f s x = if (x <= 0) then s x else f (add (s x)) (x-1)
