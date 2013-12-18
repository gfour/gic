result :: Int
result = repeat2 10000 addsx2 30;

addsx2 :: Int -> Int
addsx2 z = f sq z ;

sq :: Int -> Int
sq c = c * c ;

add :: Int -> Int -> Int
add a b = a + b ;

f :: (Int -> Int) -> Int -> Int
f s x = if (x <= 0) then s x else f (add (s x)) (x-1) ;

repeat2 :: Int -> (Int -> Int) -> Int -> Int
repeat2 n f a = repeat_aux n f a 0 ;

repeat_aux :: Int -> (Int -> Int) -> Int -> Int -> Int
repeat_aux n f a b = if n > 0 then repeat_aux (n-1) f a (b + f a - b) else b
