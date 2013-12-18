-- c :: Int
-- c = apply sq 2 ;

sq :: Int -> Int
sq a = a * a ;

result :: Int
result = 8 + 6 ;

apply :: (Int -> Int) -> Int -> Int
apply f x = f x
