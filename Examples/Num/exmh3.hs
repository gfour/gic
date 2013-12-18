result :: Int
result = ffac sq 8;

ffac :: (Int -> Int) -> Int -> Int
ffac h n = if n <= 1 then 1 else h n * ffac h (n - 1);

sq :: Int -> Int
sq a = a * a
