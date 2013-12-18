result :: Int
result = apply inc 8 + apply dec 5;

apply :: (Int -> Int) -> Int -> Int
apply f x = f x;

inc :: Int -> Int
inc a = a + 1;

dec :: Int -> Int
dec b = b - 1
