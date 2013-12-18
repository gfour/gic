data Func = FAdd Int;

result :: Int
result = repeat_many 2000 (twice (FAdd 1) 5);
twice :: Func -> Int -> Int
twice f x =  apply f (apply f x);
add :: Int -> Int -> Int
add a b = a+b;
repeat_many :: Int -> Int -> Int
repeat_many n x = if (n==0) then 0 else x+ repeat_many (n-1) x
apply :: Func -> Int -> Int
apply cl d = case cl of FAdd c -> add c d;
