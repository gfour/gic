main :: IO ()
main = putStrLn (show result)

result :: Int
result = donut four three two one 4 200;

donut :: ((((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int) -> ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int) ->
         (((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int) -> ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
donut a b c d e num = if num == 0 then 0 else a b c d e + donut a b c d (e + 17) (num - 1);

four :: (((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int) ->
         ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
four a b c d = a b c d;

three :: ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
three b c d = b c d;

two :: (Int -> Int) -> Int -> Int
two c d = c d;

one :: Int -> Int
one d = d + 1
