main :: IO ()
main = putStrLn (show result)

result :: Int
result = hfib apply add pred2 10;

hfib :: ((Int -> Int) -> Int -> Int) -> (Int -> Int -> Int) ->
        (Int -> Int) -> Int -> Int
hfib a f s n = if n <= 1 then n
               else f (hfib a f s (a s n)) (hfib a f s (a s (a s n)));

apply :: (Int -> Int) -> Int -> Int
apply g x = g x;

pred2 :: Int -> Int
pred2 m = m - 1;

add :: Int -> Int -> Int
add b c = b + c
