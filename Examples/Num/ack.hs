main :: IO ()
main = putStrLn (show result)

result :: Int
result = a 3 3;

a :: Int -> Int -> Int
a m n = if m <= 0 then (n + 1) else if n == 0 then a (m-1) 1 else a (m-1) (a m (n-1))
