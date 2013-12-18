main :: IO ()
main = putStrLn (show result)

result :: Int
result = f 10 10

f :: Int -> Int -> Int
f x y = if x < 9 then y else f (y-1) (y-1)
