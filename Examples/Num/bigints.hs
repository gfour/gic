main :: IO ()
main = putStrLn (show result)

result :: Integer
result = fact 30

fact :: Int -> Integer
fact x = if x<=1 then (toInteger 1) else (toInteger x) `mulI` fact (x-1)
