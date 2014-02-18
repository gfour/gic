module Queens where

main :: IO ()
main = putStrLn (show result)

size :: Int
size = 12

result :: Int
result = count size

count :: Int -> Int
count n = length2 (queens n)

queens :: Int -> [[Int]]
queens n = gen n n

gen :: Int -> Int -> [[Int]]
gen nq n = if n == 0 then [[]] else gen_1 nq (gen nq (n-1))

gen_1 :: Int -> [[Int]] -> [[Int]]
gen_1 nq bs =
  case bs of
    [] -> []
    b:bs' -> gen_2 nq b 1 (gen_1 nq bs')

gen_2 :: Int -> [Int] -> Int -> [[Int]] -> [[Int]]
gen_2 nq b q rest = if q <= nq then
                      if safe q 1 b then
                        (q:b) : (gen_2 nq b (q+1) rest)
                      else
                        gen_2 nq b (q+1) rest
                    else
                      rest

safe :: Int -> Int -> [Int] -> Bool
safe x d qs =
  case qs of
    []  -> True
    q:l -> (x /= q) && (x /= (q+d)) && (x /= (q-d)) && (safe x (d+1) l)

length2 :: [[Int]] -> Int
length2 bs =
  case bs of
    [] -> 0
    b : bs' -> 1 + length2 bs'
