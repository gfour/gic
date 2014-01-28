module Main where

main :: IO ()
main = putStrLn (show result)

size :: Int
size = 10

result :: Int
result = a 3 size ;

a :: Int -> Int -> Int
a m n = 
  if m <= 0 then 
    (n + 1) 
  else (if n == 0 then 
          a (m-1) 1 
        else 
          a (m-1) (a m (n-1)))
