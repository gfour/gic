-- | Reverses a long list twice.
-- 
module Main where

main :: IO ()
main = putStrLn (show result)

head2 :: [Int] -> Int
head2 hl =
  case hl of 
    cons_0:cons_1 -> cons_0 ;
                      
append1 :: [Int] -> [Int] -> [Int]
append1 xs ys =
  case xs of
    [] -> ys
    a:b -> a:(append1 b ys)

reverse1 :: [Int] -> [Int]
reverse1 xs =
  case xs of
    [] -> []
    a:b -> append1 (reverse1 b) (a:[]) ;

createlist :: Int -> [Int]
createlist n = if (n==0) then [] else n:(createlist (n-1));

result :: Int
result = head2 (reverse1 (reverse1 (createlist 3000)))
