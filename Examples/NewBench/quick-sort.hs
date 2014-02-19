module Main where

main :: IO ()
main = putStrLn (show result)

size :: Int
size = 2500

result :: Int
result = (select1 (qsort2 (nums (size+10))) (size `div` 2)) +
         (select1 (qsort2 (nums (size-10))) (size `div` 2))

append2 :: [Int] -> [Int] -> [Int]
append2 l1 l2 = case l1 of
  []  -> l2
  a:b -> a:(append2 b l2) ;

filter2 :: (Int -> Int -> Bool) -> Int -> [Int] -> [Int]
filter2 f p l = 
  case l of
    [] -> []
    a:b -> if f a p then a:(filter2 f p b) else filter2 f p b;
      
qsort2 :: [Int] -> [Int]
qsort2 l =
  case l of
    [] -> []
    a:b -> append2 (qsort2 (filter2 lt a b)) 
                   (a:(qsort2 (filter2 ge a (b))));
      
lt :: Int -> Int -> Bool
lt x y = x < y;

ge :: Int -> Int -> Bool
ge x y = x >= y;

nums :: Int -> [Int]
nums n = if (n==0) then [] else (n:(nums (n-1)));

select1 :: [Int] -> Int -> Int
select1 xs n =
  case xs of
    a:b -> if (n==0) then a else select1 b (n-1);
