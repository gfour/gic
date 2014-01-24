-- Computes the lengths of the Collatz sequences for the first 100000 natural
-- numbers.

module Main where

main :: IO ()
main = putStrLn (show result)

result :: Int
result = sum1 (mymap f (nums 1 110000))

f :: Int -> Int;
f x  = if (x==1) then 0 else if ((x `mod` 2) == 0) then 1+ (f (x `div` 2)) else 1+(f ((3*x)+1));

length1 :: [Int] -> Int;
length1 ls =
   case ls of
      [] -> 0;
      h : tl -> 1 + (length1 tl);

nums :: Int -> Int -> [Int] ;
nums a b = if (a <= b) then (a : (nums (a+1) b)) else []

mymap :: (Int -> Int) -> [Int] -> [Int] ;
mymap f xs =
  case xs of
     [] -> [];
     a : b -> (f a) : (mymap f b) ;

sum1 :: [Int] -> Int;
sum1 xs =
  case xs of
     [] -> 0;
     a : b -> a + (sum1 b);
