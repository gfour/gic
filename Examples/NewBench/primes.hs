module Main where

main :: IO ()
main = putStrLn (show result)        

size :: Int
size = 175000;

primes :: Int -> Int
primes n =
   if n <= 0 then
      2
   else if n == 1 then
      3
   else
      findPrimeMinus (n-2) 1;
      
findPrimeMinus :: Int -> Int -> Int
findPrimeMinus n i =
   if testPrime (6*i-1) 1 then
      if n == 0 then
         6*i-1
      else
         findPrimePlus (n-1) i
   else
      findPrimePlus n i;

findPrimePlus :: Int -> Int -> Int
findPrimePlus n i =
   if testPrime (6*i+1) 1 then
      if n == 0 then
         6*i+1
      else
         findPrimeMinus (n-1) (i+1)
   else
      findPrimeMinus n (i+1);

testPrime :: Int -> Int -> Bool
testPrime n i =
   if (6*i-1) * (6*i-1) > n then
      True
   else if n `mod` (6*i-1) == 0 then
      False
   else if n `mod` (6*i+1) == 0 then
      False
   else
      testPrime n (i+1);

result :: Int
result = primes size;
