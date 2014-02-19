module Main where

main :: IO ()
main = putStrLn (show result)

size :: Int
size = 10;

safe :: Int -> Int -> Int -> Int -> Int -> Bool
safe q x n s d =
   if n == 0 then
      True
   else
      ((x /= (s `mod` q)) &&
       (x /= ((s `mod` q) + d)) &&
       (x /= ((s `mod` q) - d)) &&
       safe q x (n-1) (s `div` q) (d+1));

count :: Int -> Int -> Int -> Int -> Int
count x n s q =
   if n < q then
      if x < q then
         if safe q x n s 1 then
            count 0 (n+1) (x+(q*s)) q + count (x+1) n s q
         else
            count (x+1) n s q
      else
         0
   else
      1;

data List = Nil | Cons Int List;

mymap :: (Int -> Int) -> List -> List;
mymap f xs =
  case xs of
     Nil -> Nil
     Cons a b -> (Cons (f a) (mymap f b));

sum1 :: List -> Int;
sum1 xs =
  case xs of
     Nil -> 0
     Cons a b -> a + (sum1 b);

nums1 :: Int -> Int -> List;
nums1 a b = 
  if (a <= b) then 
    (Cons a (nums2 (a+1) b)) 
  else 
    Nil

nums2 :: Int -> Int -> List;
nums2 a b = 
  if (a <= b) then 
    (Cons b (nums1 a (b-1)))
  else 
    Nil

result :: Int
result = 
  (sum1 (mymap (count 0 0 0) (nums1 1 size)) + 
   sum1 (mymap (count 0 0 0) (nums2 1 size))) `div` 2
