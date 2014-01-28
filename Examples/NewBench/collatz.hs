-- It computes the lengths of the Collatz sequences 
-- for the first 100000 natural numbers

size :: Int
size = 112000

data List = Nil | Cons Int List;

result :: Int
result = (sum1 (mymap f (nums1 1 size)) + 
          sum1 (mymap f (nums2 1 size))) `div` 2

f :: Int -> Int;
f x = 
  if (x==1) then 
    0 
  else if ((x `mod` 2) == 0) then 
         1 + (f (x `div` 2)) 
       else 
         1 + (f ((3*x)+1));

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

mymap :: (Int -> Int) -> List -> List;
mymap f xs =
  case xs of
     Nil -> Nil
     Cons a b -> (Cons (f a) (mymap f b));

sum1 :: List -> Int;
sum1 xs =
  case xs of
     Nil -> 0
     Cons a b -> a+(sum1 b);
