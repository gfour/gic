module Main where

main :: IO ()
main = putStrLn (show result)

eContFrac :: [Int]
eContFrac = 2 : (aux 2);
aux :: Int -> [Int]
aux n = 1 : (n : (1 : (aux (n+2)))) ;

-- Output a digit if we can
ratTrans :: Int -> Int -> Int -> Int -> [Int] -> [Int]
ratTrans a b c d xs =
  case xs of
    [] -> []
    h : tl -> if (((signum1 c == signum1 d) || (abs1 c < abs1 d)) && (((c+d)*(b `div` d)) <= (a+b)) && ((c+d)*(b `div` d) + (c+d) > (a+b))) 
              then 
                ((b `div` d) : (ratTrans c d (a-((b `div` d)*c)) (b-((b `div` d)*d)) xs))
              else
                (ratTrans b (a+(h*b)) d (c+(h*d)) tl); 
signum1 :: Int -> Int
signum1 x = if (x<0) then (-1) else if (x>0) then 1 else 0;
abs1 :: Int -> Int
abs1 x = if (x>=0) then x else (-x);

--Finally, we convert a continued fraction to digits by repeatedly multiplying by 10.
toDigits :: [Int] -> [Int]
toDigits l =
  case l of
    [] -> []
    a : b -> a : (toDigits (ratTrans 10 0 0 1 b));

e :: [Int]
e = toDigits eContFrac

select1 :: [Int] -> Int -> Int
select1 xs n =
  case xs of
    a : b -> if (n==0) then a else select1 b (n-1);

result :: Int
result = select1 e 1000;
