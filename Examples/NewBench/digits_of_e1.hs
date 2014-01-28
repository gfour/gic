size :: Int
size = 1500

result :: Int
result = select1 e size;

data List = Nil | Cons Int List;

eContFrac :: List
eContFrac = Cons 2 (aux 2);

aux :: Int -> List
aux n = Cons 1 (Cons n (Cons 1 (aux (n+2))));

-- Output a digit if we can
ratTrans :: Int -> Int -> Int -> Int -> List -> List
ratTrans  a b c d xs =
  case xs of
    Nil -> Nil
    Cons h tl ->
      if (((signum1 c == signum1 d) ||
           (abs1 c < abs1 d)) && (((c+d)*(b `div` d)) <= (a+b)) &&
          ((c+d)*(b `div` d) + (c+d) > (a+b))) then
        (Cons (b `div` d)
         (ratTrans c d (a-((b `div` d)*c)) (b-((b `div` d)*d)) xs))
      else
        (ratTrans b (a+(h*b)) d (c+(h*d)) tl);

signum1 :: Int -> Int
signum1 x = if (x<0) then (0-1) else if (x>0) then 1 else 0;

abs1 :: Int -> Int
abs1 x = if (x>=0) then x else (0-x);

-- Finally, we convert a continued fraction to digits
-- by repeatedly multiplying by 10.

toDigits :: List -> List
toDigits l = case l of
                Nil -> Nil
                Cons a b -> Cons a (toDigits (ratTrans 10 0 0 1 b));

e :: List
e = toDigits eContFrac

select1 :: List -> Int -> Int
select1 xs n =
  case xs of
    Cons a b -> if (n==0) then a else select1 b (n-1);
