data List = Nil | Cons Int List;

mymap :: (Int -> Int) -> List -> List;
mymap f xs =
  case xs of
     Nil -> Nil;
     Cons a b -> (Cons (f a) (mymap f b));

mysum :: List -> Int;
mysum xs =
  case xs of
     Nil -> 0;
     Cons a b -> a + (mysum b);

add :: Int -> Int -> Int
add a b = a + b

myid :: Int -> Int
myid z = z

f :: List -> Int
f l = mysum (mymap (add 1) l)

g :: List -> Int
g l = mysum (mymap myid l)

result :: Int
result = f (Cons 0 (Cons 1 (Cons 2 Nil))) * g (Cons 3 (Cons 4 Nil))
