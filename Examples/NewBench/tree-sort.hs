size :: Int
size = 5000
  
data Tree = Empty | Node Int Tree Tree
data List = Nil | Cons Int List

append1 :: List -> List -> List
append1 xs ys =
  case xs of
    Nil -> ys
    Cons a b -> Cons a (append1 b ys)

treesort :: List -> List
treesort xs = flatten1 (maketree xs)

maketree :: List -> Tree
maketree xs = 
  case xs of
    Nil -> Empty 
    Cons a b -> insert a (maketree b)

insert :: Int -> Tree -> Tree
insert x tr = 
  case tr of
    Empty ->
      Node x Empty Empty
    Node el l r ->
      if (x <= el) then 
        (Node el (insert x l) r) 
      else 
        (Node el l (insert x r))

createlist :: Int -> List
createlist n = 
  if (n==0) then Nil else (Cons n (createlist (n-1)))

flatten1 :: Tree -> List
flatten1 tr = 
  case tr of
    Empty -> Nil
    Node el l r -> append1 (append1 (flatten1 l) (Cons el Nil)) (flatten1 r);

select1 :: List -> Int -> Int
select1 xs n = 
  case xs of
    Cons a b -> if (n==0) then a else (select1 b (n-1));

result :: Int
result = select1 (treesort (createlist size)) (size `div` 2)
