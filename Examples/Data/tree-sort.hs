data Tree = Empty | Node Int Tree Tree;

append1 :: [Int] -> [Int] -> [Int]
append1 xs ys =
  case xs of
    [] -> ys
    a : b -> a : (append1 b ys);
treesort :: [Int] -> [Int]
treesort xs = flatten1 (maketree xs);
maketree :: [Int] -> Tree
maketree xs = 
  case xs of
    [] -> Empty 
    a : b -> insert a (maketree b);
insert :: Int -> Tree -> Tree
insert x tr = 
  case tr of
    Empty -> Node x Empty Empty
    Node el l r -> if (x <= el) then 
                     (Node el (insert x l) r) 
              else (Node el l (insert x r));
createlist :: Int -> [Int]
createlist n = if (n==0) then [] else n : (createlist (n-1));
flatten1 :: Tree -> [Int]
flatten1 tr = 
  case tr of
    Empty -> []
    Node el l r -> append1 (append1 (flatten1 l) ([el])) (flatten1 r);
select1 :: [Int] -> Int -> Int
select1 xs n = 
  case xs of
    a : b -> if (n==0) then a else (select1 b (n-1));
result :: Int
result = select1 (treesort (createlist 5000)) 1000
