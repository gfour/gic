data Tree = Leaf | Node Tree Tree ;

result :: Int
-- result = if isLeaf (case d of Node a1 b1 -> Leaf ; Leaf -> Node Leaf Leaf) then 1 else 0
result = if isLeaf (case d of Node a1 b1 -> Leaf ; Leaf -> Node Leaf Leaf) then
           (case d of
               Node a b -> 10
               Leaf     -> 1 
           ) + 1
         else 200
  ;
isLeaf :: Tree -> Bool
isLeaf t = case t of  
  Leaf -> True
  Node a b -> False ;
  
d :: Tree
d = Node (Node Leaf Leaf) Leaf
