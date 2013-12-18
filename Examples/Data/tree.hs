data Tree = Node Tree Tree | Leaf ;
result = countDepth t ;
t = Node (Node Leaf Leaf) (Node Leaf (Node Leaf Leaf)) ;
countDepth tr = case tr of
	Leaf -> 1
	Node node_0 node_1 -> 1 + (max2 (countDepth node_0) (countDepth node_1)) ;
-- "max" clashes with Prelude.max
max2 a b = if a > b then a else b
