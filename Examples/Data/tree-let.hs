data Tree = Node Tree Tree | Leaf;

result =
  let t = Node (Node Leaf Leaf) (Node Leaf (Node Leaf Leaf))
  in countDepth t;

countDepth tr = 
  let max a b = if a > b then a else b
  in  case tr of
	      Leaf -> 1
	      Node node_0 node_1 -> 1 + (max (countDepth node_0) (countDepth node_1));

