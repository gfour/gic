data Tree = Node Int | Tr Tree Tree

result = f tree
  
f x =  
  case x of
    Node i1  -> i1
    Tr a1 b1 ->
      case a1 of
        Node i2  -> i2
        Tr a2 b2 -> f a2 + f b2
  
tree = Tr (Node 10) (Tr (Tr (Node 20) (Node 30)) (Node 40))