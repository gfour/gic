-- Test for proper typed defunctionalization

data Node1 = NodeA deriving (Show)
data Node2 = NodeB deriving (Show)

result :: Node1
result = test1

test1 :: Node1
test1 =
  case (double2 f2) of
    NodeB -> double1 f1

f1 :: Node1 -> Node1 -> Node1
f1 x1 y1 = NodeA

f2 :: Node2 -> Node2 -> Node2
f2 x2 y2 = NodeB

-- TODO: these should also be 'g1 NodeA NodeB' etc.

double1 :: (Node1 -> Node1 -> Node1) -> Node1
double1 g1 = g1 (g1 NodeA NodeA) (g1 NodeA NodeA)

double2 :: (Node2 -> Node2 -> Node2) -> Node2
double2 g2 = g2 (g2 NodeB NodeB) (g2 NodeB NodeB)
