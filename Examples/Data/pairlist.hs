data Pair = Pair Int Int deriving Show ;
data PairList = NoPairs | Elem Pair PairList deriving Show;
result = Elem (Pair 1 2) (Elem (Pair 3 4) NoPairs)
