data List = Nil | Cons Int List ;
data Defunc = DFSq | DFAdd Int ;

result :: Int
result = f DFSq 20 ;

f :: Defunc -> Int -> Int
f s x = if (x <= 1) then apply s x else f (DFAdd (apply s x)) (x-1) ;

apply :: Defunc -> Int -> Int
apply h y = case h of DFSq -> sq y 
                      DFAdd dfadd_0 -> add dfadd_0 y ;
                      
sq :: Int -> Int                      
sq c = c * c ;

add :: Int -> Int -> Int
add a b = a + b
