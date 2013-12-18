data SList = SNil | SCons !Int SList ;
result = f 10 (g 11) ;
f a b = a + 1 ;
g x = g (x + 1) ;
test = SNil 
