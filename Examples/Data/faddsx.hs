result = f sq 5 ;
sq c = c * c ;
add a b = a + b ;
f s x = if (x <= 0) then s x else f (f (add (s x))) (x-1)
