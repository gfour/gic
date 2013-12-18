result = f sq 2 ;
f s x = if x < 1 then s x else f (g s) (x - 1) ;
g t y = f (add (t y)) y ;
sq z = z * z ;
add a b = a + b
