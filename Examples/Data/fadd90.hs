result = repeat2 4 faddsx2 3 ;
faddsx2 z = f sq z ;
sq c = c * c ;
add a b = a + b ;
f s x = if (x <= 0) then s x else f (f (add (s x))) (x-1) ;
repeat2 n f a = repeat_aux n f a 0 ;
repeat_aux n f a b = if n > 0 then repeat_aux (n-1) f a (b + f a - b) else b
