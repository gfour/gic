integrate g h a b =
   integ g h a b 0 0;

integ g h a b i s =
   if i < (b-a) then
      integ g h a b (i+1) (s + g h (a + i))
   else
      s;

trapezoid f x =
   (f(x) + f(x+1)) `div` 2;

square x = x * x;
absolute x = if x < 0 then 0-x else x;

diff g1 =
   absolute (integrate g1 square 0 1);

result =
   diff trapezoid 
