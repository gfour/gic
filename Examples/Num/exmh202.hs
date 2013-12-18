integ g h a b i s =
   if i < (b-a) then
      integ g h a b (i+1) (s + g h (a + i))
   else
      s;

{-
trapezoid f x =
   (f(x) + f(x+1)) `div` 2;

simpson13 f x =
   (f(x) + 4 * f(x+1) + f(x+2)) `div` 6;
-}

simpson38 f x =
   ((f(x) + (3 * f(x+1)) + (3 * f(x+2)) + f(x+3)) * 3) `div` 24;

square x = x * x;
{-
inverse x = 10 `div` x;
absolute x = if x < 0 then -x else x;

diff g1 g2 =
   absolute (integ g1 square 0 10 0 0 - integ g2 square 0 10 0 0) +
   absolute (integ g1 inverse 1 10 0 0 - integ g2 inverse 1 10 0 0);
-}

result =
{-
   diff trapezoid simpson13 +
   diff simpson13 simpson38 +
   diff simpson38 trapezoid
-}
   integ simpson38 square 0 730 0 0
