-- result = let y = 1 in y + (f 2 3);
result = f 2 3;
{--
f1 x1 x2 = let g a b = a + (b + (x1 + x2))
          in  let h z = x1 + (x1 + (z + 1))
              in  h (g 10 20)
--}
f x1 x2 = let g y = let z a = 1 + a  + y
                    in  x1 + x2 + y + (z 10)
              h y = x1+x2
          in  let k = (g 3) + x1
              in  k ;
-- h a b = let zz = case a of Nil -> 0+b
--        in  zz ;
              
