result = f 2 (h 0 3);
f x1 x2 = let g y = let z a = 1 + a  + y
                    in  x1 + x2 + y + (z 10)
              h y = x1+x2
          in  let k = (g 3) + x1
              in  k ;
h a b = let zz = if a==0 then 2*b else 999
        in  if a==0 then 2*zz else 888;
              
