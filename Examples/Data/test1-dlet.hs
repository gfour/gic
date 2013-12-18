data List = Nil | Cons Int List ;
result = f 2 (h Nil 3);
f x1 x2 = let g y = let z a = 1 + a  + y
                    in  x1 + x2 + y + (z 10)
              h y = x1+x2
          in  let k = (g 3) + x1
              in  k ;
h a b = let zz = case a of Nil -> 2*b
        in  case a of Nil -> 2*zz ;
              
