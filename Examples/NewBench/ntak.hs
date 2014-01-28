size :: Int
size = 32168

shuffle :: (Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Int
shuffle h x y z n =
   if n `mod` 3 == 0 then
      ntak shuffle h (n+3) (x-1) y z
   else if n `mod` 3 == 1 then
      ntak shuffle h (n+2) (y-1) z x
   else
      ntak shuffle h (n+1) (z-1) x y

ntak :: ((Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Int) -> (Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Int
ntak f h n x y z =
   if x <= y then
      h x y z
   else
      ntak f h n (f h x y z n)
                 (f h x y z (n+1))
                 (f h x y z (n+2))

third :: Int -> Int -> Int -> Int
third x y z = z

result :: Int
result = ntak shuffle third 0 
           (size `div` 1000) (size `mod` 1000 `div` 10) (size `mod` 10);
