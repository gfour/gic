-- Church numerals

module Main where

main :: IO ()
main = putStrLn (show result)

church :: Int -> (Int -> Int) -> Int -> Int
church i f x =
   if i == 0 then
      x
   else
      church (i-1) f (f x);

successor :: Int -> Int
successor i = i + 1;

unchurch :: ((Int -> Int) -> Int -> Int) -> Int
unchurch n = n successor 0;

c_succ :: ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
c_succ n f x =
   n f (f x);
   
c_plus :: ((Int -> Int) -> Int -> Int) -> 
          ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
c_plus n m f x =
   n f (m f x);

c_nonzero :: ((Int -> Int) -> Int -> Int) -> 
             ((Int -> Int) -> Int -> Int) -> 
             ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
c_nonzero n a b f x =
   n (c_nonzero_aux a f x) (b f x);

c_nonzero_aux :: ((Int -> Int) -> Int -> Int) -> 
                 (Int -> Int) -> Int -> Int -> Int
c_nonzero_aux a f x y = a f x;

c_2x0 :: ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
c_2x0 n f x = c_plus n n f x;
c_2x1 :: ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
c_2x1 n f x = n f (n f (f x));

c0 :: (Int -> Int) -> Int -> Int
c0 f x = church 0 f x;
c1 :: (Int -> Int) -> Int -> Int
c1 f x = c_2x1 c0 f x;
c3 :: (Int -> Int) -> Int -> Int
c3 f x = c_2x1 c1 f x;
c6 :: (Int -> Int) -> Int -> Int
c6 f x = c_2x0 c3 f x;
c12 :: (Int -> Int) -> Int -> Int
c12 f x = c_2x0 c6 f x;
c24 :: (Int -> Int) -> Int -> Int
c24 f x = c_2x0 c12 f x;
c48 :: (Int -> Int) -> Int -> Int
c48 f x = c_2x0 c24 f x;
c96 :: (Int -> Int) -> Int -> Int
c96 f x = c_2x0 c48 f x;
c192 :: (Int -> Int) -> Int -> Int
c192 f x = c_2x0 c96 f x;
c385 :: (Int -> Int) -> Int -> Int
c385 f x = c_2x1 c192 f x;
c771 :: (Int -> Int) -> Int -> Int
c771 f x = c_2x1 c385 f x;
c1543 :: (Int -> Int) -> Int -> Int
c1543 f x = c_2x1 c771 f x;
c3087 :: (Int -> Int) -> Int -> Int
c3087 f x = c_2x1 c1543 f x;
c6174 :: (Int -> Int) -> Int -> Int
c6174 f x = c_2x0 c3087 f x;

c7 :: (Int -> Int) -> Int -> Int
c7 f x = c_succ c6 f x;

c :: (Int -> Int) -> Int -> Int
c f x = c_nonzero c3 c6174 c7 f x;

rep :: Int -> Int -> Int
rep n x = if (n<=0) then x else (rep (n-1) x) + x - (rep (n-2) x)

size :: Int
size = 36

result :: Int
result = rep size (unchurch c);
