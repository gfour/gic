main :: IO ()
main = putStrLn (show result)

identity :: Int -> Int
identity i = i;

successor :: Int -> Int
successor i = i + 1;

successor_h :: (Int -> Int) -> Int -> Int
successor_h f x = f x + 1;

-- first x y = x;
-- second x y = y;


-- Pairs

-- type Pair a b c = (a -> b -> c) -> c

-- c_pair :: a -> b -> Pair a b c
-- c_pair x y z = z x y;

-- c_fst :: Pair a b a -> a
-- c_fst p = p first;

-- c_snd :: Pair a b b -> b
-- c_snd p = p second ;


-- Church numerals

-- type Church a = (a -> a) -> a -> a
-- type Func a = a -> a

-- church :: Int -> Church a
church :: Int -> (Int -> Int) -> Int -> Int
church i f x =
   if i == 0 then
      x
   else
      church (i-1) f (f x);

-- church_h :: Int -> Church (Func a)
church_h :: Int -> ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
church_h i f x z =
   if i == 0 then
      x z
   else
      church_h (i-1) f x (f x z);

-- unchurch :: Church Int -> Int
unchurch :: ((Int -> Int) -> Int -> Int) -> Int
unchurch n = n successor 0;

-- unchurch_h :: Church (Func Int) -> Int
unchurch_h :: (((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int) -> Int
unchurch_h n = n successor_h identity 0;

-- c_succ :: Church a -> Church a
c_succ :: ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
c_succ n f x =
   n f (f x);
   
-- c_plus :: Church a -> Church a -> Church a
-- c_plus :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Int -> Int -> Int
c_plus :: ((Int -> Int) -> Int -> Int) -> ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
c_plus n m f x =
   n f (m f x);

-- c_plus_h :: Church (Func (Func a)) -> Church a -> Church a
{-
c_plus_h n m f x =
   n c_succ m f x;
-}

-- c_nonzero :: Church a -> Church a -> Church a -> Church a
{- Not definable like this
c_nonzero n a b f x =
   n (\y -> a f x) (b f x);
-}
c_nonzero :: ((Int -> Int) -> Int -> Int) -> ((Int -> Int) -> Int -> Int) -> ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
c_nonzero n a b f x =
   if unchurch n == 0 then b f x else a f x;

-- c_times :: Church a -> Church a -> Church a
{- Not definable like this
c_times n m f =
   n (m f);
-}

-- c_reduce :: Church (Func a) -> Church a
{- Not necessary and not definable like this
c_reduce n f =
   n (\h -> \x -> f (h x)) (\x -> x);
-}

-- c_exp :: Church a -> Church (Func a) -> Church a
{- Not definable like this
c_exp n m =
   m n;
-}
c_exp :: ((Int -> Int) -> Int -> Int) -> (((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
c_exp n m f x =
   m n f x;
{-
c_exp n m =
   m (c_times n) (church 1);
-}

-- c_pred :: Church (Func (Func (Func a))) -> Church a
{- Not definable like this
c_pred n =
   n (\z -> \s -> c_nonzero s (z (church 0)) (c_plus (church 1) (z (church 0))))
     (\s -> c_nonzero s (church 0) (church 0))
     (church 1);
-}
-- c_pred :: Church (Pair (Church a) (Church a) (Church a)) -> Church a
{-
c_pred n f x =
   c_fst (n c_pred_aux_1 c_pred_aux_2) f x;
-}
-- c_pred_aux_1 :: Pair (Church a) (Church a) (Church a) ->
--                    Pair (Church a) (Church a) (Church a)
{-
c_pred_aux_1 p =
   c_pair (c_snd p) (c_succ (c_snd p));
c_pred_aux_2 =
   c_pair (church 0) (church 0);
-}

-- c_minus_h :: Church a -> Church (Func (Func a)) -> Church a
{-
c_minus_h ::
   Church a ->
      ((Church (Pair (Church a) (Church a) (Church a)) -> Church a) ->
         Church a -> Church a) ->
      Church a
-}
{-
c_minus_h n m f x = m c_pred n f x;
-}

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
c3_h :: ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int
c3_h f x z = church_h 3 f x z;
c343 :: (Int -> Int) -> Int -> Int
c343 f x = c_exp c7 c3_h f x;

c :: (Int -> Int) -> Int -> Int
c f x = c_nonzero c3 c6174 c343 f x;

result :: Int
result = unchurch c771;
