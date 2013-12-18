-- Type classes test.
-- 
-- The type class instance methods used here can be resolved statically, no
-- need for passing around dictionaries.
-- 

module Tc1 where

data Pair = Pair Int Int
data Triple = Triple Int Int Int

class Sum a where
  sumFields :: a -> Int 

instance Sum Pair where
  sumFields x =
    case x of
      Pair a b -> a + b

instance Sum Triple where
  sumFields x =
    case x of
      Triple a b c -> a + b + c

main :: IO ()
main = putStrLn (show result)

result :: Int
result = invokeTcMethods

invokeTcMethods :: Int
invokeTcMethods = (sumFields (Pair 10 20)) + (sumFields (Triple 12 3 2))
