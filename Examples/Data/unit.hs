module Main where
main = putStrLn (show result) ;

result = test u
u = ()
test x =
  case x of
    () -> 42
