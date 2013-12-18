result = putStrLn (show (142))

{-

- the code below is not recognized by GHC, only by GIC

showNum x =
  if x==0 then []
  else if x<0 then (45):(showNum (-x))
       else append (showNum (x `div` 10)) [(x `mod` 10)+48]
append as bs =
  case as of
    []   -> bs
    x:xs -> x:(append xs bs)
-}
