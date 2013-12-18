result :: IO ()
result = sequ (putStr   (takeI 3 "hello")) (
         sequ (putStr   "-"              ) (
              (putStrLn (dropI 3 "hello")) ))
dropI :: Int -> String -> String
dropI i l = if i==0 then l else case l of x:xs -> dropI (i-1) xs
takeI :: Int -> String -> String
takeI i l = if i==0 then []
           else case l of
             x : xs -> x : (takeI (i-1) xs)
sequ :: IO () -> IO () -> IO ()
sequ a b =
  do () <- a
     b 
