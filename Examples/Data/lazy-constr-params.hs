-- Test to see if constructor parameters are memoized as needed for lazy
-- languages. The two versions of the branch in the ffib function should
-- take about the same time (the second may take a bit more due to the
-- repeated thunk accesses, but the difference should be tiny here).
data Box = Box Int ;
result = case (Box boundComputation) of
  -- Box box_0 -> (box_0 + box_0) + (box_0 + (box_0 + box_0)) ;
  Box box_0 -> 5 * box_0 ;
boundComputation = fib 5 ; -- fib 35 ;
fib x = if x<2 then 1 else (fib (x-1)) + (fib (x-2))
