-- defunctionalization test
-- one closure of 'sq with type :: Int -> Int

result = double sq 10
double f x = f (f x)
sq a = a * a
