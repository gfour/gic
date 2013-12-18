-- lambda abstraction test
result = double (\z -> z+1) 10 + ((\x y -> x + y) 13 17)
double f x = f (f x)
