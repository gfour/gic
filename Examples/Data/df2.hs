-- defunctionalization test

result = (double sq 10) + (double (add3 1 2) 1)
double f x = f (f x)
sq a = a * a
add3 i j k = i + j + k
