module ModuleA(f, g, caf1, ThisOrThat(This, That)) where 

data ThisOrThat = This Int | That Bool

f :: Int -> Int
f x = x + 1

g :: Int -> Int
g y = (y * 5) + (f caf1)

caf1 :: Int
caf1 = f 10

zfun :: ThisOrThat
zfun = This 10
