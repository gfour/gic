size :: Int
size = 12

result :: Int
result = count size

count :: Int -> Int
count n = length2 (queens n)

data List = Nil | Cons Int List
data LList = LNil | LCons List LList

queens :: Int -> LList
queens n = gen n n

gen :: Int -> Int -> LList
gen nq n = if n == 0 then LCons Nil LNil else gen_1 nq (gen nq (n-1))

gen_1 :: Int -> LList -> LList
gen_1 nq bs =
  case bs of
    LNil -> LNil
    LCons b bs' -> gen_2 nq b 1 (gen_1 nq bs')

gen_2 :: Int -> List -> Int -> LList -> LList
gen_2 nq b q rest = if q <= nq then
                      if safe q 1 b then
                        LCons (Cons q b) (gen_2 nq b (q+1) rest)
                      else
                        gen_2 nq b (q+1) rest
                    else
                      rest

safe :: Int -> Int -> List -> Bool
safe x d qs =
  case qs of
    Nil -> True
    Cons q l -> (x /= q) && (x /= (q+d)) && (x /= (q-d)) && (safe x (d+1) l)

length2 :: LList -> Int
length2 bs =
  case bs of
    LNil -> 0
    LCons b bs' -> 1 + length2 bs'
