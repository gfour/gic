-- Tests records and their selection/updating functions.

data Person = Person { pAge :: Int , pShoeSize :: Int }

result :: Int
result = (nextYear p1) + (nextYear p2) + (nextYear p3) + (nextYear p4)

p1 :: Person
p1 = Person 30 40

-- test field update
p2 :: Person
p2 = p1{pAge=10}

-- test partial initialization, Haskell98 report, 3.15.2
-- needs -w to be accepted by GHCi
p3 :: Person
p3 = Person {pAge = 10}

-- named field initialization
p4 :: Person
p4 = Person {pShoeSize = 11, pAge = 10}

nextYear :: Person -> Int
nextYear p = pAge p + 1
