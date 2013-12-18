-- test for use of the ' symbol with the C back-end
data List' = Nil' | Cons' Int List'
result = head2 z'
z' = Cons' 10 (Cons' 20 Nil')
head2 x' = case x' of Cons' c cs -> c
