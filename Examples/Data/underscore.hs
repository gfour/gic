-- Test for '_' binders.

data List = Nil | Cons Int List
result = test l
l = Cons 1 (Cons 2 (Cons 3 Nil))
test x =
  case x of
    Nil      -> 0
    Cons a _ -> a
    