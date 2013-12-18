-- Test for pattern matching at the level of the dummy LAR of 'result'.

data List = Nil | Cons Int List
result =
  case l of
    Nil -> 1
    Cons x xs -> 
      case xs of
        Nil -> x+x
        Cons a as -> a+a
from i = Cons i (from (i+10))
l = from 10

