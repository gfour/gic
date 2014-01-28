data List = Nil | Cons Int List

head2 :: List -> Int
head2 hl =
  case hl of 
    Cons a b -> a
                      
append1 xs ys =
  case xs of
    Nil -> ys
    Cons a b -> Cons a (append1 b ys)

reverse1 xs =
  case xs of
    Nil -> Nil
    Cons a b -> append1 (reverse1 b) (Cons a Nil)

createlist :: Int -> List
createlist n = if (n==0) then Nil else (Cons n (createlist (n-1)))

size :: Int
size = 5000

result :: Int
result = head2 (reverse1 (reverse1 (createlist size)))
