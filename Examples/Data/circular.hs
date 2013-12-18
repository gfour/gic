-- Taking the first and the second element of an infinite stream, described
-- by an infinite data structure.
data Stream = Cons Int Stream ;
result = (head2 circ) + (head2 (tail2 circ)) ;
circ = Cons 10 circ ; 
head2 hl = case hl of Cons cons_0 cons_1 -> cons_0 ;
tail2 tl = case tl of Cons cons_0 cons_1 -> cons_1
