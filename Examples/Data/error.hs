result = head2 []
head2 l = case l of
            []   -> error "empty list!"
            x:xs -> x