result = fact 6 ;
fact x = if x < 2 then 1 else x * (fact (x - 1))
