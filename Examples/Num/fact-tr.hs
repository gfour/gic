result = fact 5 1;
fact n r = if n == 0 then r else fact (n-1) (n*r)
