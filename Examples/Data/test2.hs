result = repeat_many 200000 (twice (add 1) 5);
twice f x = f(f x);
add a b = a+b;
repeat_many n x = if (n==0) then 0 else x + repeat_many (n-1) x
