result = sum2 10000;
sum2 x = if x<=1 then 1 else x + (sum2 (x-1))
