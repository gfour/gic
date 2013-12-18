fun f x = if x == 0 then f 0 + fun f 1 else 0;
one a = a+1;
two b = b+2;
id2 t = t;
diff g = id2 (fun g 0);
result = diff one + diff two
