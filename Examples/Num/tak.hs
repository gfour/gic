result = tarai 220000 220000 220000;
tarai x y z = if y < x then tarai (tarai (x-1) y z) (tarai (y-1) z x) (tarai (z-1) x y) else z
