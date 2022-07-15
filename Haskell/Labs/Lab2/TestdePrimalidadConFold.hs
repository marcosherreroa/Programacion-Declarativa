f n x y = mod n x /= 0 && y

esPrimo n = foldr (f n) True[2..n-1]
