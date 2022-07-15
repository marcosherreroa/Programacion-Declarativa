--2b)
--Del revés
emparejCuadrados n = foldl (\xs x -> (x, x^2): xs) [] [n,n-1..1]

--Del derecho
emparejCuadrados' n = foldl (\xs x -> (x, x^2): xs) [] [n,n-1..1]

--2c)
sumaCos' n = foldr (\x y -> x * abs (cos x) + y) 0 [0..n]

--3a)

iguales f g n m = foldr (\x y -> f x == g x && y) True [n..m]

--3d)
aux p x y
 | p x = True
 | otherwise = y

ex n m p = foldr (aux p) False [n..m] 



