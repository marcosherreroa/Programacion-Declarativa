g:: [a] -> a
--g (x:xs) = head xs
--g (_:x:xs) = x
g (_:[]) = head[]

f:: Int -> Int -> Int
f 0 1 = 2
f x y
 | x > 0 = y
 | otherwise = x

--como tiene que comprobar primero el caso base, es estricta en su primer argumento
-- f undefined _ = undefined siempre
-- en cambio, f -1 undefined = -1, asi que no es estricta en el segundo arg

f':: Int -> Int -> Int
f' x y
 |x > 0 = y
 |otherwise = x

-- nuevamentes es estricta en el primer argumento, f undefined _ = undefined
-- en cambio, f 0 undefined = 0, asi que no es estricta en el segundo

h (x+y) = 12