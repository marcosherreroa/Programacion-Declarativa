-- Marcos Herrero

--1a)
cuadrados::Integral a => a -> [a]
cuadrados 0 = [0]
cuadrados n = cuadrados (n-1) ++ [n^2]

--1b)
emparejCuadrados::Integral a => a -> [(a,a)]
emparejCuadrados 0 = [(0,0)]
emparejCuadrados n = (n, n^2) : emparejCuadrados (n-1)

--1c)
sumaCos::(Eq a, Floating a) => a -> a
sumaCos 0 = 0
sumaCos n = sumaCos (n-1) + n* abs(cos(n))

--1d)
sumaMult35:: Integral a => a -> a
sumaMult35 0 = 0
sumaMult35 n
 | mod (n-1) 3 == 0 || mod (n-1) 5 == 0 = sumaMult35 (n-1) + n-1
 | otherwise = sumaMult35 (n-1)

--2a)

{-- Una opcion
cuadrados'::Integral a => a -> [a]
cuadrados' n = map (^2) [0..n]

Pero se puede tambien:
--}

cuadrados'::Integral a => a -> [a]
cuadrados' = map (^2).enumFromTo 0

--2b)
emparejCuadrados'::Integral a => a -> [(a,a)]
emparejCuadrados' n = let xs = [n, n-1..0] in zip xs $ map (^2) xs

--2c)
f::Floating a => a -> a
f n = n * abs (cos(n))

sumaCos'::(Floating a, Enum a) => a -> a
sumaCos' n = sum (map f $ enumFromTo 0 n)

--3a)
iguales::(Enum a, Eq b) => (a-> b) -> (a -> b) -> a -> a -> Bool
iguales f g n m = (map f [n..m]) == (map g [n..m]) 

--3b)
menorA::Enum a => a -> a -> (a -> Bool) -> a
menorA n m p = head $ dropWhile (not.p) [n..m]

--3c)
mayorA::Enum a => a -> (a-> Bool)-> a
mayorA n p = head (dropWhile (not.p) [n,pred(n)..])

--3d)
ex::Enum a => a -> a -> (a -> Bool) -> Bool
ex n m p = not (null (dropWhile (not.p) [n..m]))

--4a)
filter2::[a] -> (a -> Bool) -> (a -> Bool) -> ([a],[a])
filter2 xs p q = (filter p xs, filter q xs)

--4b)
filters::[a] -> [a -> Bool] -> [[a]]
filters xs ps = map (flip filter xs) ps 


