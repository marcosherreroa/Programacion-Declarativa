-- Marcos Herrero

--1a)
--Devuelve la lista de los cuadrados de los números enteros en [ini, fin]. Coste: O(n)
cuadradosAux::Integral a => a -> a -> [a]
cuadradosAux ini fin
 | fin < ini = []
 | otherwise = ini^2 : (cuadradosAux (ini+1) fin)

{--Devuelve la lista de los cuadrados de los números naturales en [0,n]. Coste: O(n)
Observación: hay que usar la función auxiliar para poder construir la lista de delante
a atrás. De otra manera, si se usa el operador ++, el coste asintótico aumenta a O(n^2)--}

cuadrados::Integral a => a -> [a]
cuadrados n = cuadradosAux 0 n
 

--1b)
-- Devuelve la lista de los números naturales en [0,n] emparejados con sus cuadrados y en orden inverso. Coste: O(n)
emparejCuadrados::Integral a => a -> [(a,a)]
emparejCuadrados 0 = [(0,0)]
emparejCuadrados n = (n, n^2) : emparejCuadrados (n-1)

--1c)
-- Devuelve la suma pedida. Coste: O(n+ coste de calcular cos(n))
sumaCos::(Integral a, Floating b) => a -> b
sumaCos 0 = 0
sumaCos n = sumaCos (n-1) + fromIntegral n * abs(cos(fromIntegral n))

--1d)
-- Devuelve la suma de los números menores que n múltiplos de 3 o 5. Coste O(n)
sumaMult35:: Integral a => a -> a
sumaMult35 0 = 0
sumaMult35 n
 | mod (n-1) 3 == 0 || mod (n-1) 5 == 0 = sumaMult35 (n-1) + n-1
 | otherwise = sumaMult35 (n-1)

--1e)
-- npot3aux pot n devuelve el número de potencias de 3 que acaban en 43 en [pot,n) .
-- Precondición: pot es potencia de 3. Coste: O(log n)

npot3aux::Integral a => a -> a -> a
npot3aux pot n
 | n <= pot = 0
 | mod pot 100 == 43 = 1 + npot3aux (3*pot) n
 | otherwise = npot3aux (3*pot) n

--npot3fin43 devuelve el número de potencias de 3 que acaban en 43 menores que n. Coste: O(log n)

npot3fin43:: Integral a => a -> a
npot3fin43 = npot3aux 1 

--1f)

-- devuelve True si n es divisible por algún número en [d,n). Coste: O(n-d)
esDivisible::Integral a => a -> a -> Bool
esDivisible n d
 | d >= n = False
 | mod n d == 0 = True
 | otherwise = esDivisible n (d+1)

-- devuelve el primer número primo posterior a n. Coste: n^2 (por el postulado de Bertrand, hay algún primo entre n y 2n)
primoMayor::Integral a => a -> a
primoMayor n
 | not (esDivisible (n+1) 2) = n+1
 | otherwise = primoMayor (n+1)

--2a)

{-- Una opción
cuadrados'::Integral a => a -> [a]
cuadrados' n = map (^2) [0..n]

Pero se puede tambien:
--}
cuadrados'::Integral a => a -> [a]
cuadrados' = map (^2).enumFromTo 0
-- Coste: O(n)

--2b)
--Coste O(n)
emparejCuadrados'::Integral a => a -> [(a,a)]
emparejCuadrados' n = let xs = [n, n-1..0] in zip xs ( map (^2) xs)

--2c)
f::Floating a => a -> a
f n = n * abs (cos n)

--Coste O(n+ coste de calcular cos(n))
sumaCos'::(Integral a, Enum a, Floating b) => a -> b
sumaCos' n = sum (map (f.fromIntegral) [0..n])

--3a)
{--Devuelve True si f(x) = g(x) para todo x en [n,m]
Coste O((m-n) (coste f + coste g))--}

iguales::(Enum a, Eq b) => (a-> b) -> (a -> b) -> a -> a -> Bool
iguales f g n m = (map f [n..m]) == (map g [n..m]) 

--3b)
{-- Devuelve el menor x en [n,m] que verifica p
Coste: O((m-n)* coste de p)--}

menorA::Enum a => a -> a -> (a -> Bool) -> a
menorA n m p = head ( filter p [n..m] )

--3c)
{-- Devuelve el mayor x menor o igual que n que verifica p
Coste no acotado, podría no acabar--}

mayorA::Enum a => a -> (a-> Bool)-> a
mayorA n p = head (filter p [n,pred(n)..])

--3d)
{-- Devuelve True si existe x en [n,m] que verifica p
Coste O((m-n)*coste de p)--}
ex::Enum a => a -> a -> (a -> Bool) -> Bool
ex n m p = any p [n..m]

--4a)
filter2::[a] -> (a -> Bool) -> (a -> Bool) -> ([a],[a])
filter2 xs p q = (filter p xs, filter q xs)

--4b)
filters::[a] -> [a -> Bool] -> [[a]]
filters xs ps = map (flip filter xs) ps 


