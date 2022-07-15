head::[a] -> a -- Coste constante
head [] = error "Lista vacía"
head (x:xs) = x

tail::[a] -> [a] -- Coste constante
tail [] = error "Lista vacía"
tail (x:xs) = xs

last::[a] -> a -- Coste lineal
last [] = error "Lista vacía"
last [x] = x
last (x:xs) = last xs

init::[a] -> [a] -- Coste lineal
init [] = error "Lista vacía"
init (x:xs) = (x: init[xs])

null::[a] -> Bool -- Coste constante
null [] = True
null _ = False

length::[a] -> Int -- Coste lineal
length [] = 0
length (x:xs) = 1 + length(xs)

(++)::[a] -> [a] -> [a] -- Coste lineal
(++) [] ys = ys
(++) (x:xs) ys = x:(xs ++ ys)
infixl 6 ++

