--Marcos Herrero

--1a)
--Con foldl
last'::[a]->a
last' [] = error "lista vacía"
last' (z:zs) = foldl (\_ y-> y) z (z:zs)

--1b) 
--Con foldl
reverse'::[a]->[a]
reverse'= foldl (\xs y -> y:xs) []

--Se puede resolver utilizando foldr, pero requiere usar concatenaciones, así que es mucho menos
--eficiente (cuadrático en vez de lineal). Con foldr:
reverse''::[a]->[a]
reverse''= foldr (\x ys -> ys ++ [x]) []

--1c)
-- foldr solo analiza la lista hasta que encuentra un elemento que no cumple la condición, así que esta función
-- se puede usar con listas infinitas (devuelve False en tiempo finito)
all'::(a->Bool)->[a]->Bool
all' p = foldr (\x y -> p x && y) True

--Con foldl siempre tendríamos que recorrer toda la lista, mucha peor opción 
all''::(a->Bool)->[a]->Bool
all'' p = foldl (\x y -> x && p y) True

--1d)
--En este caso el foldr y foldl son intercambiables, en funcionalidad.
-- Dado que siempre hay que mirar toda la lista, mejor usar foldl, que utiliza recursión final
minimum'::Ord a=>[a]->a
minimum' [] = error "lista vacía"
minimum' (z:zs) = foldl min z (z:zs)

--1e)
map'::(a->a)->[a]->[a]
map' f = foldr (\x ys -> (f x):ys) []

--Para usar foldl habría que hacer concatenaciones, y sería menos eficiente
map''::(a->a)->[a]->[a]
map'' f = foldl (\xs y -> xs ++ [f y]) []

--1f)
--Con foldr
-- en vez de usar la función decide , se puede hacer con lambda abstraccion y un if*
-- foldr (\x ys -> if p x then x:ys else ys) []
filter'::(a->Bool)->[a]->[a]
filter' p = foldr decide [] 
            where decide x ys
                    | p x   = x:ys
                    | otherwise = ys

--Con foldl (peor, porque usa concatenaciones)

filter''::(a->Bool)->[a]->[a]
filter'' p = foldl decide []
            where decide xs y
                    | p y = xs ++ [y]
                    | otherwise = xs 

--1g)
-- Con foldr. Es la buena opción porque no procesa toda la lista, así que puede acabar en listas infinitas
takeWhile' p = foldr decide [] 
               where decide x ys
                       | p x = x:ys
                       | otherwise = []

-- Con foldl no veo forma de hacerlo

--2)
--foldr1 recursivo
foldr1'::(a->a->a)->[a]->a
foldr1' f [] = error "lista vacía"
foldr1' f (x:xs) = f x (foldr1 f xs)

--foldl1 a patir de foldr1
foldl1'::(a->a->a)->[a]->a
foldl1' f xs = foldr1' (flip f) (reverse xs)

--foldl1 a partir de foldr
foldl1''::(a->a->a)->[a]->a
foldl1'' f [] = error "lista vacía"
foldl1'' f (x:xs) = foldl f x xs  

--3a)
conOpuestos::Integral a => [a]
conOpuestos = concat [[n,-n]| n<-[1..]]

conOpuestos'::Integral a => [a]
conOpuestos' = concat $ map (\n -> [n,-n]) [1..]

--3b)
listaParesNaturales::Integral a=>[(a,a)]
listaParesNaturales = [(x,s-x) | s <- [0..], x <- [0..s]]

listaParesNaturales'::Integral a=>[(a,a)]
listaParesNaturales'= concat $ map (\s -> map (\x -> (x,s-x)) [0..s]) [0..]

--4a)
sufijos::[a]->[[a]]
sufijos xs = [drop n xs | n<- [0..length xs]]

--Otra forma
sufijos'::[a]->[[a]]
sufijos' xs = map (\n -> drop n xs) [0..length xs]

--4b)
sublistas::[a]->[[a]]
sublistas xs = [take n (drop m xs)| n <- [0..length xs], m <-[0..length xs - n], n/= 0 || m == 0]

--Otra forma:
sublistas'::[a]->[[a]]
sublistas' xs = concat $ map(\n -> map (\m -> take n (drop m xs))$ filter (\m -> n/= 0 || m == 0 ) [0..length xs - n]) [0..length xs]
     
--4c)
--insert inserta el elemento x en la posición n de la lista ys
insert::Integral b => a->b -> [a] -> [a]
insert x 0 ys = x:ys
insert _ _ [] = error "fuera de rango"
insert x n (y:ys) = y:insert x (n-1) ys

permutaciones::[a]->[[a]]
permutaciones [] = []
permutaciones [x] = [[x]]
permutaciones (x:xs) = concat[map (insert x n) (permutaciones xs)| n <- [0..length xs]] 

{--SOl Susana
intercala x []
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

permutaciones []
permutaciones (x:xs) = concat [intercala x ys | ys <- permutaciones xs]
--}

--4d) Pensar como quitar los reptidos (si eso)
sumandos::Integral a => a -> [[a]]
sumandos 0 = [[]]
sumandos n = concat[map (m:) (sumandos (n-m)) | m <- [1..n]]

sumandosSinRep::Integral a => a -> [[a]]
sumandosSinRep n = sumandosDesdep n 1

sumandosDesdep::Integral a => a-> a -> [[a]]
sumandosDesdep 0 _ = [[]]
sumandosDesdep n p = concat [map (m:) (sumandosDesdep (n-m) m) | m <- [p..n], m <= n]