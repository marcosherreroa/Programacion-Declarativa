--Marcos Herrero

--1a)
last'::[a]->a
last' [] = error "lista vacía"
last' (z:zs) = foldl (\x y -> y) z (z:zs)

--1b)
reverse'::[a]->[a]
reverse'= foldl (\x y -> y:x) []

--1c)
all'::(a->Bool)->[a]->Bool
all' p = foldr (\x y -> p x && y) True

--1d)
minimum'::Ord a=>[a]->a
minimum' [] = error "lista vacía"
minimum' (z:zs) = foldr min z (z:zs)

--1e)
map'::(a->a)->[a]->[a]
map' f = foldr (\x y -> (f x):y) []

--1f)
auxFilter p x ys
 | p x = x:ys
 | otherwise = ys

filter'::(a->Bool)->[a]->[a]
filter' p = foldr (auxFilter p) []

--1g)
auxtakeWhile p x ys
 | p x = x:ys
 | otherwise = [] 

takeWhile' p = foldr (auxtakeWhile p) [] 

--3a)
lista1 = concat [[n,-n]| n<-[1..]]

--3b)
listaParesNaturales = [(x,s-x) | s <- [0..], x <- [0..s]]

--4a)
sufijos xs = [drop n xs | n<- [0..length xs]]

--4b)
--sublistas xs = [drop n [drop m xs| m<- [0..n-1]] | n<- [0..length xs]]


--4d)
--sumandos n = [[