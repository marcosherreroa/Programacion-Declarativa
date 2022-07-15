--1a)
lista1:: Integral a => [[a]]
--lista1 = [ map (^n) [1..20] | n <- [1..10]]
lista1 = [ [m^n | m <- [1..20]] n <- [1..10]]

--1b)
lista2:: Integral a => [[a]]
--lista2 = [ map (n^) [1..10] | n <- [1..20]]
lista2 = [ [n^m | m <- [1..10]] n <- [1..20]]

--2)
z:: Num a => a -> [a] -> a
z x y = x * last y

--g :: Num a => a -> [a] -> a -> a
--g x y u = (x + z x y ) * u

--lambda:: Num a => a -> [a] -> a -> (a,a)
--lambda x y u = ( g x y u, g x y (u+1))

--f:: Num a => a -> [a]
--f x y = map (lambda x y) y

--3a)
f::Integral a => a -> [a]
f n = [x*x | x <- [1..n], mod x 2 == 0]

f1::Integral a => a -> [a]
f1 n = map (^2) $ filter (\x -> mod x 2 == 0) [1..n]

f2:: Integral a => a -> [a]
f2 n = map (^2) [2,4..n]

--3b)
g::Integral a => a -> a -> [a]
g n m = [x+y | x <- [1..n], y <-[x..m]]

g1::Integral a => a -> a -> [a]
g1 n m = concat( map (\x -> map (x+) [x..m]) [1..n])

--3c)
h::Integral a => (a -> Bool) -> a -> a -> [a]
h p n m = [x + y| x <- [1..n], p(n-x), y <-[x..m]]

h1::Integral a => (a -> Bool) -> a -> a -> [a]
h1 p n m = concat (map (\x -> map (x+) [x..m]) (filter (\x -> p(n-x)) [1..n]))

--4a)
emparejDiv::Integral a => [(a,[a])]
emparejDiv = [(x, [y | y <- [1..x-1], mod x y == 0])| x <- [19..50]]
--emparejDiv = [(x, filter (\y -> mod x y == 0) [1..x-1])| x<-[19..50]]
--emparejDiv = [(x, [y | y <- [1..div x 2], mod x y == 0])| x <- [19..50]]

--4b)
numPerfectos::Integral a => [a]
numPerfectos = [x | x <- [1..999], x == sum [y| y <-[1..x-1], mod x y == 0]]

numPerfectos = [x | x <- [1..999], x == sum [y| y <-[1..div x 2], mod x y == 0]]
--4c)
--emparejDiv::Integral a => a -> a ->[(a,[a])]
--emparejDiv a b = [(x, [y | y <- [1..x-1], mod x y == 0])| x <- [a..b]]

--numPerfectos::Integral a => [a]
--numPerfectos a = [x | x <- [1..a], x == sum [y| y <-[1..x-1], mod x y == 0]]

--5)
minimoDesde::Integral a => (a -> Bool) -> a -> a
minimoDesde p n = head (dropWhile (not.p) [n..])

cond p x y
 | p x = x
 | otherwise = y

minimoDesde2::Integral a => (a -> Bool) -> a -> a
minimoDesde2 p n = foldr (cond p) 0 [n..]

minimoDesde3::Integral a => (a -> Bool) -> a -> a
minimoDesde3 p n = head [x | x <- [n..], p x]



