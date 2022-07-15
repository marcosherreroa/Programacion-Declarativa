--1a) Coste O(2^n)
fibonacciNesimo:: Integral a => a -> a
fibonacciNesimo 0 = 0
fibonacciNesimo 1 = 1
fibonacciNesimo n = fibonacciNesimo (n-1) + fibonacciNesimo (n-2)

--1b) Coste O(n)
fibonacciNesimo':: Integral a => a -> a
fibonacciNesimo' n = fibGen n 1 0
  where 
  fibGen:: Integral a => a -> a -> a -> a
  fibGen 0 f1 f0 = f0
  fibGen 1 f1 f0 = f1
  fibGen n f1 f0 = fibGen (n-1) (f1 + f0) f1

--1c) Coste O(n)
listFibonacci:: Integral a => a -> [a]
listFibonacci n = revAux (fibGenLista n [1,0]) []
  where
  fibGenLista:: Integral a => a -> [a] -> [a]
  fibGenLista 0 xs = tail xs
  fibGenLista 1 xs = xs
  fibGenLista n xs = fibGenLista (n-1) (head xs + head (tail xs): xs)
  
  revAux:: [a] -> [a] -> [a]
  revAux [] acc = acc
  revAux (x:xs) acc = revAux xs (x:acc)

--2a)
zip3':: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' xs ys zs = map (uncurry(uncurry (,,))) (zip (zip xs ys) zs)

--2b)
imparesEn:: Integral a => [a] -> [a]
imparesEn = filter odd

--2c)
escalar:: Num a => [a] -> [a] -> a
escalar xs ys = sum (zipWith (*) xs ys)

--2d)
mcdList:: Integral a => [a] -> a
mcdList = foldr gcd 0 

--3


--6c)
paresHasta:: Integral a => a -> [a]
paresHasta n = [2*x | x <- [0..div n 2]]

--6e)
mezclaParImpar::Integral a => [a] -> [a] -> [(a,a)]
mezclaParImpar xs ys = [(x,y) | x <- xs, even x, y <- ys, odd y]

--6f)
prefijos::[a] -> [[a]]
prefijos xs = [take n xs | n <- [0..length(xs)]]


        