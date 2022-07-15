doble:: Num a => a -> a
doble x = x + x

sandwich:: [Char] -> [Char] -> [Char]
sandwich xs ys = let us = xs++xs
                     vs = ys++ys
                 in us++vs++us

factorial:: Integral a => a -> a
factorial n = product [1..n]

factorial2:: Integral a => a -> a
factorial2 n = if n == 0 then 1 else
               if n > 0 then n*factorial2(n-1)
                        else error "el argumento es negativo"
     

-- con ajuste de patrones
factorial3:: Integral a => a -> a
factorial3 0 = 1
factorial3 n = n*factorial(n-1)

--con guardas
factorial4:: (Integral a, Show a) => a -> a
factorial4 n
  | n == 0 = 1
  | n > 0 = n*factorial4(n-1)
  | n < 0 = error ("el argumento "++show n++" es negativo")

-- x menor o igual que y?
f:: Real a => a -> a -> Bool
f x y = if x == 0 then True else
        if y == 0 then False
                  else f (x-1) (y-1)

--con ajuste de patrones
f2:: Real a => a -> a -> Bool
f2 0 y = True
f2 x 0 = False
f2 x y = f (x-1) (y-1)

--con guardas
f3:: Real a => a -> a -> Bool
f3 x y
  | x == 0 = True
  | y == 0 = False
  | otherwise   = f (x-1) (y-1)