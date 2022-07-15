--Ej 6
f:: Integral a => a -> a -> a
f x y = x - y

g:: Integral a => a -> a
g x = -x

--h:: Integral a => a -> a
h x = -x

--Ej 9
f::Ord a => a -> a -> a -> (a, a, a)
f x y z = if x <= y then (if y <= z then (x, y, z) else
                          if x <= z then (x, z, y)
                                    else (z, x, y))
                    else (if x <= z then (y, x ,z) else
                          if y <= z then (y, z, x)
                                    else (z, y, x))

g::Ord a => a -> a -> a -> (a, a, a)
g x y z
 |x <= y && y <= z = (x, y, z)
 |x <= y && x <= z = (x, z, y)
 |x <= y = (z, x, y)
 |x <= z = (y, x, z)
 |y <= z = (y, z, x)
 |otherwise = (z, y, x)

--Ej 10
fun:: [a] -> Bool
fun (_:_:[]) = True
fun _ = False