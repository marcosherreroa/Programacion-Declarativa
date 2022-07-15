--Marcos Herrero

--1
--definiciones de la práctica anterior
type Punto = (Int,Int)
data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Eq, Ord,Show)

--1a)

trayectoria::Punto -> [Direccion] -> [Punto]
trayectoria p [] = [p]
trayectoria (x,y) (Arriba:zs) = (x,y) : trayectoria (x,y+1) zs 
trayectoria (x,y) (Abajo:zs) = (x,y) : trayectoria (x,y-1) zs
trayectoria (x,y) (Izquierda:zs) = (x,y) : trayectoria (x-1,y) zs
trayectoria (x,y) (Derecha:zs) = (x,y) : trayectoria (x+1,y) zs

--1b)
{-- Si una trayectoria llega al borde del tablero n x n ya no se sigue
    moviendo en esa direccion, aunque se lo indique. El tablero es {0,...,n-1}x{0,...,n-1}
    Consideramos que una trayectoria está por encima de la otra en una cierta columna x0 si el mayor y que alcanza
    en esa columna es estrictamente mayor que el mayor y que alcanza la otra trayectoria en esa misma columna
--}

--trayectoriaN devuelve una lista los puntos de la trayectoria (teniendo en cuenta que no se pueden salir del tablero n x n)
trayectoriaN::Int -> Punto -> [Direccion] -> [Punto]
trayectoriaN _ p [] = [p]
trayectoriaN n (x,y) (Arriba:zs)
 | y < n - 1 = (x,y) : trayectoria (x,y+1) zs
trayectoriaN n (x,y) (Abajo:zs)
 | y > 0 = (x,y) : trayectoria (x,y-1) zs
trayectoriaN n (x,y) (Izquierda:zs)
 | x > 0 = (x,y) : trayectoria (x-1,y) zs
trayectoriaN n (x,y) (Derecha:zs)
 | x < n-1 = (x,y) : trayectoria (x,y+1) zs
trayectoriaN _ (x,y) (_ : zs)  = trayectoria (x,y) zs

--infConcreta comprueba si la trayectoria1 permanece inferior a la 2 empezando en un punto concreto, dado como parámetro
--Construye ambas trayectorias y, en cada columna del plano por la que pasan, comprueba si algún punto de la primera está por encima de la segunda
infConcreta::Int -> [Direccion] -> [Direccion] -> Punto -> Bool
infConcreta n movs movs' p = and [maximum ys <= maximum ys' | z <- [0..n-1], let ys = ysTrayCol t1 z, not(null ys), let ys' = ysTrayCol t2 z, not(null ys')]
  where 
        t1 = trayectoriaN n p movs
        t2 = trayectoriaN n p movs' 

        ysTrayCol:: [Punto] -> Int -> [Int]
        ysTrayCol t z = [y | (x,y) <- t , x == z]
        

-- inferior comprueba infConcreta para trayectorias empezando en cualquiera de los puntos del plano
inferior::Int -> [Direccion] -> [Direccion] -> Bool
inferior n movs movs' = and (map (infConcreta n movs movs') [(x,y) | x <- [0..n-1], y <- [0..n-1]])
   
--2)

data Arbol a = Nodo a [Arbol a]

--2a)
--i)
listaHojas::Arbol a -> [a]
listaHojas (Nodo x []) = [x]
listaHojas (Nodo x ys) = concat (map listaHojas ys)

--ii)
listaNodos::Arbol a -> [a]
listaNodos (Nodo x ys) = concat ([x]: map listaNodos ys)

--iii)
maximo::Ord a => Arbol a -> a
maximo (Nodo x []) = x
maximo (Nodo x ys) = max x (maximum (map maximo ys))
-- tambien se podria hacer maximo = maximum (listaNodos t) aunque tiene peor coste en memoria

rep::a -> Arbol b -> Arbol a
rep x (Nodo y []) = Nodo x []
rep x (Nodo y ys) = Nodo x (map (rep x) ys)

repMax::Ord a => Arbol a -> Arbol a
repMax t = rep (maximo t) t

--2b)

instance Eq a => Eq (Arbol a) where
 Nodo x xs == Nodo y ys = x == y && xs == ys

instance Ord a => Ord (Arbol a) where
 Nodo x xs <= Nodo y ys
  | x < y = True
  | x == y && xs < ys = True
  | otherwise = False
 
--2c)

espaciosYBarras 0 = ""
espaciosYBarras  n  = ' ':' ': ' ': '|':espaciosYBarras(n-1)

instance Show a => Show (Arbol a) where
 show t = showAux 0 t
   where
         showAux n (Nodo x []) = espaciosYBarras (n) ++ "\n" ++ espaciosYBarras (n) ++ "->"++show x++"\n"
         showAux n (Nodo x xs) = espaciosYBarras (n) ++ "\n" ++ espaciosYBarras(n) ++ "->"++ show x ++ "-\n" ++ concat( map (showAux (n+1)) xs)  
