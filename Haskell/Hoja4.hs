--Ej1
primeroQueCumple:: (a -> Bool) -> [a] -> Maybe a
primeroQueCumple p [] = Nothing
primeroQueCumple p (x:xs)
 | p x = Just x
 | otherwise = primeroQueCumple p xs

--COn orden superior
headM [] = Nothing
headM (x:xs) = Just x
primeroQueCumple' p xs = headM $ filter p xs

--Ej2 Otra opcion es definirlo como data Cj a = C [a]
data Cj a = Vacio | Cons a (Cj a) deriving Show

creaCjVacio:: Cj a
creaCjVacio = Vacio

esCjVacio:: Cj a -> Bool
esCjVacio Vacio = True
esCjVacio _ = False

elemCj::Eq a => a -> Cj a -> Bool
elemCj x Vacio = False
elemCj x (Cons y c) = x == y || elemCj x c

conjToList::Eq a => Cj a -> [a]
conjToList Vacio = []
conjToList (Cons x c)
 | elem x listc = listc
 | otherwise = x:listc
  where listc = conjToList c

instance Eq a => Ord (Cj a) where
 Vacio <= _ = True
 Cons x c1 <= c2 = elemCj x c2 && c1 <= c2
 
instance Eq a => Eq (Cj a) where
 c1 == c2 = c1 <= c2 && c2 <= c1 

--Ej3

data Temp = Kelvin Float | Celsius Float | Fahrenheit Float

aCelsius (Kelvin x) = Celsius (x-273)
aCelsius (Celsius x) = Celsius x

instance Eq Temp where
 t1 == t2 = tCelsius1 == tCelsius2 
   where Celsius tCelsius1 = aCelsius t1
         Celsius tCelsius2 = aCelsius t2
         Kelvin tCelsius2 = aCelsius t2
--Ej4

data ArbBusq a = Hoja | Nodo a (ArbBusq a) (ArbBusq a)

buscar::Ord a => a -> ArbBusq a -> Bool
buscar x Hoja = False
buscar x (Nodo y t1 t2)
 | x == y = True
 | x < y  = buscar x t1
 | otherwise = buscar x t2

insertar::Ord a => a -> ArbBusq a -> ArbBusq a
insertar x Hoja = Nodo x Hoja Hoja
insertar x (Nodo y t1 t2)
 | x <= y    = Nodo y (insertar x t1) t2
 | otherwise = Nodo y t1 (insertar x t2)

inorden::Ord a => ArbBusq a -> [a]
inorden Hoja = []
inorden (Nodo y t1 t2) = inorden t1 ++ (y:inorden t2)

treeSort::Ord a => [a] -> [a]
treeSort xs = treeSortAux xs Hoja
  where
    treeSortAux::Ord a=> [a] -> ArbBusq a -> [a]
    treeSortAux [] t = inorden t
    treeSortAux (x:xs) t = treeSortAux xs (insertar x t)

--con fold
treeSortAux foldr insertar Hoja xs