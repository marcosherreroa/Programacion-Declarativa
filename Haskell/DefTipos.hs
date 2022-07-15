type Coordenada = Float
type Punto = (Coordenada, Coordenada)

distancia:: Punto -> Punto -> Float
distancia (x,y) (x',y') = sqrt((x-x')^2 + (y-y')^2)

-- Error , no pueden ser recursivos:type T = (Int,[T])

type Terna a = (a,a,a)

data DiaSemana = L|M|X|J|V|S|D deriving (Eq, Ord, Enum, Show)
ayer L = D

data Palo = Oros|Copas|Espadas|Bastos deriving (Eq, Show)
data Carta = Carta Int Palo deriving (Eq, Show)

data Nat = Cero | Suc Nat deriving (Eq, Ord, Show)
instance Enum Nat where
 toEnum 0 = Cero
 toEnum n = Suc (toEnum (n-1))
 fromEnum Cero = 0
 fromEnum (Suc x) = 1 + fromEnum x

data List a = Nil | Cons a (List a)

--data Arbol a = Hoja a | Nodo (Arbol a) (Arbol a)
--data Arbol' a b = Hoja' a| Nodo' b (Arbol' a b) (Arbol' a b)
data ArbolGen a = Hoja a | Nodo [ArbolGen a]
data ArbolGen' a b = Hoja' a| Nodo' b [ArbolGen' a b] deriving Show

--class Eq' a where
--(==), (/=) :: a -> a -> Bool
--x == y = not (x /= y)
--x /= y = not 

type Numerador = Integer
type Denominador = Integer
infixl 7 :/
data Fraccion = Numerador :/ Denominador -- deriving (Eq, Ord) lo hace como no queremos
instance Eq Fraccion where
 a:/b == c:/d = a * d == b * c
instance Ord Fraccion where
 a:/b <= c:/d
   | signum b == signum d = a*d <= b*c
   | otherwise = a*d >= b*c
 