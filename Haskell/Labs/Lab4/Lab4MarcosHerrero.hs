--Marcos Herrero

--1)
type Punto = (Int,Int)
data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Eq, Ord,Show)

destino::Punto -> [Direccion] -> Punto
destino p [] = p
destino (x,y) (Arriba:zs) = destino (x,y+1) zs
destino (x,y) (Abajo:zs) = destino (x,y-1) zs
destino (x,y) (Izquierda:zs)= destino (x-1,y) zs
destino (x,y) (Derecha:zs) = destino (x+1,y) zs

--2)
data Nat = Cero | Suc Nat deriving (Eq,Ord)

(.+):: Nat -> Nat -> Nat
x .+ Cero = x
x .+ Suc y = Suc x .+ y
infixl 6 .+

(.*)::Nat -> Nat -> Nat
x .* Cero = Cero
x .* Suc y = x.*y .+ x
infixl 7 .*

natToInt::Nat->Int
natToInt Cero = 0
natToInt (Suc x) = natToInt x + 1

instance Show Nat where
 show nat = show (natToInt nat)

--3)
data Complex = C Double Double deriving Eq 

instance Show Complex where
  show (C 0 0) = "0"
  show (C 0 y) = show y ++ "i"
  show (C x 0) = show x
  show (C x y)
    | y > 0 = show x ++ "+" ++ show y ++ "i"
    | otherwise = show x ++ show y ++ "i"

instance Num Complex where
  C x1 y1 + C x2 y2 = C (x1+x2) (y1+y2)
  
  C x1 y1 - C x2 y2 = C (x1-x2) (y1-y2)

  C x1 y1 * C x2 y2 = C (x1*x2-y1*y2) (x1*y2+y1*x2)

  abs (C x1 y1) = C (sqrt (x1^2 + y1^2)) 0

  signum (C x y) = C (signum x) (signum y)

  fromInteger n = C (fromInteger n) 0

--4)

class Medible a where 
 medida::a -> Int

instance Medible Bool where
 medida False = 0
 medida True = 1

instance Medible a => Medible [a] where
 medida xs = sum (map (medida) xs)

instance (Medible a, Medible b ) => Medible (a,b) where
 medida (x,y) = medida x + medida y

  




