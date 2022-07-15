--Marcos Herrero

{--1a)
Devuelve un double con el número de años (con decimales) que son 10^6 segundos
Se dividen los 10^6 segundos entre 60 segundos que tiene un minuto, 60 minutos que tiene una hora, 
24 horas que tiene un día y 365 días que tiene un año --}

añosFraca:: Double
añosFraca = 10^6/60/60/24/365

{--1b)
Devuelve una tupla con 5 enteros donde: el primero es el número de años enteros, el segundo el número de días enteros
restantes, el tercero el número de horas enteras restantes, el cuarto el número de minutos enteros restantes y el
último el número de segundos restantes --}

añosEntb:: (Integer, Integer, Integer, Integer, Integer)
añosEntb = let s = 10^6
               m = div s 60
               h = div m 60
               d = div h 24
               a = div d 365 in (a, d - 365*a, h - 24*d, m - 60*h, s - 60*m)

{-- Forma alternativa: parece peor porque repite varias veces las mismas divisiones
añosentb = let x = 10^6
           s = mod 10 60 
           m = mod (div x 60) 60
           h = mod (div x 3600) 24
           d = mod (div x (3600*24)) 365
           a = div x (3600*24*365) in  (a, d, h, m, s)
--}

{--1c)
Generaliza los dos apartados anteriores al caso en que el número, 10^6, de segundos a convertir es un parámetro de entrada.
En la segunda función suponemos que el argumento será un número entero, para que el número de segundos restantes sea 
siempre entero --}

añosFrac:: Fractional a => a -> a
añosFrac x = x/60/60/24/365

añosEnt:: Integral a => a -> (a, a, a, a, a)
añosEnt s = let m = div s 60
                h = div m 60
                d = div h 24
                a = div d 365 in (a, d - 365*a, h - 24*d, m - 60*h, s - 60*m)

{--2
Escribir  " media xs = sum[xs]/length(xs)" da problemas de tipos porque length(xs) devuelve algo de tipo Int, mientras que
la operación / solo admite argumentos de la clase Fractional, a la que Int no pertenece. Se soluciona utilizando la función
fromIntegral, que cambia el tipo de un dato de la clase Integral (en este caso de tipo Int) a "Num a => a" --}

--media:: Fractional a => [a] -> a
media [] = 0
media xs = sum xs/fromIntegral(length(xs))

--3
{--3a)
Función que devuelve el número de dígitos de un entero x. Coste lineal en el número de dígitos--}

num_digitos:: Integral a => a -> a
num_digitos x
 | x < 0 = num_digitos (-x)
 | x < 10 = 1
 | otherwise = 1 + num_digitos (div x 10)

{--3b)
sumDigitos es una función auxiliar que suma los dígitos de un número entero dado. 
Coste lineal en el número de dígitos del número --}

sumDigitos:: Integral a => a -> a
sumDigitos 0 = 0
sumDigitos x = mod x 10 + sumDigitos (div x 10)

{-- reduccion calcula la reducción de un número entero x dado, utilizando la función sumDigitos como auxiliar.
Tiene, en el caso peor, coste cuadrático en el número de dígitos del número --}
 
reduccion:: Integral a => a -> a
reduccion x
 | x < 0 = reduccion (-x)
 | x < 10 = x
 | otherwise =  reduccion (sumDigitos x)

{--3c)
Función que devuelve el coeficiente binomial de dos números naturales dados n y m. 
Coste en el orden de 2^n. Si n o m son negativos, da error--}

comb:: Integral a => a -> a -> a
comb n m 
  | n < 0 || m < 0 = error "argumento negativo"
  | n < m = 0
  | m == 0 || n == m = 1
  | otherwise = comb (n-1) m + comb (n-1) (m-1)
 
--4
--Versión estricta en el primer argumento y no en el segundo
and1:: Bool -> Bool -> Bool
and1 False _ = False
and1 True True = True

--Versión estricta en el segundo argumento y no en el primero
and2:: Bool -> Bool -> Bool
and2 _ False = False
and2 True True = True

--Versión estricta en ambos argumentos
and3:: Bool -> Bool -> Bool
and3 True True = True
and3 False _ = False

--Versión no estricta en ninguno de los argumentos
and4:: Bool -> Bool -> Bool
and4 False _ = False
and4 _ False = False
and4 True True = True
