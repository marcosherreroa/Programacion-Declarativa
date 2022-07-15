-- Marcos Herrero Agustín
-- DG Matemáticas-Informática

data Rel a = R [(a,a)] deriving (Read, Show)

-- Constantes para probar la practica
-- r1,r2 y r3 son relaciones de equivalencia
-- r2 y r3 son iguales pese a estar definidas de manera diferente
-- r4 es reflexiva y simétrica pero no transitiva
-- r5 es reflexiva y transitiva pero no simétrica 
-- r6 es reflexiva pero ni simétrica ni transitiva
-- r7 es simétrica pero ni reflexiva ni transitiva
-- r8 es transitiva pero ni reflexiva ni simétrica 
-- r9 no es ni reflexiva ni simétrica ni transitiva
-- falsa no es una autentica relacion, ya que incluye repetidos


r1 = R [(True,True),(False,False)]
r2 = R [(1,1),(2,2),(3,3),(1,2),(2,1)]
r3 = R [(2,2),(1,1),(3,3),(1,2),(2,1)]
r4 = R [('a','a'),('a','b'),('b','a'),('b','b'),('b','c'),('c','b'),('c','c')]
r5 = R [(4,4),(4,5),(5,5)]
r6 = R [(2,2),(2,3),(3,2),(3,3),(3,4),(4,4)]
r7 = R [('x','y'),('y','z'),('x','z'),('y','x'),('z','y'),('z','x')]
r8 = R [(1,2),(2,3),(1,3)]
r9 = R [(1,2),(2,3),(1,4)]


falsa = R [(1,2), (3,1), (1,3), (1,2)]

--1)
esRelacion::Eq a => Rel a -> Bool
esRelacion (R []) = True
esRelacion (R (x:xs)) = not (elem x xs) && esRelacion (R xs)

--2)
--contenido conjuntista para listas
contenido::Eq a => [a] -> [a] -> Bool
contenido [] _ = True
contenido (x:xs) ys = elem x ys && contenido xs ys 

--igualdad conjuntista para listas
igConj::Eq a => [a] -> [a] -> Bool
igConj xs ys = contenido xs ys && contenido ys xs

instance Eq a => Eq (Rel a) where
 R xs == R ys = igConj xs ys
 
--3)

-- elimina los duplicados de una lista 
elimDup::Eq a => [a]->[a]
elimDup [] = []
elimDup (x:xs)
 | elem x xs = elimDup xs
 | otherwise = x:(elimDup xs)

{--
Para relaciones en las que los elementos del soporte tienen tipo en la clase Ord estaría mejor utilizar esta otra,
que además de eliminar duplicados ordena la lista. Además, es mas eficiente. No la he usado para no imponer esta 
condición sobre el soporte, que solo sabemos que ha de estar en la clase Eq

elimDupOrd::Ord a => [a]->[a]
elimDupOrd [] = []
elimDupOrd (x:xs) = elimDupOrd(menores x xs) ++ [x] ++ elimDup (mayores x xs)
   where menores::Ord a => a -> [a] -> [a]
         menores x xs = [a | a <- xs, a < x]
         mayores::Ord a => a -> [a] -> [a]
         mayores x xs = [a | a <- xs, a > x]

--}

-- Apartado a) : dominio

dominio:: (Eq a, Show a) => Rel a -> [a]
dominio (R xs)
 | not (esRelacion (R xs)) = error (show(R xs)++" no es una relación")
 | otherwise = elimDup(map fst xs)

-- rango: análogo al dominio. Nos será útil más tarde
rango:: (Eq a, Show a) => Rel a -> [a]
rango (R xs)
 | not (esRelacion (R xs)) = error (show(R xs)++" no es una relación")
 | otherwise = elimDup(map snd xs)

-- Apartado b) : soporte

soporte:: (Eq a, Show a) => Rel a -> [a]
soporte (R xs)
 | not (esRelacion (R xs)) = error (show (R xs)++" no es una relación")
 | otherwise = elimDup ( map fst xs ++ map snd xs)

-- Apartado c) : relEquivalencia

reflexiva:: (Eq a, Show a) => Rel a -> Bool
reflexiva (R xs) = igConj (soporte (R xs)) (map fst (filter (\(x,y) -> x == y) xs))
-- ya se comprueba en soporte que es una relacion

simetrica:: (Eq a, Show a) => Rel a -> Bool
simetrica (R xs) 
 | not (esRelacion (R xs)) = error (show (R xs)++" no es una relación")
 | otherwise = igConj xs (map (\(x,y) -> (y,x)) xs)

transitiva:: (Eq a, Show a) => Rel a -> Bool
transitiva (R xs)
 | not (esRelacion (R xs)) = error (show (R xs)++" no es una relación")
 | otherwise = and (map (flip elem xs)  [(x,z) | (x,y1) <- xs, (y2,z) <- xs, y1 == y2])

relEquivalencia:: (Eq a, Show a) => Rel a -> Bool
relEquivalencia r = reflexiva r && simetrica r && transitiva r

-- Apartado d) : conjCociente

conjCociente:: (Eq a, Show a) => Rel a -> [[a]]
conjCociente (R xs)
 | not(relEquivalencia (R xs)) = error (show (R xs)++" no es una relación de equivalencia")
 | otherwise = elimDup [[y | y <- rango (R xs) , elem (x,y) xs] | x <- dominio (R xs)]

-- Apartado e) : generaDiv
-- Interpreto que n <= x <= m, n <= y <= m

generaDiv :: Integral a => a -> a -> Rel a
generaDiv n m = R (concat [[(x,y) | x <- [n..m], (x == 0 && y == 0) || (x /= 0 && mod y x == 0)] | y <- [n..m]])

--Apartado f) : generaGE

generaGE:: Ord a => [a] -> Rel a
generaGE xs = R (concat [[(x,y) | x <- xs,  x >= y] | y <- xs])

--Apartado g) : cierreRST 

cierreReflexivo:: (Eq a, Show a) => Rel a -> Rel a
cierreReflexivo (R xs) 
 | not (esRelacion (R xs)) = error (show (R xs)++" no es una relación")
 | otherwise = R (elimDup (concat (xs :[[(x,x),(y,y)] | (x,y) <- xs ])))

cierreSimetrico:: (Eq a, Show a) => Rel a -> Rel a
cierreSimetrico (R xs)
 | not (esRelacion (R xs)) = error (show (R xs)++" no es una relación")
 | otherwise =  R (elimDup (concat [ xs, [(y,x) | (x,y) <- xs ]]))

cierreTransitivo:: (Eq a, Show a) => Rel a -> Rel a
cierreTransitivo (R xs) 
 | not (esRelacion (R xs)) = error (show (R xs)++" no es una relación")
 | otherwise = R (iteraCierre xs)
 where iteraCierre:: Eq a => [(a,a)] -> [(a,a)]
       iteraCierre xs = let res = elimDup (concat [ xs, [(x,z) | (x,y1) <- xs, (y2,z) <- xs, y1 == y2 ]]) in
                            if res == xs then res
                                           else iteraCierre res

cierreRST:: (Eq a , Show a) => Rel a -> Rel a
cierreRST rel = let res = cierreReflexivo (cierreTransitivo (cierreSimetrico rel)) in
                if res == rel then res
                               else cierreRST res
                

--Apartado h) : composicion

composicion:: (Eq a, Show a) => Rel a -> Rel a -> Rel a
composicion (R xs) (R ys)
  | not(esRelacion (R xs)) = error (show (R xs) ++ " no es una relación")
  | not(esRelacion (R ys)) = error (show (R xs) ++ " no es una relación")
  | otherwise  = R (elimDup [(x,z) | (x,y1) <- xs, (y2,z) <- ys, y1 == y2])


--4)

--Apartado a) : introRel

leerRelChar::IO [(Char,Char)]
leerRelChar = do putStrLn "Introduzca un par (escribir los elementos a relacionar separados por un espacio) o presione enter para terminar"
                 line <- getLine
                 let elems = words line
                 if length elems == 0 then return [] else
                  if length elems /= 2 then error "Número de elementos incorrecto"
                                      else do xs <- leerRelChar
                                              return ((read (head(elems)),read (head(tail elems))):xs)
               
      

introRelChar:: IO (Rel Char)
introRelChar = do putStrLn "Leyendo relación ..."
                  xs <- leerRelChar
                  return (R xs)
              
leerRelInt::IO [(Int,Int)]
leerRelInt = do  putStrLn "Introduzca un par (escribir los elementos a relacionar separados por un espacio) o presione enter para terminar"
                 line <- getLine
                 let elems = words line
                 if length elems == 0 then return [] else
                  if length elems /= 2 then error "Número de elementos incorrecto"
                                      else do xs <- leerRelInt
                                              return ((read (head(elems)),read (head(tail elems))):xs)
               
      

introRelInt:: IO (Rel Int)
introRelInt = do  putStrLn "Leyendo relación ..."
                  xs <- leerRelInt
                  return (R xs)


leerRelBool::IO [(Bool,Bool)]
leerRelBool = do  putStrLn "Introduzca un par (escribir los elementos a relacionar separados por un espacio) o presione enter para terminar"
                  line <- getLine
                  let elems = words line
                  if length elems == 0 then return [] else
                   if length elems /= 2 then error "Número de elementos incorrecto"
                                      else do xs <- leerRelBool
                                              return ((read (head(elems)),read (head(tail elems))):xs)
               
      

introRelBool:: IO (Rel Bool)
introRelBool = do  putStrLn "Leyendo relación ..."
                   xs <- leerRelBool
                   return (R xs)

-- Apartado b): muestraRel

imprimeLista:: Show a => [a] -> IO ()
imprimeLista [] = return ()
imprimeLista (x:xs) = do putChar ' '
                         putStr (show x)
                         putChar ' '
                         imprimeLista xs

imprimeCruces:: (Eq a, Show a) => a -> [a] -> Rel a -> IO ()
imprimeCruces _ [] _ = return ()
imprimeCruces x (y:ys) (R xs) = do if elem (x,y) xs then putStr " x " 
                                                    else putStr "   "
                                   imprimeCruces x ys (R xs)

imprimeFila:: (Eq a, Show a) => [a] -> [a] -> Rel a -> IO ()
imprimeFila [] _ _ = putChar '\n'
imprimeFila (x:xs) ran rel = do putStr (show x)
                                putStr " |"
                                imprimeCruces x ran rel
                                putStrLn "|"
                                imprimeFila xs ran rel


muestraRelInt:: IO ()
muestraRelInt = do rel <- introRelInt
                   let dom = dominio rel
                   let ran = rango rel
                   putStr (take 3 (repeat ' '))
                   imprimeLista  ran
                   putChar '\n'
                   putStr (take 3 (repeat ' '))
                   putStrLn (take (3*length(ran)) (repeat '-'))
                   imprimeFila dom ran rel

-- Para otros tipos sería bastante análogo, pero habría que retocar el espacio que se deja
              