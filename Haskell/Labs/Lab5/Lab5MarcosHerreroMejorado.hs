-- Marcos Herrero

--1)

getInt::IO Int
getInt = do xs <- getLine
            return (read xs::Int)

adivina:: Int -> IO ()
adivina n = do putStr "Introduce un número: "
               x <- getInt
               if x == n then putStrLn "¡Has acertado!" else
                 if x < n then do putStrLn "El número que buscas es mayor..."
                                  adivina n
                 else do putStrLn "El número que buscas es menor..."
                         adivina n

--2)

-- Añade todos los espacios necesarios delante de las palabras dadas
añadirEspacios::Int -> Int -> [String] -> [String]
añadirEspacios espacios sobran ps = map (addEspPalabra (espacios+1)) (take sobran ps) ++ map (addEspPalabra espacios) (drop sobran ps)
  where addEspPalabra::Int -> String -> String
        addEspPalabra n x = replicate n ' '++ x

-- Formatea una linea de texto
formatearLinea:: Int -> String -> String
formatearLinea n s = let palabras = words s
                         npalabras = length palabras
                         long = sum (map length palabras) + npalabras - 1
                         espaciosPorPalabra =  div (n-long)(npalabras-1)
                         resto = mod (n-long)(npalabras-1)
                          in  if n <= long || npalabras == 1  then unwords(palabras)
                              else unwords (head palabras: añadirEspacios espaciosPorPalabra resto (tail palabras))


--Formatea un texto
formatearTexto:: Int -> String -> String
formatearTexto n s = unlines(map (formatearLinea n) (lines s))

formatea::String -> String -> Int -> IO ()
formatea fileIn fileOut n =  do input <- readFile fileIn
                                writeFile fileOut (formatearTexto n input)
       
--3a)

type Matriz a = [[a]]

-- Devuelve True si es una matriz valida (todas las filas tienen la misma longitud)
esMatriz::[[a]] -> Bool
esMatriz [] = True
esMatriz m = and (map (== length (head m)) (map length (tail m))) 

--Devuelve True si las dos matrices tienen las mismas dimensiones
mismaDim:: Matriz a -> Matriz a -> Bool
mismaDim [] [] = True
mismaDim [] _  = False
mismaDim _ []  = False
mismaDim m1 m2 = length m1 == length m2 && length (head m1) == length (head m2) 

--i)

transp:: Matriz a -> Matriz a
transp [] = []
transp [[x]] = [[x]]
transp m
 | not (esMatriz m) = error "El argumento no es una matriz"
 | length(head m) == 0  = []
 | otherwise        = map head m : transp (map tail m)

-- Otra opcion

transp':: Matriz a -> Matriz a
transp' []    = []
transp' (x:xs)
 | not (esMatriz (x:xs)) = error "El argumento no es una matriz" 
 | otherwise             = (head x : map head xs): zipWith (:) (tail x) (transp'(map tail xs))



--ii)

sumaMat:: Num a => Matriz a -> Matriz a -> Matriz a
sumaMat m1 m2 
 | not (esMatriz m1) = error "El primer argumento no es una matriz" 
 | not (esMatriz m2) = error "El segundo argumento no es una matriz" 
 | not (mismaDim m1 m2) = error "Las matrices dadas no tienen la misma dimensión"
 | otherwise            = zipWith (zipWith(+)) m1 m2

--iii)

prodMat:: Num a => Matriz a -> Matriz a -> Matriz a
prodMat m1 m2 
 | not (esMatriz m1) = error "El primer argumento no es una matriz" 
 | not (esMatriz m2) = error "El segundo argumento no es una matriz" 
 | length (head m1) /= length m2 = error "Las matrices dadas no tienen las dimensiones adecuadas"
 | otherwise  =  [[ sum (zipWith (*) (m1 !! i) ((transp m2) !! j)) | j <- [0..length (head m2) - 1] ] | i <- [0..length m1 - 1]]

--3b)

dibujaMatriz :: Show a => Matriz a -> IO()
dibujaMatriz [] = return ()
dibujaMatriz (x:xs)
 | not (esMatriz (x:xs)) = error "El argumento dado no es una matriz"
 | otherwise             = do putStrLn (unwords (map show x))
                              dibujaMatriz xs 

-- Otra forma, sin unwords

dibujaMatriz':: Show a => Matriz a -> IO()
dibujaMatriz' [] = return ()
dibujaMatriz' (x:xs)
 | not (esMatriz (x:xs)) = error "El argumento dado no es una matriz"
 | otherwise             = do imprimirFila x
                              dibujaMatriz' xs

 where imprimirFila::Show a => [a]-> IO ()
       imprimirFila []    = putStr "\n"
       imprimirFila (x:xs) = do putStr (show x ++ " ")
                                imprimirFila xs
                              
                              


