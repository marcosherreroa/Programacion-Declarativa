
--1)a)

{--
io1a1 = do x <- getChar
           x
--}

io1a2::IO Char
io1a2 = do x <- getChar
           return x

io1a2'= getChar >>= (\x -> return x)

{--
io1a3 = do x <- getChar
           show x
--}

io1a4::IO ()
io1a4 = do x <- getChar
           print x

io1a4' = getChar >>= (\x -> print x)

--1b)
--i) No hace nada

--Bien tipado pero no funciona
io1b1:: [IO Char]
io1b1 = let x = getChar in [x,x]

-- lo siguiente lo arreglaría y sí leería dos Chars
io1b1f:: IO String
io1b1f = let x = getChar in sequence [x,x]

--ii) Lee un carácter y lo escribe una vez

io1b2::IO Char
io1b2 = do x <- getChar
           return x
           return x

io1b2' = getChar >>= (\x -> return x >> return x)

--iii) Lee un carácter y lo escribe dos veces

io1b3::IO ()
io1b3 = do x <- getChar; putStr [x,x]

io1b3' = getChar >>= (\x -> putStr [x,x])

--iv) Lee un  carácter y lo escribe dos veces

io1b4::IO()
io1b4 = do x <- getChar
           putChar x
           putChar x

io1b4' = getChar >>= (\x -> putChar x >> putChar x)

--v) Mal tipado

--io1b5 = do x <- getChar; putChar x ++ putChar x

--vi) Mal tipado

--io1b6 = do x <- getChar; putChar (x++x)

--vii) Mal tipado

--io1b7 = do x <- getChar; putStr  (x++x)

---1c)

f:: IO ()
f = do x <- getChar
       print x

g:: IO ()
g = do x <- f
       print x

--2)

palabras::String -> IO Int
palabras fileIn = do xs <- readFile fileIn
                     return (length (words xs))

--Podemos implementar nuestro propio words

words'::String -> [String]
words' xs = wordsAux xs [] where
           wordsAux::String -> String -> [String]
           wordsAux [] [] = []
           wordsAux [] p  = [reverse p]
           wordsAux (x:xs) p
             | x == ' ' || x == '\n' = reverse p : wordsAux xs []
             | otherwise             = wordsAux xs (x:p) 

--3)

palabras'::IO ()
palabras' = do putStr "Introduzca el nombre de un fichero: "
               fileIn <- getLine
               xs <- readFile fileIn
               putStrLn ("El fichero "++ fileIn ++ " tiene " ++ show (length (words xs)) ++ " palabras")

--Usando palabras
palabras''::IO()
palabras'' = do putStr "Introduzca el nombre de un fichero: "
                fileIn <- getLine
                n <- palabras fileIn
                putStrLn ("El fichero "++ fileIn ++ " tiene " ++ show n ++ " palabras")
--4)

getInt:: IO Int
getInt = do x <- getLine
            return (read x::Int)

promedia:: IO ()
promedia = promediaAux 0 0 where
  promediaAux:: Int -> Int -> IO ()
  promediaAux suma numero = do x <- getInt
                               if x == -1 then return ()
                                          else do putStrLn ("Suma: "++ show (suma + x))
                                                  putStrLn ("Promedio: "++ show ( fromIntegral(suma + x)/fromIntegral(numero + 1)))
                                                  promediaAux (suma + x) (numero + 1)
                                                  
                                                
       
