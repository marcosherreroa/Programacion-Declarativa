
eco::IO()
eco = getLine >>= \xs -> putStr xs

getInt::IO Int
getInt = getLine >>= (\xs -> return (read xs::Int))

getInt'::IO Int
getInt' = do xs <- getLine
             return (read xs::Int)

putStr'::String -> IO()
putStr' [] = return ()
putStr' (c:cs) = putChar c >>= \_ -> putStr' cs

putStr''::String -> IO()
putStr'' [] = return ()
putStr'' (c:cs) =putChar c >> putStr'' cs

putStr'''::String -> IO()
putStr''' [] = return ()
putStr''' (c:cs) = do x <- putChar c
                      putStr''' cs

putStr''''::String -> IO()
putStr'''' [] = return ()
putStr'''' (c:cs) = do putChar c
                       putStr'''' cs

putStrLn''::String -> IO()
putStrLn'' [] = putChar '\n'
putStrLn'' (c:cs) =putChar c >> putStrLn'' cs

putStrLn''''::String -> IO()
putStrLn'''' [] = putChar '\n'
putStrLn'''' (c:cs) = do putChar c
                         putStrLn'''' cs


getLine'::IO String
getLine' = getChar >>= f
          where f::Char->IO String
                f '\n' = return []
                f x    = getLine' >>= \xs -> return (x:xs)

getLine''::IO String
getLine'' = do x <- getChar
               if x == '\n' then 
                 return [] 
                 else do xs <- getLine''
                         return (x:xs)

--Lee enteros y escribe sus cubos hasta que el entero sea 0
cubos::IO()
cubos = getInt >>= \n -> if n == 0 then return () 
                                   else print(n^3) >>= \x -> cubos

cubos'::IO()
cubos' = getInt >>= \n -> if n == 0 then return () 
                                    else print(n^3) >> cubos'

cubos''::IO()
cubos'' = do n <- getInt
             if n == 0 then return ()
                       else do print(n^3)
                               cubos'

--Leer dos enteros y restarlos

resta::IO Int
resta = putStrLn "Introduzca dos enteros:" >> getInt >>= \x -> getInt >>= \y -> return (x-y)

resta'::IO Int
resta' = do putStrLn "Introduzca dos enteros:"
            x <- getInt
            y <- getInt
            return (x-y)
