--Devuelve true si x está en la lista y no es el último elemento. Coste n = tamaño de la lista
haySiguienteA::Eq a => a -> [a] -> Bool
haySiguienteA _ [] = False
haySiguienteA _ [_] = False 
haySiguienteA x (y:ys)
 | x == y = True
 | otherwise = haySiguienteA x ys

--Devuelve el elemento inmediatamente posterior a la primera aparición de x en la lista, en caso de existir. Coste n = tam de la lista
siguienteA::Eq a => a -> [a] -> a
siguienteA _ [] = error "No hay ningún elemento en las lista después del dado"
siguienteA _ [_] = error "No hay ningún elemento en la lista después del dado"
siguienteA x (y:ys) 
 | x == y = head ys
 | otherwise = siguienteA x ys

--Elimina la primera aparicion de x en la lista. Coste n = tamaño de la lista
borrarElemento::Eq a => a -> [a] -> [a]
borrarElemento _ [] = []
borrarElemento x (y:ys)
 | x == y = ys
 | otherwise = y: borrarElemento x ys


--Devuelve una lista con todos lo números primos menores o iguales que n. Coste n^2
cribaAux::Integral a=> a -> a -> a -> [a] -> [a]
cribaAux n act mult primos
 | act*mult <= n = cribaAux n act (mult+1) (borrarElemento (act*mult) primos)
 | haySiguienteA act primos && siguienteA act primos <= floor (sqrt (fromIntegral n)) = cribaAux n (siguienteA act primos) 2 primos
 | otherwise = primos


criba::Integral a => a -> [a]
criba n = cribaAux n 2 2 [2..n]

esPrimo n = elem n (criba n)