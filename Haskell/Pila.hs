data Pila = P [Either Int Char] deriving Show

pushInt x (P xs) = P (Left x:xs)
pushChar x (P xs) = P (Right x:xs)

top (P []) = error "Pila vacía"
top (P (x:xs)) = x

pop (P []) = error "Pila vacía"
pop (P (x:xs)) = P xs