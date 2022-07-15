--insertion
insert x (y:ys)
 | x <= y = x:y:ys
 | otherwise = y : insert x ys 

insertion::Ord a => [a] -> [a]
insertion [] = []
insertion (x:xs) =  

merge::Ord a=> [a] -> [a] -> [a]
merge (x:xs) (y:ys)
 | x <= y = x:(merge xs (y:ys))
 | otherwise = y:(merge (x:xs) ys)

--mergesort
mergesort::Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (take n xs) (drop n xs) where n = div (length xs) 2

--qsort
qsort:: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort(menores) ++ [x] ++ qsort(mayores)
               where menores = [a | a <- xs, a <= x]
                     mayores = [a | a <- xs, a > x]
