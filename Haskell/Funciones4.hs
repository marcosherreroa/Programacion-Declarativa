f'::Num a => (a,a) -> a
f'(x, y) = x*y

g::Num a => b -> (a,a) -> a
g x = f'

incList:: Num a => [a] -> [a]
incList [] = []
incList (x:xs) = (x+1:incList xs)

lengthList:: [[a]] -> [Int]
lengthList [] = []
lengthList (x:xs) = (length x : lengthList xs)

zip'::[a] -> [b] -> [(a,b)]
zip' = zipWith (,)