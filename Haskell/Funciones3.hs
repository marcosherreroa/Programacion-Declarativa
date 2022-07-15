soluciones':: (Double,Double,Double) -> [Double]
soluciones' (a,b,c) =
  let d = b^2-4*a*c
      e = -b/(2*a)
      r = sqrt d/(2*a)
  in if d > 0 then [e+r,e-r] else
     if d == 0 then [e]
               else []

soluciones:: (Double,Double,Double) -> [Double]
soluciones (a,b,c)
  | d > 0 = [e+r,e-r]
  | d == 0 = [e]
  | d < 0 = []
  where d = b^2-4*a*c
        e = -b/(2*a)
        r = sqrt d/(2*a)

soluciones'':: (Double,Double,Double) -> [Double]
soluciones'' (a,b,c) =
  if d > 0 then [e+r,e-r] else
  if d == 0 then [e]
            else []
  where {d = b^2-4*a*c; e = -b/(2*a); r = sqrt d/(2*a)}


reverse1:: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse xs ++ [x]

reverse2:: [a] -> [a]
reverse2 xs = revAux xs []
  where 
  revAux:: [a] -> [a] -> [a]
  revAux [] aux = aux
  revAux (x:xs) aux = revAux xs (x:aux)