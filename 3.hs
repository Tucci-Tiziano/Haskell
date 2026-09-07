import Distribution.Simple.Utils (xargs)

esParMenor :: (Integer, Integer) -> (Integer, Integer) -> Bool
esParMenor (a,b) (c,d) | a < c && b < d = True
| otherwise = False



esPar :: Integer -> Bool
esPar a | a == 0 = True
| a == 1 = False
| a > 1 = esPar(a-2)

posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (a, b, c) | esPar(a) = 1
| esPar(b) = 2
| esPar(c) = 3
| otherwise = 4

bisiesto :: Integer -> Bool
bisiesto a | mod a 100 == 0 = mod a 400 /= 0
| otherwise = mod a 4 == 0


distancia :: Float -> Float
distancia a | a < 0 = a*(-1)--a - (2*a)
| otherwise = a

distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (a, b, c) (x, y, z) = distancia(a - x) + distancia(b - y) + distancia(c - z)


ultimosDosDigitos :: Integer -> Integer
ultimosDosDigitos n = (mod n 100)

sumaDigitos :: Integer -> Integer
sumaDigitos a = (mod a 10) + ( div a 10 )

suma2Digitos :: Integer -> Integer
suma2Digitos a = sumaDigitos(ultimosDosDigitos(a))

comparar :: Integer -> Integer -> Integer
comparar a b| suma2Digitos(a) < suma2Digitos(b) = 1
| suma2Digitos(a) > suma2Digitos(b)= -1
| suma2Digitos(a) == suma2Digitos(b) = 0