npar :: Integer -> Integer
npar n = 2*n

numeros :: Integer -> [Integer]
numeros n   | n < 10 = [1,2..n]
            | otherwise = [(div n 10), 2*(div n 10)..n]


repetirBuscar :: Integer -> Integer -> Integer
repetirBuscar n veces   | veces == 0 = 2 
                        | otherwise = buscarDivisor n ((repetirBuscar n (veces - 1))+1)


buscarNDivisores :: Integer -> Integer -> [Integer]
buscarNDivisores n num  | num == 0 = [repetirBuscar n 0]
                            | (repetirBuscar n num) == 1 = []
                            | otherwise = [repetirBuscar n num] ++ buscarNDivisores n (num-1)




buscarPrimo :: Integer -> Integer
buscarPrimo a
    | primalidad a 2 == True = a
    | otherwise = buscarPrimo (a + 1)

repetirBuscarP :: Integer -> Integer
repetirBuscarP n    
    | n == 0 = 2
    | otherwise = buscarPrimo ((repetirBuscarP (n - 1))+1)

buscarNPrimos :: Integer -> [Integer]
buscarNPrimos n     
    | n == 0 = [repetirBuscarP 0]
    | otherwise = [repetirBuscarP n] ++ buscarNPrimos (n-1)



-----------------------------------------
-----------------------------------------
-----------------------------------------
-----------------------------------------
-----------------------------------------



sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m
    | n > 1 && m > 1 = q^(n+m) + sumaPotencias q (n-1) (m-1)
    | n > 1 && m == 1 = q^(n+m) + sumaPotencias q (n-1) m
    | n == 1 && m > 1 = q^(n+m) + sumaPotencias q n (m-1)
    | otherwise = q^2


buscarDivisor :: Integer -> Integer -> Integer
buscarDivisor n a
   -- | a > floor (sqrt (fromIntegral n)) = 1
    | a > ((div n 2) + 1) = 1
   -- | a == n = 1
    | mod n a == 0 = a
    | otherwise = buscarDivisor n (a + 1)

menorDivisor :: Integer -> Integer
menorDivisor n = buscarDivisor n 2

divisor :: Integer -> Integer -> Bool
divisor a b = mod a b == 0

primalidad :: Integer -> Integer -> Bool
primalidad n div    | div*div > n = True
                    | divisor n div == False = primalidad n (div+1)
                    | otherwise = False

esPrimo :: Integer -> Bool
esPrimo n = primalidad n 2



esidp :: Integer -> Integer -> Integer
esidp n a   | (sum (buscarNPrimos a)) == n = a+1
            | (sum (buscarNPrimos a)) < n = esidp n (a+1)
            | (sum (buscarNPrimos a)) > n = 0

esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = (esidp n 0 /= 0)