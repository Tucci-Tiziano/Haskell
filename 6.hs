
sacarApariciones :: String -> [String] -> [String]
sacarApariciones _ [] = []
sacarApariciones p (x:xs)   | p == x = sacarApariciones p xs
                            | otherwise = x : sacarApariciones p xs

contarProducto :: String -> [String] -> Integer
contarProducto _ [] = 0
contarProducto p (x:xs) | x == p = 1 + contarProducto p xs
                        | otherwise = contarProducto p xs

sAux :: [String]
sAux = ["a","a","a","a","a","a","a","a","b","c","a","w","w","w","a"]

generarStock :: [String] -> [(String, Integer)]
generarStock [] = []
generarStock (x:xs) = (x, contarProducto x (x:xs)) : generarStock (sacarApariciones x xs)


stockDeProducto :: [(String, Integer)] -> String-> Integer
stockDeProducto [] _ = 0
stockDeProducto ((a,b):xs) p    | a == p = b
                                | otherwise = stockDeProducto xs p 


getPrecio :: String -> [(String, Float)] -> Float
getPrecio _ [] = 0
getPrecio p ((a,b):xs)  | a == p = b
                        | otherwise = getPrecio p xs

pAux ::  [(String, Float)]
pAux = [("a",11),("b",2),("c",6),("w",1)]

dineroEnStock :: [(String, Integer)] -> [(String, Float)] -> Float
dineroEnStock [] _ = 0
dineroEnStock ((a,b):xs) precios = ( (fromIntegral b)*(getPrecio a precios)) + dineroEnStock xs precios 

aplicarOferta :: [(String, Integer)] -> [(String, Float)] -> [(String,Float)]
aplicarOferta [] _ = []
aplicarOferta ((a,b):xs) precios    | b > 10 = (a, (getPrecio a precios) * 0.8) : aplicarOferta xs precios
                                    | otherwise = (a, (getPrecio a precios)) : aplicarOferta xs precios