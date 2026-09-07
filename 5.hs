quitarPrimer :: Integer -> [Integer] -> [Integer]
quitarPrimer a [] = []
quitarPrimer a (x:xs)
    | x == a    = xs
    | otherwise = x : quitarPrimer a xs


calcMax :: Integer ->  [Integer] -> Integer
calcMax mayor [] = mayor
calcMax 0 (x:xs) = calcMax x xs
calcMax mayor (x:xs) 
    | mayor < x = calcMax x xs
    | otherwise = calcMax mayor xs 

maximo :: [Integer] -> Integer
maximo lista = calcMax 0 lista 


ord :: [Integer] -> [Integer] -> [Integer]
ord [] nueva = nueva
ord vieja nueva =   ord (quitarPrimer (maximo vieja) vieja) ( (maximo vieja) : nueva) 
                
ordenar :: [Integer] -> [Integer]
ordenar a  = ord a []  


type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

juan :: Contacto
juan = ("Juan", "1123456789")

maria :: Contacto
maria = ("Maria", "1198765432")

contactosEjem :: ContactosTel
contactosEjem = [juan, maria]

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos n [] = False
enLosContactos n ((a,b):xs) 
    | a == n = True
    | otherwise = enLosContactos n xs


multiPlicarFila :: [Integer] -> Integer
multiPlicarFila [] = 1
multiPlicarFila (x:xs) = x * (multiPlicarFila xs)


mF :: [[Integer]] -> [Integer] -> [Integer]
mF [] lista = lista
mF (x:xs) lista = (multiPlicarFila x) : (mF xs lista) 

multiplicarFilas :: [[Integer]] -> [Integer] 
multiplicarFilas a = mF a ([]) 