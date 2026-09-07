f :: Integer -> Integer
f num | num == 1 = 8
| num == 4 = 131
| num == 16 = 16

g :: Integer -> Integer
g num | num == 16 = 4
| num == 131 = 1
| num == 8 = 16

h :: Integer -> Integer
h n = f ( g n )

k :: Integer -> Integer
k n = g ( f n )



w :: Integer -> Integer
w n = g ( f ( g ( f ( g ( f ( n ) ) ) ) ) )


maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | x >= y && x >= z = x
| y >= x && y >= z = y
| z >= y && z >= x = z

recu :: Integer -> Integer -> Integer -> Integer
recu num mult num2 | num <= num2 = recu (num * mult) mult num2
| otherwise = num
--sin contar repetidos

sumaDist :: Integer -> Integer -> Integer -> Integer
sumaDist x y z | x /= y && x /= z && y == z = x
| y /= x && y /= z && x == z = y
| z /= y && z /= x && x == y = z
| x /= y && x /= z && z /= y = x+y+z

--sumar repetidos una vez
sumaDist2 :: Integer -> Integer -> Integer -> Integer
sumaDist2 x y z | x /= y && x /= z && y == z = x + z
| y /= x && y /= z && x == z = y + z
| z /= y && z /= x && x == y = z + x
| x /= y && x /= z && z /= y = x+y+z

digitoUnidades :: Integer -> Integer
digitoUnidades n = n - (div n 10) * 10

digitoUnidades2 :: Integer -> Integer
digitoUnidades2 n = mod (div n 1) 10

digitoDecenas :: Integer -> Integer
digitoDecenas n = n - (div n 100) * 100

digitoN :: Integer -> Integer -> Integer
digitoN n num = n - (div n (10^num)) * (10^num)

digitoN2 :: Integer -> Integer -> Integer
digitoN2 n num = mod (div n (10^num)) 10