module Hoja1 where
--HOJA 1

--Ej1
f1 :: Floating a => a -> a--Esto significa que a está en la clase Floating
f1 r = pi*(r)**2

--Ej2
f2 :: Integer -> Bool
f2 n = x == fromInteger (floor x)
  where x = sqrt (fromInteger n)
--floor convierte al entero más bajo;
--sqrt vale para reales habria que pasar de entero a real
--fromInteger pasa de entero a real?

--Ej3
f3 ::Real a => a -> a -> a -> Int
f3 a b c
 |d < 0 = 0
 |d == 0 = 1
 |otherwise = 2
 where d = b*b -4*a*c
 

--Ej7
{-La función cremallera (zip3) de los ejemplos pero con 3 listas.
f7 [1,2,3] ['a','b','c'] ['x','y','z'] -}

f7 :: [a] -> [b] -> [c] -> [(a,b,c)]
f7 [] _ _ = []
f7 _ _ [] = []
f7 _ _ [] = []
f7 (x:xs) (y:ys) (z:zs) = (x,y,z) : f7 xs ys zs

f7mejor :: [a] -> [b] -> [c]-> [(a,b,c)]
f7mejor (x:xs) (y:ys) (z:zs) = (x,y,z) : f7mejor xs ys zs
f7mejor _ _ _ = []

--Ej8
--El productorio de los elementos de una lista f8 [3,5,1,5,2]

f8 :: Integral a => [a] -> a
f8 [] = 1
f8 (x:xs) = x*f8(xs)

--Ej9
--Producto escalar de 2 listas. f9 [3,5,2] [1,5,8]

f9 :: Num a => [a] -> [a] -> a
f9 (x:xs) (y:ys) = x*y + f9 xs ys
f9 _ _ = 0

f91 :: Num a => [a] -> [a] -> a
f91 [] ys = 0
f91 xs [] = 0
f91 (x:xs) (y:ys) = x*y + f91 xs ys


--Ej10
-- Función recursiva que hace lo que ++
f10 :: [a] -> [a] -> [a]
f10 [] ys = ys 
f10 (x:xs) ys = x : f10 xs ys

--Ej11
--concantena [[1,2,3],[2,8],[],[3,6]] = [1,2,3,2,8,3,6]
f11 :: [[a]] -> [a]
f11 [] = []
f11 (xs:xss) = xs ++ f11 xss

--Ej12
--Devuelve True si la lista está ordenada, y False si no.
f12 :: Ord a => [a] -> Bool --a está en la clase Ord para incluir todos los objetos que sean ordenables
f12 [] = True
f12 [x] = True
f12 (x:y:zs) = x <= y && f12 (y:zs)

--Ej13
--[1,2,3,4,5,6,7,8]

f13a :: Integral a => [a] -> [a] --even hace un modulo por lo que necesitamos integral para que la operacion este en la clase
f13a [] = []
f13a (x:xs)
  | even x = f13a xs
  | otherwise = x : f13a xs

f13b :: Integral a => [a] -> [a]
f13b [] = []
f13b (x:xs)
  | even x = x : f13b xs
  | otherwise = f13b xs

--Ej14
coloca :: Integral a => [a] -> [a]
coloca xs = alterna pares impares
    where pares = f13b xs
          impares = f13a xs
          
--La lista de entrada debe tener el mismo número de pares e impares
alterna :: Integral a => [a] -> [a]-> [a]
alterna (x:xs) (y:ys) = x : y : alterna xs ys
alterna _ _ = []


--Ej15
f15 :: Integral a => a -> a
f15 x = 1 + f15' x

f15' :: Integral a => a -> a
f15' x
    | x<2 = 1
    |otherwise = 1 + f15'(x `div` 2)
    
--Ej16
f16 :: Integral a => a -> [a]
f16 x = reverse (f16' x)

f16' :: Integral a => a -> [a]
f16' x
    | x < 2 = [x]
    | otherwise = m : (f16' d)
    where d = x `div` 2
          m = x `mod` 2

--una forma de hacerlo sin recursividad
aBinario :: Integral a => a -> [a]
aBinario x = reverse (modulos ++ [1])
    where divs = takeWhile (/= 1) (iterate (`div`2) x)
          modulos = map (`mod`2) divs

--Ej 17
--Ordenacion de listas por el metodo de ordenacion por seleccion
--Consiste en buscar el menor elemento de la lista y cambiarlo por el primero

f17a :: Ord a => [a] -> [a]
f17a [] = []
f17a xs = primero : f17a (quita primero xs)
    where primero = menor xs

--se asume que la lista no es vacia ya que si no, no podriamos devolver su menor

menor :: Ord a => [a] -> a
menor [x] = x
menor (x:y:ys) = min  x (menor (y:ys))

--usando orden superior
menor2 :: Ord a => [a] -> a
menor2 xs = foldr1 min xs
--quita(x lista) quita el elemento x de la lista (si tiene varias apariciones quita la primera)

quita :: Ord a => a -> [a] -> [a]
quita x [] = []
quita x (y:ys)
    | x == y = ys
    | otherwise = y: quita x ys

--Metodo de ordenacion rapida
--Toma un pivote y coloca todos los menores a un lado y los mayores a otro-f17b

f17b :: Ord a => [a] -> [a]
f17b [] = []
f17b (x:xs) = f17b menores ++ [x] ++ f17b mayores
    where (menores,mayores) = separa x xs

separa :: Ord a => a -> [a] ->([a],[a])
separa x [] = ([],[])
separa x (y:ys)
    | x < y = (men, y:mas)
    |otherwise = (y:men,mas)
    where (men,mas) = separa x ys

--Ej18

takeLast :: Int -> [a] -> [a]
takeLast x xs = take x (reverse xs)

--Ej19

dropLast :: Int -> [a] -> [a]
dropLast x xs = drop x (reverse xs)

--Ej20
--Funcion Fibonacci no recursiva

fib1 :: Int -> Int
fib1 0 = 1
fib1 1 = 1
fib1 n = fib1(n-1) + fib1(n-2)

genFib :: Int -> Int -> [Int]
genFib m n = m : genFib n (n+m)

fib2 :: Int -> Int
fib2 n = (genFib 1 1) !! n
--Devuelve el valor de la posicion n de la lisa, donde la primera posicion es la 0

--Ej21

mcd :: Integral a => a -> a -> a
mcd n 0 = n
mcd 0 m = m
mcd n m
    |n > m = mcd m (n `mod` m)
    |otherwise = mcd n (m `mod` n)

mcm :: Integral a => a -> a -> a
mcm n m = (n*m) `div` (mcd n m)

--Ej22
-- Un numero racional a/b se representa como (a,b)

simp :: (Int, Int) -> (Int, Int)
simp (n,m)
    |b < 0 = (-a, -b)
    |otherwise = (a,b)
    where a = n `div` d
          b = m `div` d
          d = mcd (abs n) (abs m) --hay que añadir el valor absoluto por que el mcd no lo hace de números negativos

sumaR :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaR (a,b) (c,d) = simp(a*d + b*c, b*d)

restaR :: (Int, Int) -> (Int, Int) -> (Int, Int)
restaR (a,b) (c,d) = simp(a*d - b*c, b*d)

prodR :: (Int, Int) -> (Int, Int) -> (Int, Int)
prodR (a,b) (c,d) = simp(a*c, b*d)

divR :: (Int, Int) -> (Int, Int) -> (Int, Int)
divR (a,b) (c,d) = simp(a*d, b*c)


--Ej23

potenciaR ::(Int, Int) -> Int -> (Int, Int)
potenciaR (a,b) 0 = (1,1)
potenciaR (a,b) n
    |n<0 = simp (b^n,a^n)
    |otherwise = simp (a^n,b^n)

--Otra forma de hacerlo:
pot :: (Int,Int) -> Int -> (Int,Int)
pot r 0 = (1,1)
pot r n = prodR r (pot r (n-1))

--Ej24
maxL :: Ord a => [a] -> a
maxL [x] = x
maxL (x:y:xs) = max x (maxL (y:xs))

minL :: Ord a => [a] -> a
minL [x] = x
minL (x:y:xs) = min x (minL (y:xs))

--Ej25
--No puede ser vacia la lista
f25a :: Integral a => [a] -> a
f25a [x] = x
f25a (x:y:xs) = mcd x (f25a (y:xs))

f25b :: Integral a => [a] -> a
f25b [x] = x
f25b (x:y:xs) = mcm x (f25b (y:xs))

mcd2 :: Integral a => [a] -> a
mcd2 xs = foldr1 gcd xs

mcm2 :: Integral a => [a] -> a
mcm2 xs = foldr1 mcm xs