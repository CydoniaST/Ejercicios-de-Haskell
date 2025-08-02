module Hoja2 where
--HOJA 2

--Ej1 f

f1a :: Int -> [Int]
f1a n = reverse (f1a' n)

f1a' :: Int -> [Int]
f1a' n
    |n < 10 = [n]
    |otherwise = (n `mod`10) : f1a' (n `div`10)

f1b :: Int -> [Int]
f1b n = reverse(map (`mod` 10)(takeWhile (/=0) (iterate (`div`10) n)))
--Para ahorrar tantos paréntesis también podría escribirse como:
--reverse $ map (`mod`10) $takeWhile (/= 0) $iterate (`div`10) n

f1c :: Int -> Int
f1c n = mifoldl f 0 (f1a' n)
    where f x y = 10*x + y
    
--Podemos usar foldl predefinida
mifoldl :: (a -> b -> a) -> a -> [b] -> a
mifoldl  f x ys  = aux x ys
    where aux x [] = x
          aux x (y:ys) = aux (f x y) ys

--la version sin usar foldl no me sale
f1d :: [Int] -> Int
f1d xs = aEntero 0 xs

aEntero :: Int -> [Int] -> Int
aEntero acum [] = acum
aEntero acum (x:xs) = aEntero (acum*10 + x) xs

--Ej2

data Natural = Cero | SUC Natural
    deriving (Eq,Ord,Show,Read)
    
--Para incluir Natural en la clase Num:

instance Num Natural where
    
    Cero + y = y
    (SUC x) + y = SUC(x+y)
    
    Cero - y = Cero
    (SUC x) - (SUC y) = x - y
    
    Cero * y = Cero
    (SUC x) * y = x*y + y
    
    abs x = x
    
    fromInteger x
        | x > 0 = SUC (fromInteger (x-1))
        | otherwise = Cero
--Factorial no está en la clase Num por lo que hay que escribirlo fuera como función
-- Al incluirlo en la clase Num se obtiene automáticamente la operación ^   
factorial  x
    |x > 0 = x*factorial(x-1)
    |otherwise = 1


--Ej3

data Temp = Kelvin Float | Celsius Float | Fahrenheit Float
    deriving (Show,Read)
    
ceroAbs :: Float
ceroAbs = -273.15

toKelvin :: Temp -> Temp
toKelvin (Celsius x) = Kelvin(x - ceroAbs)
toKelvin (Fahrenheit x) = Kelvin(((x-32)/1.8)-ceroAbs)
toKelvin t = t --Cuando no es ni Celsius ni Fahrenheit se deja igual


toCelsius :: Temp -> Temp
toCelsius (Kelvin x) = Celsius(x + ceroAbs)
toCelsius (Fahrenheit x) = Celsius((x-32)/1.8)
toCelsius t = t

toFahrenheit :: Temp -> Temp
toFahrenheit (Celsius x) = Fahrenheit(x*1.8 + 32)
toFahrenheit (Kelvin x) = Fahrenheit(32+ (x+ceroAbs)*1.8)
toFahrenheit t = t

escala :: Temp -> String
escala (Kelvin x) = "Kelvin"
escala (Celsius x) = "Celsius"
escala (Fahrenheit x) = "Fahrenheit"

instance Ord Temp where --Para incluir este nuevo tipo dentro de la clase Ord
    compare x y = compare x' y'
        where Kelvin x' = toKelvin x
              Kelvin y' = toKelvin y

instance Eq Temp where
    x == y = x' == y' -- para incluirlo en la clase Eq
        where Kelvin x' = toKelvin x
              Kelvin y' = toKelvin y

--Ej4

data Arbol a = AVacio | Nodo a (Arbol a) (Arbol a) --Por que hay que poner la "a" en Arbol a
--Por que no ponemos deriving(Show, Read)

crearVacio :: Ord a => Arbol a
crearVacio = AVacio

esVacio :: Ord a => Arbol a -> Bool
esVacio AVacio = True
esVacio _ = False

anadir:: Ord a => a -> Arbol a -> Arbol a
anadir x AVacio = Nodo x AVacio AVacio
anadir x (Nodo y iz dr)
    | x >= y = Nodo y iz (anadir x dr)
    | otherwise = Nodo y (anadir x iz) dr
    
--Que pasa si no esta x en el arbol
eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x AVacio = AVacio
eliminar x (Nodo y iz dr)
    | x < y = Nodo y (eliminar x iz) dr
    | x > y =  Nodo y iz (eliminar x dr)
    | esVacio iz = dr --Hacemos esto para que podamos aplicar la funcion maxima de un arbol no vacio, pero para lo demas no hace falta
    | otherwise = Nodo maxIz (eliminar maxIz iz) dr
    where maxIz = maximoA iz

--Suponemos que hacemos maximo de un arbol no vacio
--Tambien se puede hacer con el menor del dr
maximoA :: Ord a => Arbol a -> a --por que no funcion si no pongo la clase ord
maximoA (Nodo y iz dr)
    |esVacio dr = y
    |otherwise = maximoA dr
    

preOrden :: Arbol a -> [a]
preOrden AVacio = []
preOrden (Nodo x iz dr) = [x] ++ (preOrden iz) ++ (preOrden dr)

inOrden :: Arbol a -> [a]
inOrden AVacio = []
inOrden (Nodo x iz dr) = (inOrden iz) ++ [x] ++ (inOrden dr)

postOrden  :: Arbol a -> [a]
postOrden AVacio = []
postOrden (Nodo x iz dr) = (postOrden iz) ++ (postOrden dr) ++ [x]

busca :: Ord a => a -> Arbol a -> Bool
busca x AVacio = False
busca x (Nodo y iz dr)
    |x == y    = True
    |x > y     = busca x dr
    |otherwise = busca y iz

--Ej5

treeSort :: Ord a => [a] -> [a]
treeSort [] = []
treeSort xs = inOrden (foldr (anadir) AVacio xs)
--treeSort xs = inOrden (listaAarbol xs)

--Esta funcion tiene la estructura del foldr
--foldr (anadir x) AVacio xs = listaAarbol xs
listaAarbol :: Ord(a) => [a] -> Arbol a
listaAarbol [] = AVacio
listaAarbol (x:xs) = anadir x (listaAarbol xs)

--Ej6

data Pila a = P [a]
 deriving (Show)--Para probar si me las funciones estan bien construidas
                --Usar apilar 4 (crearPilaVacia)
crearPilaVacia :: Pila a
crearPilaVacia = P []

es_pilaVacia :: Pila a -> Bool
es_pilaVacia (P []) = True
es_pilaVacia p = False

apilar :: Pila a -> a -> Pila a
apilar (P xs) y = P (y:xs)

--No se puede desapilar la pila vacia
desapilar :: Pila a -> Pila a
desapilar (P (x:xs)) = P xs

cima :: Pila a -> a
cima (P (x:xs)) = x


--Ej7

data Cjto a = Cj [a]
    deriving (Show)

--Hay que ponerlos en la clase Eq por que para la funcion elimina hay que comparar
crear_CjtoVacio :: Eq a => Cjto a
crear_CjtoVacio = Cj []

--Los añade al principio
--elem x xs te devuelve false si x no esta en xs, true si si está
anadirACjto :: Eq a => Cjto a -> a -> Cjto a
anadirACjto (Cj xs) y
    |elem y xs = Cj xs
    |otherwise = Cj (y:xs)

es_CjtoVacio ::Eq a => Cjto a -> Bool
es_CjtoVacio (Cj [])   = True
es_CjtoVacio conjunto  = False

eliminarDeCjto :: Eq a => Cjto a -> a -> Cjto a
eliminarDeCjto (Cj xs) y = Cj (elimina xs y)

elimina :: Eq a => [a] -> a -> [a]
elimina [] y = []
elimina (x:xs) y
    |x == y = xs
    |otherwise  = x: (elimina xs y)

pertenceACjto :: Eq a => Cjto a -> a ->Bool
pertenceACjto (Cj xs) y = elem y xs

elementosCjto :: Eq a => Cjto a -> [a]
elementosCjto (Cj xs) = xs


--Ej8

data Cjto' a = Cj' (Arbol a)

crear_CjtoVacio' :: Ord a => Cjto' a
crear_CjtoVacio' = Cj' AVacio

es_CjtoVacio' ::Ord a => Cjto' a -> Bool
es_CjtoVacio' (Cj' arb)  = esVacio arb

anadirACjto' :: Ord a => Cjto' a -> a -> Cjto' a
anadirACjto' (Cj' arb) y
    |busca y arb = Cj' arb
    |otherwise = Cj' (anadir y arb)

eliminarDeCjto' :: Ord a => Cjto' a -> a -> Cjto' a
eliminarDeCjto' (Cj' arb) y = Cj' (eliminar y arb)

pertenceACjto' :: Ord a => Cjto' a -> a -> Bool
pertenceACjto' (Cj' arb) y = busca y arb

elementosCjto' :: Ord a => Cjto' a -> [a]
elementosCjto' (Cj' arb) = inOrden arb


--Ej9

primeroQueCumple :: (a -> Bool) -> [a] -> Maybe a
primeroQueCumple p xs
    | null siCumplen  = Nothing
    | otherwise        = Just (head siCumplen)
    where siCumplen = filter p xs
    
--Ej10

inits :: [a] -> [[a]]
inits [] =[[]]
inits (x:xs) = [] : map (x:) (inits xs)

--Utilizando la familia foldl

inits2 ::[a] -> [[a]]
inits2 xs = foldr f [[]] xs
  where f x xss = [] : map (x:) xss

--init xs devuelve la lisa sin su ultimo elemento
inits3 ::[a] -> [[a]]
inits3 xs = reverse( take (n+1) (iterate init xs) )
    where n = length xs

inits4 xs = scanl (\xs x -> xs++[x]) [] xs

--Ej11

quitaDups :: Eq a => [a] -> [a]
quitaDups [] = []
quitaDups [x] = [x]
quitaDups (x:y:xs)
    | x == y   = quitaDups (x:xs)
    |otherwise = x : quitaDups (y:xs)

--usando la familia fold

quitaDups' :: Eq a => [a] -> [a]
quitaDups' xs = foldr f [] xs
    where f x [] = [x]
          f x (y:ys)
            | x == y = (y:ys)
            |otherwise = (x:y:ys)

--Ej12

lineas :: String -> [String]
lineas [] = []
lineas xs
    | null dr = [iz]
    | otherwise = iz : lineas(tail dr)
    where (iz,dr) = span (/= '\n') xs

--tail devuelve la lista sin el primer elementos

antiLineas :: [String] -> String
antiLineas xs = foldr f [] xs
  where f linea texto = linea++"\n"++texto

--Ej13

justifDr :: Int -> String -> String
justifDr n texto = unlines (map (justifDr1 n) (lines texto))

justifDr1 :: Int -> String -> String
justifDr1 n xs = blancos ++ xs
  where blancos = replicate (n-length xs) ' '
  --aquí añadimos espacios en blanco hasta llegar al máximo de caracteres n
  
--n es el ancho máximo que quieres que tenga la fila
--ancho debe ser un número mayor que el número de caracteres de cada fila
procesa ancho = do texto <- readFile "p1.txt"
                   writeFile "p2.txt" (justifDr ancho texto)


--Ej14

justifCen :: Int -> String -> String
justifCen n texto = unlines (map (justifCen1 n) (lines texto))

justifCen1 :: Int -> String -> String
justifCen1 n xs = blancos1 ++ xs ++ blancos2
  where b1 = n - (length xs)
        b2 = b1 `div` 2 
        blancos1 = replicate b2 ' '
        blancos2 = replicate (b1 - b2) ' '

procesa2 ancho = do texto <- readFile "p1.txt"
                    writeFile "p2.txt" (justifCen ancho texto)


--Ej15

{-
justifAmbos :: Int -> String -> String
justifAmbos n texto = unlines (map (justifAmbos1 n) (lines texto))

justifAmbos1 :: Int -> String -> String
justifAmbos1 n xs 
  | numHuecos < 1 = unwords pals
  | otherwise     = unwords (primera ++ pals1' ++ pals2')
  where pals           = words xs
        numHuecos      = length pals - 1
        nblancos       = n - (length (concat pals)) - numHuecos
        extraNormal    = nblancos `div` numHuecos
        numExtras      = nblancos `mod` numHuecos
        blancosNormal2 = replicate extraNormal ' '
        blancosNormal1 = ' ':blancosNormal2

        (primera,resto) = splitAt 1 pals
        (pals1,pals2)   = splitAt numExtras resto
        pals1'          = map (blancosNormal1++) pals1
        pals2'          = map (blancosNormal2++) pals2

procesaA ancho = do texto <- readFile "patata.txt"
                    writeFile "patata2.txt" (justifAmbos ancho texto)
-}

--Ej16
--Suma, resta y producto escalar de vectores, siendo los vectores [Float]
--Como precondicion los vectores de entrada deben tener la misma longitud para operarlos

type Vector = [Float]
sumaV :: Vector -> Vector -> Vector
sumaV v1 v2 = zipWith (+) v1 v2

restaV :: Vector -> Vector -> Vector
restaV v1 v2 = zipWith (-) v1 v2

prodV :: Vector -> Vector -> Float
prodV v1 v2 = sum (zipWith (*) v1 v2)

--Ej17
--suma, resta, transpuesta y producto

type Matriz = [[Float]]

sumaM :: Matriz -> Matriz -> Matriz
sumaM m1 m2 = zipWith sumaV m1 m2

restaM :: Matriz -> Matriz -> Matriz
restaM m1 m2 = zipWith restaV m1 m2

traspuesta :: Matriz -> Matriz
traspuesta [] = []
traspuesta [vector] = map (:[]) vector
traspuesta (v1:vs) = zipWith (:) v1 (traspuesta vs)


productoM :: Matriz -> Matriz -> Matriz
productoM [] _ = []
productoM (v:vs)  m2 = (map (prodV v) (traspuesta m2) ): (productoM vs m2)

--Ej18

type Polinomio = [Float]

evaluarP :: Polinomio -> Float -> Float
evaluarP [c] _ = c
evaluarP (c:cs) x = (c* evaluacion) + (evaluarP cs x)
    where evaluacion = x ** fromIntegral(length cs)

--usando la familia foldl (no lo entiendo bien)

evalua :: Polinomio -> Float -> Float
evalua pol x = foldl (f x) 0 pol 
  where f x acum coef = acum*x + coef


--Ej20

productorio xs = foldr (*) 1 xs
concat xss = foldr (++) [] xss
pares xs = filter even xs
impares xs = filter odd xs
maximo xs = foldr1 max xs
minimo xs = foldr1 min xs

mcdgen xs      = foldr1 gcd xs
mcmgen xs      = foldr1 mcm xs


mcm x y = (x * y) `div` (gcd x y)
