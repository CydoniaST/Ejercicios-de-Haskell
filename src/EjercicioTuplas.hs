module EjercicioTuplas where
    interseccion :: ((Int, Int), (Int, Int)) -> Bool
    interseccion ((x,y),(j,k)) = if x >= k then False else not (y <= j)

    doble :: Int -> Int
    doble x = 2 * x 

    negado :: Int -> Int
    negado x = negate x  

    dobleNegado :: Int -> Int
    dobleNegado x = negado (doble x)

    maximo :: (Int, Int) -> Int
    maximo (x,y) = if x > y then x else y

    maximo' :: Int -> Int -> Int
    maximo' x y = if x > y then x else y

    exp :: Int -> Int -> Int -> Int
    exp  x y = maximo' x.maximo' y

    exp' :: Int -> Int -> Int -> Int 
    exp' x = maximo'.maximo' x

    exp'' :: Int -> Int -> Int -> Int --MAXIMO CON TUPLAS UTILIZANDO LA FUNCION MAXIMO
    exp'' x y z = maximo(maximo (x,y),z)

--EJERCICIOS

    divEntera :: (Int,Int) -> (Int, Int)
    divEntera (x,y) = (x `div` y, x `rem` y) 

    --La aridad de una tupla es el num de elementos que tiene esa tupla

    ordenados :: Int->Int->Int->Bool
    ordenados x y z = if ((x < y) && (y < z)) then True else False

--Guardas
    tuplaOrdenada :: (Int, Int, Int)->(Int, Int, Int)
    tuplaOrdenada (x,y,z) 
                        | (x > y) && (y > z) = (z,y,x)
                        | (y > x) && (x > z) = (z,x,y)
                        | (y > z) && (z > x) = (x,z,y)
                        | (z > y) && (y > x) = (x,y,z)
                        |otherwise = (y,x,z)

    descomponerReal :: Float -> (Int,Int)
    descomponerReal x = (floor x, truncate(x *100) - (truncate (x))*100)

   