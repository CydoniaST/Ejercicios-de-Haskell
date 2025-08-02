module FuncionesOrdenSuperior where

    --DOS VECES
    incremento :: Int->Int
    incremento x = x+1

    dosVeces :: (Int->Int)->Int->Int
    dosVeces f x = f (f x)


    --Funcion Map que recibe una funcion y una lista y le aplica la funcion a cada uno de los elementos de la lista y devuelve la lista de elementos con esa funcion aplicada
    --Funcion Filter recibe una funcion que filtra los elementos de una lista que se apsa coomo segundo argumento 
    --Funcion all comprueba si todos lo elementos de una lista cumplen una condicion, por ejemplo - all even [1,2,4] devuelve false
    --Funcion any 

    --La funcion de orden superior foldr entra 100% en el examen (funcion de plegado de listas)
    --funciona mediante un patron recursivo aplicanco una funcion de "plegado" sobre una listas

    --sumaListaE :: [Int]->Int
    --sumaListaE [] = 0
    --sumaListaE (x:xs) = x + sumaLista xs

    --En este ejemplo de suma el valor base es 0 si la lista esta vacia y el plegador es el "+"


    --Ejemplo 2:
    --concatenar :: [Int]->[Int]
    --concatenar [] = []
    --concatenar (sublista:resto) = sublista ++ concatenar resto

    --El esquema basico de una funcion de plegado es:
    --f [] = valor
    --f (x:xs) = x ’operación’ (f xs)

    --ESQUEMA PLEGADO POR LA DERECHA = foldr (+) e [w,x,y,z] -> (w + (x + (y + (z +e)))) siendo (+) un ejemplo, pudiendo poner cualquier funcion

    --productoLista :: [[Int]]->Int
    --productoLista = foldr (*) []

    concatenar' :: [[Int]]->[Int]
    concatenar'  = foldr (++) []

    --La funcion de plegado peude ir de izq a derecha o de dch a izq
    --ESQUEMA PLEGADO POR LA IZQUIERDA = foldl (+) e [w,x,y] -> (((e + y)+ x)+ w) siendo (+) un ejemplo, pudiendo poner cualquier funcion