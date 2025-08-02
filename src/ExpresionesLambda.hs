module ExpresionesLambda where

    --Las funciones anonimas se expresan mediante lambda (barra invertida)

    --invertirLista :: [Int]->[Int]
   -- invertirLista = foldl (\x lista -> lista ++ [x]) []

    --Paso a paso:
        --  foldr (\x lista -> lista ++[x]) [] [1,2,3]
        --1º. 3       []        []  ++ [3] = [3]
        --2º  2       [3]       [3] ++ [2] = [3,2]
        --3º  1       [3,2]    [3,2]++ [1] = [3,2,1]

    --Para la funcion anterior y pasando ...> invertirLista [1,2,3]
    --x representa un elemto de la lista y lista representa el resultado


    --Ejemplos

    impares :: Int->[Int]
    impares n = map f [0..n-1]
                    where f x = x * 2 + 1

    imparesAnonima :: Int->[Int]
    imparesAnonima n = map (\x->x * 2 + 1) [0..n-1]

    --Ejercicio longitud de lista

    longitud :: [Int]-> Int
    longitud = foldr (\_ n-> n + 1) 0

    --Ejercicio funcion que recibe una lsita de funciones y un entero y devuelve ese entero con las funciones aplicadas
    
    listaFunciones :: [(Int->Int)]->Int->[Int]
    listaFunciones [] _ = []
    listaFunciones (x:xs) entero = x entero :listaFunciones xs entero

    listaFunciones' :: [Int->Int] ->Int->[Int]
    listaFunciones' fs x = foldr ( \f lista -> (f x):lista) [] fs

    --Tambien existen foldl1 y foldr1, el cambio es que estas 2 ya no reciben el valor base, solo reciben el ultimo elemnto de la lista, no trabaja con listas vacias, da error.

    --Mayor valor de una lista de enteros con plegado

    mayorLista :: [Int]->Int
    mayorLista = foldr (\x aux -> if x>aux then x else aux) 0
