module Recursividad where

    --Factorial Recursividad no final - 
    factorial :: Int -> Int
    factorial 0 = 1
    factorial n = n * factorial (n-1)

    --Longitud de lista -Esto es recursividad final
    longitud :: [Int]->Int
    longitud [] = 0
    longitud (_:xs) = 1 + longitud xs

    --Ahora lo mismo pero pasandolo a recursividad no final añadiendo parametros de acumulacion
    --longitud' :: [Int]->Int
    --longitud' lista = longAux lista 0

    --longAux :: [Int]->Int->Int
    --longAux [] r = 0
    --longAux (_:xs) r = longAux xs (r+1)

   
    --Longitud de lista  con una unica ecuacion
    longitud'' :: [Int]->Int
    longitud'' lista = if lista == [] then 0 else 1 + longitud (tail lista)


      --Longitud de lista utilizando CASE
    longitud''' :: [Int]->Int
    longitud''' lista = case(lista == [])of
                            True -> 0
                            False -> 1 + longitud''' (tail lista)

    --Apariciones de carracteres en una lista(RECURSIVIDAD NO FINAL)
    numApariciones :: [Char]->Char->Int
    numApariciones []_ = 0
    numApariciones lista letra = if ((head lista) == letra) then (1 + numApariciones (tail lista) letra) else ( numApariciones (tail lista) letra)

    --Apariciones de carracteres en una lista(RECURSIVIDAD FINAL)
    numApariciones' :: String -> Char ->Int
    numApariciones' frase letra = numAparicionesAux frase letra 0

    numAparicionesAux :: String->Char->Int->Int
    numAparicionesAux [] _ contador = contador
    numAparicionesAux frase letra contador = if (head frase == letra) then    
                                                        numAparicionesAux (tail frase) letra (contador+1)
                                                        else numAparicionesAux (tail frase) letra contador

      --NO FINAL

    sumaLista :: [Int]->Int
    sumaLista [] = 0
    sumaLista (x:xs) = x + sumaLista xs


  --FINAL O DE COLA

   -- sumaLista' :: [Int]->Int
    --sumaLista' lista = sumaListaAux lista 0

    --sumaListaAux :: [Int]->Int->Int
  