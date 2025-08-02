module Listas where
    import Data.Char
    --LISTAS
    --ejercicio1: funcion que recibe 2 numeros y que devuelve una lista de sumar, restar y multiplicar
    
    operaciones :: Int -> Int -> [Int]
    operaciones x y = [x+y, x-y, x*y]

    --ejercicio2: funcion recibe 2 string y devuelve la union de ambas si la longitud es menor o igual que 3
    union :: String -> String -> String
    union x y = if length x == length y then concat [x,y] else []

    union' :: String -> String -> String
    union' x y = if(length x <= 3) 
                 && (length y <= 3) then x++y
                 else ""

    --ejercicio3: funcion que dada una frase devuelva una cadena formada por los caracteres en mayus
    mayusculas :: String -> String
    mayusculas frase = [x | x <- frase, isUpper x]

    --ejercicio4: funcion que recibe una lista de caracteres e indique si su longitud es menor o igual a 3 ( utilizando patrones)
    --longitud :: String -> Bool
    --longitud (x1:x2:x3:xs) = False
    --longitud _ = True; 

  --Ejercicio 11 , listas de comprension 
 -- comprobacion :: Int->[Int]->[Int]->Bool
  --comprobacion
  --listasiguales ::  [Int]->[Int]
  --listasiguales 
    

    --Ejercicio(Listas y patrones)
    --Decir si la frase empieza por A o a
    empiezaPorA :: String -> Bool
    empiezaPorA ('a':_) = True
    empiezaPorA ('A':_) = True
    empiezaPorA _ = False

    --ordenAlfabetico :: String -> String ->(String,String)
    --ordenAlfabetico lista [] = ([],lista)
    --ordenAlfabetico [] lista = ([], lista)
    --ordenAlfabetico c1@(x:xs) c2@(y:ys) = if (toLower x) < (toLower y) then (c1,c2) else (c2,c1)
    