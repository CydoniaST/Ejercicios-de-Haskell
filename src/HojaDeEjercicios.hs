module HojaDeEjercicios where

    --SUMA DE 3 NUMEROS
    suma :: Int->Int->Int->Int
    suma x y z = x+y+z

    --SUCESOR A UN NUMERO
    sucesor :: Int->Int
    sucesor x = x+1

    --CUADRUPLE DE UN NUMERO
    cuadruple :: Int->Int
    cuadruple x = x*4

    --FIN DE SEMANA
    finSemana :: (String,String)
    finSemana  = ("Sabado", "Domingo")

    --SUCESOR DE SUMA DE DOBLES
    sucesorSuma :: Int->Int->Int
    sucesorSuma x y = ((x*x) + (y*y)) + 1

    --MODULO DE 2 NUMEROS ENTEROS
    modulo :: Int->Int->Int
    modulo x y = mod x y

    --BOOLEANO PAR E IMPAR
    par :: Int->Bool
    par x = even x

    impar :: Int->Bool
    impar x = not(even x)

    --FUNCION QUE DEVULVE 1 SI ES POSITIVO, -1 SI ES NEGATIVO Y 0 SI ES 0
    signum :: Int->Int
    signum x = if x>0 then 1 else if x<0 then -1 else 0 

    signum' :: Int->Int --IF ELSE ANTERIOR PERO UTILIZANDO GUARDAS
    signum' x 
                |x < 0 = -1
                |x > 0 = 1
                |otherwise = 0

    --AÑO BISIESTO
    bisiesto :: Int->Bool
    bisiesto x = if mod x 4 == 0 then True else False

    bisiesto' :: Int->Bool --IGUAL PERO CON GUARDAS
    bisiesto' x
                | mod x 4 == 0 = True
                |otherwise = False

    --SUMA DE TUPLAS
    sumaTuplas :: (Int,Int,Int,Int)->Int
    sumaTuplas (x,y,z,k) = x+y+z+k