module DefinicionFunciones where

    esPar :: Int -> Bool
    esPar x = (x `mod` 2) == 0

    esImpar :: Int -> Bool
    esImpar = not.esPar

-- Funciones Currificadas, funciones que reciben varios argumentos y que son simplificadas hasta que reciben un solo argumento

    suma :: Int->Int->Int
    suma x y = x+y

    sucesor :: Int->Int
    sucesor = suma 1

    multiplo :: Integer->Integer->Bool
    multiplo x y = y `mod` x == 0

    esPar' :: Integer->Bool
    esPar' = multiplo 2

    --Tipos predefinidos
    --Int e Integer
    -- +, -, *, `div`, `mod`, abs, negate, even(par), odd(impar)
    --Float, Double
    --Truncate(quitar decimales), round, floor(entero inferior), ceiling(entero superior)
    --fromInteger(convertir de Integer a Float o Double)
    --Bool
    -- &&, ||, not, otherwise
    --Char
    --ord, chr, isUpper, isDigit, toLower
    -- ==, /=(distinto), <, <=, >, >= 
