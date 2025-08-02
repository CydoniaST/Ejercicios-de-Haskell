module HojaDeEjercicios2 where

    --ORDENAR DE MENOR A MAYOR
    ordenadosMenor :: Int->Int->Int->Bool
    ordenadosMenor x y z = if ((x < y) && (y < z)) then True else False

    --LO ANTERIOR PERO CON GUARDAS
    ordenadosMenor' :: Int->Int->Int->Bool
    ordenadosMenor' x y z
                            |(x < y) && (y < z) = True
                            |otherwise = False