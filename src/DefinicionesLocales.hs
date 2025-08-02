module DefinicionesLocales where

    --Ejemplo raices
    raices :: Float->Float->Float->(Float,Float)

    raices a b c
        |disc >= 0 = ((-b+raizDisc)/demon, (-b-raizDisc)/demon)
        |otherwise = error "raices complejas"
            where
                disc = b*b - 4*a*c
                raizDisc = sqrt disc
                demon = 2*a