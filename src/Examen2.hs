 module Examen2 where
--     --Ejercicio 2:

--     palabraMasLarga :: String -> String
--     palabraMasLarga lista = foldr(\x acum -> if (length x) > (length acum) then x else acum) [] (separarPalabras lista [] [])

    
--     separarPalabras :: String -> String -> [String] -> [String]
--     separarPalabras [] pal acum = acum ++ [pal]
--     separarPalabras (x:xs) pal acum = if x == ' ' then separarPalabras xs [] (acum++[pal]) else separarPalabras xs (pal++[x]) acum

--     --Ejercicio 3:
--     parejaPosicionCoincidente :: [(Int, a)] -> [a]
--     parejaPosicionCoincidente lista = foldr(\i acum -> if (coincide (lista!!i) (i+1)) then (posB (lista!!i)):acum else acum ) [] [0..length(lista)-1]

--     coincide :: (Int, a) -> Int -> Bool
--     coincide (a, _) i = i==a

--     posB :: (Int, a) -> a
--     posB (_, b) = b

--     --Ejercicio 4:
--     data List a =  Vacia | Cons a (List a) deriving Show

--     l1 :: List Int
--     l1 = Cons 1 (Cons 2 (Cons 3 Vacia))
--     l2 :: List Int
--     l2 = Cons 10 (Cons 20 (Cons 30 Vacia))

--     suma :: Num a => List a -> List a -> List a
--     suma Vacia Vacia = Vacia
--     suma (Cons d1 le1) (Cons d2 le2) = (Cons (d1+d2) (suma le1 le2))
--     suma Vacia c2 = c2
--     suma c1 Vacia = c1

--     --Ejercicio 5:

--     --a)
--     type Nombre = String
--     type Estrellas = Int
--     type Ciudad = String
--     type Pais = String


--     data Capacidad = C {individuales :: Int,
--                         dobles :: Int,
--                         triples :: Int,
--                         suites :: Int} deriving Show

--     data Hotel = H Nombre Estrellas Ciudad Pais Capacidad deriving Show

--     nh :: [Hotel]
--     nh = [(H "Palace" 5 "Madrid" "Espana" (C 24 10 1 4)), (H "Palace" 4 "Madrid" "Espana" (C 24 15 1 4)), (H "Palace" 2 "Barcelona" "Espana" (C 20 12 1 4))]
--     --b)
--     instance Eq Hotel where
--         (==) (H _ _ _ _ c1) (H _ _ _ _ c2) = c1 == c2

--     instance Ord Hotel where
--         compare (H _ _ _ _ c1) (H _ _ _ _ c2) = compare c1 c2
    
--     instance Eq Capacidad where
--         (==) (C i1 d1 _ _) (C i2 d2 _ _) = i1 == i2 && d1 == d2
    
--     instance Ord Capacidad where
--         compare (C i1 d1 _ _) (C i2 d2 _ _) = if (compare i1 i2) == EQ then (compare d1 d2) else (compare i1 i2)

--     order :: Ord a => [a] -> [a]
--     order [] = []
--     order (x:xs) =
--         order   menores ++ [x] ++ order mayores
--         where   menores = [y | y <- xs, y <= x]
--                 mayores = [y | y <- xs, y > x] 

--     --c)
--     data CasaRural = CR {
--                         nombre :: String,
--                         espigas :: Int,
--                         direccion :: String
--                         }

--     class Impuesto a where
--         impuesto :: a -> Double

--     instance Impuesto Hotel where
--         impuesto (H _ e _ _ c) = if ((individuales c) + (dobles c) + (triples c) + (suites c)) > 30 && e >= 3 then 345.5 else if ((individuales c) + (dobles c) + (triples c) + (suites c)) > 30 && e < 3 then 225.5 else 150.5

--     instance Impuesto CasaRural where
--         impuesto (CR _ e _) = if e > 3 then 250.5 else 180.5


