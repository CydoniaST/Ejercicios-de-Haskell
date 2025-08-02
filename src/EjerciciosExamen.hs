module EjerciciosExamen where
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

    --Listas
    pruebaZip :: [Int]->String->[(Int,Char)]
    pruebaZip [] _ = []
    pruebaZip _ [] = []
    pruebaZip (x:xs) (y:ys) = (x,y) : zip xs ys

    --EJERCICIO 9
    esVocal :: Char->Bool
    esVocal c = if ((c == 'a') || (c == 'e') || (c == 'i') || (c == 'o') || (c == 'u')) then True else False --acabalo bien

    codificar :: [(Char,Char)]->[Char]
    codificar lista = [x | (x,y)<-lista, esVocal y]

    --EJERCICIO 10
    filtrarTuplas :: Int->[(Int,Int)]->[(Int,Int)]
    filtrarTuplas n lista = [(x,y) | (x,y)<-lista, odd x , x > n]

    --EJERCICIO 11
    --ternaPitagorica


    --EJERCICIOS EXAMEN

    --Ejercicio 2 Altura de un arbol
    data Arbol a = AV | Rama (Arbol a) a (Arbol a)

    maximo :: Int->Int->Int
    maximo x y = if x >= y then x else y

    altura :: Arbol a -> Int
    altura AV = 0
    altura (Rama iz _ de) = 1 + (maximo (altura iz) (altura de))


    --Ejercicio 3
    type Arte = String
    type Nombre = String
    type Nacimiento = Int
    data Artista = A Nombre Arte Nacimiento

    artistas :: [Artista]
    artistas = [(A "Cervantes" "Literatura" 1547),
               (A "Velazques" "Pintura" 1599),
               (A "Picasso" "Pintura" 1881),
               (A "Beethoven" "Musica" 1770),
               (A "Poincare" "Ciencia" 1854),
               (A "Borromini" "Arquitectura" 1599)] 


    instance Show Artista where
        show (A nombre arte nacimiento) = nombre ++ " - " ++ arte ++ " nacido en el año " ++ show(nacimiento) ++ "\n"

    pintores :: [Artista] -> [Artista]
    pintores lista = [ (A nombre arte nacimiento) | (A nombre arte nacimiento)<-lista,  arte == "Pintura" && nacimiento > 1500]

    --Ejercicio 4
 


    data TipoEvento = EventoFamiliar | Boda | FiestaInfantil | ReunionEmpresa deriving (Eq, Show)

    --Apartado a
    type Direccion = String
    type Capacidad = Int
    type ServicioCocina = Bool
    
    data Actividad = Escalada | Saltos | Bolos deriving Show

    data Datos = D Nombre Direccion Capacidad deriving Show

    data Espacios = EspacioInfantil Datos
                    | Loft Datos ServicioCocina
                    | EspacioDeportivo Datos Actividad deriving Show

    data Empresa = E Nombre [Espacios]
 
    --Apartado B

    puedeCelebrar :: Espacios -> TipoEvento -> Bool
    puedeCelebrar (EspacioInfantil _) evento = evento == FiestaInfantil
    puedeCelebrar (Loft _ _) evento = evento == EventoFamiliar || evento==Boda 
    puedeCelebrar (EspacioDeportivo _ _ ) evento = evento == EventoFamiliar

    --Apartado C

    buscar :: TipoEvento -> Empresa -> [Espacios]
    buscar evento (E _ empresa) = foldr (\espacios ac -> if puedeCelebrar espacios evento then ac++[espacios] else ac) [] empresa

    --Apartado D 

    instance Eq Datos where
        (D _ _ c1) == (D _ _ c2) = c1==c2

    instance Eq Espacios where
        (EspacioInfantil d1) == (EspacioInfantil dg2) = d1==dg2
        (Loft d1 _) == (Loft d2 _) =d1==d2
        (EspacioDeportivo d1 _)== (EspacioDeportivo d2 _)=d1==d2
        _==_=False

    espaciosIguales :: Empresa -> Empresa ->[Espacios]
    espaciosIguales (E _ []) _ = []
    espaciosIguales (E n1 (x:xs)) (E n2 lista) = if (elem x lista) then 
        x:espaciosIguales(E n2 xs) (E n2 lista)
            else espaciosIguales (E n1 xs)(E n2 lista)


--Ejercicios Listas de Comprension

    apariciones :: String->Char->Int
    apariciones frase letra = length([x | x <- frase, x == letra])

    primerElemento :: ((String,Int),(String,Int),(String,Int)) ->(String,String,String)
    primerElemento ((a, _),(b, _),(c, _)) = (a,b,c)

    -- suma4Elementos:: [Int]->Bool
    -- suma4Elementos (w:x:y:z:_) = if (suma < 10) then True else False
    --     where
    --         suma = w+x+y+z
    -- suma4Elementos (x,y,z) = if ((x+y+z) < 10) then True else False
    -- suma4Elementos (x,y) = if ((x+y) < 10) then True else False
    -- suma4Elementos (x) = if (x < 10) then True else False

    suma4Elementos:: [Int]->Bool
    suma4Elementos lista = if (suma < 10) then True else False
        where suma = sumaListas (take 4 lista)

    sumaListas :: [Int] -> Int
    sumaListas lista = foldr (+) 0 lista 

--Elemento dentro de una lista polimorfica recursivo
    contieneP :: (Eq a)=> a->[a]->Bool
    contieneP _ [] = False
    contieneP e (x:xs) = (e == x) || contieneP e xs

--Elementos iguales
    elementosIguales :: Int -> [Int] -> Bool
    elementosIguales _ [] = False
    elementosIguales x lista = all (== x) lista

    elementosIguales' :: Int -> [Int] -> Bool
    elementosIguales' x lista = foldl (&&) True listaBool
        where listaBool = (map (==x) lista)

    sumaListas'' :: (Num a) => [[a]] -> a
    sumaListas'' lista = sumaListasP (map (sumaListasP) lista)

    sumaListasP :: (Num a) => [a] -> a
    sumaListasP lista = foldr (+) 0 lista 

--Primer y ultimo elemento de lista EJERCICIO 6 TEMA 3
    -- instance Show where
    --     show frase = "La primera letra de la frase es:  " ++ take 1 String ++ "y la ultima letra es: " ++ take last String

    -- primerYultimo :: String -> String
    -- primerYultimo = show

--Ejercicios 9 tema 5


--Ejercicio repaso 
    -- listaMelliza :: [a] -> ([a],[a])
    -- listaMelliza lista = ([ x | (x,y) <- zip lista [1..], even y], [ x | (x,y) <- zip lista [1..], odd y])

    listaMelliza' :: [a] -> ([a],[a])
    listaMelliza' lista = (pares, impares) 
        where
            pares = foldr (\x acum -> if (even x) then (lista!!x):acum else acum) [] [0..length(lista)-1]
            impares = foldr (\x acum -> if (odd x) then (lista!!x):acum else acum) [] [0..length(lista)-1]

    --Ejercicio 2 Examen de Julio 2023
    sumaListasPlegado :: Num a => [[a]] -> a
    sumaListasPlegado [] = 0
    --sumaListasPlegado (x:xs) = (foldr(\ y acum -> y + acum) 0 x) + sumaListasPlegado xs  
    sumaListasPlegado (x:xs) = (foldr(+) 0 x) + sumaListasPlegado xs  

    sumaConsecutivos :: [Int]->[Int]
    sumaConsecutivos xs = [x + y | (x, y) <- zip xs (tail xs)]
    --sumaConsecutivos xs = [x + y | (x, y) <- zip xs xs] -- Esto suma la lista 1 a la lista 2
    --sumaConsecutivos xs = [x + y | (x, y) <- zip (tail xs) (tail xs)] --Esto hace lo mismo que lo anterior pero excluyendo el primer elemento de las listas
    
    --Eliminar Repetidos Ejercicio 8 Tema 5-1

    -- eliminarDuplicados :: [Int]->[Int]
    -- eliminarDuplicados [x:xs] = 


--Ejercicio 6 Tema 5 2
                                --iz         de
    data Arbol2 a = AV2 | Rama2 (Arbol2 a) a (Arbol2 a) deriving Show

    pertenece :: Eq a => Arbol2 a -> a -> Bool
    pertenece AV2 _ = False
    pertenece (Rama2 iz x de) y = if x == y then True else (pertenece iz y) || (pertenece de y)

--Ejercicio 1 Examen febrero 2003
    data Arbol' a = AV' | Rama' (Arbol' a) a (Arbol' a) deriving Eq

    insertarElemArbol :: Ord a => a -> Arbol' a -> Arbol' a
    insertarElemArbol x AV' = (Rama' AV' x AV')
    insertarElemArbol x (Rama' iz y de)
                                        | (x > y)  = Rama' iz y (insertarElemArbol x de)
                                        | otherwise = Rama' (insertarElemArbol x iz) y de

    -- eliminarElemArbol :: Arbol' a-> a -> Arbol' a
    -- eliminarElemArbol AV' x = (Rama' AV' x AV') 
    -- eliminarElemArbol (Rama' iz y de) x 
    --                                     |x > y = if (pertenece (Rama' iz y de) x) then (Rama' iz y (eliminarElemArbol x de)) else (Rama' iz y de)
    --                                     |otherwise = if (pertenece (Rama' iz y de) x) then (Rama' iz y de) else (Rama' iz y (eliminarElemArbol x de))
--Ejercicio 2 Examen febrero 2003

    sinVocalesNiConsonantes :: [Char] ->(String, String)
    sinVocalesNiConsonantes palabra = (vocales,consonantes)
        where
            vocales = foldr(\ x acum -> if (esVocal x) then x:acum else acum)[] palabra
            consonantes = foldr(\ x acum -> if (esVocal x) then acum else x:acum )[] palabra

--REPASO LISTAS
    posiciones :: [Int]->[(Int,Int)]
    posiciones lista =  zip lista [0..]

    longitud' :: [Int]->Int
    longitud' [] = 0
    longitud' lista = sum [x*0 +1 | x <- lista]

    -- contenido :: [Int]->Int->Bool
    -- contenido lista y = [result | result <- lista, x == y]

    primerosPares :: [(Char, Int)] -> String
    primerosPares lista = [ fst x| x<- lista, snd x`mod`2 == 0]

    --EXAMEN MARZO 2022 Ejercicio 2

    palabraAux :: String ->String->[String]-> [String]
    palabraAux [] palabra acum = acum++[palabra]
    palabraAux (x:xs) palabra acum = if x==' ' then palabraAux xs [] (acum++[palabra]) else palabraAux xs (palabra++[x]) acum

    palabraLarga :: String -> String
    palabraLarga [] = ""
    palabraLarga frase = foldr(\x acum -> if length x > length acum then x else acum)[] (palabraAux frase [][])

--Ejercicio 3 Febrero 2003

--APARTADO A
    type NumeroPisos = Int
    type NumeroPuertasPorPiso = Int
    type Calle = String
    type Numero = Int
    type Ciudad = String
    type MetrosCuadrados = Int

    data TamPiscina = VEINTE_POR_DIEZ 
                        | VEINTICINCO_POR_DIEZ 
                        | VEINTICINCO_POR_QUINCE

    data Servicios =  Limpieza NumeroPisos NumeroPuertasPorPiso
                            -- | MantenerPiscina TamPiscina
                            | MantenerJardin MetrosCuadrados
    data Comunidad = C{
                        calle ::Calle,
                        numero :: Numero,
                        ciudad :: Ciudad,
                        serviciosContratados :: [Servicios]
                        }
    
    type EmpresaServicios = [Comunidad]


    instance Show Comunidad where
        show (C calleC numeroC ciudadC serviciosContratadosC) = "Comunidad de vecinos sita en Calle "++ calleC ++ " número " ++ show(numeroC) ++ " en " ++ ciudadC ++
                                                            "\nServicios contratados:\n" ++
                                                                    concatMap showServicio serviciosContratadosC
                                                                    where
                                                                        showServicio (Limpieza numPisos numPuertas) =
                                                                            " - Servicio de limpieza para " ++ show numPisos ++ " pisos con " ++ show numPuertas ++ " puertas por piso\n"
                                                                        -- showServicio (MantenerPiscina tam) =
                                                                        --     " - Cuidado de piscina de tamaño " ++ "\n"
                                                                        showServicio (MantenerJardin medidas) =
                                                                            " - Mantenimiento del jardín de " ++ show medidas ++ " cuadrados\n"
    edificio :: EmpresaServicios
    edificio = [    
                C "Tulipan" 2 "Móstoles" [Limpieza 7 4, MantenerJardin 210]
                ]

    --Ejercicio 4 Marzo 2022

    data List a = Vacia | Cons a (List a) deriving Show

    l1 :: List Int
    l1 = Cons 1(Cons 2(Cons 3 Vacia))

    l2 :: List Int
    l2 = Cons 10(Cons 20(Cons 30 Vacia))

    sumaListasRecursiva :: List Int -> List Int -> List Int
    sumaListasRecursiva Vacia Vacia = Vacia
    sumaListasRecursiva (Cons x y) Vacia = (Cons x y)
    sumaListasRecursiva Vacia (Cons x y) = (Cons x y)
    sumaListasRecursiva (Cons x xs) (Cons y ys) = (Cons (x+y) (sumaListasRecursiva xs ys))

--Ejercicio 3 Febrero 2023
    separar :: String -> (String,String)

    separar palabra = (vocales, consonantes)
        where
            vocales = foldr(\ x acum -> if esVocal x then x:acum else acum) [] palabra
            consonantes = foldr(\ x acum -> if esVocal x then acum else x:acum)[] palabra

    separarNumeros ::[Int] -> ([Int], [Int])

    separarNumeros lista = (pares,impares)
        where
            pares = foldr(\ x acum -> if even x then x:acum else acum)[] lista
            impares = foldr(\ x acum -> if odd x then x:acum else acum)[] lista

--Ejercicio 3 Marzo 2023

    -- parejaPosicionesCoincidentes :: [(Int,a)]->[(Int,a)]
    
    -- parejaPosicionesCoincidentes 

--Ejercicio 5 Marzo 2022

    type NombreHotel = String
    type Estrellas = Int
    
    data Ubicacion = Ciudad | Pais

    type NumHabitaciones = Int

    data TipoHabitaciones = INDIVIDUALES | DOBLES | TRIPLES | SUITES 

    data Hotel = H NombreHotel Estrellas Ubicacion NumHabitaciones TipoHabitaciones

    data CapacidadHotel = CH {
                            individuales :: Int,
                            dobles :: Int,
                            triples :: Int,
                            suites :: Int
                        }
    

    -- invertirNumero :: Int -> Int
    -- invertirNumero x = 

    -- invertirAux :: Int -> Int -> Int
    -- invertirAux


    invertirString :: String -> String
    invertirString = foldr (\xs x -> x ++ [xs]) []

    f :: [Int] -> [Int]
    f  = foldr (\ x y -> if even x then [x+2]++y else y) [] 

    data Arbolito a = ArbolitoV | Ramita (Arbolito a) a (Arbolito a) deriving Show

    -- busqueda :: Eq a => Arbol a -> a -> a -> [a]
    
    -- busqueda ArbolitoV _ _= []
    
    -- busqueda (Rama iz x de) y k
    --                             | (x == y) = [x:busqueda iz y] || [x:busquea y de]
    --                             | (x == k) = [x:busqueda iz k] || [x : busqueda k de]

    p :: [a] -> [[a]]
    p [] = [[]]
    p (x:xs) = []: map (x:) (p xs)

    misterio :: Eq a=> [a] -> [(a, Int)]
    misterio [] = []
    misterio l@(x:xs) = (x, freq): misterio l_sin_x where
                        freq = length (filter (== x) l)
                        l_sin_x = filter (/= x) xs