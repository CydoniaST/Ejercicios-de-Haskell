module PracticaExamen where

--PRACTICA 1
   -- import System.IO()
    --import System.IO.Unsafe()

   -- lecturaDatos:: IO [String]
    --lecturaDatos = do
      --      content <- readFile "personajes.csv"
        --    return (lines content)

    --lineasFichero :: [String]
    --lineasFichero = unsafePerformIO (lecturaDatos)

    --data Continent = Europe | Asia | Africa | NorthAmerica | Unknown | SouthAmerica deriving Show

    --type Ciudad = String
    --type Pais = String
    --data Lugar = L Ciudad Pais Continent deriving Show

    --data Persona = P {nombre :: String,
     --               sexo :: String,
      --              nacimiento :: Int,
       --             lugar :: Lugar,
        --            profesion :: String} deriving Show

    --Recibir linea y delimitador
    --partirLinea :: String -> Char ->[String]
    --partirLinea "" _ = []
    --partirLinea linea d = partirAux linea d [] []

    --partirAux :: String -> Char -> String -> [String] -> [String]
    --partirAux [] _ inicio ac = ac ++ [inicio]
    --partirAux (x:xs) d palabra ac = 
      --  if x == d
        --    then partirAux xs d [] (ac ++ [palabra])
          --  else partirAux xs d (palabra ++ [x]) ac
    
    --Convertir string a continente
    --parseContinente :: String -> Continent
    --parseContinente c = case c of
    --    "Europe"-> Europe
    --    "Asia"-> Asia
    --    "Africa"-> Africa
    --    "North America" -> NorthAmerica
    --    "South America" -> SouthAmerica
    --    _-> Unknown

    --parsePersonaje :: String -> Persona
    --parsePersonaje linea = P (datos !! 0) (datos !! 1) (read(datos !! 2)) (L (datos !! 3) (datos !! 4) (parseContinente (datos !! 5))) (datos !! 6)
    --                            where
     --                               datos = Prelude.take 7 (partirLinea linea ';')

    --personaHistorico :: [Persona]
    --personaHistorico = map parsePersonaje (tail lineasFichero)

    --name :: Persona -> String
    --name (P n _ _ _ _) = n
    
    --names :: [Persona] -> [String]
    --names lista = [name p | p <- lista]

    
    
--EJERCICIOS

