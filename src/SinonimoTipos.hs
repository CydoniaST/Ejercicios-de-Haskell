module SinonimoTipos where

    type Nota = Int 
    type DNI = String
    type Expediente = Int
    type Alumno = (DNI, Nota, Expediente)

    datosAlumno :: Alumno->Bool
    datosAlumno (_,nota,_) = nota >= 5

--CREAR DIFERENTES TIPOS DE DATOS !!

--En los Enum siempre añadir al final "deriving show"

    data Alumno' = A DNI Expediente Nota deriving Show

    datosAlumno' :: Alumno' -> Bool
    datosAlumno' (A _ _ nota') = nota' >= 5

    data Alumno'' = A' {dni :: String,
                        expediente :: Int,
                        notas :: Float} deriving Show

    datoAlumno'' :: Alumno''->Bool
    datoAlumno'' (A' _ _ nota) = nota >= 5.0

    data Matriz = M [[Int]]

    m :: Matriz
    m = M [[1,2,3,4],[5,6,7,8]]

    matriz :: Matriz -> (Int,Int)
    matriz (M []) = (0, 0)
    matriz (M fil) = (length fil, length (head fil))

    --Funcion que devuelve una lista de los dias de la semana laborables

    data Semana =  Lunes | Martes | Miercoles | Jueves | Viernes | Sabado| Domingo deriving Show

    diasLaborables :: [Semana]
    diasLaborables = [Lunes, Martes, Miercoles, Jueves, Viernes]




