import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: Int,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en el universidad o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante al universidad
registrarEntrada :: Int -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada estudianteId tiempo universidad =
    Estudiante estudianteId tiempo Nothing : universidad

-- Función para registrar la salida de un estudiante del universidad
registrarSalida :: Int -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida estudianteId tiempo universidad =
    map (\est -> if estudianteId == idEstudiante est then est { salida = Just tiempo } else est) universidad

-- Función para buscar un estudiante por su idEstudiante en el universidad
buscarEstudiante :: Int -> [Estudiante] -> Maybe Estudiante
buscarEstudiante estudianteId universidad =
    find (\est -> estudianteId == idEstudiante est && isNothing (salida est)) universidad
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un estudiante permaneció en el universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarUniversidad :: [Estudiante] -> IO ()
guardarUniversidad universidad = do
    withFile "universidad.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante universidad))
    putStrLn "universidad guardado en el archivo universidad.txt."

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarUniversidad :: IO [Estudiante]
cargarUniversidad = do
    contenido <- withFile "universidad.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante idEstudiante entrada salida) =
    "Estudiante {idEstudiante = \"" ++ show idEstudiante ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los estudiantes en el universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en el universidad."
listarEstudiantes estudiantes = do
    putStrLn "estudiantes en el universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el universidad desde el archivo de texto
    universidad <- cargarUniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión de Universidad!"

    -- Ciclo principal del programa
    cicloPrincipal universidad

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por idEstudiante"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la idEstudiante del estudiante:"
            estudianteId <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizado = registrarEntrada (read estudianteId) tiempoActual universidad
            putStrLn $ "estudiante con idEstudiante " ++ estudianteId ++ " ingresado al universidad."
            guardarUniversidad universidadActualizado
            cicloPrincipal universidadActualizado

        "2" -> do
            putStrLn "Ingrese la idEstudiante del estudiante a salir:"
            estudianteId <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizado = registrarSalida (read estudianteId) tiempoActual universidad
            putStrLn $ "estudiante con idEstudiante " ++ estudianteId ++ " salido del universidad."
            guardarUniversidad universidadActualizado
            cicloPrincipal universidadActualizado

        "3" -> do
            putStrLn "Ingrese la idEstudiante del estudiante a buscar:"
            estudianteId <- getLine
            case buscarEstudiante (read estudianteId) universidad of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con idEstudiante " ++ estudianteId ++ " se encuentra en el Universidad."
                    putStrLn $ "Tiempo en universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "estudiante no encontrado en el universidad."
            cicloPrincipal universidad

        "4" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad