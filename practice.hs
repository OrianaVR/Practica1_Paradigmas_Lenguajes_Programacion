import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)
import Data.Text (splitOn)
import Data.List (uncons)


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
tiempoEnUniversidad :: Estudiante -> UTCTime -> NominalDiffTime
tiempoEnUniversidad estudiante tiempoActual =
    case salida estudiante of
        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada estudiante)
        Nothing           -> diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarUniversidad :: [Estudiante] -> IO ()
guardarUniversidad universidad = do
    resultado <- reintentar 5 $ do
        withFile "universidad.txt" WriteMode $ \handle -> do
            mapM_ (hPutStrLn handle . mostrarEstudiante) universidad
            hFlush handle  -- Asegura que los datos se escriban inmediatamente
    case resultado of
        Left ex -> putStrLn $ "Error guardando el universidad: " ++ show ex
        Right _ -> putStrLn "Se ha guardado en el archivo universidad.txt."


-- Función para reintentar una operación en caso de error
reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
reintentar n accion = do
    resultado <- catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
    case resultado of
        Left ex -> do
            threadDelay 1000000  -- Esperar 1 segundo antes de reintentar
            reintentar (n - 1) accion
        Right val -> return (Right val)

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarUniversidad :: IO [Estudiante]
cargarUniversidad = do
    resultado <- try (readFile "universidad.txt") :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Error cargando en universidad: " ++ show ex
            return []
        Right contenido -> do
            let lineas = lines contenido
            return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un vehículo como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante estudiante =
   show (idEstudiante estudiante) ++ "," ++ show (entrada estudiante) ++ "," ++ show (salida estudiante)

-- Función para cargar la información de los estudiantes desde un archivo de texto
leerUniversidad :: IO [Estudiante]
leerUniversidad = do
    contenido <- readFile "universidad.txt"
    let lineas = lines contenido
    return (mapMaybe parsearEstudiante lineas)
    where
        parsearEstudiante :: String -> Maybe Estudiante
        parsearEstudiante linea = case splitOn "," linea of
            [idEstudiante, entrada, salida] -> Just $ Estudiante (read idEstudiante) (read entrada) (readMaybeSalida salida)
            _ -> Nothing

        readMaybeSalida :: String -> Maybe UTCTime
        readMaybeSalida "Nothing" = Nothing
        readMaybeSalida salida = Just (read salida)

        splitOn :: Eq a => [a] -> [a] -> [[a]]
        splitOn delim str = case uncons delim of
            Just (d, _) -> case break (== d) str of
                (a, _ : b) -> a : splitOn delim b
                (a, [])    -> [a]
            Nothing -> [str]  -- Si el delimitador está vacío, devolvemos la cadena original




-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada del estudiante"
    putStrLn "2. Registrar salida del estudiante"
    putStrLn "3. Buscar estudiante por id"
    putStrLn "4. Listar los estudiantes de la universidad"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el id del estudiante:"
            estudianteId <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizado = registrarEntrada (read estudianteId:: Int) tiempoActual universidad
            putStrLn $ "Estudiante con id " ++ estudianteId ++ " ingresado al universidad."
            guardarUniversidad universidadActualizado
            cicloPrincipal universidadActualizado

        "2" -> do
            putStrLn "Ingrese el id del estudiante a salir:"
            estudianteId <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizado = registrarSalida (read estudianteId :: Int) tiempoActual universidad
            putStrLn $ "Estudiante con id " ++ estudianteId ++ " salido de la universidad."
            guardarUniversidad universidadActualizado
            cicloPrincipal universidadActualizado

        "3" -> do
            putStrLn "Ingrese el id del estudiante a buscar:"
            estudianteId <- getLine
            case buscarEstudiante (read estudianteId :: Int) universidad of
                Just estudiante -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnUniversidad estudiante tiempoActual
                    putStrLn $ "El estudiante con id " ++ estudianteId ++ " se encuentra en la universidad."
                    putStrLn $ "Tiempo en universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal universidad
        "4" -> do
            putStrLn "Mostrando Lista de estudiantes dentro del universidad"
            -- Leer el universidad actualizado
            universidadActualizado <- leerUniversidad
            mapM_ (\est -> putStrLn $ "Id: " ++ show (idEstudiante est) ++ ", Entrada: " ++ show (entrada est) ++ ", Salida: " ++ show (salida est)) universidadActualizado
            cicloPrincipal universidadActualizado  -- Mantenemos el universidad actualizado




        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el universidad desde el archivo de texto
    universidad <- cargarUniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión de la Universidad!"

    -- Ciclo principal del programa
    cicloPrincipal universidad

