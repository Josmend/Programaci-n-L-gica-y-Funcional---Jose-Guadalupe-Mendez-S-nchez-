import Data.Char (toUpper)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- Ejercicio 1


operaciones :: [(String, Int -> Double)]
operaciones =
  [ ("opSin", sin . fromIntegral), 
    ("opCos", cos . fromIntegral), 
    ("opTan", tan . fromIntegral), 
    ("opExp", exp . fromIntegral), 
    ("opLog", log . fromIntegral) 
  ]


aplicarOperacion :: String -> Int -> [(Int, Double)]
aplicarOperacion operacion cantidad =
  [ ( i,
      case lookup operacion operaciones of 
        Just funcion -> funcion i 
        Nothing -> error "Operación no válida" 
    )
    | i <- [1 .. cantidad] 
  ]


mostrarTabla :: [(Int, Double)] -> IO ()
mostrarTabla tabla = do
  putStrLn "Tabla de valores y resultados:" 
  mapM_ (\(i, j) -> printf "%d\t%.4f\n" i j) tabla 


calculadora :: IO ()
calculadora = do
  putStr "Introduce la operación a aplicar (opSin, opCos, opTan, opExp, opLog): " 
  hFlush stdout 
  operacion <- getLine 
  putStr "Introduce un entero positivo: " 
  hFlush stdout 
  cantidadStr <- getLine
  let cantidad = read cantidadStr :: Int 
  mostrarTabla (aplicarOperacion operacion cantidad) 



-- Ejercicio 2


filtrarLista :: (a -> Bool) -> [a] -> [a]
filtrarLista condicion lista = filter condicion lista

esPar :: Int -> Bool
esPar numero = numero `mod` 2 == 0



-- Ejercicio 3


clasificarCalificaciones :: [Int] -> [String]
clasificarCalificaciones = map clasificar
  where
    clasificar calificacion
      | calificacion >= 95 && calificacion <= 100 = "Excelente" 
      | calificacion >= 85 && calificacion <= 94 = "Notable" 
      | calificacion >= 75 && calificacion <= 84 = "Bueno" 
      | calificacion >= 70 && calificacion <= 74 = "Suficiente" 
      | otherwise = "Desempeño insuficiente" 



-- Ejercicio 4


convertirNotas :: [(String, Int)] -> [(String, String)]
convertirNotas base = [(map toUpper materia, convertirCalificacion calificacion) | (materia, calificacion) <- base]
  where
    convertirCalificacion calificacion
      | calificacion >= 95 && calificacion <= 100 = "Excelente" 
      | calificacion >= 85 && calificacion <= 94 = "Notable" 
      | calificacion >= 75 && calificacion <= 84 = "Bueno" 
      | calificacion >= 70 && calificacion <= 74 = "Suficiente" 
      | otherwise = "Desempeño insuficiente" 



-- Ejercicio 5


data Propiedad = Propiedad
  { añoConstruccion :: Int,
    metrosCuadrados :: Int,
    habitaciones :: Int,
    tieneGaraje :: Bool,
    zonaLocalidad :: Char
  }
  deriving (Show)


calcularPrecio :: Propiedad -> Float
calcularPrecio propiedad =
  let precioBase = fromIntegral (metrosCuadrados propiedad * 1000 + habitaciones propiedad * 5000 + if tieneGaraje propiedad then 15000 else 0) 
      antiguedad = fromIntegral (2024 - añoConstruccion propiedad) 
      coeficienteZona = if zonaLocalidad propiedad == 'B' then 1.5 else 1.0 
      precio = precioBase * (1 - antiguedad / 100) * coeficienteZona 
   in precio


buscarPropiedades :: [Propiedad] -> Float -> [Propiedad]
buscarPropiedades propiedades presupuesto =
  filter (\propiedad -> calcularPrecio propiedad <= presupuesto) propiedades 


propiedades :: [Propiedad]
propiedades = [ Propiedad { añoConstruccion = 2000, metrosCuadrados = 100, habitaciones = 3, tieneGaraje = True, zonaLocalidad = 'A' }
              , Propiedad { añoConstruccion = 2012, metrosCuadrados = 60, habitaciones = 2, tieneGaraje = True, zonaLocalidad = 'B' }
              , Propiedad { añoConstruccion = 1980, metrosCuadrados = 120, habitaciones = 4, tieneGaraje = False, zonaLocalidad = 'A' }
              , Propiedad { añoConstruccion = 2005, metrosCuadrados = 75, habitaciones = 3, tieneGaraje = True, zonaLocalidad = 'B' }
              , Propiedad { añoConstruccion = 2015, metrosCuadrados = 90, habitaciones = 2, tieneGaraje = False, zonaLocalidad = 'A' }
              ]

presupuestoPropiedades :: Float
presupuestoPropiedades = 150000.0

propiedadesEncontradas :: [Propiedad]
propiedadesEncontradas = buscarPropiedades propiedades presupuestoPropiedades
