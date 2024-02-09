atipico :: [Float] -> Float -> Bool
atipico muestra n =
  let media = sum muestra / fromIntegral (length muestra)
      desviacion = sqrt $ sum [(x - media) ^ 2 | x <- muestra] / fromIntegral (length muestra - 1)
      puntuacion = (n - media) / desviacion
  in abs puntuacion > 3

datosAtipicos :: [Float] -> [Float]
datosAtipicos muestra =
  filter (atipico muestra) muestra


