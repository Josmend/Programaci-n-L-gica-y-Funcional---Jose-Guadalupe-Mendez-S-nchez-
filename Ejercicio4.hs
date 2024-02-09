calificaciones :: [(String, Float)] -> [(String, String)]
calificaciones calif = [(materia, calificacion) | (materia, mark) <- calif, let calificacion | mark >= 95 = "Excelente" | mark >= 85 = "Notable" | mark >= 75 = "Bueno" | mark >= 70 = "Suficiente" | otherwise = "Desempeño insuficiente"]
