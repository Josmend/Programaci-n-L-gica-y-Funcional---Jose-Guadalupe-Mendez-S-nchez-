
funcionLista :: (a -> b) -> [a] -> [b]
funcionLista _ [] = []
funcionLista f (x:xs) = f x : funcionLista f xs

duplicar :: Int -> Int
duplicar x = x * 2

