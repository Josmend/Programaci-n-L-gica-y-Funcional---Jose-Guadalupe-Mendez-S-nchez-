sumarLista :: [Int] -> Int
sumarLista [] = 0
sumarLista (x:xs) = x + sumarLista xs

factorial :: Int -> Int
factorial 0 = 1 
factorial n = n * factorial (n - 1) 

numerosPares :: Int -> [Int]
numerosPares n = [x | x <- [0..n], even x]

longitudCadena :: String -> Int
longitudCadena = length

reversoLista :: [a] -> [a]
reversoLista [] = [] 
reversoLista (x:xs) = reversoLista xs ++ [x]

duplicarElementos :: [Int] -> [Int]
duplicarElementos = map (*2)

filtrarPares :: [Int] -> [Int]
filtrarPares [] = []                   
filtrarPares (x:xs) 
  | even x     = x : filtrarPares xs   
  | otherwise  = filtrarPares xs  

fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

esPalindromo :: String -> Bool
esPalindromo s = s == reverse s