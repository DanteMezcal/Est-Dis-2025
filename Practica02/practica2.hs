longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista(xs)

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento [] z b = [z]
agregaElemento (x:xs) z b = if b
  then z:(x:xs)
  else (x:xs) ++ [z]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = 0
maximoLista (x:xs) =
  if x > maximoLista(xs)
  then x
  else maximoLista(xs)
  
indice :: [a] -> Int -> a
indice [] z = error "El indice esta fuera del rango o la lista esta vacia"
indice (x:xs) 0 = x
indice (x:xs) z =
  if z <= longitud(x:xs)
  then indice(xs) (z - 1)
  else error "El indice esta fuera del rango de la lista"
  
--indice :: [a] -> Int -> a
--indice (x:xs) 1 = x
--indice (x:xs) i = indice xs (i - 1)


divisores :: Int -> [Int]
divisores 0 = []
divisores n = [x | x <- [1..n], n `mod` x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [z | z <- (xs), x /= z]

numerosPares :: [Int] -> [Int]
numerosPares (x:xs) = [z | z <- (x:xs), z `mod` 2 == 0 ]
