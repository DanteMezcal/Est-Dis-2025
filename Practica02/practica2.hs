longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumaLista :: Num x => [x] -> x
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

agregarElemento :: [a] -> a -> Bool -> [a]
agregarElemento [] elemento boolean = [elemento]
agregarElemento (x:xs) elemento boolean =
    if boolean
    then elemento : (x:xs)    
    else (x:xs) ++ [elemento] 

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = 0
maximoLista (x:xs) =
  if x > maximoLista xs
  then x
  else maximoLista xs

indice :: [a] -> Int -> a
indice (x:xs) 1 = x
indice (x:xs) i = indice xs (i - 1)

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [z | z <- xs, z /= x]

--numerosPares :: Num a => [a] -> [a]
--numerosPares [] = []
--numerosPares (x:xs) = [x | x <- [a], x]

--prueba :: Int -> Int -> Int
--prueba x y = x `div` y 
