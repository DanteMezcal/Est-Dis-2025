--1
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)

--2
sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista(xs)

--3
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento lista elemto condicion = 
  if condicion 
    then elemto : lista 
    else lista ++ [elemto]

--4
maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "No se puede calcular el máximo de una lista vacía, pues no tiene elementos que calcular, ¡teoría de conjustos mijoo!"
maximoLista [x] = x 
maximoLista (x:xs) =
  if x > maximoLista xs
  then x
  else maximoLista xs

--5
indice :: [a] -> Int -> a
indice [] z = error "El indice está fuera del rango o la lista está vacía"
indice (x:xs) z =
  if z < 0 then error "El indice es menor que 0"
  else if z >= longitud (x:xs) then error "El indice está fuera del rango de la lista"
  else if z == 0 then x
  else indice xs (z - 1)

--6
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

--7
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [z | z <- (xs), x /= z]

--8
numerosPares :: [Int] -> [Int]
numerosPares (x:xs) = [z | z <- (x:xs), z `mod` 2 == 0 ]
