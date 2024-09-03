longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

agregarElemento :: [a] -> a -> Bool -> [a]
agregarElemento (x:xs) a trueFalse =
    if trueFalse
    then a:(x:xs)
    else (x:xs) ++ [a]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = 0
maximoLista (x:xs) =
  if x > maximoLista xs
  then x
  else maximoLista xs

indice :: [a] -> Int -> a
indice [] = 0
indice (x:xs) = 
