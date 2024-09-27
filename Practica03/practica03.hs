data List a = Void | Node a (List a) deriving Show

--1
longitud :: List a -> Int
longitud Void = 0
longitud (Node x xs) = 1 + longitud xs

--2
estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void a = False
estaContenido (Node x xs) a = 
  if a == x
  then True
  else estaContenido xs a

--3
convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = (Node x(convertirAEstructura xs))

--4
convertirAlista :: List a -> [a]
convertirAlista Void = []
convertirAlista (Node x xs) = [x] ++ convertirAlista xs

--5
conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x xs) =
  if estaContenido xs x
  then conjunto xs
  else (Node x (conjunto xs))

--6
eliminarIndice :: List a -> Int -> List a
eliminarIndice (Node x xs) 0 = xs 
eliminarIndice (Node x xs) a =
  if a >= 0 && a <= (longitud (Node x xs)) - 1
  then (Node x(eliminarIndice xs (a - 1)))
  else error "El indice fuera del rango permitido"

--7
insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void _ ele = Node ele Void
insertarIndice (Node x xs) 0 ele = (Node ele (Node x xs))
insertarIndice (Node x xs) ind ele =
  if ind > 0 && ind <= (longitud (Node x xs)) - 1
  then (Node x(insertarIndice xs (ind - 1) ele))
  else error "Indice fuera del rango permitido"

--8
recorrerLista :: List a -> Int -> List a
recorrerLista Void reco = Void 
recorrerLista (Node x xs) 0 = (Node x xs)
recorrerLista (Node x xs) r = (xs (recorrerLista x (r-1)))
  --(xs (recorrerLista x (r - 1)))
  --xs x
  --(recorrerLista xs (reco - 1))
  --(Node x (recorrerLista xs (reco - 1)))
