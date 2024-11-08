data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

--1
longitud :: Arbol a -> Int
longitud ArbolVacio = 0
longitud (Raiz a t1 t2) = 1 + longitud t1 + longitud t2

--2
profundidad :: Arbol a -> Int
profundidad ArbolVacio = 0
profundidad (Raiz a t1 t2) = 1 + max (profundidad t1) (profundidad t2)

--3
ancho :: Arbol a -> Int 
ancho ArbolVacio = 0
ancho (Raiz a ArbolVacio ArbolVacio) = 1
ancho (Raiz a t1 t2) = ancho t1 + ancho t2

--4
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []

--InOrder
recorrido (Raiz a t1 t2) InOrder = recorrido t1 InOrder ++ [a] ++ recorrido t2 InOrder

--PreOrder
recorrido (Raiz a t1 t2) PreOrder = [a] ++ recorrido t1 PreOrder ++ recorrido t2 PreOrder

--PostOrder
recorrido (Raiz a t1 t2) PosOrder = recorrido t1 PosOrder ++ recorrido t2 PosOrder ++ [a]

--5
chuleta :: [[a]] -> [[a]] -> [[a]]
chuleta [] [] = []
chuleta xs [] = xs
chuleta [] ys = ys
chuleta (x:xs) (y:ys) = (x++y) : chuleta xs ys

niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz a t1 t2) = [a] : chuleta (niveles t1)  (niveles t2)
  
--6 maximo
-- los _ funcionan para idnetificar al subarbol izquierdo, pobrecito, lo ignoramos
maximo :: (Ord a) => Arbol a -> a
maximo ArbolVacio = error "El árbol está vacío"
maximo (Raiz valor _ ArbolVacio) = valor
maximo (Raiz _ _ derecho) = maximo derecho

--6 minimo
-- Por lo que investigué los máximos siempre estarán en los subárboles derechos, por lo que si se respeta eso si jala
-- ahora le toca ser ignorado al subárbol derecho y usamos _
minimo :: (Ord a) => Arbol a -> a
minimo ArbolVacio = error "El árbol está vacío"
minimo (Raiz valor ArbolVacio _) = valor
minimo (Raiz _ izquierdo _) = minimo izquierdo

--7
eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio _ = ArbolVacio
eliminar (Raiz a ArbolVacio t2) _ = t2
eliminar (Raiz a t1 ArbolVacio) _ = t1
eliminar (Raiz a t1 t2) eli =
  if eli == a
  then (Raiz (minimo t2) t1 (eliminar t2 (minimo t2)))
  else if eli < a
          then Raiz a (eliminar t1 eli) t2 
          else Raiz a t1 (eliminar t2 eli)
  
