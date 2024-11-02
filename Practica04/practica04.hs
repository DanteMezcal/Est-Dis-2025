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
  
--6 maximo
maximo :: Ord a => Arbol a -> a 
maximo ArbolVacio = error "Un arbol vacio no puede tener maximos"
maximo (Raiz a ArbolVacio ArbolVacio) = a
maximo (Raiz a t1 t2) = max a (max (maximo t1) (maximo t2))
  
--6 minimo
minimo :: Ord a => Arbol a -> a
minimo ArbolVacio = error "Un arbol vacio no puede tener minimos"
minimo (Raiz a ArbolVacio ArbolVacio) = a
minimo (Raiz a t1 t2) = min a (min (minimo t1) (minimo t2))
