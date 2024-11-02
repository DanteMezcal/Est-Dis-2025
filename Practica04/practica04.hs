data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

--1
longitud :: Arbol a -> Int
longitud ArbolVacio = 0
longitud (Raiz a izquierda derecha) = 1 + longitud izquierda + longitud derecha

--2
profundidad :: Arbol a -> Int
profundidad ArbolVacio = 0
profundidad (Raiz a izquierda derecha) = 1 + profundidad (Raiz a max izquierda derecha)

--profundidad (Raiz 5 (Raiz 3 ArbolVacio ArbolVacio) (Raiz 6 ArbolVacio ArbolVacio))
