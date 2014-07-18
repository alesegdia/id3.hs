module Id3 where

import Data.List
import Data.Function
import DataSet
import Data.Maybe
import DataTrain
import DataTest

data DecisionTree = Node Attr [DecisionBranch] | Leaf Class deriving Show
data DecisionBranch = Branch AttrVal DecisionTree deriving Show

claseError :: Class
claseError = 99

id3 :: DataSet -> DecisionTree
id3 ([],_,_)      = error "DataSet vacio!"
id3 dset@(exs,attrs,_)
	| mismaclase = (Leaf (head (classes dset)))
	| nejemplos dset == 0 = (Leaf claseError) -- no entra nunca
	| nattrs dset == 0 = (Leaf (claseMayoritaria dset)) --nattrs dset == 0
	| otherwise = (Node
					(attrs !! bestAttr)
					[Branch attrval (id3 (reduce dset bestAttr attrval)) |
						attrval <- (attrValues dset bestAttr)])
	    where
	    	bestAttr = mejorGanancia dset
	    	mismaclase = (length (classes dset)) == 1

nejemplos :: DataSet -> Int
nejemplos ds@(exs,_,_) = length ((filter (not . null) exs))

-- Clase a la que pertenecen el mayor número de ejemplos
-- con claseMayoritaria = claseError -> 58% -> 46%
-- con claseMayoritaria = como esta ahora -> 61% -> 75%
claseMayoritaria :: DataSet -> Class
claseMayoritaria ds
	| repetidos (reverse ret) = claseError
	| otherwise = snd $ last $ ret
		where ret = sort [ (countItemsFromClass ds a, a) | a <- (classes ds) ]

repetidos :: (Num a, Eq a) => [(a,a)] -> Bool
repetidos (x:[]) = False
repetidos (x:y:xs) = (snd x) == (snd y)

-- DataSet solo con ejemplos con el atributo <attr> valiendo <attrval>, con esa columna quitada
reduce :: DataSet -> Attr -> AttrVal -> DataSet
reduce dset@(exs,headers,cls) attr attrval =
	([(remove ex1 attr) | ex1 <- exs, (ex1 !! attr) == attrval], remove headers attr, cls)

-- Niveles de un DecisionTree
niveles :: DecisionTree -> Int
niveles dtree = niveles' 0 dtree
niveles' :: Int -> DecisionTree -> Int
niveles' idx (Leaf _) = idx+1
niveles' idx (Node _ branches) = last $ sort $ [ (niveles' (idx+1) (getNodo branch)) | branch <- branches]

-- Número de nodos de un DecisionTree
contarNodos :: DecisionTree -> Int
contarNodos (Leaf _) = 1
contarNodos (Node _ branches) = 1 + sum [ contarNodos (getNodo branch) | branch <- branches ]

-- Número de hojas de un DecisionTree
contarHojas :: DecisionTree -> Int
contarHojas (Leaf _) = 1
contarHojas (Node _ branches) = sum [ contarHojas (getNodo branch) | branch <- branches ]

contarHojasClase :: DecisionTree -> Class -> Int
contarHojasClase (Leaf x) y
	| x == y = 1
	| otherwise = 0
contarHojasClase (Node _ branches) clase = sum [ contarHojasClase (getNodo branch) clase | branch <- branches ]

-- Número de hojas con clasificación no definida
contarHojasError :: DecisionTree -> Int
contarHojasError dt = contarHojasClase dt claseError

-- Número de hojas con clasificación definida
contarHojasValidas :: DecisionTree -> Int
contarHojasValidas dtree = (contarHojas dtree) - (contarHojasError dtree)

-- Validar todos los individuos de un dataset con un arbol
validarTodos :: DataSet -> DecisionTree -> Int
validarTodos ds@(exs,_,_) dtree = sum [ 1 | ex <- exs, validar ex (Just dtree) ]

-- Validar un individuo con un árbol
validar :: Example -> Maybe DecisionTree -> Bool
--validar [] _ = False
validar ex Nothing = False
validar ex (Just (Leaf clase)) = claseejemplo == clase
	where claseejemplo = last ex
validar ex (Just (Node attr branches)) = validar ex hijo
	where hijo = encontrarHijo (ex !! attr) branches

encontrarHijo :: AttrVal -> [DecisionBranch] -> Maybe DecisionTree
encontrarHijo _ [] = Nothing
encontrarHijo attrval1 ((Branch attrval2 ret):xs)
	| attrval1 == attrval2       = Just ret
	| otherwise 				 = encontrarHijo attrval1 xs

-- Accessor para el DecisionTree asociado a una DecisionBranch
getNodo :: DecisionBranch -> DecisionTree
getNodo (Branch _ nodo) = nodo

-- getIndex :: (Num b, Eq a) => [a] -> b -> a
getIndex [] _ = error "error"
getIndex (x:xs) idx
	| idx == 0 = x
	| idx > 0 = getIndex xs (idx-1)
	| otherwise = error "error"

-- Atributo con mejor ganancia (menor entropía). Devuelve un índice válido para acceder.
mejorGanancia :: DataSet -> Attr
mejorGanancia ds@([],_,_) = error "dataset vaciooo!!"
mejorGanancia ds = snd (head (sort (entropiaDS ds)))

-- Entropia de los atributos de un dataset. Devuelve índices válidos para acceder.
entropiaDS :: DataSet -> [(Double,Attr)]
entropiaDS ds = [ ((entropia attr),attr) | attr <- [0..(nattrs ds)-1]]
	where entropia attr = ii ds attr

-- Entropía total de un dataset
-- i :: DataSet -> Double
-- i ds = sum [ loga (countItemsFromClass ds clase) n | clase <- (classes ds) ]
--	where n = tamds ds

-- Entropía del atributo <attr> en un dataset
ii :: DataSet -> Attr -> Double
ii ds attr = sum [((nij ds attr attrval)*(iij ds attr attrval))/(tamds ds) | attrval <- (attrValues ds attr)]

-- Entropía de informacion del valor <attrval> en el atributo <attr>
iij :: DataSet -> Attr -> AttrVal -> Double
iij ds attr attrval = - sum [ (loga (nIJC clase) nIJ) | clase <- (classes ds)]
	where
		nIJC clase = nijc ds attr attrval clase
		nIJ = nij ds attr attrval

-- Número de elementos tales que el atributo <attr> vale <attrval>
nij :: DataSet -> Attr -> AttrVal -> Double
nij ds attr attrval = sum([1 | a <- (atributos ds attr), a == attrval])

-- Numero de elementos de la clase <clase> tales que el atributo <attr> vale <attrval>
nijc :: DataSet -> Attr -> AttrVal -> Class -> Double
nijc ds@(exs,str,cls) attr attrval clase = sum([1 | a <- (atributos ((itemsFromClass exs clase),str,cls) attr), a == attrval])

-- Lista de atributos de la columna <attr>
atributos :: DataSet -> Attr -> [AttrVal]
atributos ds attr = map (\x -> (getIndex x attr)) (examples ds)

-- Ayuda para el cómputo de expresiones de la forma (a/b) * log2(a/b)
loga :: Double -> Double -> Double
loga 0 _ = 0
loga _ 0 = error "no hay denominador!"
loga num den = cociente * (logBase 2 cociente)
	where cociente = num/den

{--
 - nc   = nº ejemplos de clase c
 - n    = nº ejemplos
 - nij  = nº ejemplos con valor j en atributo i
 - nijc = nº ejemplos con valor j en atributo i de la clase c
--}



remove :: (Num b, Eq b) => [a] -> b -> [a]
remove [] _ = []
remove (x:xs) 0 = xs
remove (x:xs) n = (x:(remove xs (n-1)))
