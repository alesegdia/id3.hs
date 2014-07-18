import Id3
import DataSet
import System.IO
import DataTrain
import DataTest
import GHC.Float

-- numero de hojas
-- numero de nodos
-- numero de hojas que clasifican -> numero de reglas buenas
-- 		* numero de hojas que no dan error
-- error: todo

info :: DataSet -> DecisionTree -> IO()
info ds dt = do
				putStr "Niveles del árbol: "
				putStrLn (show (niveles dt))
				putStr "Nodos del árbol: "
				putStrLn (show (contarNodos dt))
				putStr "Validación del arbol: "
				putStrLn (show (validarTodos ds dt))
				putStr "Precisión: "
				putStrLn (show ((int2Double (validarTodos ds dt)) / tam))
				putStr "Hojas del arbol: "
				putStrLn (show (contarHojas dt))
				putStr "Hojas válidas del arbol: "
				putStrLn (show (contarHojasValidas dt))
				putStr "Hojas erróneas del arbol: "
				putStrLn (show (contarHojasError dt))
				putStr "Hojas clase 0 del arbol: "
				putStrLn (show (contarHojasClase dt 0))
				putStr "Hojas clase 1 del arbol: "
				putStrLn (show (contarHojasClase dt 1))
					where tam = (tamds ds)



infoArbol :: DataSet -> Double -> Double -> IO ()
infoArbol ds p1 p2 =
	let arbol = id3 (partDS2 ds p1 p2)
	in do info datatest arbol

main =
	let
		arbol1 = id3 (partDS2 datatrain 0.00 0.25)
		arbol2 = id3 (partDS2 datatrain 0.25 0.50)
		arbol3 = id3 (partDS2 datatrain 0.50 0.75)
		arbol4 = id3 (partDS2 datatrain 0.75 1.00)
		in do
		putStrLn "--------------------"
		putStrLn "Primer cuarto del árbol"
		info datatest arbol1
		putStrLn "--------------------"
		putStrLn "Segundo cuarto del árbol"
		info datatest arbol2
		putStrLn "--------------------"
		putStrLn "Tercer cuarto del árbol"
		info datatest arbol3
		putStrLn "--------------------"
		putStrLn "Último cuarto del árbol"
		info datatest arbol4

		putStrLn "\n\n"
		putStrLn "--------------------"
		putStrLn "\n--------------------"
		putStrLn "GENERACIÓN DE ÁRBOL CON TRAINSET"
		putStrLn "--------------------"
		putStrLn "2% de los datos"
		infoArbol datatrain 0.98 1.00
		putStrLn "-----------------"
		putStrLn "10% de los datos"
		infoArbol datatrain 0.90 1.00
		putStrLn "-----------------"
		putStrLn "30% de los datos"
		infoArbol datatrain 0.70 1.00
		putStrLn "-----------------"
		putStrLn "40% de los datos"
		infoArbol datatrain 0.60 1.00
		putStrLn "-----------------"
		putStrLn "60% de los datos"
		infoArbol datatrain 0.40 1.00
		putStrLn "-----------------"
		putStrLn "100% de los datos"
		infoArbol datatrain 0.00 1.00
