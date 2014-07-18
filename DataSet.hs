module DataSet where

import Data.List
import Data.Function

type DataSet = ([Example] , Header, [String])
type Header = [Attr]
type Example = [AttrVal]
type Attr = Int
type AttrVal = Int
type Gain = Double
type Class = Int

-- Datos de prueba ---------------
ds11::DataSet
ds11 = (
	[[1,0,2,1,1],
	[1,0,1,2,0],
	[1,2,0,1,1],
	[0,2,1,1,1],
	[1,1,1,1,1],
	[2,2,1,1,0]],
	[0,1,2,3],
	--["a","co","n","cu","clase"],
	["cancerigeno","normal"] )

ds22::DataSet
ds22 = (
	[[1],
	[0],
	[1],
	[1],
	[1],
	[0]],
	[],
	--["a","co","n","cu","clase"],
	["cancerigeno","normal"] )

ds33::DataSet
ds33 = (
	[[],
	[],
	[],
	[],
	[],
	[]],
	[0,1,2,3],
	--["a","co","n","cu","clase"],
	["cancerigeno","normal"] )


ds44::DataSet
ds44 = (
	[[1,1],
	[1,0],
	[1,1],
	[0,1],
	[1,1],
	[2,0]],
	[1,2,3],
	--["a","co","n","cu","clase"],
	["cancerigeno","normal"] )


defclases :: DataSet -> [String]
defclases (_,_,cl) = cl

examples :: DataSet -> [Example]
examples (exs,_,_) = exs

headers :: DataSet -> Header
headers (_,hdr,_) = hdr

-- data Par = DS (Gain,Attr)
--     deriving (Ord(>))
--
-- (>) :: Par -> Par -> Bool
-- (>) _ _ = False

-- Diferentes AttrVal de un Attr en un DataSet
attrValues :: DataSet -> Attr -> [AttrVal]
attrValues dset@(y,x,_) attr =
	nub [ w !! attr | w <- y ]
	 --nub ((transpose  y) !! attr)

-- Elementos de una determinada clase
itemsFromClass :: [Example] -> Class -> [Example]
itemsFromClass [] clase = []
itemsFromClass (x:[]) clase
	| (last x) == clase = [x]
	| otherwise = []
itemsFromClass (x:xs) clase =
	((itemsFromClass [x] clase) ++ (itemsFromClass xs clase))

-- Número de elementos de una determinada clase ---------------
countItemsFromClass :: (Num a) => DataSet -> Class -> a
countItemsFromClass dataset@(exs,_,_) clase = fromIntegral $ length $ itemsFromClass exs clase

-- Distintas clases que contiene un dataset -------------------
classes :: DataSet -> [Int]
classes dataset@(exs,_,_) =
	nub [ last x | x <- exs, not (null x) ]

-- Particion de un dataset entre 0 y P*(tamds DataSet)
partDS :: DataSet -> Double -> DataSet
partDS ds@(exs,str,cls) p = (take numexs exs,str,cls)
	where numexs = truncate (p * (tamds ds))

-- Particion de un dataset entre P1*(tamds DataSet) y P2*(tamds DataSet)
partDS2 :: DataSet -> Double -> Double -> DataSet
partDS2 ds@(exs,str,cls) p1 p2 = (take (numexs2-numexs1) (drop numexs1 exs), str, cls)
	where
			numexs1 = truncate (p1 * (tamds ds)) -- (fromRational (fromIntegral (length exs))))
			numexs2 = truncate (p2 * (tamds ds)) -- (fromRational (fromIntegral (length exs))))

-- Tamaño en cantidad de ejemplos de un dataset
tamds :: DataSet -> Double
tamds ([],_,_) = 0
tamds ((x:xs),hdr,cls) = 1 + tamds (xs,hdr,cls)

-- Número de atributos en un dataset
nattrs :: DataSet -> Int
nattrs ds@(exs,hdr,_) = (length hdr)

