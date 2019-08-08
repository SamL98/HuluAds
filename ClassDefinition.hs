module ClassDefinition where
import Data.Word
import Data.List.Split
import Util
import Header
import StringUtil

data ClassDef = ClassDef { name :: String
						 , classIdx :: Int
						 , dataOff :: Int } deriving (Show)

classDef :: [Word8] -> Header -> [Int] -> ClassDef
classDef bytes fileHeader classDefUints = 
	let 
		classIdx = classDefUints !! 0
		typeIdx = (section bytes $ types fileHeader) !! classIdx
		dataOff = (section bytes $ strings fileHeader) !! typeIdx
		className = readString bytes dataOff 
	in
		ClassDef { name=className
				 , classIdx=classIdx
				 , dataOff=(classDefUints !! 6) }

getClassDefs :: [Word8] -> Header -> [ClassDef]
getClassDefs bytes fileHeader =
	let
		cDefSectInfo = classes fileHeader
		cDefSect = section bytes cDefSectInfo 
		chunkSize = (itemSize cDefSectInfo) `div` u32Size 
	in
		map (classDef bytes fileHeader) $ chunksOf chunkSize cDefSect
