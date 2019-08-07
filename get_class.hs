import System.Environment
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word
import Data.List
import Data.List.Split
import Codec.Binary.UTF8.String

data Section = Section { offset :: Int
			   		   , size :: Int
					   , itemSize :: Int } deriving (Show)

data Header = Header { strings :: Section
					 , types :: Section
					 , protos :: Section
					 , fields :: Section
					 , methods :: Section
					 , classes :: Section
					 , datas :: Section } deriving(Show)

data ClassDef = ClassDef { name :: String
						 , classIdx :: Int
						 , dataOff :: Int } deriving (Show)

u32Size = 4
headerStart = 56
headerSize = 14 * 4
sectionItemSizes = [0x4, 0x4, 0xc, 0x8, 0x8, 0x20, 0x1] -- dummy 1 for data since stored as actual size in header

unpackInt :: [Word8] -> Int
unpackInt [] = 0
unpackInt (x:xs) = let upper = unpackInt xs in
                   (.|.) (upper `shift` 8) $ fromIntegral x

takeRange :: Int -> Int -> [a] -> [a]
takeRange offset size = (take size) . (drop offset)

cvtRangeToUints :: Int -> Int -> [Word8] -> [Int]
cvtRangeToUints offset size = map unpackInt . (chunksOf u32Size) . (takeRange offset size)

header :: [Word8] -> Header
header bytes =
	let 
		headerUintPairs = chunksOf 2 $ cvtRangeToUints headerStart headerSize bytes
		pairsAndSize = zip headerUintPairs sectionItemSizes
		sections = map (\(x,y) -> Section { offset=(x !! 1)
										  , size=(x !! 0)*y
										  , itemSize=y }) pairsAndSize
	in
		Header { strings=(sections !! 0)
			   , types=(sections !! 1)
			   , protos=(sections !! 2)
			   , fields=(sections !! 3)
			   , methods=(sections !! 4)
			   , classes=(sections !! 5)
			   , datas=(sections !! 6) }


className :: [String] -> ClassDef -> String
className strs cDef = strs !! (classIdx cDef)


readUleb128 :: [Word8] -> (Int, Int)
readUleb128 [] = (0, 0)
readUleb128 (firstByte:rest)
	| firstByte .&. 0x80 == 0 = (1, lsbs)
	| otherwise = 
		let (nBytes, value) = readUleb128 rest in
		(1 + nBytes, (value `shift` 7) .|. lsbs)
	where lsbs = fromIntegral $ firstByte .&. 0x7f


readString :: [Word8] -> Int -> String
readString bytes off =
	let 
		strStart = drop off bytes
		(nBytes, strLen) = readUleb128 strStart in
	decode $ takeRange nBytes strLen strStart


section :: [Word8] -> Section -> [Int]
section bytes sect = cvtRangeToUints (offset sect) (size sect) bytes


classDef :: [Word8] -> Header -> [Int] -> ClassDef
classDef bytes fileHeader classDefUints = 
	let 
		typeIdx = (section bytes $ types fileHeader) !! (classDefUints !! 0)
		dataOff = (section bytes $ strings fileHeader) !! typeIdx
		className = readString bytes dataOff in
	ClassDef { name=className
			 , classIdx=(classDefUints !! 0)
			 , dataOff=(classDefUints !! 6) }


getClassDefs :: [Word8] -> Header -> [ClassDef]
getClassDefs bytes fileHeader =
	let
		cDefSectInfo = classes fileHeader
		cDefSect = section bytes cDefSectInfo 
		chunkSize = (itemSize cDefSectInfo) `div` u32Size in
	map (classDef bytes fileHeader) $ chunksOf chunkSize cDefSect


includeClassName :: String -> ClassDef -> Bool
includeClassName target cdef =
	let search = name cdef in
	isInfixOf target search && (not $ '$' `elem` search)


main = do
	args <- getArgs
	bs <- BS.readFile "client/classes.dex"
	let 
		bytes = BS.unpack bs
		fileHeader = header bytes
		cDefs = getClassDefs bytes fileHeader
		cDef = head $ filter (includeClassName (args !! 0)) cDefs
	print cDef
