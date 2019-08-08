module Header where
import Util
import Data.Word
import Data.List.Split

data Header = Header { strings :: Section
					 , types :: Section
					 , protos :: Section
					 , fields :: Section
					 , methods :: Section
					 , classes :: Section
					 , datas :: Section } deriving(Show)

headerStart = 56
headerSize = 14 * 4
sectionItemSizes = [0x4, 0x4, 0xc, 0x8, 0x8, 0x20, 0x1] -- dummy 1 for data since stored as actual size in header

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
