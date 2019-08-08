module Method where
import Data.Word
import Data.List.Split
import StringUtil
import Util
import Header
import ClassData

data Method = Method { mClassIdx :: Int
					 , mProtoIdx :: Int
					 , mName :: String } deriving (Show)

methodSize = 8

method :: [Word8] -> Header -> EncodedMethod -> Method
method bytes fileHeader encMeth =
	let
		methodSect = sectionBytes bytes (methods fileHeader)
		stringSect = section bytes (strings fileHeader)
		methodBytes = takeRange (methodIdx encMeth * methodSize) methodSize methodSect
		nameIdx = unpackInt $ drop 4 methodBytes
		dataOff = head $ drop nameIdx stringSect
	in
		Method { mClassIdx=(unpackInt $ take 2 methodBytes)
			   , mProtoIdx=(unpackInt $ takeRange 2 2 methodBytes)
			   , mName=(readString bytes dataOff) }
