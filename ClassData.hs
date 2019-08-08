module ClassData where
import Data.Word
import Data.List.Split
import ClassDefinition
import StringUtil

data EncodedMethod = EncodedMethod { methodIdx :: Int
								   , codeOff :: Int } deriving (Show)

data ClassData = ClassData { encMethods :: [EncodedMethod] } deriving (Show)

dummyMeth = EncodedMethod { methodIdx=0, codeOff=0 }

encMeth :: Int -> [Int] -> (Int, EncodedMethod)
encMeth prevMethIdx ulebs = 
	let 
		meth = EncodedMethod { methodIdx=(ulebs !! 0 + prevMethIdx)
							 , codeOff=(ulebs !! 2) }
	in
		(methodIdx meth, meth)

accumMethod :: [(Int, EncodedMethod)] -> [Int] -> [(Int, EncodedMethod)]
accumMethod prev ulebs =
	let prevIdx = methodIdx $ snd $ head prev in
	(encMeth prevIdx ulebs) : prev

classData :: [Word8] -> ClassData
classData bytes =
	let
		(nbsf, nStatFields) = readUleb128 bytes
		(nbif, nInstFields) = readUleb128 $ drop nbsf bytes
		(nbdm, nDirecMethods) = readUleb128 $ drop (nbsf+nbif) bytes
		(nbvm, _) = readUleb128 $ drop (nbsf+nbif+nbdm) bytes
		arrStart = drop (nbsf+nbif+nbdm+nbvm) bytes
		(nbf, _) = readUlebs ((nStatFields + nInstFields) * 2) arrStart
		(_, encMethUlebs) = readUlebs (nDirecMethods * 3) $ drop nbf arrStart
		encMeths = map snd $ init $ foldl accumMethod [(0, dummyMeth)] $ chunksOf 3 encMethUlebs
	in
		ClassData { encMethods=encMeths }
