module StringUtil where
import Data.Bits
import Data.Word
import Codec.Binary.UTF8.String
import Util

readUleb128 :: [Word8] -> (Int, Int)
readUleb128 [] = (0, 0)
readUleb128 (firstByte:rest)
	| firstByte .&. 0x80 == 0 = (1, lsbs)
	| otherwise = 
		let (nBytes, value) = readUleb128 rest in
		(1 + nBytes, (value `shift` 7) .|. lsbs)
	where lsbs = fromIntegral $ firstByte .&. 0x7f

readUlebs :: Int -> [Word8] -> (Int, [Int])
readUlebs n bytes
	| n == 0 = (0, [])
	| otherwise = 
		let 
			(nBytes, value) = readUleb128(bytes) 
			(nRest, restUlebs) = readUlebs (n-1) $ drop nBytes bytes in
		(nBytes + nRest, value : restUlebs)

readString :: [Word8] -> Int -> String
readString bytes off =
	let 
		strStart = drop off bytes
		(nBytes, strLen) = readUleb128 strStart in
	decode $ takeRange nBytes strLen strStart
