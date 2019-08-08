module Util where
import Data.Word
import Data.Bits
import Data.List.Split

data Section = Section { offset :: Int
			   		   , size :: Int
					   , itemSize :: Int } deriving (Show)

u32Size = 4

unpackInt :: [Word8] -> Int
unpackInt [] = 0
unpackInt (x:xs) = let upper = unpackInt xs in
                   (.|.) (upper `shift` 8) $ fromIntegral x

takeRange :: Int -> Int -> [a] -> [a]
takeRange offset size = (take size) . (drop offset)

cvtRangeToUints :: Int -> Int -> [Word8] -> [Int]
cvtRangeToUints offset size = map unpackInt . (chunksOf u32Size) . (takeRange offset size)

section :: [Word8] -> Section -> [Int]
section bytes sect = cvtRangeToUints (offset sect) (size sect) bytes

sectionBytes :: [Word8] -> Section -> [Word8]
sectionBytes bytes sect = takeRange (offset sect) (size sect) bytes
