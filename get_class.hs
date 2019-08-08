import System.Environment
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Data.List
import Data.List.Split

import Util
import StringUtil
import Header
import ClassDefinition
import ClassData
import Method

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
		cData = classData $ drop (dataOff cDef) bytes
		methods = map (method bytes fileHeader) $ encMethods cData
	print cDef
	print cData
	print methods
