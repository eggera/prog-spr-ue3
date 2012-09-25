

-- The main entry point to the students registration program


module Main where


import System.IO
import DB
import Utilities

-- this tip was posted on http://stackoverflow.com/questions/296792/haskell-io-and-closing-files
import Control.DeepSeq (rnf)
-- rnf means "reduce to normal form"

import qualified Data.ByteString as Str




main = do 
	file 		<- openFile dbName ReadMode
	contents 	<- hGetContents file
	rnf contents `seq` hClose file -- force the whole file to be read, then close

	if null contents then do
				putStrLn "Create new Entry"
				db <- return (DB.new)  -- write a new entry to fill db with valid data
				db <- return (DB.addCourse db "The secrets of Astrophysics")
				writeFile dbName (show db)
	else do
				putStrLn "Create new Entry"
				db <- return (read contents)
				db <- return (DB.addCourse db "The secrets of Astrophysics")
				writeFile dbName (show db)
	




