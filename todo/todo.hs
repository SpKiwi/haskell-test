import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
import Data.Char

-- Command line arguments

-- main :: IO [()]
-- main = do
-- 	prName <- getProgName
-- 	prArgs <- getArgs
-- 	putStrLn "The prog's"
-- 	putStrLn prName
-- 	putStrLn "The args'r"
-- 	mapM putStrLn prArgs

-- view add delete
-- view   - todo operation file
-- add    - todo operation file details
-- remove - operation file numberToDelete

main = do
	(command:args) <- getArgs
	let (Just operation) = lookup command dispatch
	operation args

dispatch :: [(String, [String] -> IO ())]
dispatch = [
	("add", add),
	("view", view),
	("remove", remove)
	]

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName ("\n" ++ todoItem)

view :: [String] -> IO ()
view [fileName] = do
	contents <- readFile fileName
	let numLines = zipWith (\n line -> show n ++ " - " ++ line) [0..] (lines contents)
	putStrLn $ unlines numLines

remove :: [String] -> IO ()
remove [fileName, n] = do
	handle <- openFile fileName ReadMode
	(tempPath, tempHandle) <- openTempFile "." "temp"
	contents <- hGetContents handle
	let allLines = lines contents
	let newLines = delete (allLines !! read n) allLines
	hPutStrLn tempHandle $ unlines newLines
	hClose handle
	hClose tempHandle
	removeFile fileName
	renameFile tempPath fileName










