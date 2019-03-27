import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
import Data.Char

-- just with handle
-- main = do  
--     handle <- openFile "avriil.txt" ReadMode  
--     contents <- hGetContents handle  
--     putStr contents  
--     hClose handle  

-- same but using the withFile
-- main = do
-- 	withFile "avriil.txt" ReadMode (\handle -> do
-- 		contents <- hGetContents handle
-- 		putStrLn contents
-- 		)

withFileM :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFileM path mode f = do
	handle <- openFile path mode
	result <- f handle
	hClose handle
	return result

-- same but with readFile
-- main = do
-- 	contents <- readFile "avriil.txt"
-- 	putStrLn $ unlines $ take 3 $ lines contents

-- capsify it
-- main = do
-- 	contents <- readFile "avriil.txt"
-- 	writeFile "avriil_caps.txt" $ map toUpper contents

-- create a todo list
-- main = do
-- 	todoItem <- getLine
-- 	appendFile "todo.txt" $ todoItem ++ ("\n")

-- removing from file
-- main = do
-- 	handle <- openFile "todo.txt" ReadMode
-- 	(tempPath, tempHandle) <- openTempFile "." "temp"
-- 	contents <- hGetContents handle
-- 	let numLines = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ lines contents
-- 	putStrLn "Those are your lines"
-- 	putStrLn $ unlines numLines
-- 	putStrLn "Print line number to delete"
-- 	numberString <- getLine
-- 	let number = read numberString
-- 	let newLines = delete (numLines !! number) numLines
-- 	putStrLn "Those are new lines"
-- 	putStrLn $ unlines newLines
-- 	hPutStr tempHandle $ unlines newLines
-- 	hClose handle
-- 	hClose tempHandle
-- 	renameFile tempPath "todo_remove.txt"

-- Command line arguments

main = do
	prName <- getProgName
	prArgs <- getArgs
	putStrLn "The prog's"
	putStrLn prName
	putStrLn "The args'r"
	mapM putStrLn prArgs

















	
