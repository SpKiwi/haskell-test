import System.Environment  
import System.IO  
import System.Directory
import System.IO.Error

main = toTry `catchIOError` handleError
	
toTry :: IO ()
toTry = do
	(name:_) <- getArgs
	fileExists <- doesFileExist name
	if fileExists
		then do
			content <- readFile name
			putStrLn $ show $ length $ lines content
		else do
			putStrLn "no such file"

handleError :: IOError -> IO ()
handleError e = if isDoesNotExistError e
	then 
		case ioeGetFileName e of 
			Just path -> putStrLn path
			Nothing -> putStrLn "unknown location"
	else ioError e
