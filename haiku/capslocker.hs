import Control.Monad
import Data.Char

main = forever $ do
	putStrLn "Print something"
	c <- getLine
	putStrLn $ map toUpper c

-- getContent produces 
-- hGetContents: illegal operation (handle is closed)