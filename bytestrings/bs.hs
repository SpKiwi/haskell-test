import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S 
import System.Environment

main = do
	(firstFile:secondFile:[]) <- getArgs
	content <- B.readFile firstFile
	B.writeFile secondFile content

