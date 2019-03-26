import Control.Monad
import Data.Char

-- working sample
-- main = forever $ do
-- 	contents <- getLine
-- 	putStrLn $ printShorts contents

printShorts :: String -> String
printShorts io = if (length io > 20)
	then io
	else "pathetic"

-- main = do  
--     contents <- getContents  
--     putStr (shortLinesOnly contents)  
  
-- shortLinesOnly :: String -> String  
-- shortLinesOnly input =   
--     let allLines = lines input  
--         shortLines = filter (\line -> length line < 10) allLines  
--         result = unlines shortLines  
--     in  result  

-- main = interact $ unlines . filter ((<10) . length) . lines

main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\x -> if (isPalindrome x) then "palindrome" else "not palindrome") . lines
	where isPalindrome xs = reverse xs == xs





















