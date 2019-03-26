import System.IO

main = do  
    handle <- openFile "avriil.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  

  -- Another way of doing what we just did is to use the withFile function, which has a type signature