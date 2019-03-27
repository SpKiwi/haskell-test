import Data.Char
import Control.Monad

-- main = do
-- 	emptyResFromContainer <- putStrLn "hello, im haskel! n whoo r you?"
-- 	let name = getLine
-- 	name1 <- name
-- 	putStrLn ("hello m " ++ name1 ++ ", nice seen ya")

-- COMMENT
-- cant bind last putStrLn to anything as this is the result
-- name1 can be omitted, thats just 4fun

-- main = do
-- 	putStrLn "hello, im haskel! n whoo r you?"
-- 	name <- getLine
-- 	putStrLn "niiice, so whut??"
-- 	nothings <- getLine
-- 	let caps1 = map toUpper name
-- 	let caps2 = map toUpper nothings
-- 	putStrLn ("wel done " ++ caps1 ++ ", yay " ++ caps2)

-- main = do
-- 	buffer <- getLine
-- 	if (null buffer)
-- 		then return ()
-- 		else do
-- 			putStrLn $ reverseWords buffer
-- 			main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

putStrLnM :: String -> IO ()
putStrLnM [] = return ()
putStrLnM (x:xs) = do
	putChar x
	putStrLnM xs

-- main = do
-- 	c <- getChar
-- 	when (c /= ' ') $ do
-- 		putChar c
-- 		main

-- main = do
-- 	name <- getLine
-- 	surname <- getLine
-- 	age <- getLine
-- 	print [name, surname, age]
-- same code is written below with sequence

-- main = do
-- 	sq <- sequence [getLine, getLine, getLine]
-- 	print sq

-- main = forever $ do
-- 	s <- getLine
-- 	putStrLn $ map toUpper s

-- forM

-- main = do   
--     colors <- forM [1,2,3,4] (\a -> do  
--         putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
--         color <- getLine  
--         return color)  
--     putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
--     mapM putStrLn colors  

-- main = do
-- 	colors <- forM [1,2,3,4] (\a -> do
-- 		putStrLn ("Which char do you associate this number with? " ++ show a)
-- 		color <- getLine
-- 		return color
-- 		)
-- 	putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
-- 	mapM putStrLn colors

main = do 
	colors <- forM [1,2,3,4] (\a -> do
		putStrLn ("Pick association for " ++ show a)
		l <- getLine
		return l)
	putStrLn "The strings you associated with 1, 2, 3, 4 are: "
	forM colors putStrLn















































