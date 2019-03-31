
-- resolveRpn :: (Num a) => String -> a
-- resolveRpn = calcRpn . reverse . filter (/=' ')

-- calcRpn :: (Num a) => String -> a
-- calcRpn (x:xs) = foldl 
-- 	| x == '+' = 4
-- 	| x == '-' = 4
-- 	| x == '*' = 4
-- 	| x == '/' = 4

rpn :: (Num a, Read a) => String -> a
rpn = head . foldl foldingFunction [] . words
	where
		foldingFunction (x:y:xs) "+" = (x + y):xs
		foldingFunction (x:y:xs) "-" = (x - y):xs
		foldingFunction (x:y:xs) "*" = (x * y):xs
		foldingFunction xs numberString = (read numberString):xs

-- Heathrow to London