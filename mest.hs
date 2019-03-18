import Data.List
import Data.Char
import Data.Function (on)

numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub

-- Data.List

-- all (>11) .  map (+2) $ [1..10]
-- any (>11) .  map (+2) $ [1..10]

-- take 10 . iterate (+1) $ 1

splitMiddle :: [a] -> ([a], [a])
splitMiddle xs = splitAt (round ((toRational (length xs)) / 2)) xs

-- takeWhile (<10000) . map (^3) $ [1..]

stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]  

-- head $ dropWhile (\(a,_,_,_) -> a < 1000) $ stock
-- span (==10) [1..10]

-- With that, we actually just made a function that behaves like isInfixOf. isInfixOf searches for a sublist within a list and returns True if the sublist we're looking for is somewhere inside the target list.

groupt = groupBy (\x y -> (x > 0) == (y > 0)) [-10..10]
groupt1 = groupBy ((==) `on` (>0)) [-10..10]
xst = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]  

sortt =   sortBy (\x y -> compare (length x) (length y)) xst
sortton = sortBy (compare `on` length) xst

-- wordst xs = filter (\x -> not (any isSpace x)) (groupBy ((==) `on` isSpace) xs)
wordst xs = filter (not . any isSpace) (groupBy ((==) `on` isSpace) xs)


-- map generalCategory '1'

encode :: Int -> String -> String
encode _ [] = []
encode shift (x:xs) = chr ((ord x) + shift):encode shift xs

encode1 :: Int -> String -> String
encode1 shift msg = map chr (map (\ch -> (ord ch) + shift) msg)

encode2 :: Int -> String -> String
encode2 shift = map chr . map (\ch -> (ord ch) + shift)

decode :: Int -> String -> String
decode shift = encode (negate shift)

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]  

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key = snd . head . filter (\(k, v) -> k == key)

findKeySafe :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeySafe _ [] = Nothing
findKeySafe key ((k, v):xs) = if (k == key)
	then Just v
	else findKeySafe key xs

-- This is a textbook recursive function that operates on a list





