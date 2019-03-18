import Data.List
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









