doubleMe :: Num x => x -> x
doubleMe x = x + x

justSum :: Num x => x -> x -> x
justSum x y = x + y

doubleUs :: Num x => x -> x -> x
doubleUs x y = doubleMe x + doubleMe y

smallMultiply :: (Ord n, Num n) => n -> n
smallMultiply number = if number <= 100 
	then doubleMe number 
	else number

squareEven :: (Integral n, Ord n) => n -> n
squareEven x = if x `rem` 2 == 0
	then x*x
	else -1

boomBang :: Integral a => [a] -> [String]
boomBang list = [if (x > 10)
	then "Boom"
	else "Bang"
	|
	x <- list,
	odd x
	]

mulLists :: Integral a => [a] -> [a] -> [a]
mulLists first second = [x*y | x <- first, y <- second, odd (x*y)]

nouns :: [String]
nouns = ["hobo","frog","pope"]  

adjectives :: [String]
adjectives = ["lazy","grouchy","scheming"] 

haveFun :: [String]
haveFun = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

myLength :: Num a => String -> a
myLength word = sum [1 | _ <- word]

keepUppercase :: String -> String
keepUppercase word = [symbol | symbol <- word, symbol `elem` ['A'..'Z']]

listOfLists :: [[Int]]
listOfLists = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

squareOdds :: (Integral a) => [a] -> [a]
squareOdds numbers = [number ^ 2 | number <- numbers, odd number]

removeOdd :: [[Int]]
removeOdd = [[element | element <- list, odd element] | list <- listOfLists]

triangles :: [(Integer, Integer, Integer)]
triangles = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2]

removeLowerThanTen :: [Int] -> [Int]
removeLowerThanTen numbers = [number | number <- numbers, number >= 10]


lucky :: (Num a, Eq a) => a -> String
lucky 7 = "You are lucky"
lucky number = "Not so lucky"

factorial :: Integer -> Integer
factorial 0 = 1
factorial number = number * factorial (number - 1)

vectorSum :: (Int, Int) -> (Int, Int) -> (Int, Int)
vectorSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

myHead :: [a] -> a
myHead [] = error "Empty array lol"
myHead (x:_) = x

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

firstTwo :: (Show a) => [a] -> String
firstTwo [] = "Empty array lol"
firstTwo (x:[]) = "Only one element " ++ show x
firstTwo (x:y:[]) = "Only two elements " ++ show x ++ " and " ++ show y
firstTwo (x:y:_) = "First are " ++ show x ++ " and " ++ show y

anotherLength :: [a] -> Int
anotherLength [] = 0
anotherLength (_:xs) = 1 + anotherLength xs

bmi :: (Ord a, Fractional a) => a -> a -> String
bmi height weight
	| bmi <= skinny = "Skinny"
	| bmi <= ok = "OK"
	| otherwise = "Fat"
	where 
		bmi = weight / height ^ 2
		skinny = 18.5
		ok = 25.0

myMax :: (Ord a) => a -> a -> a
myMax a b
	| a > b = a
	| otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
	| a > b = GT
	| a == b = EQ
	| otherwise = LT

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
	where
		(f:_) = firstName
		(l:_) = lastName

myTail :: [a] -> [a]
myTail [] = error "Empty array lol"
myTail (x:xs) = xs

sameWord :: String -> String
sameWord [] = ""
sameWord (x:xs) = [x] ++ (sameWord xs)

newBmi :: (Fractional a) => [(a, a)] -> [a]
newBmi bmis = [bmi weight height | (weight, height) <- bmis]
	where bmi weight height = weight / height ^ 2

rectangleArea :: (Num a) => a -> a -> a
rectangleArea w h =
	let 
		width = w
		height = h
	in
		width * height

anotherBmi :: (Fractional a, Ord a) => [(a, a)] -> [a]
anotherBmi bmis = [bmi | (weight, height) <- bmis, let bmi = (weight / height ^ 2), bmi >= 25.0]


mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

caseSum :: (Num a) => [a] -> a
caseSum (x:xs) = case x:xs of 
	[] -> 0
	x:[] -> x
	x:xs -> x + caseSum xs

betterCaseSum :: (Num a) => [a] -> a
betterCaseSum list = case list of 
	[] -> 0
	x:xs -> x + caseSum xs

caseHead :: [a] -> a
caseHead list = case list of
	[] -> error "Empty list, no heads here!"
	(x:_) -> x

maxRecursion :: (Num a, Ord a) => [a] -> a
maxRecursion [] = error "empty list lol"
maxRecursion [x] = x
maxRecursion (x:xs) = max x (maxRecursion xs)

-- todo comment to myself why cant Int be used instead of Integral
myReplicate :: (Integral a, Ord a) => a -> b -> [b]
myReplicate n x
	| n <= 0 = []
	| otherwise = x:myReplicate (n - 1) x

myTake :: (Integral a, Ord a) => a -> [b] -> [b]
myTake n _
	| n <= 0 = []
myTake _ [] = []
myTake n (x:xs) = x:myTake (n - 1) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myRepeat :: a -> [a]
myRepeat x = x:myRepeat x

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x, y):myZip xs ys

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = if x == y
	then True
	else myElem x ys

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
	let
		smallerSorted = quickSort (filter (<=x) xs)
		biggerSorted = quickSort (filter (>x) xs)
	in 
		smallerSorted ++ [x] ++ biggerSorted

-- note haskell functions work like this
-- so it can be called like: ((multThree 2) 3) 2
multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x * y * z  

multTwoWithNine = multThree 9 

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- note infix function
isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A'..'Z'])

sumOfThree a b c = a + b + c
sumOfTwoAndTwoOther = sumOfThree 2

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyNTimes :: (Integral a) => a -> (b -> b) -> b -> b
applyNTimes n f x
	| n == 1 = f x
	| otherwise = f (applyNTimes (n - 1) f x)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith zipper (x:xs) (y:ys) = zipper x y:myZipWith zipper xs ys

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f x y = f y x

printFirst :: (Show a) => a -> a -> String
printFirst a b = show a

-- show (flip (printFirst "lol" "pidr"))
-- show (printFirst "lol" "pidr")
-- zipWith (flip div) [2,2..] [10,8,6]

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x:myMap f xs

trueMap :: [a] -> [Int]
trueMap = map meinCampf
	where meinCampf a = 1488

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
	| f x = x:myFilter f xs
	| otherwise = myFilter f xs

-- with list comprehension
-- largestDivider :: (Integral a) => [a] -> a -> a
-- largestDivider numbers divider = listMax [number | number <- numbers, isDivisible number divider]

-- with filter
largestDivider :: (Integral a) => [a] -> a -> a
largestDivider numbers divider = listMax (filter (`isDivisible` divider) numbers)

isDivisible :: (Integral a) => a -> a -> Bool
isDivisible number divider = number `rem` divider == 0
		
listMax :: (Ord a) => [a] -> a		
listMax [] = error "empty list"
listMax [y] = y
listMax (y:ys) = max y (listMax ys)

-- sum (takeWhile (<10000) [x^2 | x <- [1..], odd (x^2)])
-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- For our next problem, we'll be dealing with Collatz sequences

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
	| odd x = x:chain (x*3 + 1)
	| otherwise = x:chain (x `div` 2)

-- length (filter (>15) (map length (map chain [1..100])))
-- length (filter (>15) (map chain [1..100]))  
-- ((map (*) [3,4]) !! 0) 4
