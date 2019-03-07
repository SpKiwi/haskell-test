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










