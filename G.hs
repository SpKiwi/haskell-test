module G
(
	Point(..), -- all constructors
	Shape(Circle, Rectangle), -- specify constructors
	surface,
	nudge
) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (find)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1) 

nudge :: Shape -> Point -> Shape
nudge (Circle _ r) point = Circle point r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) (Point x3 y3) = 
	Rectangle (Point (x1 + x3) (y1 + y3)) (Point (x2 + x3) (y2 + y3))

-- nudge (Rectangle (Point 1 1) (Point 2 2)) $ Point 1 1
-- Record syntax ...

data Test = Test {
	t :: String,
	t1 :: String
} deriving (Show)

data Person = Person { 
	name :: String,
	age :: Int,
	height :: Float,
	phone :: String,
	flavor :: String
} deriving (Show)

karen = Person "Kharen" 23 174.0 "+375292900000" "Vanilla"

data Car = Car {
	company :: String,
	model :: String,
	year :: Int
} deriving (Show)

tellCar (Car a b c) = "This " ++ a ++ " " ++ b ++ " was made in " ++ show c

-- todo note!!!! SUPER IMPORTANT
-- type constructor = value constructor1 | value constructor2 (or not!)
data Vector a = Vector a a a deriving (Show)

vPlus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a b c) `vPlus` (Vector i j k) = Vector (a+i) (b+j) (c+k)

vMult :: (Num a) => Vector a -> a -> Vector a
(Vector a b c) `vMult` m = Vector (a*m) (b*m) (c*m)

vScalar :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a b c) `vScalar` (Vector i j k) = Vector (a*i) (b*j) (c*k)

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
	deriving (Show, Eq, Ord, Read, Bounded, Enum)

type Ints = [Int]
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

type AssocList k v = [(k, v)]
type IntMap = Map.Map Int

isInPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
isInPhoneBook name pNumber pBook = (name, pNumber) `elem` pBook

data Failure = Failure {
	reason :: String,
	code :: Int
} deriving (Show)

apiCall :: String -> Either Failure String
apiCall "ok" = Right "allOk"
apiCall x = Left $ Failure "something wrong" 1488

data LockerState = Taken | Free deriving (Show, Eq)
type ErrorMessage = String
type Code = String
type Lockers = Map.Map Int (LockerState, Code)

-- locker can be taken, locker can be not existing

lockers :: Lockers
lockers = Map.fromList[
    	(100,(Taken,"ZD39I")),
    	(101,(Free,"JAH3I")),
    	(103,(Free,"IQSA9")),
    	(105,(Free,"QOTSA")),
    	(109,(Taken,"893JJ")),
    	(110,(Taken,"99292"))
    	]

lockerLookup :: Int -> Lockers -> Either ErrorMessage Code
lockerLookup x xs = case Map.lookup x xs of
	Nothing -> Left "no such locker, succ"
	Just (state, code) -> if (state == Taken)
			then Left "thas takennn!"
			else Right code

-- todo uncomment
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- data ListA a = EmptyA | ConsA { -- same as the top one
-- 	headA :: a,
-- 	tailA :: List a
-- } deriving (Show, Read, Eq, Ord)

infixr 5 :-:
data List a = Empty | a :-: List a deriving (Show, Read, Eq, Ord)

infixr 5 $$ 
($$) :: List a -> List a -> List a
Empty $$ ys = ys
(x :-: xs) $$ ys = x :-: (xs $$ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
instance Functor Tree where
	fmap _ EmptyTree = EmptyTree
	fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

class Functorm f where
	fmapm :: (a -> b) -> f a -> f b

instance Functorm (Either a) where  
    fmapm f (Right x) = Right (f x)  
    fmapm f (Left y) = Left y  

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x EmptyTree = singleton x
insertTree x (Node y left right)
	| x == y = Node y left right
	| x < y = Node y (insertTree x left) right
	| x > y = Node y left (insertTree x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node y left right)
	| x == y = True
	| x < y = treeElem x left 
	| x > y = treeElem x right

-- foldl treeInsert EmptyTree [1,2,3,4,5,6,8]
numsTree = foldr insertTree EmptyTree [1,2,3,4,5,6,8]

class Eqm a where
	($==) :: a -> a -> Bool
	($/=) :: a -> a -> Bool
	x $== y = not (x $/= y)
	x $/= y = not (x $== y)

data Light = Red | Yellow | Green

instance Eqm Light where
	Red $== Red = True
	Yellow $== Yellow = True 
	Green $== Green = True 
	_ $== _ = False

instance Show Light where
	show Red = "Red Light"
	show Yellow = "Yellow Light"
	show Green = "Green Light"

instance (Eqm a) => Eqm (Maybe a) where  
    Just x $== Just y = x $== y  
    Nothing $== Nothing = True  
    _ $== _ = False  

data Human = Man | Woman deriving (Eq)

class Showm a where
	showm :: a -> String

instance Showm Human where
	showm Man = show "maaan"
	showm Woman = show "woman"

instance (Showm a) => Showm (Maybe a) where
	showm Nothing = show "noooon"
	showm (Just x) = showm x

class YesNo a where
	yesno :: a -> Bool

instance YesNo Int where
	yesno 0 = False
	yesno _ = True

instance YesNo [a] where
	yesno [] = False
	yesno _ = True

instance YesNo Bool where
	yesno = id

instance YesNo (Maybe a) where
	yesno Nothing = False
	yesno _ = True

yesNoIf :: (YesNo a) => a -> b -> b -> b
yesNoIf yesNoVal yesRes noRes = if (yesno yesNoVal) then yesRes else noRes

class Tofu t where
	tofu :: j a -> t a j

data Frank el container = Frank { 
	frankField :: container el 
} deriving (Show)















