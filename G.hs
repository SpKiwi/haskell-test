module G
(
	Point(..), -- all constructors
	Shape(Circle, Rectangle), -- specify constructors
	surface,
	nudge
) where

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

-- Type synonyms ...


























