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
