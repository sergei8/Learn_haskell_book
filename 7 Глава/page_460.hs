data Shape = Cyrcle Point Float |
             Rectangle Point Point deriving Show
-- data Shape = Cyrcle Float Float Float |
--              Rectangle Float Float Float Float deriving Show
data Point = Point Float Float deriving (Show)

area :: Shape -> Float
area (Cyrcle _ radius) = 2 * pi * radius ^ 2
area (Rectangle (Point x y) (Point x' y') ) = (x - x') * (y' - y)
-- area (Rectangle x y x' y') = (x - x') * (y' - y)
