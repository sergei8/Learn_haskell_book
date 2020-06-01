data Shape = Cyrcle { center :: Point
                    , radius ::Float } |
             Rectangle {leftTop :: Point
                      , righBottom ::Point } deriving Show
data Point = Point { x :: Float
                  , y :: Float }  deriving (Show)

area :: Shape -> Float
area (Cyrcle _ radius) = 2 * pi * radius ^ 2
area (Rectangle (Point x y) (Point x' y') ) = (x - x') * (y' - y)

moveShape :: Shape -> Float -> Float -> Shape
moveShape (Cyrcle (Point x y) r) x' y'
          = Cyrcle (newPoint (x,y) (x',y')) r
moveShape (Rectangle (Point x y) (Point x1 y1) ) x' y'
          = Rectangle (newPoint (x,x') (y,y')) (newPoint (x1,x') (y1,y'))

newPoint :: (Float,Float) -> (Float,Float) -> Point
newPoint p p' = Point (fst p + fst p') (snd p + snd p')
