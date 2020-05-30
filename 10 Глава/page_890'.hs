-- data Node = Node Road Road | Node Road
-- data Road = Road Int Node

data Section = Section {getA :: Int, getB :: Int, getC :: Int}
                  deriving (Show)
type RoadSystem = [Section]
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathToA, pathToB) (Section a b c) =
  (newPathToA, newPathToB) where
    timeToA        = sum $ map snd pathToA
    timeToB        = sum $ map snd pathToB
    forwardTimeToA = timeToA + a
    forwardTimeToB = timeToB + b
    crossTimeToA   = timeToB + b + c
    crossTimeToB   = timeToA + a + c
    newPathToA     =
      if forwardTimeToA <= crossTimeToA
        then (A, a) : pathToA
        else (C, c) : (B, b) : pathToA
    newPathToB     =
      if forwardTimeToB <= crossTimeToB
        then (B, b) : pathToB
        else (C, c) : (A, a) : pathToB

optimalPath :: RoadSystem -> Path
optimalPath getPath =
  if (sum [ snd a | a <- bestAPath]) <= (sum [ snd b | b <-  bestBPath])
    then reverse bestAPath
    else reverse bestBPath
     where
      (bestAPath, bestBPath) = foldl roadStep ([], []) getPath

getPath :: RoadSystem
getPath =
  [
    Section 50 10 30
  , Section 5  90 20
  , Section 49 2  25
  , Section 10 8  0
  ]
