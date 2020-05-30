-- data Node = Node Road Road | Node Road
-- data Road = Road Int Node
import Data.List.Split (chunksOf)

-- data Section = Section {getA :: Int, getB :: Int, getC :: Int}
data Section = Section {a :: Int, b :: Int, c :: Int}
                  deriving (Show)
type RoadSystem = [Section]
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

main = do
  path <- getPath
  let oPath = optimalPath path
  putStrLn $ show oPath

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

optimalPath :: RoadSystem -> (Path, Int)
optimalPath getPath =
  if sumA <= sumB
    then (reverse bestAPath, sumA)
    else (reverse bestBPath, sumB)
     where
      (bestAPath, bestBPath) = foldl roadStep ([], []) getPath
      sumA = sum [ snd a | a <- bestAPath]
      sumB = sum [ snd b | b <- bestBPath]

getPath :: IO RoadSystem
getPath = do
  contents <- getContents
  let contentsList = lines contents
  let path = foldl (\p (a:b:c:[]) -> Section (toInt a) (toInt b) (toInt c) : p)
             [] (chunksOf 3 contentsList)
  -- let path =
  --       [
  --         Section 50 10 30
  --       , Section 5  90 20
  --       , Section 49 2  25
  --       , Section 10 8  0
  --       ]
  return path

toInt :: String -> Int
toInt x = read x :: Int
