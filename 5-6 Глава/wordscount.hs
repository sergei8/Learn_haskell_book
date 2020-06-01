import Data.List (words, nub, sort)

type WordsCount = [(String, Int)]

wordsCount :: String -> Maybe WordsCount
wordsCount [] = Nothing
wordsCount str = Just [(w, countWord w wordsList) | w <- uniqWordsList str] where
  uniqWordsList xs = nub wordsList
  wordsList = words str

countWord :: String -> [String]-> Int
countWord w ws = length $ filter (== w) ws
