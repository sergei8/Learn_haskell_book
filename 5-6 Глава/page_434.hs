import Data.Char (digitToInt)

convertDict :: [(String, String)] -> [(String,[Int])]
convertDict = map (\(k,v) -> (k, stringToInt v))

stringToInt :: String -> [Int]
stringToInt []     = []
stringToInt (s:ss) = if s >= '0' && s <= '9'
                     then digitToInt s : stringToInt ss
                     else stringToInt ss
