main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly line =
   unlines $ filter (\line -> length line < 5) $ lines line
