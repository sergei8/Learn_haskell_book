main = do
  contents <- getLine
  if length contents < 15
    then putStrLn contents
    else return ()

checkLen :: String -> Bool
checkLen s  | length s < 15 = True
            | otherwise     = False
