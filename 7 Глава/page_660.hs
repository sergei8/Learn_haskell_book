import Data.Char

main = do
  str <- getLine
  -- let str' = rev str
  putStrLn $ rev str

rev :: String -> String
rev [] = []
rev (x:xs) = rev xs ++ [x]
