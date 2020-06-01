import System.IO

main = do
  withFile "file.txt" ReadMode
    (\ handle -> do
       contents <- hGetContents handle
       putStr $ modifyContents contents
    )

modifyContents :: String -> String
modifyContents =
  unlines . map (++ "aaaa") . lines
