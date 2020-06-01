import System.IO

-- main = do
  -- handle <- openFile "file.txt" ReadMode
  -- contents <- hGetContents handle
  -- putStr contents
  -- hClose handle

main =
  openFile "file.txt" ReadMode >>= hGetContents >>= putStr . (++ "aaa\n")
