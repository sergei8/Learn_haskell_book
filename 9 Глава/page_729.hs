import System.IO
import Control.Exception

main = do
  bracket (openFile "file1.txt" ReadMode)
          (\h -> do
            -- putStr "file does't exist"
            hClose h
          )
          (\h -> do
            contents <- hGetContents h
            putStr contents
            hClose h
          )
