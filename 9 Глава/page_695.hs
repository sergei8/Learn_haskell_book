import Control.Monad
import Data.Char

main = forever $ do
  l <- getLine
  -- putStrLn $ foldl (\l' c -> l' ++ [toUpper c]) "" l
  putStrLn $ map toUpper l
