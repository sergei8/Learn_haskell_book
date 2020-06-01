import Data.List
import Data.Char

main = do
  -- l <- getLine
  l' <- (intersperse '-' $ reverse $ map (\x -> toUpper x )) getLine
  putStrLn l'
