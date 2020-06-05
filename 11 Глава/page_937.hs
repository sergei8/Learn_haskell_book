import Data.List
import Data.Char

main = do
  l <- getLine
  let l' = intersperse '-' $ reverse $ map (\x -> toUpper x ) l
  putStrLn l'
