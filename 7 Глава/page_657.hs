import Data.Char

main = do
  fn <- getLine
  ln <- getLine
  let fn' = map toUpper fn
  let ln' = map toUpper ln
  putStrLn $ fn' ++ " " ++ ln'
  -- return expression
