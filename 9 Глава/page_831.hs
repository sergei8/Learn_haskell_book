import Control.Exception
import System.Environment

main = do

  args <- getArgs
  -- проверка строки аргументов
  params <- try ( checkArgs args ) :: IO (Either SomeException (Float, Float))
  case params of
    Left _       -> putStrLn $ "Error in params " ++ show args
    Right (x, y) -> do
      -- получить и вывести результат
      res <- try (return ( x / y )) :: IO (Either SomeException Float)
      case res of
        Left e -> print e
        Right r | show r == "Infinity" -> print "Devide by 0"
                | otherwise            -> print r

-- / конвертировать строку аргументов в числа
checkArgs :: [String] -> IO (Float, Float)
checkArgs (a : b : []) =
  return (read a :: Float, read b :: Float)
